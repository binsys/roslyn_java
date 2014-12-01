// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.Internal.Log;
using Microsoft.CodeAnalysis.Options;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.WorkspaceServices;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{
    /// <summary>
    /// A workspace provides access to a active set of source code projects and documents and their
    /// associated syntax trees, compilations and semantic models. A workspace has a current solution
    /// that is an immutable snapshot of the projects and documents. This property may change over time 
    /// as the workspace is updated either from live interactions in the environment or via call to the
    /// workspace's ApplyChanges method.
    /// </summary>
    public abstract partial class Workspace : IDisposable
    {
        private readonly IWorkspaceServiceProvider workspaceServicesProvider;
        private readonly BranchId primaryBranchId;

        // forces serialization of mutation calls from host (OnXXX methods). Must take this lock before taking stateLock.
        private readonly NonReentrantLock serializationLock = new NonReentrantLock();

        // this lock guards all the mutable fields (do not share lock with derived classes)
        private readonly ReaderWriterLockSlim stateLock = new ReaderWriterLockSlim(LockRecursionPolicy.NoRecursion);
        private Solution latestSolution;

        private readonly IWorkspaceTaskScheduler taskQueue;

        // test hooks.
        internal static bool TestHookStandaloneProjectsDoNotHoldReferences = false;

        internal bool TestHookPartialSolutionsDisabled { get; set; }
        
        internal Workspace(IWorkspaceServiceProvider workspaceServicesProvider)
        {
            this.primaryBranchId = BranchId.GetNextId();

            this.workspaceServicesProvider = workspaceServicesProvider;

            // queue used for sending events
            var workspaceTaskSchedulerFactory = workspaceServicesProvider.GetService<IWorkspaceTaskSchedulerFactory>();
            this.taskQueue = workspaceTaskSchedulerFactory.CreateTaskQueue();

            // initialize with empty solution
            this.latestSolution = CreateSolution(SolutionId.CreateNewId());
        }

        /// <summary>
        /// primary branch id that current solution has
        /// </summary>
        internal BranchId PrimaryBranchId
        {
            get { return this.primaryBranchId; }
        }

        /// <summary>
        /// Override this property if the workspace supports partial semantics for documents.
        /// </summary>
        protected internal virtual bool SupportsPartialSemantics
        {
            get { return false; }
        }

        /// <summary>
        /// The kind of the workspace. This is generally WorkspaceKind.Host if originating from the host environment, but may be 
        /// any other name use for a specific kind or workspace.
        /// </summary>
        public string Kind
        {
            get { return workspaceServicesProvider.Kind; }
        }

        /// <summary>
        /// Create a new empty solution instance associated with this workspace.
        /// </summary>
        protected internal Solution CreateSolution(SolutionInfo solutionInfo)
        {
            return new Solution(this, this.workspaceServicesProvider, solutionInfo);
        }

        /// <summary>
        /// Create a new empty solution instance associated with this workspace.
        /// </summary>
        protected internal Solution CreateSolution(SolutionId id)
        {
            return CreateSolution(SolutionInfo.Create(id, VersionStamp.Create()));
        }

        internal IWorkspaceServiceProvider GetWorkspaceServicesInternal()
        {
            return this.workspaceServicesProvider;
        }

        /// <summary>
        /// The current solution. 
        /// 
        /// The solution is an immutable model of the current set of projects and source documents.
        /// It provides access to source text, syntax trees and semantics.
        /// 
        /// This property may change as the workspace reacts to changes in the environment or 
        /// after ApplyChanges is called.
        /// </summary>
        public virtual Solution CurrentSolution
        {
            get
            {
                using (this.stateLock.DisposableRead())
                {
                    return this.latestSolution;
                }
            }
        }

        /// <summary>
        /// Set's the current solution of this workspace. This method does not raise a workspace change event.
        /// </summary>
        protected Solution SetCurrentSolution(Solution solution)
        {
            using (this.stateLock.DisposableWrite())
            {
                Solution curSol = this.latestSolution;

                Solution sol = solution;
                solution = sol.WithNewWorkspace(this, curSol.WorkspaceVersion + 1);

                this.latestSolution = solution;
            }

            return solution;
        }

        /// <summary>
        /// Get's the set of all global options.
        /// </summary>
        public OptionSet GetOptions()
        {
            return this.workspaceServicesProvider.GetService<IOptionService>().GetOptions();
        }

        /// <summary>
        /// Executes an action as a background task, as part of a sequential queue of tasks.
        /// </summary>
        protected internal Task ScheduleTask(Action action, string taskName = "Workspace.Task")
        {
            return taskQueue.ScheduleTask(action, taskName);
        }

        /// <summary>
        /// Execute a function as a background task, as part of a sequential queue of tasks.
        /// </summary>
        protected internal Task<T> ScheduleTask<T>(Func<T> func, string taskName = "Workspace.Task")
        {
            return taskQueue.ScheduleTask(func, taskName);
        }

        /// <summary>
        /// Override this method to act immediately when the text of a document has changed, as opposed
        /// to waiting for the corresponding workspace changed event to fire asynchronously.
        /// </summary>
        protected virtual void OnDocumentTextChanged(Document document)
        {
        }

        /// <summary>
        /// Override this method to act immediately when a document is closing, as opposed
        /// to waiting for the corresponding workspace changed event to fire asynchronously.
        /// </summary>
        protected virtual void OnDocumentClosing(DocumentId documentId)
        {
        }

        /// <summary>
        /// Clears all solution data and empties the current solution.
        /// </summary>
        protected void ClearSolution()
        {
            var oldSolution = this.CurrentSolution;
            this.ClearSolutionData();

            this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.SolutionCleared, oldSolution, this.CurrentSolution);
        }

        /// <summary>
        /// This method is called when a solution is cleared.
        /// 
        /// Override this method if you want to do additional work when a solution is cleared. 
        /// Call the base method at the end of your method.
        /// </summary>
        protected virtual void ClearSolutionData()
        {
            // clear any open documents
            this.ClearOpenDocuments();

            this.SetCurrentSolution(this.CreateSolution(this.CurrentSolution.Id));
        }

        /// <summary>
        /// This method is called when an individual project is removed.
        /// 
        /// Override this method if you want to do additional work when a project is removed.
        /// Call the base method at the end of your method.
        /// </summary>
        protected virtual void ClearProjectData(ProjectId projectId)
        {
            this.ClearOpenDocuments(projectId);
        }

        /// <summary>
        /// This method is called to clear an individual document is removed.
        /// 
        /// Override this method if you want to do additional work when a document is removed.
        /// Call the base method at the end of your method.
        /// </summary>
        protected virtual void ClearDocumentData(DocumentId documentId)
        {
            this.ClearOpenDocument(documentId);
        }

        /// <summary>
        /// This method is called during ApplyChanges to add a project reference to a project.
        /// 
        /// Override this method to implement the capability of adding project references.
        /// </summary>
        protected virtual void AddProjectReference(ProjectId projectId, ProjectReference projectReference)
        {
            throw new NotSupportedException();
        }

        /// <summary>
        /// This method is called during ApplyChanges to remove a project reference from a project.
        /// 
        /// Override this method to implement the capability of removing project references.
        /// </summary>
        protected virtual void RemoveProjectReference(ProjectId projectId, ProjectReference projectReference)
        {
            throw new NotSupportedException();
        }

        /// <summary>
        /// This method is called during ApplyChanges to add a metadata reference to a project.
        /// 
        /// Override this method to implement the capability of adding metadata references.
        /// </summary>
        protected virtual void AddMetadataReference(ProjectId projectId, MetadataReference metadataReference)
        {
            throw new NotSupportedException();
        }

        /// <summary>
        /// This method is called during ApplyChanges to remove a metadata reference from a project.
        /// 
        /// Override this method to implement the capability of removing metadata references.
        /// </summary>
        protected virtual void RemoveMetadataReference(ProjectId projectId, MetadataReference metadataReference)
        {
            throw new NotSupportedException();
        }

        /// <summary>
        /// This method is called during ApplyChanges to add a new document to a project.
        /// 
        /// Override this method to implement the capability of adding documents.
        /// </summary>
        protected virtual void AddDocument(DocumentId documentId, IEnumerable<string> folders, string name, SourceText text = null, SourceCodeKind sourceCodeKind = SourceCodeKind.Regular)
        {
            throw new NotSupportedException();
        }

        /// <summary>
        /// This method is called during ApplyChanges to remove a document from a project.
        /// 
        /// Override this method to implement the capability of removing documents.
        /// </summary>
        protected virtual void RemoveDocument(DocumentId documentId)
        {
            throw new NotSupportedException();
        }

        /// <summary>
        /// This method is called to change the text of a document.
        /// 
        /// Override this method to implement the capability of changing document text.
        /// </summary>
        protected virtual void ChangedDocumentText(DocumentId id, SourceText text)
        {
            throw new NotSupportedException();
        }

        /// <summary>
        /// Disposes this workspace. The workspace can longer be used after it is disposed.
        /// </summary>
        public void Dispose()
        {
            this.Dispose(finalize: false);
        }

        /// <summary>
        /// Call this method when the workspace is disposed.
        /// 
        /// Override this method to do addition work when the workspace is disposed.
        /// Call this method at the end of your method.
        /// </summary>
        protected virtual void Dispose(bool finalize)
        {
        }

        /// <summary>
        /// Call this method to respond to a solution being opened in the host environment.
        /// </summary>
        protected internal void OnSolutionAdded(SolutionInfo solutionInfo)
        {
            var oldSolution = this.CurrentSolution;
            var solutionId = solutionInfo.Id;

            using (this.serializationLock.DisposableWait())
            {
                CheckSolutionIsEmpty();
                this.SetCurrentSolution(this.CreateSolution(solutionInfo));
            }

            solutionInfo.Projects.Do(p => OnProjectAdded(p, silent: true));

            var newSolution = this.CurrentSolution;

            this.RegisterSolution(solutionInfo);

            this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.SolutionAdded, oldSolution, newSolution);
        }

        /// <summary>
        /// Call this method to respond to a solution being reloaded in the host environment.
        /// </summary>
        protected internal void OnSolutionReloaded(SolutionInfo reloadedSolutionInfo)
        {
            var oldSolution = this.CurrentSolution;

            Solution newSolution;

            using (this.serializationLock.DisposableWait())
            {
                newSolution = this.CreateSolution(reloadedSolutionInfo);
                newSolution = this.SetCurrentSolution(newSolution);

                reloadedSolutionInfo.Projects.Do(pi => OnProjectAdded_NoLock(pi, silent: true));

                newSolution = this.AdjustReloadedSolution(oldSolution, this.latestSolution);
                newSolution = this.SetCurrentSolution(newSolution);

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.SolutionReloaded, oldSolution, newSolution);
            }
        }

        /// <summary>
        /// This method is called when the solution is removed from the workspace.
        /// 
        /// Override this method if you want to do additional work when the solution is removed. 
        /// Call the base method at the end of your method.
        /// Call this method to respond to a solution being removed/cleared/closed in the host environment.
        /// </summary>
        protected internal void OnSolutionRemoved()
        {
            using (this.serializationLock.DisposableWait())
            {
                var oldSolution = this.CurrentSolution;

                this.ClearSolutionData();

                // reset to new empty solution
                this.SetCurrentSolution(this.CreateSolution(SolutionId.CreateNewId()));

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.SolutionRemoved, oldSolution, this.CurrentSolution);

                // At this point, no one will need to pull any more items out of the old solution,
                // so clear all objects out of the caches. but this has to run after the workspace event
                // since the workspace event holds onto old solution which makes most of items in the cache alive
                // also, here we don't clear text cache since doing that right after clearing out other caches
                // make us to dump evicted texts unnecessarily to MMF. instead, we will let the text to be eventually
                // replaced with new texts and those texts belong to old solution will be removed without being written to MMF
                this.ScheduleTask(() =>
                {
                    this.workspaceServicesProvider.GetService<ICompilationCacheService>().Clear();
                    this.workspaceServicesProvider.GetService<ISyntaxTreeCacheService>().Clear();
                }, "Workspace.ClearCache");
            }
        }

        /// <summary>
        /// Call this method to respond to a project being added/opened in the host environment.
        /// </summary>
        protected internal void OnProjectAdded(ProjectInfo projectInfo)
        {
            this.OnProjectAdded(projectInfo, silent: false);
        }

        private void OnProjectAdded(ProjectInfo projectInfo, bool silent)
        {
            using (this.serializationLock.DisposableWait())
            {
                this.OnProjectAdded_NoLock(projectInfo, silent);
            }
        }

        private void OnProjectAdded_NoLock(ProjectInfo projectInfo, bool silent)
        {
            var projectId = projectInfo.Id;

            CheckProjectIsNotInCurrentSolution(projectId);

            var oldSolution = this.CurrentSolution;
            var newSolution = this.SetCurrentSolution(oldSolution.AddProject(projectInfo));
            this.RegisterProjectAndDocuments(projectInfo);

            if (!silent)
            {
                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.ProjectAdded, oldSolution, newSolution, projectId);
            }

            // NOTE: don't add documents and references directly, they are deferred.
        }

        /// <summary>
        /// Call this method to respond to a project being reloaded in the host environment.
        /// </summary>
        protected internal virtual void OnProjectReloaded(ProjectInfo reloadedProjectInfo)
        {
            using (this.serializationLock.DisposableWait())
            {
                var projectId = reloadedProjectInfo.Id;

                CheckProjectIsInCurrentSolution(projectId);

                var oldSolution = this.CurrentSolution;
                var newSolution = oldSolution.RemoveProject(projectId).AddProject(reloadedProjectInfo);

                // register all the documents & project
                this.RegisterProjectAndDocuments(reloadedProjectInfo);

                newSolution = this.AdjustReloadedProject(oldSolution.GetProject(projectId), newSolution.GetProject(projectId)).Solution;
                newSolution = this.SetCurrentSolution(newSolution);

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.ProjectReloaded, oldSolution, newSolution, projectId);
            }
        }

        private void RegisterProjectAndDocuments(ProjectInfo projectInfo)
        {
            foreach (var doc in projectInfo.Documents)
            {
                this.RegisterDocument(doc);
            }

            this.RegisterProject(projectInfo);
        }

        /// <summary>
        /// This method is called when a document is first observed.
        /// </summary>
        protected virtual void RegisterDocument(DocumentInfo documentInfo)
        {
        }

        /// <summary>
        /// This method is called when a project if first observed, either when it is first added or accessed.
        /// </summary>
        protected virtual void RegisterProject(ProjectInfo projectInfo)
        {
        }

        /// <summary>
        /// This method is called when a solution is first observed.
        /// </summary>
        /// <param name="solutionInfo"></param>
        protected virtual void RegisterSolution(SolutionInfo solutionInfo)
        {
        }

        /// <summary>
        /// Call this method to respond to a project being removed from the host environment.
        /// </summary>
        protected internal virtual void OnProjectRemoved(ProjectId projectId)
        {
            using (this.serializationLock.DisposableWait())
            {
                CheckProjectIsInCurrentSolution(projectId);
                this.CheckProjectCanBeRemoved(projectId);

                var oldSolution = this.CurrentSolution;

                this.ClearProjectData(projectId);
                var newSolution = this.SetCurrentSolution(oldSolution.RemoveProject(projectId));

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.ProjectRemoved, oldSolution, newSolution, projectId);
            }
        }

        protected virtual void CheckProjectCanBeRemoved(ProjectId projectId)
        {
            CheckProjectDoesNotContainOpenDocuments(projectId);
        }

        /// <summary>
        /// Call this method when a project's assembly name is changed in the host environment.
        /// </summary>
        protected internal void OnAssemblyNameChanged(ProjectId projectId, string assemblyName)
        {
            using (this.serializationLock.DisposableWait())
            {
                CheckProjectIsInCurrentSolution(projectId);

                var oldSolution = this.CurrentSolution;
                var newSolution = this.SetCurrentSolution(oldSolution.WithProjectAssemblyName(projectId, assemblyName));

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.ProjectChanged, oldSolution, newSolution, projectId);
            }
        }

        /// <summary>
        /// Call this method when a project's output file path is changed in the host environment.
        /// </summary>
        protected internal void OnOutputFilePathChanged(ProjectId projectId, string outputFilePath)
        {
            using (this.serializationLock.DisposableWait())
            {
                CheckProjectIsInCurrentSolution(projectId);

                var oldSolution = this.CurrentSolution;
                var newSolution = this.SetCurrentSolution(oldSolution.WithProjectOutputFilePath(projectId, outputFilePath));

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.ProjectChanged, oldSolution, newSolution, projectId);
            }
        }

        /// <summary>
        /// Call this method when a project's compilation options are changed in the host environment.
        /// </summary>
        protected internal void OnCompilationOptionsChanged(ProjectId projectId, CompilationOptions options)
        {
            using (this.serializationLock.DisposableWait())
            {
                CheckProjectIsInCurrentSolution(projectId);

                var oldSolution = this.CurrentSolution;
                var newSolution = this.SetCurrentSolution(oldSolution.WithProjectCompilationOptions(projectId, options));

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.ProjectChanged, oldSolution, newSolution, projectId);
            }
        }

        /// <summary>
        /// Call this method when a project's parse options are changed in the host environment.
        /// </summary>
        protected internal void OnParseOptionsChanged(ProjectId projectId, ParseOptions options)
        {
            using (this.serializationLock.DisposableWait())
            {
                CheckProjectIsInCurrentSolution(projectId);

                var oldSolution = this.CurrentSolution;
                var newSolution = this.SetCurrentSolution(oldSolution.WithProjectParseOptions(projectId, options));

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.ProjectChanged, oldSolution, newSolution, projectId);
            }
        }

        /// <summary>
        /// Call this method when a project reference is added to a project in the host environment.
        /// </summary>
        protected internal void OnProjectReferenceAdded(ProjectId projectId, ProjectReference projectReference)
        {
            using (this.serializationLock.DisposableWait())
            {
                CheckProjectIsInCurrentSolution(projectId);
                CheckProjectIsInCurrentSolution(projectReference.ProjectId);
                CheckProjectDoesNotHaveProjectReference(projectId, projectReference);

                // Can only add this P2P reference if it would not cause a circularity.
                CheckProjectDoesNotHaveTransitiveProjectReference(projectId, projectReference.ProjectId);

                var oldSolution = this.CurrentSolution;
                var newSolution = this.SetCurrentSolution(oldSolution.AddProjectReference(projectId, projectReference));

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.ProjectChanged, oldSolution, newSolution, projectId);
            }
        }

        /// <summary>
        /// Call this method when a project reference is removed from a project in the host environment.
        /// </summary>
        protected internal void OnProjectReferenceRemoved(ProjectId projectId, ProjectReference projectReference)
        {
            using (this.serializationLock.DisposableWait())
            {
                CheckProjectIsInCurrentSolution(projectId);
                CheckProjectIsInCurrentSolution(projectReference.ProjectId);
                CheckProjectHasProjectReference(projectId, projectReference);

                var oldSolution = this.CurrentSolution;
                var newSolution = this.SetCurrentSolution(oldSolution.RemoveProjectReference(projectId, projectReference));

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.ProjectChanged, oldSolution, newSolution, projectId);
            }
        }

        /// <summary>
        /// Call this method when a metadata reference is added to a project in the host environment.
        /// </summary>
        protected internal void OnMetadataReferenceAdded(ProjectId projectId, MetadataReference metadataReference)
        {
            using (this.serializationLock.DisposableWait())
            {
                CheckProjectIsInCurrentSolution(projectId);
                CheckProjectDoesNotHaveMetadataReference(projectId, metadataReference);

                var oldSolution = this.CurrentSolution;
                var newSolution = this.SetCurrentSolution(oldSolution.AddMetadataReference(projectId, metadataReference));

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.ProjectChanged, oldSolution, newSolution, projectId);
            }
        }

        /// <summary>
        /// Call this method when a metadata reference is removed from a project in the host environment.
        /// </summary>
        protected internal void OnMetadataReferenceRemoved(ProjectId projectId, MetadataReference metadataReference)
        {
            using (this.serializationLock.DisposableWait())
            {
                CheckProjectIsInCurrentSolution(projectId);
                CheckProjectHasMetadataReference(projectId, metadataReference);

                var oldSolution = this.CurrentSolution;
                var newSolution = this.SetCurrentSolution(oldSolution.RemoveMetadataReference(projectId, metadataReference));

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.ProjectChanged, oldSolution, newSolution, projectId);
            }
        }

        /// <summary>
        /// Call this method when a document is added to a project in the host environment.
        /// </summary>
        protected internal void OnDocumentAdded(DocumentInfo documentInfo)
        {
            using (this.serializationLock.DisposableWait())
            {
                var documentId = documentInfo.Id;

                CheckProjectIsInCurrentSolution(documentId.ProjectId);
                CheckDocumentIsNotInCurrentSolution(documentId);

                var oldSolution = this.CurrentSolution;

                RegisterDocument(documentInfo);

                var newSolution = this.SetCurrentSolution(oldSolution.AddDocument(documentInfo));

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.DocumentAdded, oldSolution, newSolution, documentId: documentId);
            }
        }

        /// <summary>
        /// Call this method when a document is reloaded in the host environment.
        /// </summary>
        protected internal void OnDocumentReloaded(DocumentInfo newDocumentInfo)
        {
            using (this.serializationLock.DisposableWait())
            {
                var documentId = newDocumentInfo.Id;

                CheckProjectIsInCurrentSolution(documentId.ProjectId);
                CheckDocumentIsInCurrentSolution(documentId);

                var oldSolution = this.CurrentSolution;

                RegisterDocument(newDocumentInfo);

                var newSolution = this.SetCurrentSolution(oldSolution.RemoveDocument(documentId).AddDocument(newDocumentInfo));

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.DocumentReloaded, oldSolution, newSolution, documentId: documentId);
            }
        }

        /// <summary>
        /// Call this method when a document is removed from a project in the host environment.
        /// </summary>
        protected internal void OnDocumentRemoved(DocumentId documentId)
        {
            using (this.serializationLock.DisposableWait())
            {
                CheckDocumentIsInCurrentSolution(documentId);

                this.CheckDocumentCanBeRemoved(documentId);

                var oldSolution = this.CurrentSolution;

                this.ClearDocumentData(documentId);

                var newSolution = this.SetCurrentSolution(oldSolution.RemoveDocument(documentId));

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.DocumentRemoved, oldSolution, newSolution, documentId: documentId);
            }
        }

        protected virtual void CheckDocumentCanBeRemoved(DocumentId documentId)
        {
            CheckDocumentIsClosed(documentId);
        }

        /// <summary>
        /// Call this method when the text of a document is changed on disk.
        /// </summary>
        protected internal void OnDocumentTextLoaderChanged(DocumentId documentId, TextLoader loader)
        {
            using (this.serializationLock.DisposableWait())
            {
                CheckDocumentIsInCurrentSolution(documentId);

                var oldSolution = this.CurrentSolution;

                var newSolution = oldSolution.WithDocumentTextLoader(documentId, loader, PreservationMode.PreserveValue);
                newSolution = this.SetCurrentSolution(newSolution);

                var newDocument = newSolution.GetDocument(documentId);
                this.OnDocumentTextChanged(newDocument);

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.DocumentChanged, oldSolution, newSolution, documentId: documentId);
            }
        }

        /// <summary>
        /// Call this method when the text of a document is updated in the host environment.
        /// </summary>
        protected internal void OnDocumentTextChanged(DocumentId documentId, SourceText newText, PreservationMode mode)
        {
            using (this.serializationLock.DisposableWait())
            {
                CheckDocumentIsInCurrentSolution(documentId);

                var oldSolution = this.CurrentSolution;
                var newSolution = this.SetCurrentSolution(oldSolution.WithDocumentText(documentId, newText, mode));

                var newDocument = newSolution.GetDocument(documentId);
                this.OnDocumentTextChanged(newDocument);

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.DocumentChanged, oldSolution, newSolution, documentId: documentId);
            }
        }

        /// <summary>
        /// Call this method when the SourceCodeKind of a document changes in the host environment.
        /// </summary>
        protected internal void OnDocumentSourceCodeKindChanged(DocumentId documentId, SourceCodeKind sourceCodeKind)
        {
            using (this.serializationLock.DisposableWait())
            {
                CheckDocumentIsInCurrentSolution(documentId);

                var oldSolution = this.CurrentSolution;
                var newSolution = this.SetCurrentSolution(oldSolution.WithDocumentSourceCodeKind(documentId, sourceCodeKind));

                var newDocument = newSolution.GetDocument(documentId);
                this.OnDocumentTextChanged(newDocument);

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.DocumentChanged, oldSolution, newSolution, documentId: documentId);
            }
        }

        /// <summary>
        /// Determines if the specific kind of change is supported by the ApplyChanges method.
        /// </summary>
        public virtual bool CanApplyChange(ApplyChangesKind feature)
        {
            return false;
        }

        /// <summary>
        /// Apply changes made to a solution back to the workspace.
        /// 
        /// The specified solution must be one that originated from this workspace. If it is not, or the workspace
        /// has been updated since the solution was obtained from the workspace, then this method returns false.
        /// </summary>
        public virtual bool TryApplyChanges(Solution newSolution)
        {
            using (Logger.LogBlock(FeatureId.Workspace, FunctionId.Workspace_ApplyChanges, "ApplyChanges", CancellationToken.None))
            {
                Solution oldSolution;

                // make sure that newSolution is a branch of the current solution
                using (this.stateLock.DisposableRead())
                {
                    var curSol = this.latestSolution;
                    var newSol = newSolution;

                    // if solution did not original from this workspace then fail
                    if (newSolution.Workspace != this)
                    {
                        return false;
                    }

                    // if the workspace has already accepted an update, then fail
                    if (newSol.WorkspaceVersion != curSol.WorkspaceVersion)
                    {
                        return false;
                    }

                    oldSolution = this.latestSolution;
                }

                // the given solution must be a branched one.
                // otherwise, there should be no change to apply.
                if (oldSolution.BranchId == newSolution.BranchId)
                {
                    CheckNoChanges(oldSolution, newSolution);
                    return true;
                }

                var solutionChanges = newSolution.GetChanges(oldSolution);

                if (solutionChanges.GetRemovedProjects().Any() && !this.CanApplyChange(ApplyChangesKind.RemoveProject))
                {
                    throw new NotSupportedException(WorkspacesResources.RemovingProjectsNotSupported);
                }

                if (solutionChanges.GetAddedProjects().Any() && !this.CanApplyChange(ApplyChangesKind.AddProject))
                {
                    throw new NotSupportedException(WorkspacesResources.AddingProjectsNotSupported);
                }

                // process all project changes
                foreach (var projectChanges in solutionChanges.GetProjectChanges())
                {
                    this.ApplyProjectChanges(projectChanges);
                }

                return true;
            }
        }

        /// <summary>
        /// This method is called during ApplyChanges for each project that has been added, removed or changed.
        /// 
        /// Override this method if you want to modify how project changes are applied.
        /// </summary>
        protected virtual void ApplyProjectChanges(ProjectChanges projectChanges)
        {
            // removed project references
            foreach (var removedProjectReference in projectChanges.GetRemovedProjectReferences())
            {
                this.RemoveProjectReference(projectChanges.ProjectId, removedProjectReference);
            }

            // added project references
            foreach (var addedProjectReference in projectChanges.GetAddedProjectReferences())
            {
                this.AddProjectReference(projectChanges.ProjectId, addedProjectReference);
            }

            // removed metadata references
            foreach (var metadata in projectChanges.GetRemovedMetadataReferences())
            {
                this.RemoveMetadataReference(projectChanges.ProjectId, metadata);
            }

            // added metadata references
            foreach (var metadata in projectChanges.GetAddedMetadataReferences())
            {
                this.AddMetadataReference(projectChanges.ProjectId, metadata);
            }

            // removed documents
            foreach (var documentId in projectChanges.GetRemovedDocuments())
            {
                this.RemoveDocument(documentId);
            }

            // added documents
            foreach (var documentId in projectChanges.GetAddedDocuments())
            {
                var doc = projectChanges.NewProject.GetDocument(documentId);
                var text = doc.GetTextAsync(CancellationToken.None).WaitAndGetResult(CancellationToken.None); // needs wait
                this.AddDocument(documentId, doc.Folders, doc.Name, text, doc.SourceCodeKind);
            }

            // changed documents
            foreach (var documentId in projectChanges.GetChangedDocuments())
            {
                var oldDoc = projectChanges.OldProject.GetDocument(documentId);
                var newDoc = projectChanges.NewProject.GetDocument(documentId);

                // see whether we can get oldText
                SourceText oldText;
                if (!oldDoc.TryGetText(out oldText))
                {
                    // we can't get old text, there is not much we can do except replacing whole text.
                    var currentText = newDoc.GetTextAsync(CancellationToken.None).WaitAndGetResult(CancellationToken.None); // needs wait
                    this.ChangedDocumentText(documentId, currentText);
                    continue;
                }

                // see whether we can get new text
                SourceText newText;
                if (!newDoc.TryGetText(out newText))
                {
                    // okay, we have old text, but no new text. let document to determine text changes
                    var textChanges = newDoc.GetTextChangesAsync(oldDoc, CancellationToken.None).WaitAndGetResult(CancellationToken.None); // needs wait
                    this.ChangedDocumentText(documentId, oldText.WithChanges(textChanges));
                    continue;
                }

                // we have both old and new text, just update using the new text.
                this.ChangedDocumentText(documentId, newText);
            }
        }

        [Conditional("DEBUG")]
        private void CheckNoChanges(Solution oldSolution, Solution newSolution)
        {
            var changes = newSolution.GetChanges(oldSolution);
            Contract.ThrowIfTrue(changes.GetAddedProjects().Any());
            Contract.ThrowIfTrue(changes.GetRemovedProjects().Any());
            Contract.ThrowIfTrue(changes.GetProjectChanges().Any());
        }

        #region Checks and Asserts
        /// <summary>
        /// Throws an exception is the solution is not empty.
        /// </summary>
        protected void CheckSolutionIsEmpty()
        {
            if (this.CurrentSolution.ProjectIds.Any())
            {
                throw new ArgumentException(WorkspacesResources.WorkspaceIsNotEmpty);
            }
        }

        /// <summary>
        /// Throws an exception if the project is not part of the current solution.
        /// </summary>
        protected void CheckProjectIsInCurrentSolution(ProjectId projectId)
        {
            if (!this.CurrentSolution.ContainsProject(projectId))
            {
                throw new ArgumentException(string.Format(
                    WorkspacesResources.ProjectOrDocumentNotInWorkspace,
                    this.GetProjectName(projectId)));
            }
        }

        /// <summary>
        /// Throws an exception is the project is part of the current solution.
        /// </summary>
        protected void CheckProjectIsNotInCurrentSolution(ProjectId projectId)
        {
            if (this.CurrentSolution.ContainsProject(projectId))
            {
                throw new ArgumentException(string.Format(
                    WorkspacesResources.ProjectOrDocumentAlreadyInWorkspace,
                    this.GetProjectName(projectId)));
            }
        }

        /// <summary>
        /// Throws an exception if a project does not have a specific project reference.
        /// </summary>
        protected void CheckProjectHasProjectReference(ProjectId fromProjectId, ProjectReference projectReference)
        {
            if (!this.CurrentSolution.GetProject(fromProjectId).ProjectReferences.Contains(projectReference))
            {
                throw new ArgumentException(string.Format(
                    WorkspacesResources.ProjectNotReferenced,
                    this.GetProjectName(projectReference.ProjectId)));
            }
        }

        /// <summary>
        /// Throws an exception if a project already has a specific project reference.
        /// </summary>
        protected void CheckProjectDoesNotHaveProjectReference(ProjectId fromProjectId, ProjectReference projectReference)
        {
            if (this.CurrentSolution.GetProject(fromProjectId).ProjectReferences.Contains(projectReference))
            {
                throw new ArgumentException(string.Format(
                    WorkspacesResources.ProjectAlreadyReferenced,
                    this.GetProjectName(projectReference.ProjectId)));
            }
        }

        /// <summary>
        /// Throws an exception if project has a transitive reference to another project.
        /// </summary>
        protected void CheckProjectDoesNotHaveTransitiveProjectReference(ProjectId fromProjectId, ProjectId toProjectId)
        {
            var transitiveReferences = GetTransitiveProjectReferences(toProjectId);
            if (transitiveReferences.Contains(fromProjectId))
            {
                throw new ArgumentException(string.Format(
                    WorkspacesResources.CausesCircularProjectReference,
                    this.GetProjectName(fromProjectId), this.GetProjectName(toProjectId)));
            }
        }

        private ISet<ProjectId> GetTransitiveProjectReferences(ProjectId project, ISet<ProjectId> projects = null)
        {
            projects = projects ?? new HashSet<ProjectId>();
            if (projects.Add(project))
            {
                this.CurrentSolution.GetProject(project).ProjectReferences.Do(p => GetTransitiveProjectReferences(p.ProjectId, projects));
            }

            return projects;
        }

        /// <summary>
        /// Throws an exception if a project does not have a specific metadata reference.
        /// </summary>
        protected void CheckProjectHasMetadataReference(ProjectId projectId, MetadataReference metadataReference)
        {
            if (!this.CurrentSolution.GetProject(projectId).MetadataReferences.Contains(metadataReference))
            {
                throw new ArgumentException(WorkspacesResources.MetadataIsNotReferenced);
            }
        }

        /// <summary>
        /// Throws an exception if a project already has a specific metadata reference.
        /// </summary>
        protected void CheckProjectDoesNotHaveMetadataReference(ProjectId projectId, MetadataReference metadataReference)
        {
            if (this.CurrentSolution.GetProject(projectId).MetadataReferences.Contains(metadataReference))
            {
                throw new ArgumentException(WorkspacesResources.MetadataIsAlreadyReferenced);
            }
        }

        /// <summary>
        /// Throws an exception if a document is not part of the current solution.
        /// </summary>
        protected void CheckDocumentIsInCurrentSolution(DocumentId documentId)
        {
            if (this.CurrentSolution.GetDocument(documentId) == null)
            {
                throw new ArgumentException(string.Format(
                    WorkspacesResources.ProjectOrDocumentNotInWorkspace,
                    this.GetDocumentName(documentId)));
            }
        }

        /// <summary>
        /// Throws an exception if a document is already part of the current solution.
        /// </summary>
        protected void CheckDocumentIsNotInCurrentSolution(DocumentId documentId)
        {
            if (this.CurrentSolution.ContainsDocument(documentId))
            {
                throw new ArgumentException(string.Format(
                    WorkspacesResources.ProjectOrDocumentAlreadyInWorkspace,
                    this.GetDocumentName(documentId)));
            }
        }

        /// <summary>
        /// Gets the name to use for a project in an error message.
        /// </summary>
        protected virtual string GetProjectName(ProjectId projectId)
        {
            var project = this.CurrentSolution.GetProject(projectId);
            var name = project != null ? project.Name : "<Project" + projectId.Id + ">";
            return name;
        }

        /// <summary>
        /// Gets the name to use for a document in an error message.
        /// </summary>
        protected virtual string GetDocumentName(DocumentId documentId)
        {
            var document = this.CurrentSolution.GetDocument(documentId);
            var name = document != null ? document.Name : "<Document" + documentId.Id + ">";
            return name;
        }
        #endregion
    }
}