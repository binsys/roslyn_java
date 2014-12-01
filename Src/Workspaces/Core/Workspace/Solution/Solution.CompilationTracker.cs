﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.Internal.Log;
using Microsoft.CodeAnalysis.LanguageServices;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{
    public partial class Solution
    {
        /// <summary>
        /// Tracks the changes made to a project and provides the facility to get a lazily built
        /// compilation for that project.  As the compilation is being built, the partial results are
        /// stored as well so that they can be used in the 'in progress' workspace snapshot.
        /// </summary>
        private partial class CompilationTracker
        {
            private static readonly Func<ProjectState, string> logBuildCompilationAsync = LogBuildCompilationAsync;

            public ProjectState ProjectState { get; private set; }

            /// <summary>
            /// Access via the <see cref="ReadState"/> and <see cref="WriteState"/> methods.
            /// </summary>
            private State stateDoNotAccessDirectly;

            // guarantees only one thread is building at a time
            private readonly AsyncSemaphore buildLock = new AsyncSemaphore(initialCount: 1);

            private CompilationTracker(
                ProjectState project,
                State state)
            {
                Contract.ThrowIfNull(project);

                this.ProjectState = project;
                this.stateDoNotAccessDirectly = state;
            }

            /// <summary>
            /// Creates a tracker for the provided project.  The tracker will be in the 'empty' state
            /// and will have no extra information beyond the project itself.
            /// </summary>
            public CompilationTracker(ProjectState project)
                : this(project, State.Empty)
            {
            }

            private State ReadState()
            {
                return Volatile.Read(ref this.stateDoNotAccessDirectly);
            }

            private void WriteState(State state)
            {
                Volatile.Write(ref this.stateDoNotAccessDirectly, state);
            }

            /// <summary>
            /// Returns true if this tracker currently either points to a compilation, has an in-progress
            /// compilation being computed, or has a skeleton reference.  Note: this is simply a weak
            /// statement about the tracker at this exact moment in time.  Immediately after this returns
            /// the tracker might change and may no longer have a final compilation (for example, if the
            /// retainer let go of it) or might not have an in-progress compilation (for example, if the
            /// background compiler finished with it).
            /// 
            /// Because of the above limitations, this should only be used by clients as a weak form of
            /// information about the tracker.  For example, a client may see that a tracker has no
            /// compilation and may choose to throw it away knowing that it could be reconstructed at a
            /// later point if necessary.
            /// </summary>
            public bool HasCompilation
            {
                get
                {
                    var state = this.ReadState();
                    return state.Compilation.HasValue || state.DeclarationOnlyCompilation != null;
                }
            }

            /// <summary>
            /// Creates a new instance of the compilation info, retaining any already built
            /// compilation state as the now 'old' state
            /// </summary>
            public CompilationTracker Fork(
                ProjectState newProject,
                CompilationTranslationAction translate = null,
                CancellationToken cancellationToken = default(CancellationToken),
                bool clone = false)
            {
                var state = this.ReadState();

                ValueSource<Compilation> baseCompilationSource = state.Compilation;
                var baseCompilation = baseCompilationSource.GetValue(cancellationToken);
                if (baseCompilation != null)
                {
                    // We have some pre-calculated state to incrementally update
                    var newInProgressCompilationSource = clone
                        ? new WeakConstantValueSource<Compilation>(baseCompilation.Clone())
                        : baseCompilationSource;

                    var intermediateProjects = state is InProgressState
                        ? ((InProgressState)state).IntermediateProjects
                        : ImmutableList.Create<ValueTuple<ProjectState, CompilationTranslationAction>>();

                    var newIntermediateProjects = translate == null
                         ? intermediateProjects
                         : intermediateProjects.Add(ValueTuple.Create(this.ProjectState, translate));

                    var newState = State.Create(newInProgressCompilationSource, newIntermediateProjects);

                    return new CompilationTracker(newProject, newState);
                }

                var declarationOnlyCompilation = state.DeclarationOnlyCompilation;
                if (declarationOnlyCompilation != null)
                {
                    if (translate != null)
                    {
                        var compilationSource = clone
                            ? (ValueSource<Compilation>)new WeakConstantValueSource<Compilation>(declarationOnlyCompilation)
                            : (ValueSource<Compilation>)new ConstantValueSource<Compilation>(declarationOnlyCompilation);

                        var intermediateProjects =
                            ImmutableList.Create<ValueTuple<ProjectState, CompilationTranslationAction>>(ValueTuple.Create(this.ProjectState, translate));

                        return new CompilationTracker(newProject, new InProgressState(compilationSource, intermediateProjects));
                    }

                    return new CompilationTracker(newProject, new LightDeclarationState(declarationOnlyCompilation));
                }

                // We have nothing.  Just make a tracker that only points to the new project.  We'll have
                // to rebuild its compilation from scratch if anyone asks for it.
                return new CompilationTracker(newProject);
            }

            /// <summary>
            /// Creates a fork with the same final project.
            /// </summary>
            public CompilationTracker Clone()
            {
                return this.Fork(this.ProjectState, clone: true);
            }

            public CompilationTracker FreezePartialStateWithTree(Solution solution, DocumentState docState, SyntaxTree tree, CancellationToken cancellationToken)
            {
                ProjectState inProgressProject;
                Compilation inProgressCompilation;

                GetPartialCompilationState(solution, docState.Id, out inProgressProject, out inProgressCompilation, cancellationToken);

                if (!inProgressCompilation.SyntaxTrees.Contains(tree))
                {
                    var existingTree = inProgressCompilation.SyntaxTrees.FirstOrDefault(t => t.FilePath == tree.FilePath);
                    if (existingTree != null)
                    {
                        inProgressCompilation = inProgressCompilation.ReplaceSyntaxTree(existingTree, tree);
                        inProgressProject = inProgressProject.UpdateDocument(docState, textChanged: false, recalculateDependentVersions: false);
                    }
                    else
                    {
                        inProgressCompilation = inProgressCompilation.AddSyntaxTrees(tree);
                        Debug.Assert(!inProgressProject.DocumentIds.Contains(docState.Id));
                        inProgressProject = inProgressProject.AddDocument(docState);
                    }
                }

                // The user is asking for an in progress snap.  We don't want to create it and then a
                // have the compilation immediately disappear.  So we force it to stay around with a ConstantValueSource
                return new CompilationTracker(inProgressProject,
                    new FinalState(new ConstantValueSource<Compilation>(inProgressCompilation)));
            }

            /// <summary>
            /// Tries to get the latest snapshot of the compilation without waiting for it to be
            /// fully built. This method takes advantage of the progress side-effect produced during
            /// BuildCompilation. It will either return the already built compilation, any
            /// in-progress compilation or any known old compilation in that order of preference.
            /// The compilation state that is returned will have a compilation that is retained so
            /// that it cannot disappear.
            /// </summary>
            private void GetPartialCompilationState(
                Solution solution,
                DocumentId id,
                out ProjectState inProgressProject,
                out Compilation inProgressCompilation,
                CancellationToken cancellationToken)
            {
                var state = this.ReadState();
                inProgressCompilation = state.Compilation.GetValue(cancellationToken);

                // check whether we can bail out quickly for typing case
                var inProgressState = state as InProgressState;

                // all changes left for this document is modifying the given document.
                // we can use current state as it is since we will replace the document with latest document anyway.
                if (inProgressState != null &&
                    inProgressCompilation != null &&
                    inProgressState.IntermediateProjects.All(t => TouchDocumentActionForDocument(t, id)))
                {
                    inProgressProject = this.ProjectState;
                    return;
                }

                inProgressProject = inProgressState != null ? inProgressState.IntermediateProjects.First().Item1 : this.ProjectState;

                // if we already have a final compilation we are done.
                if (inProgressCompilation != null && state is FinalState)
                {
                    return;
                }

                // 1) if we have an in-progress compilation use it.  
                // 2) If we don't, then create a simple empty compilation/project. 
                // 3) then, make sure that all it's p2p refs and whatnot are correct.
                if (inProgressCompilation == null)
                {
                    inProgressProject = inProgressProject.RemoveAllDocuments();
                    inProgressCompilation = this.CreateEmptyCompilation(solution);
                }

                // first remove all project from the project and compilation.
                inProgressProject = inProgressProject.WithProjectReferences(ImmutableList.Create<ProjectReference>());

                // Now add in back a consistent set of project references.  For project references
                // try to get either a CompilationReference or a SkeletonReference. This ensures
                // that the in-progress project only reports a reference to another project if it
                // could actually get a reference to that project's metadata.
                var metadataReferences = new List<MetadataReference>();
                var newProjectReferences = new List<ProjectReference>();
                metadataReferences.AddRange(this.ProjectState.MetadataReferences);

                foreach (var projectReference in this.ProjectState.ProjectReferences)
                {
                    var referencedProject = solution.GetProject(projectReference.ProjectId);
                    if (referencedProject != null)
                    {
                        if (referencedProject.IsSubmission)
                        {
                            var compilation = solution.GetCompilationAsync(projectReference.ProjectId, cancellationToken).WaitAndGetResult(cancellationToken);
                            inProgressCompilation = inProgressCompilation.WithPreviousSubmission(compilation);
                        }
                        else
                        {
                            // get the latest metadata for the partial compilation of the referenced project.
                            var metadata = solution.GetPartialMetadataReference(projectReference, this.ProjectState, cancellationToken);

                            if (metadata == null)
                            {
                                // if we failed to get the metadata, check to see if we previously had existing metadata and reuse it instead.
                                metadata = inProgressCompilation.References.FirstOrDefault(r => solution.GetProjectId(r) == projectReference.ProjectId);
                            }

                            if (metadata != null)
                            {
                                newProjectReferences.Add(projectReference);
                                metadataReferences.Add(metadata);
                            }
                        }
                    }
                }

                inProgressProject = inProgressProject.AddProjectReferences(newProjectReferences);
                if (!Enumerable.SequenceEqual(inProgressCompilation.References, metadataReferences))
                {
                    inProgressCompilation = inProgressCompilation.WithReferences(metadataReferences);
                }
            }

            private bool TouchDocumentActionForDocument(ValueTuple<ProjectState, CompilationTranslationAction> tuple, DocumentId id)
            {
                var touchDocumentAction = tuple.Item2 as CompilationTranslationAction.TouchDocumentAction;
                return touchDocumentAction != null && touchDocumentAction.DocumentId == id;
            }

            /// <summary>
            /// Gets the final compilation if it is available.
            /// </summary>
            public bool TryGetCompilation(out Compilation compilation)
            {
                var state = this.ReadState();
                return state.FinalCompilation.TryGetValue(out compilation) && compilation != null;
            }

            public Task<Compilation> GetCompilationAsync(Solution solution, CancellationToken cancellationToken)
            {
                Compilation compilation;
                if (this.TryGetCompilation(out compilation))
                {
                    // PERF: This is a hot code path and Task<TResult> isn't cheap,
                    // so cache the completed tasks to reduce allocations. We also
                    // need to avoid keeping a strong reference to the Compilation,
                    // so use a ConditionalWeakTable.
                    return SpecializedTasks.FromResult(compilation);
                }
                else
                {
                    return GetOrBuildCompilationAsync(solution, lockGate: true, cancellationToken: cancellationToken);
                }
            }

            private static string LogBuildCompilationAsync(ProjectState state)
            {
                return string.Join(",", state.AssemblyName, state.DocumentIds.Count);
            }

            private async Task<Compilation> GetOrBuildDeclarationCompilationAsync(Solution solution, CancellationToken cancellationToken)
            {
                cancellationToken.ThrowIfCancellationRequested();

                using (await this.buildLock.DisposableWaitAsync(cancellationToken).ConfigureAwait(false))
                {
                    var state = this.ReadState();

                    // we are already in the final stage. just return it.
                    var compilation = state.FinalCompilation.GetValue(cancellationToken);
                    if (compilation != null)
                    {
                        return compilation;
                    }

                    compilation = state.Compilation.GetValue(cancellationToken);
                    if (compilation == null)
                    {
                        // let's see whether we have declaration only compilation
                        if (state.DeclarationOnlyCompilation != null)
                        {
                            // okay, move to full declaration state. do this so that declaration only compilation never
                            // realize symbols.
                            this.WriteState(new FullDeclarationState(this.Retain(solution, state.DeclarationOnlyCompilation)));
                            return state.DeclarationOnlyCompilation;
                        }

                        // We've got nothing.  Build it from scratch :(
                        return await BuildDeclarationCompilationFromScratchAsync(solution, cancellationToken).ConfigureAwait(false);
                    }
                    else if (state is FullDeclarationState)
                    {
                        // we have full declaration, just use it.
                        return state.Compilation.GetValue(cancellationToken);
                    }
                    else if (state is InProgressState)
                    {
                        // We have an in progress compilation.  Build off of that.
                        return await BuildDeclarationCompilationFromInProgressAsync(solution, state as InProgressState, compilation, cancellationToken).ConfigureAwait(false);
                    }
                    else
                    {
                        throw Contract.Unreachable;
                    }
                }
            }

            private async Task<Compilation> GetOrBuildCompilationAsync(
                Solution solution,
                bool lockGate,
                CancellationToken cancellationToken)
            {
                using (Logger.LogBlock(FeatureId.CompilationTracker, FunctionId.CompilationTracker_BuildCompilationAsync,
                              logBuildCompilationAsync, this.ProjectState, cancellationToken))
                {
                    cancellationToken.ThrowIfCancellationRequested();

                    var state = this.ReadState();

                    // Try to get the built compilation.  If it exists, then we can just return that.
                    var finalCompilation = state.FinalCompilation.GetValue(cancellationToken);
                    if (finalCompilation != null)
                    {
                        return finalCompilation;
                    }

                    // Otherwise, we actually have to build it.  Ensure that only one thread is trying to
                    // build this compilation at a time.
                    if (lockGate)
                    {
                        using (await this.buildLock.DisposableWaitAsync(cancellationToken).ConfigureAwait(false))
                        {
                            return await BuildCompilationAsync(solution, cancellationToken).ConfigureAwait(false);
                        }
                    }
                    else
                    {
                        return await BuildCompilationAsync(solution, cancellationToken).ConfigureAwait(false);
                    }
                }
            }

            /// <summary>
            /// Builds the compilation matching the project state. In the process of building, also
            /// produce in progress snapshots that can be accessed from other threads.
            /// </summary>
            private Task<Compilation> BuildCompilationAsync(
                Solution solution,
                CancellationToken cancellationToken)
            {
                cancellationToken.ThrowIfCancellationRequested();

                var state = this.ReadState();

                // if we already have a compilation, we must be already done!  This can happen if two
                // threads were waiting to build, and we came in after the other succeeded.
                var compilation = state.FinalCompilation.GetValue(cancellationToken);
                if (compilation != null)
                {
                    return SpecializedTasks.FromResult(compilation);
                }

                compilation = state.Compilation.GetValue(cancellationToken);
                if (compilation == null)
                {
                    // this can happen if compilation is already kicked out from the cache.
                    // check whether the state we have support declaration only compilation
                    if (state.DeclarationOnlyCompilation != null)
                    {
                        // we have declaration only compilation. build final one from it.
                        return FinalizeCompilationAsync(solution, state.DeclarationOnlyCompilation, cancellationToken);
                    }

                    // We've got nothing.  Build it from scratch :(
                    return BuildCompilationFromScratchAsync(solution, state, cancellationToken);
                }
                else if (state is FullDeclarationState)
                {
                    // We have a declaration compilation, use it to reconstruct the final compilation
                    return this.FinalizeCompilationAsync(solution, compilation, cancellationToken);
                }
                else if (state is InProgressState)
                {
                    // We have an in progress compilation.  Build off of that.
                    return BuildFinalStateFromInProgressStateAsync(solution, state as InProgressState, compilation, cancellationToken);
                }
                else
                {
                    throw Contract.Unreachable;
                }
            }

            private async Task<Compilation> BuildCompilationFromScratchAsync(
                Solution solution, State state, CancellationToken cancellationToken)
            {
                var compilation = await BuildDeclarationCompilationFromScratchAsync(solution, cancellationToken).ConfigureAwait(false);
                return await FinalizeCompilationAsync(solution, compilation, cancellationToken).ConfigureAwait(false);
            }

            private async Task<Compilation> BuildDeclarationCompilationFromScratchAsync(
                Solution solution, CancellationToken cancellationToken)
            {
                var compilation = CreateEmptyCompilation(solution);

                foreach (var document in this.ProjectState.OrderedDocumentStates)
                {
                    cancellationToken.ThrowIfCancellationRequested();
                    compilation = compilation.AddSyntaxTrees(await document.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false));
                }

                this.WriteState(new FullDeclarationState(this.Retain(solution, compilation)));
                return compilation;
            }

            private Compilation CreateEmptyCompilation(Solution solution)
            {
                var compilationFactory = this.ProjectState.LanguageServices.GetService<ICompilationFactoryService>();

                if (this.ProjectState.IsSubmission)
                {
                    return compilationFactory.CreateSubmissionCompilation(
                        this.ProjectState.AssemblyName,
                        this.ProjectState.CompilationOptions,
                        this.ProjectState.HostObjectType);
                }
                else
                {
                    return compilationFactory.CreateCompilation(
                        this.ProjectState.AssemblyName,
                        this.ProjectState.CompilationOptions);
                }
            }

            private async Task<Compilation> BuildFinalStateFromInProgressStateAsync(
                Solution solution, InProgressState state, Compilation inProgressCompilation, CancellationToken cancellationToken)
            {
                var compilation = await BuildDeclarationCompilationFromInProgressAsync(solution, state, inProgressCompilation, cancellationToken).ConfigureAwait(false);
                return await FinalizeCompilationAsync(solution, compilation, cancellationToken).ConfigureAwait(false);
            }

            private async Task<Compilation> BuildDeclarationCompilationFromInProgressAsync(
                Solution solution, InProgressState state, Compilation inProgressCompilation, CancellationToken cancellationToken)
            {
                Contract.Requires(inProgressCompilation != null);
                var intermediateProjects = state.IntermediateProjects;

                while (intermediateProjects.Count > 0)
                {
                    cancellationToken.ThrowIfCancellationRequested();

                    var intermediateProject = intermediateProjects[0];
                    var inProgressProject = intermediateProject.Item1;
                    var action = intermediateProject.Item2;

                    inProgressCompilation = await action.InvokeAsync(inProgressCompilation, cancellationToken).ConfigureAwait(false);
                    intermediateProjects = intermediateProjects.RemoveAt(0);

                    this.WriteState(State.Create(
                        this.Retain(solution, inProgressCompilation),
                        intermediateProjects));
                }

                return inProgressCompilation;
            }

            // Add all appropriate references to the compilation and set it as our final compilation
            // state.
            private async Task<Compilation> FinalizeCompilationAsync(
                Solution solution,
                Compilation compilation,
                CancellationToken cancellationToken)
            {
                var newReferences = new List<MetadataReference>();
                newReferences.AddRange(this.ProjectState.MetadataReferences);

                foreach (var projectReference in this.ProjectState.ProjectReferences)
                {
                    var referencedProject = solution.GetProject(projectReference.ProjectId);

                    // Even though we're creating a final compilation (vs. an in progress compilation),
                    // it's possible that the target project has been removed.
                    if (referencedProject != null)
                    {
                        // If both projects are submissions, we'll count this as a previous submission link
                        // instead of a regular metadata reference
                        if (referencedProject.IsSubmission)
                        {
                            // if the referenced project is a submission project must be a submission as well:
                            Debug.Assert(this.ProjectState.IsSubmission);

                            var previousSubmissionCompilation =
                                await solution.GetCompilationAsync(projectReference.ProjectId, cancellationToken).ConfigureAwait(false);
                            compilation = compilation.WithPreviousSubmission(previousSubmissionCompilation);
                        }
                        else
                        {
                            var metadataReference = await solution.GetMetadataReferenceAsync(
                                projectReference, this.ProjectState, cancellationToken).ConfigureAwait(false);

                            // The compilation doesn't want to receive a null entry in the set
                            // of references it is constructed with. A reference can fail to be
                            // created if a skeleton assembly could not be constructed.
                            if (metadataReference != null)
                            {
                                newReferences.Add(metadataReference);
                            }
                        }
                    }
                }

                if (!Enumerable.SequenceEqual(compilation.References, newReferences))
                {
                    compilation = compilation.WithReferences(newReferences);
                }

                this.WriteState(new FinalState(this.Retain(solution, compilation)));

                return compilation;
            }

            /// <summary>
            /// Get a metadata reference to this compilation info's compilation with respect to
            /// another project. For cross language references produce a skeletal assembly. If the
            /// compilation is not available, it is built. If a skeletal assembly reference is
            /// needed and does not exist, it is also built.
            /// </summary>
            internal async Task<MetadataReference> GetMetadataReferenceAsync(
                Solution solution,
                ProjectState fromProject,
                ProjectReference projectReference,
                CancellationToken cancellationToken)
            {
                Compilation compilation;

                // if we already have the compilation and its right kind then use it.
                if (this.ProjectState.LanguageServices == fromProject.LanguageServices
                    && this.TryGetCompilation(out compilation))
                {
                    return compilation.ToMetadataReference(projectReference.Alias, projectReference.EmbedInteropTypes);
                }

                // If same language then we can wrap the other project's compilation into a compilation reference
                if (this.ProjectState.LanguageServices == fromProject.LanguageServices)
                {
                    // otherwise, base it off the compilation by building it first.
                    compilation = await this.GetCompilationAsync(solution, cancellationToken).ConfigureAwait(false);
                    return compilation.ToMetadataReference(projectReference.Alias, projectReference.EmbedInteropTypes);
                }
                else
                {
                    // otherwise get a metadata only image reference that is built by emitting the metadata from the referenced project's compilation and re-importing it.
                    return await this.GetMetadataOnlyImageReferenceAsync(solution, projectReference, cancellationToken).ConfigureAwait(false);
                }
            }

            /// <summary>
            /// Attempts to get (without waiting) a metadata reference to a possibly in progress
            /// compilation. Actual compilation references are preferred over skeletal assembly
            /// references.  Could potentially return null if nothing can be provided.
            /// </summary>
            internal MetadataReference GetPartialMetadataReference(Solution solution, ProjectState fromProject, ProjectReference projectReference, CancellationToken cancellationToken)
            {
                var state = this.ReadState();

                // get compilation in any state it happens to be in right now.
                Compilation compilation;
                if (state.Compilation.TryGetValue(out compilation)
                    && compilation != null
                    && this.ProjectState.LanguageServices == fromProject.LanguageServices)
                {
                    // if we have a compilation and its the correct language, use a simple compilation reference
                    return compilation.ToMetadataReference(projectReference.Alias, projectReference.EmbedInteropTypes);
                }

                return null;
            }

            /// <summary>
            /// Gets a metadata reference to a the metadata-only-image corresponding to the compilation.
            /// </summary>
            private async Task<MetadataReference> GetMetadataOnlyImageReferenceAsync(
                Solution solution, ProjectReference projectReference, CancellationToken cancellationToken)
            {
                using (Logger.LogBlock(FeatureId.SkeletonAssembly, FunctionId.SkeletonAssembly_GetMetadataOnlyImage, cancellationToken))
                {
                    var projectId = this.ProjectState.Id;
                    var version = await this.GetDependentSemanticVersionAsync(solution, cancellationToken).ConfigureAwait(false);

                    // get or build compilation up to decleration state. this compilation will be used to provide live xml doc comment
                    var declarationCompilation = await this.GetOrBuildDeclarationCompilationAsync(solution, cancellationToken: cancellationToken).ConfigureAwait(false);

                    MetadataReference reference;
                    if (!MetadataOnlyReference.TryGetReference(solution, projectReference, declarationCompilation, version, out reference))
                    {
                        // using async build lock so we don't get multiple consumers attempting to build metadata-only images for the same compilation.
                        using (await this.buildLock.DisposableWaitAsync(cancellationToken).ConfigureAwait(false))
                        {
                            // okay, we still don't have one. bring the compilation to final state since we are going to use it to create skeleton assembly
                            var compilation = await this.GetOrBuildCompilationAsync(solution, lockGate: false, cancellationToken: cancellationToken).ConfigureAwait(false);
                            reference = MetadataOnlyReference.GetOrBuildReference(solution, projectReference, compilation, version, cancellationToken);
                        }
                    }

                    return reference;
                }
            }

            /// <summary>
            /// Converts a compilation into a ValueSource employing an external compilation retention policy.
            /// </summary>
            private ValueSource<Compilation> Retain(Solution solution, Compilation compilation)
            {
                if (solution.CompilationCaches != null)
                {
                    // if solution supports compilation caches, get appropreate compilation cache.
                    // this will make the primary compilation cache not to be polluted by ones from branched solution.
                    //
                    // secondary cache could get inProgress compilation that belongs to the primary branch, but as soon as
                    // background compiler brings those compilations to final state, it will be moved to the primary cache.
                    //
                    // another case is if a state got moved to a final state by a branched solution, the final compilation can
                    // be in a secondary cache.
                    //
                    // we can add a bit more information in compilation tracker and fork compilation tracker a bit more to let
                    // it know which branch the compilation tracker belongs to.
                    //
                    // but all those cases where a compilation goes to the secondary cache should be either temporary or about ones
                    // the editor currently don't use (no opened file from the compilation). 
                    //
                    // so until we have data that says differently, let's live logic simple.
                    var cache = solution.BranchId == solution.Workspace.PrimaryBranchId ?
                        solution.CompilationCaches.Primary : solution.CompilationCaches.Secondary;

                    return new CachedObjectSource<Compilation>(compilation, cache);
                }
                else
                {
                    return new ConstantValueSource<Compilation>(compilation);
                }
            }

            #region Versions

            // Dependent Versions are stored on compilation tracker so they are more likely to survive when unrelated solution branching occurs.

            private AsyncLazy<VersionStamp> lazyDependentVersion;
            private AsyncLazy<VersionStamp> lazyDependentSemanticVersion;

            internal async Task<VersionStamp> GetDependentVersionAsync(Solution solution, CancellationToken cancellationToken)
            {
                if (this.lazyDependentVersion == null)
                {
                    // note: solution is captured here, but it will go away once GetValueAsync executes.
                    Interlocked.CompareExchange(ref this.lazyDependentVersion, new AsyncLazy<VersionStamp>(c => ComputeDependentVersionAsync(solution, c), cacheResult: true), null);
                }

                return await this.lazyDependentVersion.GetValueAsync(cancellationToken).ConfigureAwait(false);
            }

            private async Task<VersionStamp> ComputeDependentVersionAsync(Solution solution, CancellationToken cancellationToken)
            {
                var projectState = this.ProjectState;
                var projVersion = projectState.Version;
                var docVersion = await projectState.GetLatestDocumentVersionAsync(cancellationToken).ConfigureAwait(false);

                var version = docVersion.GetNewerVersion(projVersion);
                foreach (var dependentProjectReference in projectState.ProjectReferences)
                {
                    cancellationToken.ThrowIfCancellationRequested();

                    if (solution.ContainsProject(dependentProjectReference.ProjectId))
                    {
                        var dependentProjectVersion = await solution.GetDependentVersionAsync(dependentProjectReference.ProjectId, cancellationToken).ConfigureAwait(false);
                        version = dependentProjectVersion.GetNewerVersion(version);
                    }
                }

                return version;
            }

            internal async Task<VersionStamp> GetDependentSemanticVersionAsync(Solution solution, CancellationToken cancellationToken)
            {
                if (this.lazyDependentSemanticVersion == null)
                {
                    // note: solution is captured here, but it will go away once GetValueAsync executes.
                    Interlocked.CompareExchange(ref this.lazyDependentSemanticVersion, new AsyncLazy<VersionStamp>(c => ComputeDependentSemanticVersionAsync(solution, c), cacheResult: true), null);
                }

                return await this.lazyDependentSemanticVersion.GetValueAsync(cancellationToken).ConfigureAwait(false);
            }

            private async Task<VersionStamp> ComputeDependentSemanticVersionAsync(Solution solution, CancellationToken cancellationToken)
            {
                var projectState = this.ProjectState;
                var version = await projectState.GetSemanticVersionAsync(cancellationToken).ConfigureAwait(false);

                foreach (var dependentProjectReference in projectState.ProjectReferences)
                {
                    cancellationToken.ThrowIfCancellationRequested();

                    if (solution.ContainsProject(dependentProjectReference.ProjectId))
                    {
                        var dependentProjectVersion = await solution.GetDependentSemanticVersionAsync(dependentProjectReference.ProjectId, cancellationToken).ConfigureAwait(false);
                        version = dependentProjectVersion.GetNewerVersion(version);
                    }
                }

                return version;
            }
            #endregion
        }
    }
}