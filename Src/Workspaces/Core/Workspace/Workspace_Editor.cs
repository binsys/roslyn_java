﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{
    // The parts of a workspace that deal with open documents
    public abstract partial class Workspace
    {
        // open documents
        private readonly Dictionary<ProjectId, ISet<DocumentId>> projectToOpenDocumentsMap = new Dictionary<ProjectId, ISet<DocumentId>>();

        // text buffer maps
        private readonly Dictionary<SourceTextContainer, DocumentId> bufferToDocumentInCurrentContextMap = new Dictionary<SourceTextContainer, DocumentId>();
        private readonly Dictionary<SourceTextContainer, ImmutableList<DocumentId>> bufferToDocumentIdMap = new Dictionary<SourceTextContainer, ImmutableList<DocumentId>>();
        private readonly Dictionary<DocumentId, TextTracker> textTrackers = new Dictionary<DocumentId, TextTracker>();

        /// <summary>
        /// True if this workspace supports manually opening and closing documents.
        /// </summary>
        public virtual bool CanOpenDocuments
        {
            get { return false; }
        }

        /// <summary>
        /// True if this workspace supports manually changing the ative context document of a text buffer.
        /// </summary>
        internal virtual bool CanChangeActiveContextDocument
        {
            get { return false; }
        }

        private static void RemoveIfEmpty<TKey, TValue>(IDictionary<TKey, ISet<TValue>> dictionary, TKey key)
        {
            ISet<TValue> values;
            if (dictionary.TryGetValue(key, out values))
            {
                if (values.Count == 0)
                {
                    dictionary.Remove(key);
                }
            }
        }

        private void ClearOpenDocuments()
        {
            List<DocumentId> docIds;
            using (this.stateLock.DisposableRead())
            {
                docIds = this.projectToOpenDocumentsMap.Values.SelectMany(x => x).ToList();
            }

            foreach (var docId in docIds)
            {
                this.ClearOpenDocument(docId);
            }
        }

        private void ClearOpenDocuments(ProjectId projectId)
        {
            ISet<DocumentId> openDocs;
            using (this.stateLock.DisposableRead())
            {
                this.projectToOpenDocumentsMap.TryGetValue(projectId, out openDocs);
            }

            if (openDocs != null)
            {
                foreach (var docId in openDocs)
                {
                    this.ClearOpenDocument(docId);
                }
            }
        }

        protected void ClearOpenDocument(DocumentId documentId)
        {
            DocumentId currentContextDocumentId;
            using (this.stateLock.DisposableWrite())
            {
                currentContextDocumentId = this.ClearOpenDocument_NoLock(documentId);
            }

            if (currentContextDocumentId != null && this.CanChangeActiveContextDocument)
            {
                SetDocumentContext(currentContextDocumentId);
            }
        }

        /// <returns>The DocumentId of the current context document for the buffer that was 
        /// previously attached to the given documentId, if any</returns>
        private DocumentId ClearOpenDocument_NoLock(DocumentId documentId)
        {
            this.stateLock.AssertCanWrite();

            ISet<DocumentId> openDocIds;

            if (this.projectToOpenDocumentsMap.TryGetValue(documentId.ProjectId, out openDocIds) && openDocIds != null)
            {
                openDocIds.Remove(documentId);
            }

            RemoveIfEmpty(this.projectToOpenDocumentsMap, documentId.ProjectId);

            // Stop tracking the buffer or update the documentId associated with the buffer.
            TextTracker tracker;
            if (this.textTrackers.TryGetValue(documentId, out tracker))
            {
                tracker.Disconnect();
                this.textTrackers.Remove(documentId);

                var currentContextDocumentId = RemoveTextToDocumentIdMapping_NoLock(tracker.TextContainer, documentId);
                if (currentContextDocumentId != null)
                {
                    return currentContextDocumentId;
                }
                else
                {
                    // No documentIds are attached to this buffer, so stop tracking it.
                    this.UnregisterText(tracker.TextContainer);
                }
            }

            return null;
        }

        /// <summary>
        /// Open the specified document.
        /// </summary>
        public virtual void OpenDocument(DocumentId documentId, bool activate = true)
        {
            throw new NotSupportedException();
        }

        /// <summary>
        /// Close the specified document.
        /// </summary>
        public virtual void CloseDocument(DocumentId documentId)
        {
            throw new NotSupportedException();
        }

        protected void CheckProjectDoesNotContainOpenDocuments(ProjectId projectId)
        {
            if (ProjectHasOpenDocuments(projectId))
            {
                throw new ArgumentException(string.Format(WorkspacesResources.ProjectContainsOpenDocuments, this.GetProjectName(projectId)));
            }
        }

        private bool ProjectHasOpenDocuments(ProjectId projectId)
        {
            using (this.stateLock.DisposableRead())
            {
                return this.projectToOpenDocumentsMap.ContainsKey(projectId);
            }
        }

        /// <summary>
        /// Determines if the document is currently open in the host environment.
        /// </summary>
        public virtual bool IsDocumentOpen(DocumentId documentId)
        {
            using (this.stateLock.DisposableRead())
            {
                var openDocuments = this.GetProjectOpenDocuments_NoLock(documentId.ProjectId);
                return openDocuments != null && openDocuments.Contains(documentId);
            }
        }

        /// <summary>
        /// Gets a list of the currently opened documents.
        /// </summary>
        public virtual IEnumerable<DocumentId> GetOpenDocumentIds(ProjectId projectId = null)
        {
            using (this.stateLock.DisposableRead())
            {
                if (this.projectToOpenDocumentsMap.Count == 0)
                {
                    return SpecializedCollections.EmptyEnumerable<DocumentId>();
                }

                if (projectId != null)
                {
                    ISet<DocumentId> documentIds;
                    if (this.projectToOpenDocumentsMap.TryGetValue(projectId, out documentIds))
                    {
                        return documentIds;
                    }

                    return SpecializedCollections.EmptyEnumerable<DocumentId>();
                }

                return this.projectToOpenDocumentsMap.SelectMany(kvp => kvp.Value).ToImmutableList();
            }
        }

        /// <summary>
        /// Gets the ids for documents associated with a text container.
        /// Documents are normally associated with a text container when the documents are opened.
        /// </summary>
        public virtual IEnumerable<DocumentId> GetRelatedDocumentIds(SourceTextContainer container)
        {
            if (container == null)
            {
                throw new ArgumentNullException("container");
            }

            using (this.stateLock.DisposableRead())
            {
                return GetRelatedDocumentIds_NoLock(container);
            }
        }

        private IEnumerable<DocumentId> GetRelatedDocumentIds_NoLock(SourceTextContainer container)
        {
            ImmutableList<DocumentId> docIds;
            if (this.bufferToDocumentIdMap.TryGetValue(container, out docIds))
            {
                return docIds;
            }
            else
            {
                return SpecializedCollections.EmptyEnumerable<DocumentId>();
            }
        }

        /// <summary>
        /// Gets the id for the document associated with the given text container in its current context.
        /// Documents are normally associated with a text container when the documents are opened.
        /// </summary>
        /// <param name="container"></param>
        /// <returns></returns>
        public virtual DocumentId GetDocumentIdInCurrentContext(SourceTextContainer container)
        {
            if (container == null)
            {
                throw new ArgumentNullException("container");
            }

            using (this.stateLock.DisposableRead())
            {
                DocumentId docId;
                bool foundValue = this.bufferToDocumentInCurrentContextMap.TryGetValue(container, out docId);

                if (foundValue)
                {
                    return docId;
                }
                else
                {
                    Debug.Assert(!this.bufferToDocumentIdMap.ContainsKey(container) ||
                                 !this.bufferToDocumentIdMap[container].Any());
                    return null;
                }
            }
        }

        /// <summary>
        /// 
        /// </summary>
        internal virtual void SetDocumentContext(DocumentId documentId)
        {
            throw new NotSupportedException();
        }

        /// <summary>
        /// 
        /// </summary>
        protected void OnDocumentContextUpdated(DocumentId documentId)
        {
            // TODO: remove linear search

            SourceTextContainer container = null;
            using (this.stateLock.DisposableRead())
            {
                container = bufferToDocumentIdMap.Where(kvp => kvp.Value.Contains(documentId)).Select(kvp => kvp.Key).FirstOrDefault();
            }

            if (container != null)
            {
                OnDocumentContextUpdated(documentId, container);
            }
        }

        /// <summary>
        /// 
        /// </summary>
        internal void OnDocumentContextUpdated(DocumentId documentId, SourceTextContainer container)
        {
            using (this.stateLock.DisposableWrite())
            {
                bufferToDocumentInCurrentContextMap[container] = documentId;
            }

            var document = CurrentSolution.GetDocument(documentId);
            this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.DocumentChanged, this.CurrentSolution, this.CurrentSolution, documentId: documentId);
            var task = this.RaiseDocumentActiveContextChangedEventAsync(document); // don't await this
        }

        protected void CheckDocumentIsClosed(DocumentId documentId)
        {
            if (this.IsDocumentOpen(documentId))
            {
                throw new ArgumentException(
                    string.Format(WorkspacesResources.DocumentIsOpen,
                    this.GetDocumentName(documentId)));
            }
        }

        protected void CheckDocumentIsOpen(DocumentId documentId)
        {
            if (!this.IsDocumentOpen(documentId))
            {
                throw new ArgumentException(string.Format(
                    WorkspacesResources.DocumentIsNotOpen,
                    this.GetDocumentName(documentId)));
            }
        }

        private ISet<DocumentId> GetProjectOpenDocuments_NoLock(ProjectId project)
        {
            this.stateLock.AssertCanRead();

            ISet<DocumentId> openDocs;

            projectToOpenDocumentsMap.TryGetValue(project, out openDocs);
            return openDocs;
        }

        protected internal void OnDocumentOpened(DocumentId documentId, SourceTextContainer textContainer, bool isCurrentContext = true)
        {
            CheckDocumentIsInCurrentSolution(documentId);
            CheckDocumentIsClosed(documentId);

            using (this.serializationLock.DisposableWait())
            {
                var oldSolution = this.CurrentSolution;
                var oldDocument = oldSolution.GetDocument(documentId);
                var oldText = oldDocument.GetTextAsync(CancellationToken.None).WaitAndGetResult(CancellationToken.None);

                using (this.stateLock.DisposableWrite())
                {
                    var openDocuments = GetProjectOpenDocuments_NoLock(documentId.ProjectId);
                    if (openDocuments != null)
                    {
                        openDocuments.Add(documentId);
                    }
                    else
                    {
                        this.projectToOpenDocumentsMap.Add(documentId.ProjectId, new HashSet<DocumentId> { documentId });
                    }
                }

                // keep open document text alive by using PreserveIdentity
                var newText = textContainer.CurrentText;
                var currentSolution = oldSolution;

                if (oldText != newText)
                {
                    if (oldText.HasSameText(newText))
                    {
                        // if the supplied text is the same as the previous text, then add with same version
                        var version = oldDocument.GetTextVersionAsync(CancellationToken.None).WaitAndGetResult(CancellationToken.None);
                        var newTextAndVersion = TextAndVersion.Create(newText, version, oldDocument.FilePath);
                        currentSolution = oldSolution.WithDocumentText(documentId, newTextAndVersion, PreservationMode.PreserveIdentity);
                    }
                    else
                    {
                        currentSolution = oldSolution.WithDocumentText(documentId, newText, PreservationMode.PreserveIdentity);
                    }
                }

                var newSolution = this.SetCurrentSolution(currentSolution);

                var tracker = new TextTracker(this, documentId, textContainer);
                this.textTrackers.Add(documentId, tracker);
                this.AddTextToDocumentIdMapping_NoLock(textContainer, documentId, isCurrentContext);
                tracker.Connect();

                var newDoc = newSolution.GetDocument(documentId);
                this.OnDocumentTextChanged(newDoc);

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.DocumentChanged, oldSolution, newSolution, documentId: documentId);
                var tsk = this.RaiseDocumentOpenedEventAsync(newDoc); // don't await this
            }

            // register outside of lock since it may call user code.
            this.RegisterText(textContainer);
        }

        protected internal void OnDocumentClosed(DocumentId documentId, TextLoader reloader)
        {
            this.CheckDocumentIsInCurrentSolution(documentId);
            this.CheckDocumentIsOpen(documentId);

            using (this.serializationLock.DisposableWait())
            {
                // forget any open document info
                DocumentId currentContextDocumentId;
                using (this.stateLock.DisposableWrite())
                {
                    currentContextDocumentId = this.ClearOpenDocument_NoLock(documentId);
                }

                if (currentContextDocumentId != null && this.CanChangeActiveContextDocument)
                {
                    // Closing this document did not result in the buffer closing, so some 
                    // document is now the current context of that buffer. Fire the appropriate
                    // events to set that document as the current context of that buffer.
                    SetDocumentContext(currentContextDocumentId);
                }

                var oldSolution = this.CurrentSolution;
                var oldDocument = oldSolution.GetDocument(documentId);

                this.OnDocumentClosing(documentId);

                var newSolution = oldSolution.WithDocumentTextLoader(documentId, reloader, PreservationMode.PreserveValue);
                newSolution = this.SetCurrentSolution(newSolution);

                var newDoc = newSolution.GetDocument(documentId);
                this.OnDocumentTextChanged(newDoc);

                this.RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind.DocumentChanged, oldSolution, newSolution, documentId: documentId); // don't wait for this
                this.RaiseDocumentClosedEventAsync(newDoc); // don't wait for this
            }
        }

        private void AddTextToDocumentIdMapping_NoLock(SourceTextContainer textContainer, DocumentId id, bool isCurrentContext)
        {
            ImmutableList<DocumentId> docIds;
            if (this.bufferToDocumentIdMap.TryGetValue(textContainer, out docIds))
            {
                this.bufferToDocumentIdMap[textContainer] = docIds.Add(id);
            }
            else
            {
                this.bufferToDocumentIdMap[textContainer] = ImmutableList.Create(id);
            }

            if (isCurrentContext || !bufferToDocumentInCurrentContextMap.ContainsKey(textContainer))
            {
                this.bufferToDocumentInCurrentContextMap[textContainer] = id;
            }
        }

        /// <returns>The DocumentId of the current context document attached to the textContainer, if any.</returns>
        private DocumentId RemoveTextToDocumentIdMapping_NoLock(SourceTextContainer textContainer, DocumentId id)
        {
            ImmutableList<DocumentId> docIds;
            if (this.bufferToDocumentIdMap.TryGetValue(textContainer, out docIds))
            {
                docIds = docIds.Remove(id);
                if (docIds.Count > 0)
                {
                    this.bufferToDocumentIdMap[textContainer] = docIds;

                    if (this.bufferToDocumentInCurrentContextMap[textContainer] == id)
                    {
                        // The current context document for this buffer has been removed, but there are
                        // other documents associated with the buffer. Arbitrarily choose the first
                        // remaining document as the new current context document.
                        var newCurrentContextDocumentId = docIds.First();
                        this.bufferToDocumentInCurrentContextMap[textContainer] = newCurrentContextDocumentId;
                        return newCurrentContextDocumentId;
                    }
                    else
                    {
                        return this.bufferToDocumentInCurrentContextMap[textContainer];
                    }
                }
                else
                {
                    this.bufferToDocumentIdMap.Remove(textContainer);
                    this.bufferToDocumentInCurrentContextMap.Remove(textContainer);
                }
            }

            return null;
        }

        private SourceText GetOpenDocumentText(Solution solution, DocumentId documentId)
        {
            CheckDocumentIsOpen(documentId);

            // text should always be preserved, so TryGetText will succeed.
            SourceText text;
            var doc = solution.GetDocument(documentId);
            doc.TryGetText(out text);
            return text;
        }

        /// <summary>
        ///  This method is called during OnSolutionReload.  Override this method if you want to manipulate
        ///  the reloaded solution.
        /// </summary>
        protected virtual Solution AdjustReloadedSolution(Solution oldSolution, Solution reloadedSolution)
        {
            var newSolution = reloadedSolution;

            // keep open documents using same text
            foreach (var docId in this.GetOpenDocumentIds())
            {
                if (newSolution.ContainsDocument((DocumentId)docId))
                {
                    newSolution = newSolution.WithDocumentText((DocumentId)docId, this.GetOpenDocumentText(oldSolution, (DocumentId)docId), PreservationMode.PreserveIdentity);
                }
            }

            return newSolution;
        }

        protected virtual Project AdjustReloadedProject(Project oldProject, Project reloadedProject)
        {
            var oldSolution = oldProject.Solution;
            var newSolution = reloadedProject.Solution;

            // keep open documents open using same text
            foreach (var docId in this.GetOpenDocumentIds(oldProject.Id))
            {
                if (newSolution.ContainsDocument((DocumentId)docId))
                {
                    newSolution = newSolution.WithDocumentText((DocumentId)docId, this.GetOpenDocumentText(oldSolution, (DocumentId)docId), PreservationMode.PreserveIdentity);
                }
            }

            return newSolution.GetProject(oldProject.Id);
        }
    }
}