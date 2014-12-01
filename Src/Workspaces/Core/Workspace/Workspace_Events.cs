﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Threading.Tasks;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{
    public abstract partial class Workspace
    {
        private readonly EventMap eventMap = new EventMap();
        private const string WorkspaceChangeEventName = "WorkspaceChanged";
        private const string WorkspaceFailedEventName = "WorkspaceFailed";
        private const string DocumentOpenedEventName = "DocumentOpened";
        private const string DocumentClosedEventName = "DocumentClosed";
        private const string DocumentActiveContextChangedName = "DocumentActiveContextChanged";

        /// <summary>
        /// An event raised whenever the current solution is changed.
        /// </summary>
        public event EventHandler<WorkspaceChangeEventArgs> WorkspaceChanged
        {
            add
            {
                this.eventMap.AddEventHandler(WorkspaceChangeEventName, value);
            }

            remove
            {
                this.eventMap.RemoveEventHandler(WorkspaceChangeEventName, value);
            }
        }

        protected Task RaiseWorkspaceChangedEventAsync(WorkspaceChangeKind kind, Solution oldSolution, Solution newSolution, ProjectId projectId = null, DocumentId documentId = null)
        {
            if (newSolution == null)
            {
                throw new ArgumentNullException("newSolution");
            }

            if (projectId == null && documentId != null)
            {
                projectId = documentId.ProjectId;
            }

            var handlers = this.eventMap.GetEventHandlers<EventHandler<WorkspaceChangeEventArgs>>(WorkspaceChangeEventName);
            if (handlers != null)
            {
                return this.ScheduleTask(() =>
                {
                    var args = new WorkspaceChangeEventArgs(kind, oldSolution, newSolution, projectId, documentId);
                    foreach (var handler in handlers)
                    {
                        handler(this, args);
                    }
                }, "Workspace.WorkspaceChanged");
            }
            else
            {
                return SpecializedTasks.EmptyTask;
            }
        }

        /// <summary>
        /// An event raised whenever the workspace or part of its solution model
        /// fails to access a file or other external resource.
        /// </summary>
        public event EventHandler<WorkspaceDiagnosticEventArgs> WorkspaceFailed
        {
            add
            {
                this.eventMap.AddEventHandler(WorkspaceFailedEventName, value);
            }

            remove
            {
                this.eventMap.RemoveEventHandler(WorkspaceFailedEventName, value);
            }
        }

        private Task RaiseWorkspaceFailedEventAsync(WorkspaceDiagnostic diagnostic)
        {
            var handlers = this.eventMap.GetEventHandlers<EventHandler<WorkspaceDiagnosticEventArgs>>(WorkspaceFailedEventName);
            if (handlers != null)
            {
                return this.ScheduleTask(() =>
                {
                    var args = new WorkspaceDiagnosticEventArgs(diagnostic);
                    foreach (var handler in handlers)
                    {
                        handler(this, args);
                    }
                }, "Workspace.WorkspaceFailed");
            }
            else
            {
                return SpecializedTasks.EmptyTask;
            }
        }

        protected internal virtual void OnWorkspaceFailed(WorkspaceDiagnostic diagnostic)
        {
            this.RaiseWorkspaceFailedEventAsync(diagnostic);
        }

        /// <summary>
        /// An event that is fired when a documents is opened in the editor.
        /// </summary>
        public event EventHandler<DocumentEventArgs> DocumentOpened
        {
            add
            {
                this.eventMap.AddEventHandler(DocumentOpenedEventName, value);
            }

            remove
            {
                this.eventMap.RemoveEventHandler(DocumentOpenedEventName, value);
            }
        }

        protected Task RaiseDocumentOpenedEventAsync(Document document)
        {
            var handlers = this.eventMap.GetEventHandlers<EventHandler<DocumentEventArgs>>(DocumentOpenedEventName);
            if (handlers != null)
            {
                return this.ScheduleTask(() =>
                {
                    var args = new DocumentEventArgs(document);
                    foreach (var handler in handlers)
                    {
                        handler(this, args);
                    }
                }, "Workspace.WorkspaceChanged");
            }
            else
            {
                return SpecializedTasks.EmptyTask;
            }
        }

        /// <summary>
        /// An event that is fired when a document is closed in the editor.
        /// </summary>
        public event EventHandler<DocumentEventArgs> DocumentClosed
        {
            add
            {
                this.eventMap.AddEventHandler(DocumentClosedEventName, value);
            }

            remove
            {
                this.eventMap.RemoveEventHandler(DocumentClosedEventName, value);
            }
        }

        protected Task RaiseDocumentClosedEventAsync(Document document)
        {
            var handlers = this.eventMap.GetEventHandlers<EventHandler<DocumentEventArgs>>(DocumentClosedEventName);
            if (handlers != null)
            {
                return this.ScheduleTask(() =>
                {
                    var args = new DocumentEventArgs(document);
                    foreach (var handler in handlers)
                    {
                        handler(this, args);
                    }
                }, "Workspace.DocumentClosed");
            }
            else
            {
                return SpecializedTasks.EmptyTask;
            }
        }

        /// <summary>
        /// An event that is fired when the active context document associated with a buffer 
        /// changes.
        /// </summary>
        internal event EventHandler<DocumentEventArgs> DocumentActiveContextChanged
        {
            add
            {
                this.eventMap.AddEventHandler(DocumentActiveContextChangedName, value);
            }

            remove
            {
                this.eventMap.RemoveEventHandler(DocumentActiveContextChangedName, value);
            }
        }

        protected Task RaiseDocumentActiveContextChangedEventAsync(Document document)
        {
            var handlers = this.eventMap.GetEventHandlers<EventHandler<DocumentEventArgs>>(DocumentActiveContextChangedName);
            if (handlers != null)
            {
                return this.ScheduleTask(() =>
                {
                    var args = new DocumentEventArgs(document);
                    foreach (var handler in handlers)
                    {
                        handler(this, args);
                    }
                }, "Workspace.WorkspaceChanged");
            }
            else
            {
                return SpecializedTasks.EmptyTask;
            }
        }
    }
}