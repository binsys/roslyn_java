﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{
    /* This is the static API on Workspace that lets you associate text containers with workspace instances */
    public abstract partial class Workspace
    {
        private static readonly ConditionalWeakTable<SourceTextContainer, WorkspaceRegistration> bufferToWorkspaceRegistrationMap =
            new ConditionalWeakTable<SourceTextContainer, WorkspaceRegistration>();

        /// <summary>
        /// Gets the workspace associated with the specific text container.
        /// </summary>
        public static bool TryGetWorkspace(SourceTextContainer textContainer, out Workspace workspace)
        {
            if (textContainer == null)
            {
                throw new ArgumentNullException("textContainer");
            }

            var registration = GetWorkspaceRegistration(textContainer);
            workspace = registration.Workspace;

            return workspace != null;
        }

        /// <summary>
        /// Register a correspondence between a text container and a workspace.
        /// </summary>
        protected void RegisterText(SourceTextContainer textContainer)
        {
            if (textContainer == null)
            {
                throw new ArgumentNullException("textContainer");
            }

            GetWorkspaceRegistration(textContainer).SetWorkspaceAndRaiseEvents(this);
        }

        /// <summary>
        /// Unregister a correspondence between a text container and a workspace.
        /// </summary>
        protected void UnregisterText(SourceTextContainer textContainer)
        {
            if (textContainer == null)
            {
                throw new ArgumentNullException("textContainer");
            }

            GetWorkspaceRegistration(textContainer).SetWorkspaceAndRaiseEvents(null);
        }

        private static WorkspaceRegistration CreateRegistration(SourceTextContainer container)
        {
            return new WorkspaceRegistration();
        }

        private static ConditionalWeakTable<SourceTextContainer, WorkspaceRegistration>.CreateValueCallback createRegistration = CreateRegistration;

        /// <summary>
        /// Returns a <see cref="WorkspaceRegistration" /> for a given text container.
        /// </summary>
        public static WorkspaceRegistration GetWorkspaceRegistration(SourceTextContainer textContainer)
        {
            if (textContainer == null)
            {
                throw new ArgumentNullException("textContainer");
            }

            return bufferToWorkspaceRegistrationMap.GetValue(textContainer, createRegistration);
        }
    }
}