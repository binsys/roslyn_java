﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.Shared.Collections;
using Microsoft.CodeAnalysis.Shared.Utilities;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.WorkspaceServices;

namespace Microsoft.CodeAnalysis.Extensions
{
#if MEF
    [ExportWorkspaceServiceFactory(typeof(IExtensionManager), WorkspaceKind.Any)]
#endif
    internal class ServicesLayerExtensionManager : IWorkspaceServiceFactory
    {
        public IWorkspaceService CreateService(IWorkspaceServiceProvider workspaceServices)
        {
            return new ExtensionManager();
        }

        private class ExtensionManager : AbstractExtensionManager
        {
        }
    }
}