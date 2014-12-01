﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.IO;
using Microsoft.CodeAnalysis.WorkspaceServices;

namespace Microsoft.CodeAnalysis.GeneratedCodeRecognition
{
#if MEF
    [ExportWorkspaceServiceFactory(typeof(IGeneratedCodeRecognitionService), WorkspaceKind.Any)]
#endif
    internal class GeneratedCodeRecognitionServiceFactory : IWorkspaceServiceFactory
    {
        private static readonly IGeneratedCodeRecognitionService Singleton = new GeneratedCodeRecognitionService();

        public IWorkspaceService CreateService(IWorkspaceServiceProvider workspaceServices)
        {
            return Singleton;
        }

        private class GeneratedCodeRecognitionService : IGeneratedCodeRecognitionService
        {
            public bool IsGeneratedCode(Document document)
            {
                return IsFileNameForGeneratedCode(document.Name);
            }

            private static bool IsFileNameForGeneratedCode(string fileName)
            {
                if (fileName.StartsWith("TemporaryGeneratedFile_", StringComparison.OrdinalIgnoreCase))
                {
                    return true;
                }

                string extension = Path.GetExtension(fileName);
                if (extension != string.Empty)
                {
                    fileName = Path.GetFileNameWithoutExtension(fileName);

                    if (fileName.EndsWith("AssemblyInfo", StringComparison.OrdinalIgnoreCase) ||
                        fileName.EndsWith(".designer", StringComparison.OrdinalIgnoreCase) ||
                        fileName.EndsWith(".generated", StringComparison.OrdinalIgnoreCase) ||
                        fileName.EndsWith(".g", StringComparison.OrdinalIgnoreCase) ||
                        fileName.EndsWith(".g.i", StringComparison.OrdinalIgnoreCase) ||
                        fileName.EndsWith(".AssemblyAttributes", StringComparison.OrdinalIgnoreCase))
                    {
                        return true;
                    }
                }

                return false;
            }
        }
    }
}
