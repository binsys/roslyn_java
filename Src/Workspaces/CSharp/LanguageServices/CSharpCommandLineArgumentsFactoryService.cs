﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using Microsoft.CodeAnalysis.LanguageServices;

namespace Microsoft.CodeAnalysis.CSharp
{
#if MEF
    [ExportLanguageService(typeof(ICommandLineArgumentsFactoryService), LanguageNames.CSharp)]
#endif
    internal class CSharpCommandLineArgumentsFactoryService : ICommandLineArgumentsFactoryService
    {
        public CommandLineArguments CreateCommandLineArguments(IEnumerable<string> arguments, string baseDirectory, bool isInteractive)
        {
            var parser = isInteractive ? CSharpCommandLineParser.Interactive : CSharpCommandLineParser.Default;
            return parser.Parse(arguments, baseDirectory);
        }
    }
}