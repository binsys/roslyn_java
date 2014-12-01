﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.LanguageServices
{
    internal interface ICommandLineArgumentsFactoryService : ILanguageService
    {
        CommandLineArguments CreateCommandLineArguments(IEnumerable<string> arguments, string baseDirectory, bool isInteractive);
    }
}