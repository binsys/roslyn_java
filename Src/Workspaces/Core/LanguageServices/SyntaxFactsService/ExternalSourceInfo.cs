﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using Microsoft.CodeAnalysis.Text;
namespace Microsoft.CodeAnalysis.LanguageServices
{
    internal struct ExternalSourceInfo
    {
        public readonly int? StartLine;
        public readonly bool Ends;

        public ExternalSourceInfo(int? startLine, bool ends)
        {
            this.StartLine = startLine;
            this.Ends = ends;
        }
    }
}
