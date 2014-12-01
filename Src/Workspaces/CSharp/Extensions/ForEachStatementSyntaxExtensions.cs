﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp.Extensions
{
    internal static class ForEachStatementSyntaxExtensions
    {
        public static bool IsTypeInferred(this ForEachStatementSyntax forEachStatement, SemanticModel semanticModel)
        {
            return forEachStatement.Type.IsTypeInferred(semanticModel);
        }
    }
}
