﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis.CodeGeneration;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp.CodeGeneration
{
    internal partial class StatementGenerator : AbstractCSharpCodeGenerator
    {
        internal static SyntaxList<StatementSyntax> GenerateStatements(IEnumerable<SyntaxNode> statements)
        {
            return statements.OfType<StatementSyntax>().ToSyntaxList();
        }

        internal static BlockSyntax GenerateBlock(IMethodSymbol method)
        {
            return SyntaxFactory.Block(
                StatementGenerator.GenerateStatements(CodeGenerationMethodInfo.GetStatements(method)));
        }
    }
}