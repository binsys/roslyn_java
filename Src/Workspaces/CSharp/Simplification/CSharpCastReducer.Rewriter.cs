﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Threading;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Options;

namespace Microsoft.CodeAnalysis.CSharp.Simplification
{
    internal partial class CSharpCastReducer
    {
        private class Rewriter : AbstractExpressionRewriter
        {
            public Rewriter(OptionSet optionSet, CancellationToken cancellationToken)
                : base(optionSet, cancellationToken)
            {
            }

            public override SyntaxNode VisitCastExpression(CastExpressionSyntax node)
            {
                return SimplifyExpression(
                    node,
                    newNode: base.VisitCastExpression(node),
                    simplifier: SimplifyCast);
            }
        }
    }
}
