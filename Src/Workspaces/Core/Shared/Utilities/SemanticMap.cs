﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.Shared.Utilities
{
    internal partial class SemanticMap
    {
        private readonly Dictionary<SyntaxNode, SymbolInfo> expressionToInfoMap =
            new Dictionary<SyntaxNode, SymbolInfo>();

        private readonly Dictionary<SyntaxToken, SymbolInfo> tokenToInfoMap =
            new Dictionary<SyntaxToken, SymbolInfo>();

        private SemanticMap()
        {
        }

        internal static SemanticMap From(SemanticModel semanticModel, SyntaxNode node, CancellationToken cancellationToken)
        {
            var map = new SemanticMap();
            var walker = new Walker(semanticModel, map, cancellationToken);
            walker.Visit(node);
            return map;
        }

        public IEnumerable<ISymbol> AllReferencedSymbols
        {
            get
            {
                return expressionToInfoMap.Values.Concat(tokenToInfoMap.Values).Select(info => info.Symbol).Distinct();
            }
        }
    }
}