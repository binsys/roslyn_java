﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis.Options;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Shared.Extensions.ContextQuery;

namespace Microsoft.CodeAnalysis.Recommendations
{
    internal abstract class AbstractRecommendationService : IRecommendationService
    {
        protected abstract Tuple<IEnumerable<ISymbol>, AbstractSyntaxContext> GetRecommendedSymbolsAtPositionWorker(
            Workspace workspace, SemanticModel semanticModel, int position, OptionSet options, CancellationToken cancellationToken);

        public IEnumerable<ISymbol> GetRecommendedSymbolsAtPosition(
            Workspace workspace, SemanticModel semanticModel, int position, OptionSet options, CancellationToken cancellationToken)
        {
            var result = GetRecommendedSymbolsAtPositionWorker(workspace, semanticModel, position, options, cancellationToken);

            var symbols = result.Item1;
            var context = result.Item2;

            symbols = symbols.Where(s => ShouldIncludeSymbol(s, context, cancellationToken));
            return symbols;
        }

        private bool ShouldIncludeSymbol(ISymbol symbol, AbstractSyntaxContext context, CancellationToken cancellationToken)
        {
            var isMember = false;
            switch (symbol.Kind)
            {
                case SymbolKind.NamedType:
                    var namedType = (INamedTypeSymbol)symbol;
                    if (namedType.SpecialType == SpecialType.System_Void)
                    {
                        return false;
                    }

                    break;

                case SymbolKind.Method:
                    var methodSymbol = (IMethodSymbol)symbol;
                    if (methodSymbol.MethodKind == MethodKind.EventAdd ||
                        methodSymbol.MethodKind == MethodKind.EventRemove ||
                        methodSymbol.MethodKind == MethodKind.EventRaise ||
                        methodSymbol.MethodKind == MethodKind.PropertyGet ||
                        methodSymbol.MethodKind == MethodKind.PropertySet)
                    {
                        return false;
                    }

                    isMember = true;
                    break;

                case SymbolKind.Event:
                case SymbolKind.Field:
                case SymbolKind.Property:
                    isMember = true;
                    break;
            }

            if (context.IsAttributeNameContext)
            {
                return symbol.IsOrContainsAccessibleAttribute(context.SemanticModel.Compilation);
            }

            if (context.IsEnumTypeMemberAccessContext)
            {
                return symbol.Kind == SymbolKind.Field;
            }

            // In an expression or statement context, we don't want to display instance members declared in outer containing types.
            if ((context.IsStatementContext || context.IsAnyExpressionContext) &&
                !symbol.IsStatic &&
                isMember &&
                context.GetOuterTypes(cancellationToken).Contains(symbol.ContainingType))
            {
                return false;
            }

            var namespaceSymbol = symbol as INamespaceSymbol;
            if (namespaceSymbol != null)
            {
                return namespaceSymbol.ContainsAccessibleTypesOrNamespaces(context.SemanticModel.Compilation.Assembly);
            }

            return true;
        }
    }
}
