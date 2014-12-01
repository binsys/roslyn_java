﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Extensions.ContextQuery;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Options;
using Microsoft.CodeAnalysis.Recommendations;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Shared.Extensions.ContextQuery;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.Recommendations
{
#if MEF
    using Microsoft.CodeAnalysis.LanguageServices;

    [ExportLanguageService(typeof(IRecommendationService), LanguageNames.CSharp)]
#endif
    internal class CSharpRecommendationService : AbstractRecommendationService
    {
        protected override Tuple<IEnumerable<ISymbol>, AbstractSyntaxContext> GetRecommendedSymbolsAtPositionWorker(
            Workspace workspace, SemanticModel semanticModel, int position, OptionSet options, CancellationToken cancellationToken)
        {
            var context = CSharpSyntaxContext.CreateContext(workspace, semanticModel, position, cancellationToken);

            var filterOutOfScopeLocals = options.GetOption(RecommendationOptions.FilterOutOfScopeLocals, semanticModel.Language);
            var symbols = GetSymbolsWorker(context, filterOutOfScopeLocals, cancellationToken);

            var hideAdvancedMembers = options.GetOption(RecommendationOptions.HideAdvancedMembers, semanticModel.Language);
            symbols = symbols.FilterToVisibleAndBrowsableSymbols(hideAdvancedMembers, semanticModel.Compilation);

            return Tuple.Create<IEnumerable<ISymbol>, AbstractSyntaxContext>(symbols, context);
        }

        private static IEnumerable<ISymbol> GetSymbolsWorker(
            CSharpSyntaxContext context,
            bool filterOutOfScopeLocals,
            CancellationToken cancellationToken)
        {
            if (context.IsInNonUserCode ||
                context.IsPreProcessorDirectiveContext)
            {
                return SpecializedCollections.EmptyEnumerable<ISymbol>();
            }

            // TODO: don't show completion set at namespace name part to match Dev10 behavior
            // if we want to provide new feature that shows all existing namespaces later, remove this
            if (context.IsNamespaceDeclarationNameContext)
            {
                return SpecializedCollections.EmptyEnumerable<ISymbol>();
            }

            if (context.IsRightOfNameSeparator)
            {
                return GetSymbolsOffOfContainer(context, cancellationToken);
            }
            else
            {
                return GetSymbolsForCurrentContext(context, filterOutOfScopeLocals, cancellationToken);
            }
        }

        private static IEnumerable<ISymbol> GetSymbolsForCurrentContext(
            CSharpSyntaxContext context,
            bool filterOutOfScopeLocals,
            CancellationToken cancellationToken)
        {
            if (context.IsGlobalStatementContext)
            {
                // Script and interactive
                return GetSymbolsForGlobalStatementContext(context, cancellationToken);
            }
            else if (context.IsAnyExpressionContext || context.IsStatementContext)
            {
                return GetSymbolsForExpressionOrStatementContext(context, filterOutOfScopeLocals, cancellationToken);
            }
            else if (context.IsTypeContext || context.IsNamespaceContext)
            {
                return GetSymbolsForTypeOrNamespaceContext(context, cancellationToken);
            }
            else if (context.IsLabelContext)
            {
                return GetSymbolsForLabelContext(context, cancellationToken);
            }
            else if (context.IsTypeArgumentOfConstraintContext)
            {
                return GetSymbolsForTypeArgumentOfConstraintClause(context, cancellationToken);
            }

            return SpecializedCollections.EmptyEnumerable<ISymbol>();
        }

        private static IEnumerable<ISymbol> GetSymbolsOffOfContainer(
            CSharpSyntaxContext context,
            CancellationToken cancellationToken)
        {
            // Ensure that we have the correct token in A.B| case
            var node = context.TargetToken.Parent;

            if (node.CSharpKind() == SyntaxKind.SimpleMemberAccessExpression)
            {
                return GetSymbolsOffOfExpression(context, ((MemberAccessExpressionSyntax)node).Expression, cancellationToken);
            }
            else if (node.CSharpKind() == SyntaxKind.PointerMemberAccessExpression)
            {
                return GetSymbolsOffOfDereferencedExpression(context, ((MemberAccessExpressionSyntax)node).Expression, cancellationToken);
            }
            else if (node.CSharpKind() == SyntaxKind.QualifiedName)
            {
                return GetSymbolsOffOfName(context, ((QualifiedNameSyntax)node).Left, cancellationToken);
            }
            else if (node.CSharpKind() == SyntaxKind.AliasQualifiedName)
            {
                return GetSymbolsOffOffAlias(context, ((AliasQualifiedNameSyntax)node).Alias, cancellationToken);
            }
            else
            {
                return SpecializedCollections.EmptyEnumerable<ISymbol>();
            }
        }

        private static IEnumerable<ISymbol> GetSymbolsForGlobalStatementContext(
            CSharpSyntaxContext context,
            CancellationToken cancellationToken)
        {
            var syntaxTree = context.SyntaxTree;
            var position = context.Position;
            var token = context.LeftToken;

            // The following code is a hack to get around a binding problem when asking binding
            // questions immediately after a using directive. This is special-cased in the binder
            // factory to ensure that using directives are not within scope inside other using
            // directives. That generally works fine for .cs, but it's a problem for interactive
            // code in this case:
            //
            // using System;
            // |

            if (token.CSharpKind() == SyntaxKind.SemicolonToken &&
                token.IsParentKind(SyntaxKind.UsingDirective) &&
                position >= token.Span.End)
            {
                var compUnit = (CompilationUnitSyntax)syntaxTree.GetRoot(cancellationToken);
                if (compUnit.Usings.Count > 0 && compUnit.Usings.Last().GetLastToken() == token)
                {
                    token = token.GetNextToken(includeZeroWidth: true);
                }
            }

            var symbols = context.SemanticModel
                .LookupSymbols(token.SpanStart);

            return symbols;
        }

        private static IEnumerable<ISymbol> GetSymbolsForTypeArgumentOfConstraintClause(
            CSharpSyntaxContext context,
            CancellationToken cancellationToken)
        {
            var enclosingSymbol = context.LeftToken.Parent
                .AncestorsAndSelf()
                .Select(n => context.SemanticModel.GetDeclaredSymbol(n))
                .WhereNotNull()
                .FirstOrDefault();

            var symbols = enclosingSymbol != null
                ? enclosingSymbol.GetTypeArguments()
                : SpecializedCollections.EmptyEnumerable<ISymbol>();

            return symbols;
        }

        private static IEnumerable<ISymbol> GetSymbolsOffOffAlias(
            CSharpSyntaxContext context,
            IdentifierNameSyntax alias,
            CancellationToken cancellationToken)
        {
            var aliasSymbol = context.SemanticModel.GetAliasInfo(alias, cancellationToken);
            if (aliasSymbol == null)
            {
                return SpecializedCollections.EmptyEnumerable<ISymbol>();
            }

            return context.SemanticModel.LookupNamespacesAndTypes(
                alias.SpanStart,
                aliasSymbol.Target);
        }

        private static IEnumerable<ISymbol> GetSymbolsForLabelContext(
            CSharpSyntaxContext context,
            CancellationToken cancellationToken)
        {
            var symbols = context.SemanticModel.LookupLabels(context.LeftToken.SpanStart);

            return symbols;
        }

        private static IEnumerable<ISymbol> GetSymbolsForTypeOrNamespaceContext(
            CSharpSyntaxContext context,
            CancellationToken cancellationToken)
        {
            var symbols = context.SemanticModel.LookupNamespacesAndTypes(context.LeftToken.SpanStart);

            return symbols;
        }

        private static IEnumerable<ISymbol> GetSymbolsForExpressionOrStatementContext(
            CSharpSyntaxContext context,
            bool filterOutOfScopeLocals,
            CancellationToken cancellationToken)
        {
            // Check if we're in an interesting situation like this:
            //
            //     i          // <-- here
            //     I = 0;

            // The problem is that "i I = 0" causes a local to be in scope called "I".  So, later when
            // we look up symbols, it masks any other 'I's in scope (i.e. if there's a field with that 
            // name).  If this is the case, we do not want to filter out inaccessible locals.
            if (filterOutOfScopeLocals)
            {
                if (context.LeftToken.Parent.IsFoundUnder<LocalDeclarationStatementSyntax>(d => d.Declaration.Type))
                {
                    filterOutOfScopeLocals = false;
                }
            }

            IEnumerable<ISymbol> symbols = context.LeftToken.Parent.IsInStaticContext()
                ? context.SemanticModel.LookupStaticMembers(context.LeftToken.SpanStart)
                : context.SemanticModel.LookupSymbols(context.LeftToken.SpanStart);

            // The symbols may include local variables that are declared later in the method and
            // should not be included in the completion list, so remove those. Filter them away,
            // unless we're in the debugger, where we show all locals in scope.
            if (filterOutOfScopeLocals)
            {
                symbols = symbols.Where(symbol => !symbol.IsInaccessibleLocal(context.Position));
            }

            return symbols;
        }

        private static IEnumerable<ISymbol> GetSymbolsOffOfName(
            CSharpSyntaxContext context,
            NameSyntax name,
            CancellationToken cancellationToken)
        {
            // Check if we're in an interesting situation like this:
            //
            //     int i = 5;
            //     i.          // <-- here
            //     List<string> ml = new List<string>();

            // The problem is that "i.List<string>" gets parsed as a type.  In this case we need to
            // try binding again as if "i" is an expression and not a type.  In order to do that, we
            // need to speculate as to what 'i' meant if it wasn't part of a local declaration's
            // type.
            if (name.IsFoundUnder<LocalDeclarationStatementSyntax>(d => d.Declaration.Type))
            {
                var speculativeBinding = context.SemanticModel.GetSpeculativeSymbolInfo(name.SpanStart, name, SpeculativeBindingOption.BindAsExpression);
                var container = context.SemanticModel.GetSpeculativeTypeInfo(name.SpanStart, name, SpeculativeBindingOption.BindAsExpression).Type;
                return GetSymbolsOffOfBoundExpression(context, name, name, speculativeBinding, container, cancellationToken);
            }

            // We're in a name-only context, since if we were an expression we'd be a
            // MemberAccessExpressionSyntax. Thus, let's do other namespaces and types.
            var nameBinding = context.SemanticModel.GetSymbolInfo(name, cancellationToken);

            var symbol = nameBinding.Symbol as INamespaceOrTypeSymbol;
            if (symbol != null)
            {
                IEnumerable<ISymbol> symbols = context.SemanticModel.LookupNamespacesAndTypes(
                    position: name.SpanStart,
                    container: symbol);

                // Filter the types when in a using directive, but not an alias.
                // 
                // Cases:
                //    using | -- Show namespaces (and static types in C# v6)
                //    using A = B.| -- Show namespace and types
                var usingDirective = name.GetAncestorOrThis<UsingDirectiveSyntax>();
                if (usingDirective != null && usingDirective.Alias == null)
                {
                    // Do we also have inclusion of static types?
                    if (((CSharpParseOptions)context.SyntaxTree.Options).LanguageVersion >= LanguageVersion.CSharp6)
                    {
                        symbols = symbols.Where(s => s.IsNamespace() || s.IsStaticType()).ToList();
                    }
                    else
                    {
                        symbols = symbols.Where(s => s.IsNamespace()).ToList();
                    }
                }

                if (symbols.Any())
                {
                    return symbols;
                }
            }

            return SpecializedCollections.EmptyEnumerable<ISymbol>();
        }

        private static IEnumerable<ISymbol> GetSymbolsOffOfExpression(
            CSharpSyntaxContext context,
            ExpressionSyntax originalExpression,
            CancellationToken cancellationToken)
        {
            var expression = originalExpression.WalkDownParentheses();
            var leftHandBinding = context.SemanticModel.GetSymbolInfo(expression, cancellationToken);
            var container = context.SemanticModel.GetTypeInfo(expression, cancellationToken).Type;

            // TODO remove this when 531549 which causes GetTypeInfo to return an error type is fixed
            if (container.IsErrorType())
            {
                container = leftHandBinding.Symbol.GetSymbolType() as ITypeSymbol;
            }

            var normalSymbols = GetSymbolsOffOfBoundExpression(context, originalExpression, expression, leftHandBinding, container, cancellationToken);

            // Check for the Color Color case.
            if (originalExpression.CanAccessInstanceAndStaticMembersOffOf(context.SemanticModel, cancellationToken))
            {
                var speculativeSymbolInfo = context.SemanticModel.GetSpeculativeSymbolInfo(expression.SpanStart, expression, SpeculativeBindingOption.BindAsTypeOrNamespace);

                var typeMembers = GetSymbolsOffOfBoundExpression(context, originalExpression, expression, speculativeSymbolInfo, container, cancellationToken);

                normalSymbols = normalSymbols.Concat(typeMembers);
            }

            return normalSymbols;
        }

        private static IEnumerable<ISymbol> GetSymbolsOffOfDereferencedExpression(
            CSharpSyntaxContext context,
            ExpressionSyntax originalExpression,
            CancellationToken cancellationToken)
        {
            var expression = originalExpression.WalkDownParentheses();
            var leftHandBinding = context.SemanticModel.GetSymbolInfo(expression, cancellationToken);

            var container = context.SemanticModel.GetTypeInfo(expression, cancellationToken).Type;
            if (container is IPointerTypeSymbol)
            {
                container = ((IPointerTypeSymbol)container).PointedAtType;
            }

            return GetSymbolsOffOfBoundExpression(context, originalExpression, expression, leftHandBinding, container, cancellationToken);
        }

        private static IEnumerable<ISymbol> GetSymbolsOffOfBoundExpression(
            CSharpSyntaxContext context,
            ExpressionSyntax originalExpression,
            ExpressionSyntax expression,
            SymbolInfo leftHandBinding,
            INamespaceOrTypeSymbol container,
            CancellationToken cancellationToken)
        {
            var useBaseReferenceAccessibility = false;
            var excludeInstance = false;
            var excludeStatic = false;
            var symbol = leftHandBinding.GetBestOrAllSymbols().FirstOrDefault();

            if (symbol != null)
            {
                // If the thing on the left is a type, namespace or alias and the original
                // expression was parenthesized, we shouldn't show anything in IntelliSense.
                if (originalExpression.IsKind(SyntaxKind.ParenthesizedExpression) &&
                    symbol.MatchesKind(SymbolKind.NamedType,
                                       SymbolKind.Namespace,
                                       SymbolKind.Alias))
                {
                    return SpecializedCollections.EmptyEnumerable<ISymbol>();
                }

                // If the thing on the left is a lambda expression, we shouldn't show anything.
                if (symbol.Kind == SymbolKind.Method &&
                    ((IMethodSymbol)symbol).MethodKind == MethodKind.AnonymousFunction)
                {
                    return SpecializedCollections.EmptyEnumerable<ISymbol>();
                }

                // If the thing on the left is an event that can't be used as a field, we shouldn't show anything
                if (symbol.Kind == SymbolKind.Event &&
                    !context.SemanticModel.IsEventUsableAsField(originalExpression.SpanStart, (IEventSymbol)symbol))
                {
                    return SpecializedCollections.EmptyEnumerable<ISymbol>();
                }

                // If the thing on the left is a this parameter (e.g. this or base) and we're in a static context,
                // we shouldn't show anything
                if (symbol.IsThisParameter() &&
                    expression.IsInStaticContext())
                {
                    return SpecializedCollections.EmptyEnumerable<ISymbol>();
                }

                // What is the thing on the left?
                switch (symbol.Kind)
                {
                    case SymbolKind.NamedType:
                    case SymbolKind.Namespace:
                        excludeInstance = true;
                        container = (INamespaceOrTypeSymbol)symbol;
                        break;

                    case SymbolKind.Alias:
                        excludeInstance = true;
                        container = ((IAliasSymbol)symbol).Target;
                        break;

                    case SymbolKind.Parameter:
                        var parameter = (IParameterSymbol)symbol;

                        excludeStatic = true;

                        // case:
                        //    base.|
                        if (parameter.IsThis && !object.Equals(parameter.Type, container))
                        {
                            useBaseReferenceAccessibility = true;
                        }

                        break;

                    default:
                        excludeStatic = true;
                        break;
                }
            }
            else if (container != null)
            {
                excludeStatic = true;
            }
            else
            {
                return SpecializedCollections.EmptyEnumerable<ISymbol>();
            }

            Debug.Assert(!excludeInstance || !excludeStatic);
            Debug.Assert(!excludeInstance || !useBaseReferenceAccessibility);

            var position = originalExpression.SpanStart;
            IEnumerable<ISymbol> symbols = useBaseReferenceAccessibility
                ? context.SemanticModel.LookupBaseMembers(position)
                : excludeInstance
                    ? context.SemanticModel.LookupStaticMembers(position, container)
                    : context.SemanticModel.LookupSymbols(position, container, includeReducedExtensionMethods: true);

            // If we're showing instance members, don't include nested types
            return excludeStatic
                ? symbols.Where(s => !s.IsStatic && !(s is ITypeSymbol))
                : symbols;
        }
    }
}
