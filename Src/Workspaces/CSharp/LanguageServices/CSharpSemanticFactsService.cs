﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Extensions.ContextQuery;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.LanguageServices;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
#if MEF
    [ExportLanguageService(typeof(ISemanticFactsService), LanguageNames.CSharp)]
#endif
    internal class CSharpSemanticFactsService : ISemanticFactsService
    {
        public bool SupportsImplicitInterfaceImplementation
        {
            get
            {
                return true;
            }
        }

        public bool ExposesAnonymousFunctionParameterNames
        {
            get
            {
                return false;
            }
        }

        public bool IsExpressionContext(SemanticModel semanticModel, int position, CancellationToken cancellationToken)
        {
            var csharpModel = (SemanticModel)semanticModel;
            return csharpModel.SyntaxTree.IsExpressionContext(
                position,
                csharpModel.SyntaxTree.FindTokenOnLeftOfPosition(position, cancellationToken),
                attributes: true, cancellationToken: cancellationToken, semanticModelOpt: csharpModel);
        }

        public bool IsStatementContext(SemanticModel semanticModel, int position, CancellationToken cancellationToken)
        {
            var csharpModel = (SemanticModel)semanticModel;
            return csharpModel.SyntaxTree.IsStatementContext(
                position, csharpModel.SyntaxTree.FindTokenOnLeftOfPosition(position, cancellationToken), cancellationToken);
        }

        public bool IsTypeContext(SemanticModel semanticModel, int position, CancellationToken cancellationToken)
        {
            var csharpModel = (SemanticModel)semanticModel;
            return csharpModel.SyntaxTree.IsTypeContext(position, cancellationToken, csharpModel);
        }

        public bool IsNamespaceContext(SemanticModel semanticModel, int position, CancellationToken cancellationToken)
        {
            var csharpModel = (SemanticModel)semanticModel;
            return csharpModel.SyntaxTree.IsNamespaceContext(position, cancellationToken, csharpModel);
        }

        public bool IsTypeDeclarationContext(SemanticModel semanticModel, int position, CancellationToken cancellationToken)
        {
            var csharpModel = (SemanticModel)semanticModel;
            return csharpModel.SyntaxTree.IsTypeDeclarationContext(
                position, csharpModel.SyntaxTree.FindTokenOnLeftOfPosition(position, cancellationToken), cancellationToken);
        }

        public bool IsMemberDeclarationContext(SemanticModel semanticModel, int position, CancellationToken cancellationToken)
        {
            var csharpModel = (SemanticModel)semanticModel;
            return csharpModel.SyntaxTree.IsMemberDeclarationContext(
                position, csharpModel.SyntaxTree.FindTokenOnLeftOfPosition(position, cancellationToken), cancellationToken);
        }

        public bool IsPreProcessorDirectiveContext(SemanticModel semanticModel, int position, CancellationToken cancellationToken)
        {
            var csharpModel = (SemanticModel)semanticModel;
            return csharpModel.SyntaxTree.IsPreProcessorDirectiveContext(
                position, csharpModel.SyntaxTree.FindTokenOnLeftOfPosition(position, cancellationToken, includeDirectives: true), cancellationToken);
        }

        public bool IsGlobalStatementContext(SemanticModel semanticModel, int position, CancellationToken cancellationToken)
        {
            var csharpModel = (SemanticModel)semanticModel;
            return csharpModel.SyntaxTree.IsGlobalStatementContext(position, cancellationToken);
        }

        public bool IsLabelContext(SemanticModel semanticModel, int position, CancellationToken cancellationToken)
        {
            var csharpModel = (SemanticModel)semanticModel;
            return csharpModel.SyntaxTree.IsLabelContext(position, cancellationToken);
        }

        public bool IsAttributeNameContext(SemanticModel semanticModel, int position, CancellationToken cancellationToken)
        {
            var csharpModel = (SemanticModel)semanticModel;
            return csharpModel.SyntaxTree.IsAttributeNameContext(position, cancellationToken);
        }

        public bool IsWrittenTo(SemanticModel semanticModel, SyntaxNode node, CancellationToken cancellationToken)
        {
            return (node as ExpressionSyntax).IsWrittenTo();
        }

        public bool IsOnlyWrittenTo(SemanticModel semanticModel, SyntaxNode node, CancellationToken cancellationToken)
        {
            return (node as ExpressionSyntax).IsOnlyWrittenTo();
        }

        public bool IsInOutContext(SemanticModel semanticModel, SyntaxNode node, CancellationToken cancellationToken)
        {
            return (node as ExpressionSyntax).IsInOutContext();
        }

        public bool IsInRefContext(SemanticModel semanticModel, SyntaxNode node, CancellationToken cancellationToken)
        {
            return (node as ExpressionSyntax).IsInRefContext();
        }

        public bool CanReplaceWithRValue(SemanticModel semanticModel, SyntaxNode expression, CancellationToken cancellationToken)
        {
            return (expression as ExpressionSyntax).CanReplaceWithRValue(semanticModel, cancellationToken);
        }

        public string GenerateNameForExpression(SemanticModel semanticModel, SyntaxNode expression, bool capitalize = false)
        {
            return semanticModel.GenerateNameForExpression((ExpressionSyntax)expression, capitalize);
        }

        public ISymbol GetDeclaredSymbol(SemanticModel semanticModel, SyntaxToken token, CancellationToken cancellationToken)
        {
            var location = token.GetLocation();
            var q = from node in token.GetAncestors<SyntaxNode>()
                    let symbol = semanticModel.GetDeclaredSymbol(node, cancellationToken)
                    where symbol != null && symbol.Locations.Contains(location)
                    select symbol;

            return q.FirstOrDefault();
        }

        public bool LastEnumValueHasInitializer(INamedTypeSymbol namedTypeSymbol)
        {
            var enumDecl = namedTypeSymbol.DeclaringSyntaxReferences.Select(r => r.GetSyntax()).OfType<EnumDeclarationSyntax>().FirstOrDefault();
            if (enumDecl != null)
            {
                var lastMember = enumDecl.Members.LastOrDefault();
                if (lastMember != null)
                {
                    return lastMember.EqualsValue != null;
                }
            }

            return false;
        }

        public bool SupportsParameterizedProperties
        {
            get
            {
                return false;
            }
        }

        public bool SupportsParameterizedEvents
        {
            get
            {
                return true;
            }
        }

        public bool TryGetSpeculativeSemanticModel(SemanticModel oldSemanticModel, SyntaxNode oldNode, SyntaxNode newNode, out SemanticModel speculativeModel)
        {
            Contract.Requires(oldNode.CSharpKind() == newNode.CSharpKind());

            var model = oldSemanticModel;

            // currently we only support method. field support will be added later.
            var oldMethod = oldNode as BaseMethodDeclarationSyntax;
            var newMethod = newNode as BaseMethodDeclarationSyntax;
            if (oldMethod == null || newMethod == null || oldMethod.Body == null)
            {
                speculativeModel = null;
                return false;
            }

            SemanticModel csharpModel;
            bool success = model.TryGetSpeculativeSemanticModelForMethodBody(oldMethod.Body.OpenBraceToken.Span.End, newMethod, out csharpModel);
            speculativeModel = csharpModel;
            return success;
        }

        public ImmutableHashSet<string> GetAliasNameSet(SemanticModel model, CancellationToken cancellationToken)
        {
            var original = (SemanticModel)model.GetOriginalSemanticModel();
            if (!original.SyntaxTree.HasCompilationUnitRoot)
            {
                return ImmutableHashSet.Create<string>();
            }

            var root = original.SyntaxTree.GetCompilationUnitRoot(cancellationToken);
            var builder = ImmutableHashSet.CreateBuilder<string>(StringComparer.Ordinal);

            AppendAliasNames(root.Usings, builder);
            AppendAliasNames(root.Members.OfType<PackageDeclarationSyntax>(), builder, cancellationToken);

            return builder.ToImmutable();
        }

        private static void AppendAliasNames(SyntaxList<UsingDirectiveSyntax> usings, ImmutableHashSet<string>.Builder builder)
        {
            foreach (var @using in usings)
            {
                if (@using.Alias == null || @using.Alias.Name == null)
                {
                    continue;
                }

                @using.Alias.Name.Identifier.ValueText.AppendToAliasNameSet(builder);
            }
        }

        private void AppendAliasNames(IEnumerable<PackageDeclarationSyntax> namepsaces, ImmutableHashSet<string>.Builder builder, CancellationToken cancellationToken)
        {
            foreach (var @namespace in namepsaces)
            {
                cancellationToken.ThrowIfCancellationRequested();

                AppendAliasNames(@namespace.Usings, builder);
                AppendAliasNames(@namespace.Members.OfType<PackageDeclarationSyntax>(), builder, cancellationToken);
            }
        }

        public ForEachSymbols GetForEachSymbols(SemanticModel semanticModel, SyntaxNode forEachStatement)
        {
            var csforEachStatement = forEachStatement as ForEachStatementSyntax;
            if (csforEachStatement != null)
            {
                var info = semanticModel.GetForEachStatementInfo(csforEachStatement);
                return new ForEachSymbols(
                    info.GetEnumeratorMethod,
                    info.MoveNextMethod,
                    info.CurrentProperty,
                    info.DisposeMethod,
                    info.ElementType);
            }
            else
            {
                return default(ForEachSymbols);
            }
        }

        public bool IsAssignableTo(ITypeSymbol fromSymbol, ITypeSymbol toSymbol, Compilation compilation)
        {
            return fromSymbol != null &&
                toSymbol != null &&
                ((CSharpCompilation)compilation).ClassifyConversion(fromSymbol, toSymbol).IsImplicit;
        }
    }
}