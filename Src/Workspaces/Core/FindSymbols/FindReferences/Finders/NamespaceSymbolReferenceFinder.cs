﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.LanguageServices;

namespace Microsoft.CodeAnalysis.FindSymbols.Finders
{
    internal class NamespaceSymbolReferenceFinder : AbstractReferenceFinder<INamespaceSymbol>
    {
        private static readonly SymbolDisplayFormat globalNamespaceFormat = new SymbolDisplayFormat(SymbolDisplayGlobalNamespaceStyle.Included);

        protected override bool CanFind(INamespaceSymbol symbol)
        {
            return true;
        }

        protected override Task<IEnumerable<Document>> DetermineDocumentsToSearchAsync(
            INamespaceSymbol symbol,
            Project project,
            IImmutableSet<Document> documents,
            CancellationToken cancellationToken)
        {
            return FindDocumentsAsync(project, documents, cancellationToken, GetNamespaceIdentifierName(symbol, project));
        }

        private static string GetNamespaceIdentifierName(INamespaceSymbol symbol, Project project)
        {
            return symbol.IsGlobalNamespace
                ? symbol.ToDisplayString(globalNamespaceFormat)
                : symbol.Name;
        }

        protected override async Task<IEnumerable<ReferenceLocation>> FindReferencesInDocumentAsync(
            INamespaceSymbol symbol,
            Document document,
            CancellationToken cancellationToken)
        {
            var identifierName = GetNamespaceIdentifierName(symbol, document.Project);
            var syntaxFactsService = LanguageService.GetService<ISyntaxFactsService>(document);

            var nonAliasReferences = await FindReferencesInTokensAsync(symbol,
                document,
                await document.GetIdentifierOrGlobalNamespaceTokensWithTextAsync(identifierName, cancellationToken).ConfigureAwait(false),
                (SyntaxToken t) => syntaxFactsService.TextMatch(t.ValueText, identifierName),
                cancellationToken).ConfigureAwait(false);

            var aliasReferences = await FindAliasReferencesAsync(nonAliasReferences, symbol, document, cancellationToken).ConfigureAwait(false);
            return nonAliasReferences.Concat(aliasReferences);
        }
    }
}