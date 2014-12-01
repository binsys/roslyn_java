﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis.FindSymbols;
using Microsoft.CodeAnalysis.LanguageServices;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Simplification;

namespace Microsoft.CodeAnalysis.Rename
{
    internal static class RenameUtilities
    {
        internal static SyntaxToken UpdateAliasAnnotation(SyntaxToken token, ISymbol aliasSymbol, string replacementText)
        {
            // If the below Single() assert fails then it means the token has gone through a rename session where
            // it obtained an AliasSyntaxAnnotation and it is going through another rename session. Make sure the token
            // has only one annotation pertaining to the current session or try to extract only the current session annotation
            var originalAliasAnnotation = token.GetAnnotations(AliasAnnotation.Kind).Single();
            var originalAliasName = AliasAnnotation.GetAliasName(originalAliasAnnotation);

            if (originalAliasName == aliasSymbol.Name)
            {
                token = token.WithoutAnnotations(originalAliasAnnotation);
                var replacementAliasAnnotation = AliasAnnotation.Create(replacementText);
                token = token.WithAdditionalAnnotations(replacementAliasAnnotation);
            }

            return token;
        }

        internal static IEnumerable<ISymbol> GetSymbolsTouchingPosition(int position, SemanticModel semanticModel, Workspace workspace, CancellationToken cancellationToken)
        {
            var bindableToken = semanticModel.SyntaxTree.GetRoot(cancellationToken).FindToken(position, findInsideTrivia: true);
            var symbols = semanticModel.GetSymbols(bindableToken, workspace, bindLiteralsToUnderlyingType: false, cancellationToken: cancellationToken);

            // if there are more than one symbol, then remove the alias symbols.
            // When using (not declaring) an alias, the alias symbol and the target symbol are returned
            // by GetSymbols
            if (symbols.Count() > 1)
            {
                symbols = symbols.Where(s => s.Kind != SymbolKind.Alias);
            }

            return symbols;
        }
    }
}