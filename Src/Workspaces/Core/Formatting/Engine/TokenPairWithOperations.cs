﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using Microsoft.CodeAnalysis.Formatting.Rules;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Formatting
{
    /// <summary>
    /// it holds onto space and wrapping operation need to run between two tokens.
    /// </summary>
    internal struct TokenPairWithOperations
    {
        private readonly TokenStream tokenStream;

        public AdjustSpacesOperation SpaceOperation { get; private set; }
        public AdjustNewLinesOperation LineOperation { get; private set; }

        public int PairIndex { get; private set; }

        public TokenPairWithOperations(
            TokenStream tokenStream,
            int tokenPairIndex,
            AdjustSpacesOperation spaceOperations,
            AdjustNewLinesOperation lineOperations) :
            this()
        {
            Contract.ThrowIfNull(tokenStream);

            Contract.ThrowIfFalse(0 <= tokenPairIndex && tokenPairIndex < tokenStream.TokenCount - 1);

            this.tokenStream = tokenStream;
            this.PairIndex = tokenPairIndex;

            SpaceOperation = spaceOperations;
            LineOperation = lineOperations;
        }

        public SyntaxToken Token1
        {
            get
            {
                return this.tokenStream.GetToken(this.PairIndex);
            }
        }

        public SyntaxToken Token2
        {
            get
            {
                return this.tokenStream.GetToken(this.PairIndex + 1);
            }
        }
    }
}
