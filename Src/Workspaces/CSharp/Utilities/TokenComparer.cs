﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Globalization;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp.Utilities
{
    internal class TokenComparer : IComparer<SyntaxToken>
    {
        private const string SystemNamespace = "System";

        public static readonly IComparer<SyntaxToken> NormalInstance = new TokenComparer(specialCaseSystem: false);
        public static readonly IComparer<SyntaxToken> SystemFirstInstance = new TokenComparer(specialCaseSystem: true);

        private readonly bool specialCaseSystem;

        private TokenComparer(bool specialCaseSystem)
        {
            this.specialCaseSystem = specialCaseSystem;
        }

        private static bool IsSystem(string s)
        {
            return s == SystemNamespace;
        }

        public int Compare(SyntaxToken x, SyntaxToken y)
        {
            if (specialCaseSystem &&
                x.GetPreviousToken(includeSkipped: true).CSharpKind() == SyntaxKind.ImportKeyword &&
                y.GetPreviousToken(includeSkipped: true).CSharpKind() == SyntaxKind.ImportKeyword)
            {
                var token1IsSystem = IsSystem(x.ValueText);
                var token2IsSystem = IsSystem(y.ValueText);

                if (token1IsSystem && !token2IsSystem)
                {
                    return -1;
                }
                else if (!token1IsSystem && token2IsSystem)
                {
                    return 1;
                }
            }

            return CompareWorker(x, y);
        }

        private int CompareWorker(SyntaxToken x, SyntaxToken y)
        {
            if (x == y)
            {
                return 0;
            }

            // By using 'ValueText' we get the value that is normalized.  i.e.
            // @class will be 'class', and unicode escapes will be converted
            // to actual unicode.  This allows sorting to work properly across
            // tokens that have different source representations, but which
            // mean the same thing.
            var string1 = x.ValueText;
            var string2 = y.ValueText;

            // First check in a case insensitive manner.  This will put 
            // everything that starts with an 'a' or 'A' above everything
            // that starts with a 'b' or 'B'.
            var compare = string.Compare(string1, string2, CultureInfo.InvariantCulture,
                CompareOptions.IgnoreCase | CompareOptions.IgnoreNonSpace | CompareOptions.IgnoreWidth);
            if (compare != 0)
            {
                return compare;
            }

            // Now, once we've grouped such that 'a' words and 'A' words are
            // together, sort such that 'a' words come before 'A' words.
            return string.Compare(string1, string2, CultureInfo.InvariantCulture,
                CompareOptions.IgnoreNonSpace | CompareOptions.IgnoreWidth);
        }
    }
}