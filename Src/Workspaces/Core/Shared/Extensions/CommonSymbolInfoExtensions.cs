﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Shared.Extensions
{
    internal static class CommonSymbolInfoExtensions
    {
        public static IEnumerable<ISymbol> GetAllSymbols(this SymbolInfo info)
        {
            return GetAllSymbolsWorker(info).Distinct();
        }

        private static IEnumerable<ISymbol> GetAllSymbolsWorker(this SymbolInfo info)
        {
            if (info.Symbol != null)
            {
                yield return info.Symbol;
            }

            foreach (var symbol in info.CandidateSymbols)
            {
                yield return symbol;
            }
        }

        public static ISymbol GetAnySymbol(this SymbolInfo info)
        {
            return info.GetAllSymbols().FirstOrDefault();
        }

        public static IEnumerable<ISymbol> GetBestOrAllSymbols(this SymbolInfo info)
        {
            if (info.Symbol != null)
            {
                return SpecializedCollections.SingletonEnumerable(info.Symbol);
            }
            else if (info.CandidateSymbols.Length > 0)
            {
                return info.CandidateSymbols;
            }

            return SpecializedCollections.EmptyEnumerable<ISymbol>();
        }
    }
}