﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.Shared.Extensions
{
    internal static partial class ISymbolExtensions
    {
        public static bool IsValueParameter(this ISymbol symbol)
        {
            if (symbol is IParameterSymbol)
            {
                var method = symbol.ContainingSymbol as IMethodSymbol;
                if (method != null)
                {
                    if (method.MethodKind == MethodKind.EventAdd ||
                        method.MethodKind == MethodKind.EventRemove ||
                        method.MethodKind == MethodKind.PropertySet)
                    {
                        return symbol.Name == "value";
                    }
                }
            }

            return false;
        }
    }
}