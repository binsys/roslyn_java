﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.Shared.Extensions
{
    internal partial class ITypeSymbolExtensions
    {
        private class MinimalAccessibilityVisitor : SymbolVisitor<Accessibility>
        {
            public static readonly SymbolVisitor<Accessibility> Instance = new MinimalAccessibilityVisitor();

            public override Accessibility DefaultVisit(ISymbol node)
            {
                throw new NotImplementedException();
            }

            public override Accessibility VisitAlias(IAliasSymbol symbol)
            {
                return symbol.Target.Accept(this);
            }

            public override Accessibility VisitArrayType(IArrayTypeSymbol symbol)
            {
                return symbol.ElementType.Accept(this);
            }

            public override Accessibility VisitDynamicType(IDynamicTypeSymbol symbol)
            {
                return Accessibility.Public;
            }

            public override Accessibility VisitNamedType(INamedTypeSymbol symbol)
            {
                var accessibility = symbol.DeclaredAccessibility;

                foreach (var arg in symbol.TypeArguments)
                {
                    accessibility = CommonAccessibilityUtilities.Minimum(accessibility, arg.Accept(this));
                }

                if (symbol.ContainingType != null)
                {
                    accessibility = CommonAccessibilityUtilities.Minimum(accessibility, symbol.ContainingType.Accept(this));
                }

                return accessibility;
            }

            public override Accessibility VisitPointerType(IPointerTypeSymbol symbol)
            {
                return symbol.PointedAtType.Accept(this);
            }

            public override Accessibility VisitTypeParameter(ITypeParameterSymbol symbol)
            {
                // TODO(cyrusn): Do we have to consider the constraints?
                return Accessibility.Public;
            }
        }
    }
}