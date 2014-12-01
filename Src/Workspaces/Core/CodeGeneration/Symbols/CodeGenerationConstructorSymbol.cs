﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CodeGeneration
{
    internal class CodeGenerationConstructorSymbol : CodeGenerationMethodSymbol
    {
        public CodeGenerationConstructorSymbol(
            INamedTypeSymbol containingType,
            IList<AttributeData> attributes,
            Accessibility accessibility,
            SymbolModifiers modifiers,
            IList<IParameterSymbol> parameters) :
            base(containingType,
                 attributes,
                 accessibility,
                 modifiers,
                 returnType: null,
                 explicitInterfaceSymbolOpt: null,
                 name: string.Empty,
                 typeParameters: SpecializedCollections.EmptyList<ITypeParameterSymbol>(),
                 parameters: parameters,
                 returnTypeAttributes: SpecializedCollections.EmptyList<AttributeData>())
        {
        }

        public override MethodKind MethodKind
        {
            get
            {
                return MethodKind.Constructor;
            }
        }
    }
}