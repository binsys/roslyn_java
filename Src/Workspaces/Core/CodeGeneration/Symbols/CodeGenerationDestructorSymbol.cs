﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CodeGeneration
{
    internal class CodeGenerationDestructorSymbol : CodeGenerationMethodSymbol
    {
        public CodeGenerationDestructorSymbol(
            INamedTypeSymbol containingType,
            IList<AttributeData> attributes) :
            base(containingType,
                 attributes,
                 Accessibility.NotApplicable,
                 default(SymbolModifiers),
                 returnType: null,
                 explicitInterfaceSymbolOpt: null,
                 name: string.Empty,
                 typeParameters: SpecializedCollections.EmptyList<ITypeParameterSymbol>(),
                 parameters: SpecializedCollections.EmptyList<IParameterSymbol>(),
                 returnTypeAttributes: SpecializedCollections.EmptyList<AttributeData>())
        {
        }

        public override MethodKind MethodKind
        {
            get
            {
                return MethodKind.Destructor;
            }
        }
    }
}