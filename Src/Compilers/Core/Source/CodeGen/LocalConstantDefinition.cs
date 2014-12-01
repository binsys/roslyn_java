﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.Cci;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CodeGen
{
    /// <summary>
    /// We need a CCI representation for local constants because they are emitted as locals in
    /// PDB scopes to improve the debugging experience (see LocalScopeProvider.GetConstantsInScope).
    /// </summary>
    internal class LocalConstantDefinition : ILocalDefinition
    {
        private readonly string name;
        private readonly Location location;
        private readonly IMetadataConstant compileTimeValue;
        private readonly bool isDynamic;

        //Gives the synthesized dynamic attributes of the local definition
        private readonly ImmutableArray<TypedConstant> dynamicTransformFlags;

        public LocalConstantDefinition(string name, Location location, IMetadataConstant compileTimeValue, bool isDynamic = false,
            ImmutableArray<TypedConstant> dynamicTransformFlags = default(ImmutableArray<TypedConstant>))
        {
            Debug.Assert(!string.IsNullOrEmpty(name));
            Debug.Assert(compileTimeValue != null);

            this.name = name;
            this.location = location;
            this.compileTimeValue = compileTimeValue;
            this.isDynamic = isDynamic;
            this.dynamicTransformFlags = dynamicTransformFlags;
        }

        public string Name
        {
            get { return name; }
        }

        public Location Location
        {
            get { return location; }
        }

        public IMetadataConstant CompileTimeValue
        {
            get { return compileTimeValue; }
        }

        public ITypeReference Type
        {
            get { return this.compileTimeValue.Type; }
        }

        public bool IsConstant
        {
            get { return true; }
        }

        public IEnumerable<Microsoft.Cci.ICustomModifier> CustomModifiers
        {
            get { return SpecializedCollections.EmptyEnumerable<Microsoft.Cci.ICustomModifier>(); }
        }

        public bool IsModified
        {
            get { return false; }
        }

        public bool IsPinned
        {
            get { return false; }
        }

        public bool IsReference
        {
            get { return false; }
        }

        public bool IsDynamic
        {
            get { return this.isDynamic; }
        }

        public bool IsCompilerGenerated
        {
            get { return false; }
        }

        public ImmutableArray<TypedConstant> DynamicTransformFlags
        {
            get { return this.dynamicTransformFlags; }
        }
    }
}