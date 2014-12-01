﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Immutable;
using Roslyn.Utilities;
using System.Collections.Generic;
using Cci = Microsoft.Cci;

namespace Microsoft.CodeAnalysis.Emit.NoPia
{
    internal sealed class VtblGap : Cci.IMethodDefinition
    {
        public readonly Cci.ITypeDefinition ContainingType;
        private readonly string name;

        public VtblGap(Cci.ITypeDefinition containingType, string name)
        {
            this.ContainingType = containingType;
            this.name = name;
        }

        Cci.IMethodBody Cci.IMethodDefinition.GetBody(Context context)
        {
            return null;
        }

        IEnumerable<Cci.IGenericMethodParameter> Cci.IMethodDefinition.GenericParameters
        {
            get { return SpecializedCollections.EmptyEnumerable<Cci.IGenericMethodParameter>(); }
        }

        bool Cci.IMethodDefinition.HasDeclarativeSecurity
        {
            get { return false; }
        }

        bool Cci.IMethodDefinition.IsAbstract
        {
            get { return false; }
        }

        bool Cci.IMethodDefinition.IsAccessCheckedOnOverride
        {
            get { return false; }
        }

        bool Cci.IMethodDefinition.IsConstructor
        {
            get { return false; }
        }

        bool Cci.IMethodDefinition.IsExternal
        {
            get { return false; }
        }

        bool Cci.IMethodDefinition.IsHiddenBySignature
        {
            get { return false; }
        }

        bool Cci.IMethodDefinition.IsNewSlot
        {
            get { return false; }
        }

        bool Cci.IMethodDefinition.IsPlatformInvoke
        {
            get { return false; }
        }

        bool Cci.IMethodDefinition.IsRuntimeSpecial
        {
            get { return true; }
        }

        bool Cci.IMethodDefinition.IsSealed
        {
            get { return false; }
        }

        bool Cci.IMethodDefinition.IsSpecialName
        {
            get { return true; }
        }

        bool Cci.IMethodDefinition.IsStatic
        {
            get { return false; }
        }

        bool Cci.IMethodDefinition.IsVirtual
        {
            get { return false; }
        }

        System.Reflection.MethodImplAttributes Cci.IMethodDefinition.GetImplementationAttributes(Context context)
        {
            return System.Reflection.MethodImplAttributes.Managed | System.Reflection.MethodImplAttributes.Runtime;
        }

        IEnumerable<Cci.IParameterDefinition> Cci.IMethodDefinition.Parameters
        {
            get { return SpecializedCollections.EmptyEnumerable<Cci.IParameterDefinition>(); }
        }

        Cci.IPlatformInvokeInformation Cci.IMethodDefinition.PlatformInvokeData
        {
            get { return null; }
        }

        bool Cci.IMethodDefinition.RequiresSecurityObject
        {
            get { return false; }
        }

        IEnumerable<Cci.ICustomAttribute> Cci.IMethodDefinition.ReturnValueAttributes
        {
            get { return SpecializedCollections.EmptyEnumerable<Cci.ICustomAttribute>(); }
        }

        bool Cci.IMethodDefinition.ReturnValueIsMarshalledExplicitly
        {
            get { return false; }
        }

        Cci.IMarshallingInformation Cci.IMethodDefinition.ReturnValueMarshallingInformation
        {
            get { return null; }
        }

        ImmutableArray<byte> Cci.IMethodDefinition.ReturnValueMarshallingDescriptor
        {
            get { return default(ImmutableArray<byte>); }
        }

        IEnumerable<Cci.SecurityAttribute> Cci.IMethodDefinition.SecurityAttributes
        {
            get { return SpecializedCollections.EmptyEnumerable<Cci.SecurityAttribute>(); }
        }

        Cci.ITypeDefinition Cci.ITypeDefinitionMember.ContainingTypeDefinition
        {
            get { return ContainingType; }
        }

        Cci.TypeMemberVisibility Cci.ITypeDefinitionMember.Visibility
        {
            get { return Cci.TypeMemberVisibility.Public; }
        }

        Cci.ITypeReference Cci.ITypeMemberReference.GetContainingType(Context context)
        {
            return ContainingType;
        }

        IEnumerable<Cci.ICustomAttribute> Cci.IReference.GetAttributes(Context context)
        {
            return SpecializedCollections.EmptyEnumerable<Cci.ICustomAttribute>();
        }

        void Cci.IReference.Dispatch(Cci.MetadataVisitor visitor)
        {
            visitor.Visit((Cci.IMethodDefinition)this);
        }

        Cci.IDefinition Cci.IReference.AsDefinition(Context context)
        {
            return this;
        }

        string Cci.INamedEntity.Name
        {
            get { return name; }
        }

        bool Cci.IMethodReference.AcceptsExtraArguments
        {
            get { return false; }
        }

        ushort Cci.IMethodReference.GenericParameterCount
        {
            get { return 0; }
        }

        bool Cci.IMethodReference.IsGeneric
        {
            get { return false; }
        }

        Cci.IMethodDefinition Cci.IMethodReference.GetResolvedMethod(Context context)
        {
            return this;
        }

        IEnumerable<Cci.IParameterTypeInformation> Cci.IMethodReference.ExtraParameters
        {
            get { return SpecializedCollections.EmptyEnumerable<Cci.IParameterTypeInformation>(); }
        }

        Cci.IGenericMethodInstanceReference Cci.IMethodReference.AsGenericMethodInstanceReference
        {
            get { return null; }
        }

        Cci.ISpecializedMethodReference Cci.IMethodReference.AsSpecializedMethodReference
        {
            get { return null; }
        }

        Cci.CallingConvention Cci.ISignature.CallingConvention
        {
            get { return Cci.CallingConvention.Default | Cci.CallingConvention.HasThis; }
        }

        ushort Cci.ISignature.ParameterCount
        {
            get { return 0; }
        }

        IEnumerable<Cci.IParameterTypeInformation> Cci.ISignature.GetParameters(Context context)
        {
            return SpecializedCollections.EmptyEnumerable<Cci.IParameterTypeInformation>();
        }

        IEnumerable<Cci.ICustomModifier> Cci.ISignature.ReturnValueCustomModifiers
        {
            get { return SpecializedCollections.EmptyEnumerable<Cci.ICustomModifier>(); }
        }

        bool Cci.ISignature.ReturnValueIsByRef
        {
            get { return false; }
        }

        bool Cci.ISignature.ReturnValueIsModified
        {
            get { return false; }
        }

        Cci.ITypeReference Cci.ISignature.GetType(Context context)
        {
            return context.Module.GetPlatformType(Cci.PlatformType.SystemVoid, context);
        }
    }
}
