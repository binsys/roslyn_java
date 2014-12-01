﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Immutable;
using Cci = Microsoft.Cci;

namespace Microsoft.CodeAnalysis.Emit.NoPia
{
    internal abstract partial class EmbeddedTypesManager<
        TPEModuleBuilder,
        TEmbeddedTypesManager,
        TSyntaxNode,
        TAttributeData,
        TSymbol,
        TAssemblySymbol,
        TNamedTypeSymbol,
        TFieldSymbol,
        TMethodSymbol,
        TEventSymbol,
        TPropertySymbol,
        TParameterSymbol,
        TTypeParameterSymbol,
        TEmbeddedType,
        TEmbeddedField,
        TEmbeddedMethod,
        TEmbeddedEvent,
        TEmbeddedProperty,
        TEmbeddedParameter,
        TEmbeddedTypeParameter>
    {
        internal abstract class CommonEmbeddedField : CommonEmbeddedMember<TFieldSymbol>, Cci.IFieldDefinition
        {
            public readonly TEmbeddedType ContainingType;

            protected CommonEmbeddedField(TEmbeddedType containingType, TFieldSymbol underlyingField) :
                base(underlyingField)
            {
                this.ContainingType = containingType;
            }

            public TFieldSymbol UnderlyingField
            {
                get
                {
                    return UnderlyingSymbol;
                }
            }

            protected abstract Cci.IMetadataConstant GetCompileTimeValue(Context context);
            protected abstract bool IsCompileTimeConstant { get; }
            protected abstract bool IsNotSerialized { get; }
            protected abstract bool IsReadOnly { get; }
            protected abstract bool IsRuntimeSpecial { get; }
            protected abstract bool IsSpecialName { get; }
            protected abstract bool IsStatic { get; }
            protected abstract bool IsMarshalledExplicitly { get; }
            protected abstract Cci.IMarshallingInformation MarshallingInformation { get; }
            protected abstract ImmutableArray<byte> MarshallingDescriptor { get; }
            protected abstract int? TypeLayoutOffset { get; }
            protected abstract Cci.TypeMemberVisibility Visibility { get; }
            protected abstract string Name { get; }

            Cci.IMetadataConstant Cci.IFieldDefinition.GetCompileTimeValue(Context context)
            {
                return GetCompileTimeValue(context);
            }

            Cci.ISectionBlock Cci.IFieldDefinition.FieldMapping
            {
                get
                {
                    return null;
                }
            }

            bool Cci.IFieldDefinition.IsCompileTimeConstant
            {
                get
                {
                    return IsCompileTimeConstant; ;
                }
            }

            bool Cci.IFieldDefinition.IsMapped
            {
                get
                {
                    return false;
                }
            }

            bool Cci.IFieldDefinition.IsNotSerialized
            {
                get
                {
                    return IsNotSerialized;
                }
            }

            bool Cci.IFieldDefinition.IsReadOnly
            {
                get
                {
                    return IsReadOnly;
                }
            }

            bool Cci.IFieldDefinition.IsRuntimeSpecial
            {
                get
                {
                    return IsRuntimeSpecial;
                }
            }

            bool Cci.IFieldDefinition.IsSpecialName
            {
                get
                {
                    return IsSpecialName;
                }
            }

            bool Cci.IFieldDefinition.IsStatic
            {
                get
                {
                    return IsStatic;
                }
            }

            bool Cci.IFieldDefinition.IsMarshalledExplicitly
            {
                get
                {
                    return IsMarshalledExplicitly;
                }
            }

            Cci.IMarshallingInformation Cci.IFieldDefinition.MarshallingInformation
            {
                get
                {
                    return MarshallingInformation;
                }
            }

            ImmutableArray<byte> Cci.IFieldDefinition.MarshallingDescriptor
            {
                get
                {
                    return MarshallingDescriptor;
                }
            }

            uint Cci.IFieldDefinition.Offset
            {
                get
                {
                    var offset = TypeLayoutOffset;
                    return (uint)(offset ?? 0);
                }
            }

            Cci.ITypeDefinition Cci.ITypeDefinitionMember.ContainingTypeDefinition
            {
                get
                {
                    return ContainingType;
                }
            }

            Cci.TypeMemberVisibility Cci.ITypeDefinitionMember.Visibility
            {
                get
                {
                    return Visibility;
                }
            }

            Cci.ITypeReference Cci.ITypeMemberReference.GetContainingType(Context context)
            {
                return ContainingType;
            }

            void Cci.IReference.Dispatch(Cci.MetadataVisitor visitor)
            {
                visitor.Visit((Cci.IFieldDefinition)this);
            }

            Cci.IDefinition Cci.IReference.AsDefinition(Context context)
            {
                return this;
            }

            string Cci.INamedEntity.Name
            {
                get
                {
                    return Name;
                }
            }

            Cci.ITypeReference Cci.IFieldReference.GetType(Context context)
            {
                return UnderlyingField.GetType(context);
            }

            Cci.IFieldDefinition Cci.IFieldReference.GetResolvedField(Context context)
            {
                return this;
            }

            Cci.ISpecializedFieldReference Cci.IFieldReference.AsSpecializedFieldReference
            {
                get
                {
                    return null;
                }
            }

            bool Cci.IFieldReference.IsContextualNamedEntity
            {
                get
                {
                    return false;
                }
            }
        }
    }
}
