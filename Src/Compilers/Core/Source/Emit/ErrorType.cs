﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Reflection.Metadata;
using Roslyn.Utilities;
using Cci = Microsoft.Cci;

namespace Microsoft.CodeAnalysis.Emit
{
    /// <summary>
    /// Error type symbols should be replaced with an object of this class 
    /// in the translation layer for emit.
    /// </summary>
    internal class ErrorType : Cci.INamespaceTypeReference
    {
        public static readonly ErrorType Singleton = new ErrorType();

        /// <summary>
        /// For the name we will use a word "Error" followed by a guid, generated on the spot.
        /// </summary>
        private static readonly string name = "Error" + Guid.NewGuid().ToString("B");

        Cci.IUnitReference Cci.INamespaceTypeReference.GetUnit(Context context)
        {
            return ErrorAssembly.Singleton;
        }

        string Cci.INamespaceTypeReference.NamespaceName
        {
            get
            {
                return "";
            }
        }

        ushort Cci.INamedTypeReference.GenericParameterCount
        {
            get
            {
                return 0;
            }
        }

        bool Cci.INamedTypeReference.MangleName
        {
            get
            {
                return false;
            }
        }

        bool Cci.ITypeReference.IsEnum
        {
            get
            {
                return false;
            }
        }

        bool Cci.ITypeReference.IsValueType
        {
            get
            {
                return false;
            }
        }

        Cci.ITypeDefinition Cci.ITypeReference.GetResolvedType(Context context)
        {
            return null;
        }

        Cci.PrimitiveTypeCode Cci.ITypeReference.TypeCode(Context context)
        {
            return Cci.PrimitiveTypeCode.NotPrimitive;
        }

        TypeHandle Cci.ITypeReference.TypeDef
        {
            get
            {
                return default(TypeHandle);
            }
        }

        Cci.IGenericMethodParameterReference Cci.ITypeReference.AsGenericMethodParameterReference
        {
            get
            {
                return null;
            }
        }

        Cci.IGenericTypeInstanceReference Cci.ITypeReference.AsGenericTypeInstanceReference
        {
            get
            {
                return null;
            }
        }

        Cci.IGenericTypeParameterReference Cci.ITypeReference.AsGenericTypeParameterReference
        {
            get
            {
                return null;
            }
        }

        Cci.INamespaceTypeDefinition Cci.ITypeReference.AsNamespaceTypeDefinition(Context context)
        {
            return null;
        }

        Cci.INamespaceTypeReference Cci.ITypeReference.AsNamespaceTypeReference
        {
            get
            {
                return this;
            }
        }

        Cci.INestedTypeDefinition Cci.ITypeReference.AsNestedTypeDefinition(Context context)
        {
            return null;
        }

        Cci.INestedTypeReference Cci.ITypeReference.AsNestedTypeReference
        {
            get
            {
                return null;
            }
        }

        Cci.ISpecializedNestedTypeReference Cci.ITypeReference.AsSpecializedNestedTypeReference
        {
            get
            {
                return null;
            }
        }

        Cci.ITypeDefinition Cci.ITypeReference.AsTypeDefinition(Context context)
        {
            return null;
        }

        System.Collections.Generic.IEnumerable<Cci.ICustomAttribute> Cci.IReference.GetAttributes(Context context)
        {
            return SpecializedCollections.EmptyEnumerable<Cci.ICustomAttribute>();
        }

        void Cci.IReference.Dispatch(Cci.MetadataVisitor visitor)
        {
            visitor.Visit((Cci.INamespaceTypeReference)this);
        }

        Cci.IDefinition Cci.IReference.AsDefinition(Context context)
        {
            return null;
        }

        string Cci.INamedEntity.Name
        {
            get
            {
                return name;
            }
        }

        /// <summary>
        /// A fake containing assembly for an ErrorType object.
        /// </summary>
        private class ErrorAssembly : Cci.IAssemblyReference
        {
            public static readonly ErrorAssembly Singleton = new ErrorAssembly();
            /// <summary>
            /// For the name we will use a word "Error" followed by a guid, generated on the spot.
            /// </summary>
            private static readonly string name = "Error" + Guid.NewGuid().ToString("B");

            string Cci.IAssemblyReference.Culture
            {
                get
                {
                    return "";
                }
            }

            bool Cci.IAssemblyReference.IsRetargetable
            {
                get
                {
                    return false;
                }
            }

            System.Reflection.AssemblyContentType Cci.IAssemblyReference.ContentType
            {
                get
                {
                    return System.Reflection.AssemblyContentType.Default;
                }
            }

            System.Collections.Generic.IEnumerable<byte> Cci.IAssemblyReference.PublicKeyToken
            {
                get
                {
                    return null;
                }
            }

            System.Version Cci.IAssemblyReference.Version
            {
                get
                {
                    return new System.Version(0, 0, 0, 0);
                }
            }

            Cci.IAssemblyReference Cci.IModuleReference.GetContainingAssembly(Context context)
            {
                return this;
            }

            System.Collections.Generic.IEnumerable<Cci.ICustomAttribute> Cci.IReference.GetAttributes(Context context)
            {
                return SpecializedCollections.EmptyEnumerable<Cci.ICustomAttribute>();
            }

            void Cci.IReference.Dispatch(Cci.MetadataVisitor visitor)
            {
                visitor.Visit((Cci.IAssemblyReference)this);
            }

            Cci.IDefinition Cci.IReference.AsDefinition(Context context)
            {
                return null;
            }

            string Cci.INamedEntity.Name
            {
                get
                {
                    return name;
                }
            }
        }
    }
}