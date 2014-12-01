﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Reflection.Metadata;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{
    internal abstract class MetadataDecoder<TypeSymbol, MethodSymbol, FieldSymbol, AssemblySymbol, Symbol>
        where TypeSymbol : class, Symbol, ITypeSymbol
        where MethodSymbol : class, Symbol, IMethodSymbol
        where FieldSymbol : class, Symbol, IFieldSymbol
        where AssemblySymbol : class, Symbol, IAssemblySymbol
        where Symbol : class, ISymbol
    {
        protected readonly PEModule Module;

        // Identity of an assembly containing the module, or null if the module is a standalone module
        protected readonly AssemblyIdentity ContainingAssemblyIdentity;

        internal MetadataDecoder(PEModule module, AssemblyIdentity containingAssemblyIdentity)
        {
            Debug.Assert(module != null);
            this.Module = module;
            this.ContainingAssemblyIdentity = containingAssemblyIdentity;
        }

        internal TypeSymbol GetTypeOfToken(Handle token)
        {
            bool isNoPiaLocalType;
            return GetTypeOfToken(token, out isNoPiaLocalType);
        }

        internal TypeSymbol GetTypeOfToken(Handle token, out bool isNoPiaLocalType)
        {
            Debug.Assert(!token.IsNil);

            TypeSymbol type = null;
            HandleType tokenType = token.HandleType;

            if (tokenType == HandleType.Type)
            {
                type = GetTypeOfTypeDef((TypeHandle)token, out isNoPiaLocalType, isContainingType: false);
            }
            else if (tokenType == HandleType.TypeSpecification)
            {
                isNoPiaLocalType = false;
                type = GetTypeOfTypeSpec((TypeSpecificationHandle)token);
            }
            else if (tokenType == HandleType.TypeReference)
            {
                type = GetTypeOfTypeRef((TypeReferenceHandle)token, out isNoPiaLocalType);
            }
            else
            {
                isNoPiaLocalType = false;
                type = GetUnsupportedMetadataTypeSymbol();
            }

            Debug.Assert(type != null);
            return type;
        }

        private TypeSymbol GetTypeOfTypeSpec(TypeSpecificationHandle typeSpec)
        {
            TypeSymbol ptype;

            try
            {
                BlobReader memoryReader = this.Module.GetTypeSpecificationSignatureReaderOrThrow(typeSpec);

                bool refersToNoPiaLocalType;
                ptype = DecodeTypeOrThrow(ref memoryReader, out refersToNoPiaLocalType);
            }
            catch (BadImageFormatException mrEx)
            {
                ptype = GetUnsupportedMetadataTypeSymbol(mrEx); // an exception from metadata reader.
            }
            catch (UnsupportedSignatureContent)
            {
                ptype = GetUnsupportedMetadataTypeSymbol(); // unsupported signature content
            }

            return ptype;
        }

        /// <exception cref="UnsupportedSignatureContent">If the encoded type is invalid.</exception>
        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        private TypeSymbol DecodeTypeOrThrow(ref BlobReader ppSig, out bool refersToNoPiaLocalType)
        {
            SignatureTypeCode typeCode = ppSig.ReadSignatureTypeCode();
            return DecodeTypeOrThrow(ref ppSig, typeCode, out refersToNoPiaLocalType);
        }

        /// <exception cref="UnsupportedSignatureContent">If the encoded type is invalid.</exception>
        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        private TypeSymbol DecodeTypeOrThrow(ref BlobReader ppSig, SignatureTypeCode typeCode, out bool refersToNoPiaLocalType)
        {
            TypeSymbol typeSymbol;
            int paramPosition;
            ImmutableArray<ModifierInfo> modifiers;

            refersToNoPiaLocalType = false;

            // Switch on the type.
            switch (typeCode)
            {
                case SignatureTypeCode.Void:
                case SignatureTypeCode.Boolean:
                case SignatureTypeCode.SByte:
                case SignatureTypeCode.Byte:
                case SignatureTypeCode.Int16:
                case SignatureTypeCode.UInt16:
                case SignatureTypeCode.Int32:
                case SignatureTypeCode.UInt32:
                case SignatureTypeCode.Int64:
                case SignatureTypeCode.UInt64:
                case SignatureTypeCode.Single:
                case SignatureTypeCode.Double:
                case SignatureTypeCode.Char:
                case SignatureTypeCode.String:
                case SignatureTypeCode.IntPtr:
                case SignatureTypeCode.UIntPtr:
                case SignatureTypeCode.Object:
                case SignatureTypeCode.TypedReference:
                    typeSymbol = GetSpecialType(typeCode.ToSpecialType());
                    break;

                case SignatureTypeCode.TypeHandle:
                    typeSymbol = ResolveSignatureTypeHandleOrThrow(ref ppSig, out refersToNoPiaLocalType);
                    break;

                case SignatureTypeCode.Array:
                    int countOfDimensions;
                    int countOfBounds;
                    int countOfLowerBounds;

                    typeSymbol = DecodeTypeOrThrow(ref ppSig, out refersToNoPiaLocalType);
                    if (!TryReadCompressedInteger(ref ppSig, out countOfDimensions) ||
                        !TryReadCompressedInteger(ref ppSig, out countOfBounds))
                    {
                        throw new UnsupportedSignatureContent();
                    }

                    // If bounds are specified, ignore them -- we don't support it
                    for (int i = 0; i < countOfBounds; i++)
                    {
                        int _;
                        TryReadCompressedInteger(ref ppSig, out _);
                    }

                    if (!TryReadCompressedInteger(ref ppSig, out countOfLowerBounds))
                    {
                        throw new UnsupportedSignatureContent();
                    }

                    // Also ignore lower bounds since we don't support anything but zero
                    for (int i = 0; i < countOfLowerBounds; i++)
                    {
                        int _;
                        TryReadCompressedInteger(ref ppSig, out _);
                    }

                    typeSymbol = GetArrayTypeSymbol((int)countOfDimensions, typeSymbol);
                    break;

                case SignatureTypeCode.SZArray:
                    modifiers = DecodeModifiersOrThrow(ref ppSig, out typeCode);
                    typeSymbol = DecodeTypeOrThrow(ref ppSig, typeCode, out refersToNoPiaLocalType);
                    typeSymbol = GetSZArrayTypeSymbol(typeSymbol, modifiers);
                    break;

                case SignatureTypeCode.Pointer:
                    modifiers = DecodeModifiersOrThrow(ref ppSig, out typeCode);
                    typeSymbol = DecodeTypeOrThrow(ref ppSig, typeCode, out refersToNoPiaLocalType);
                    typeSymbol = MakePointerTypeSymbol(typeSymbol, modifiers);
                    break;

                case SignatureTypeCode.GenericTypeParameter:
                    if (!TryReadCompressedInteger(ref ppSig, out paramPosition))
                    {
                        throw new UnsupportedSignatureContent();
                    }

                    typeSymbol = GetGenericTypeParamSymbol(paramPosition);
                    break;

                case SignatureTypeCode.GenericMethodParameter:
                    if (!TryReadCompressedInteger(ref ppSig, out paramPosition))
                    {
                        throw new UnsupportedSignatureContent();
                    }

                    typeSymbol = GetGenericMethodTypeParamSymbol(paramPosition);
                    break;

                case SignatureTypeCode.GenericTypeInstance:
                    SignatureTypeCode elementTypeCode = ppSig.ReadSignatureTypeCode();
                    if (elementTypeCode != SignatureTypeCode.TypeHandle)
                    {
                        throw new UnsupportedSignatureContent();
                    }

                    Handle tokenGeneric = ppSig.ReadTypeHandle();
                    int argumentCount;
                    if (!TryReadCompressedInteger(ref ppSig, out argumentCount))
                    {
                        throw new UnsupportedSignatureContent();
                    }

                    TypeSymbol generic = GetTypeOfToken(tokenGeneric, out refersToNoPiaLocalType);
                    Debug.Assert(!refersToNoPiaLocalType || generic.TypeKind == TypeKind.Error);

                    var arguments = new TypeSymbol[argumentCount];
                    var argumentRefersToNoPiaLocalType = new bool[argumentCount];

                    for (int argumentIndex = 0; argumentIndex < argumentCount; argumentIndex++)
                    {
                        arguments[argumentIndex] = DecodeTypeOrThrow(ref ppSig, out argumentRefersToNoPiaLocalType[argumentIndex]);
                    }

                    // The instantiated type might have a generic parent, in which case some or all of the type
                    // arguments might actually be for the parent.

                    typeSymbol = SubstituteTypeParameters(generic, arguments, argumentRefersToNoPiaLocalType);

                    foreach (bool flag in argumentRefersToNoPiaLocalType)
                    {
                        if (flag)
                        {
                            refersToNoPiaLocalType = true;
                            break;
                        }
                    }

                    break;

                default:
                    throw new UnsupportedSignatureContent();
            }

            return typeSymbol;
        }

        /// <exception cref="UnsupportedSignatureContent">If the encoded type is invalid.</exception>
        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        private TypeSymbol ResolveSignatureTypeHandleOrThrow(ref BlobReader ppSig, out bool isNoPiaLocalType)
        {
            TypeSymbol typeSymbol;

            Handle token = ppSig.ReadTypeHandle();
            HandleType tokenType = token.HandleType;

            if (tokenType == HandleType.Type)
            {
                typeSymbol = GetTypeOfTypeDef((TypeHandle)token, out isNoPiaLocalType, isContainingType: false);
            }
            else if (tokenType == HandleType.TypeReference)
            {
                typeSymbol = GetTypeOfTypeRef((TypeReferenceHandle)token, out isNoPiaLocalType);
            }
            else
            {
                throw new UnsupportedSignatureContent();
            }

            Debug.Assert(typeSymbol != null);

            // tomat: Breaking change
            // Metadata spec II.23.2.16 (Short form signatures) requires primitive types to be encoded using a short form:
            // 
            //  "The general specification for signatures leaves some leeway in how to encode certain items. For
            //   example, it appears valid to encode a String as either 
            //     long-form: (ELEMENT_TYPE_CLASS, TypeRef-to-System.String )
            //     short-form: ELEMENT_TYPE_STRING
            //   Only the short form is valid."
            // 
            // Native compilers accept long form signatures (actually IMetadataImport does).
            // When a MemberRef s emitted the signature blob is copied from the metadata reference to the resulting assembly. 
            // Such assembly doesn't PEVerify but the CLR type loader matches the MemberRef with the original signature 
            // (since they are identical copies).
            // 
            // Roslyn doesn't copy signature blobs to the resulting assembly, it encodes the MemberRef using the 
            // correct short type codes. If we allowed long forms in a signature we would produce IL that PEVerifies but
            // the type loader isn't able to load it since the MemberRef signature wouldn't match the original signature.
            // 
            // Rather then producing broken code we report an error at compile time.

            if (typeSymbol.SpecialType.HasShortFormSignatureEncoding())
            {
                throw new UnsupportedSignatureContent();
            }

            return typeSymbol;
        }

        // MetaImport::GetTypeOfTypeRef
        private TypeSymbol GetTypeOfTypeRef(TypeReferenceHandle typeRef, out bool isNoPiaLocalType)
        {
            // This is a cache similar to one used by native compiler in MetaImport::GetTypeOfTypeRef.
            // TypeRef tokens are unique only within a Module
            ConcurrentDictionary<TypeReferenceHandle, TypeSymbol> cache = GetTypeRefHandleToTypeMap();
            TypeSymbol result;

            if (cache != null && cache.TryGetValue(typeRef, out result))
            {
                isNoPiaLocalType = false; // We do not cache otherwise.
                return result;
            }

            try
            {
                string name, @namespace;
                Handle resolutionScope;
                Module.GetTypeRefPropsOrThrow(typeRef, out name, out @namespace, out resolutionScope);
                Debug.Assert(MetadataHelpers.IsValidMetadataIdentifier(name));
                MetadataTypeName mdName;

                if (@namespace.Length == 0)
                {
                    mdName = MetadataTypeName.FromTypeName(name);
                }
                else
                {
                    mdName = MetadataTypeName.FromNamespaceAndTypeName(@namespace, name);
                }

                result = GetTypeByNameOrThrow(ref mdName, resolutionScope, out isNoPiaLocalType);
            }
            catch (BadImageFormatException mrEx)
            {
                result = GetUnsupportedMetadataTypeSymbol(mrEx); // an exception from metadata reader.
                isNoPiaLocalType = false;
            }

            Debug.Assert(result != null);

            // Cache the result, but only if it is not a local type because the cache doesn't retain this information.
            if (cache != null && !isNoPiaLocalType)
            {
                TypeSymbol result1 = cache.GetOrAdd(typeRef, result);
                Debug.Assert(result1.Equals(result));
            }

            return result;
        }

        // MetaImport::GetTypeByName
        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        private TypeSymbol GetTypeByNameOrThrow(
            ref MetadataTypeName fullName,
            Handle tokenResolutionScope,
            out bool isNoPiaLocalType)
        {
            HandleType tokenType = tokenResolutionScope.HandleType;

            // TODO: I believe refs can be parented by a def tokens too, not common, but.
            //       Should also do NoPia related checks.

            // The resolution scope should be either a type ref, an assembly or a module.
            if (tokenType == HandleType.TypeReference)
            {
                TypeSymbol psymContainer = null;

                psymContainer = GetTypeOfToken(tokenResolutionScope);

                Debug.Assert(fullName.NamespaceName.Length == 0);
                isNoPiaLocalType = false;
                return LookupNestedTypeDefSymbol(psymContainer, ref fullName);
            }
            else if (tokenType == HandleType.AssemblyReference)
            {
                // TODO: Can refer to the containing assembly?
                isNoPiaLocalType = false;
                return LookupTopLevelTypeDefSymbol(Module.GetAssemblyReferenceIndexOrThrow((AssemblyReferenceHandle)tokenResolutionScope), ref fullName);
            }
            else if (tokenType == HandleType.ModuleReference)
            {
                return LookupTopLevelTypeDefSymbol(Module.GetModuleRefNameOrThrow((ModuleReferenceHandle)tokenResolutionScope),
                                                   ref fullName,
                                                   out isNoPiaLocalType);
            }
            else if (tokenResolutionScope == Handle.ModuleDefinition)
            {
                // The last case is a little bit strange.  Here, the TypeRef's TypeDef
                // lives in the same module as the TypeRef itself.  This is represented
                // as a resolution scope of 0x00000001.
                return LookupTopLevelTypeDefSymbol(ref fullName, out isNoPiaLocalType);
            }
            else
            {
                isNoPiaLocalType = false;
                return GetUnsupportedMetadataTypeSymbol();
            }
        }

        private TypeSymbol GetTypeOfTypeDef(TypeHandle typeDef)
        {
            bool isNoPiaLocalType;
            return GetTypeOfTypeDef(typeDef, out isNoPiaLocalType, isContainingType: false);
        }

        private TypeSymbol GetTypeOfTypeDef(TypeHandle typeDef, out bool isNoPiaLocalType, bool isContainingType)
        {
            isNoPiaLocalType = false;

            try
            {
                // This is a cache similar to one used in MetaImport::GetTypeOfToken by native compiler.
                // TypeDef tokens are unique within Module.
                // This cache makes lookup of top level types about twice as fast, about three times as fast if 
                // EmittedNameToTypeMap in LookupTopLevelType doesn't contain the name. 
                // It is likely that gain for nested types will be bigger because we don’t cache names of nested types.

                ConcurrentDictionary<TypeHandle, TypeSymbol> cache = GetTypeHandleToTypeMap();

                TypeSymbol result;

                if (cache != null && cache.TryGetValue(typeDef, out result))
                {
                    if (!Module.IsNestedTypeDefOrThrow(typeDef) && Module.IsNoPiaLocalType(typeDef))
                    {
                        isNoPiaLocalType = true;
                    }
                    else
                    {
                        isNoPiaLocalType = false;
                    }

                    return result;
                }

                MetadataTypeName mdName;
                string name = Module.GetTypeDefNameOrThrow(typeDef);
                Debug.Assert(MetadataHelpers.IsValidMetadataIdentifier(name));

                string interfaceGuid;
                string scope;
                string identifier;

                if (Module.IsNestedTypeDefOrThrow(typeDef))
                {
                    // first resolve nesting type 
                    TypeHandle containerTypeDef = Module.GetContainingTypeOrThrow(typeDef);

                    // invalid metadata?
                    if (containerTypeDef.IsNil)
                    {
                        isNoPiaLocalType = false;
                        return GetUnsupportedMetadataTypeSymbol();
                    }

                    TypeSymbol container = GetTypeOfTypeDef(containerTypeDef, out isNoPiaLocalType, isContainingType: true);

                    if (isNoPiaLocalType)
                    {
                        // Types nested into local types are not supported.
                        if (!isContainingType)
                        {
                            isNoPiaLocalType = false;
                        }

                        return GetUnsupportedMetadataTypeSymbol();
                    }
                    else
                    {
                        mdName = MetadataTypeName.FromTypeName(name);
                        return LookupNestedTypeDefSymbol(container, ref mdName);
                    }
                }
                else
                {
                    string namespaceName = Module.GetTypeDefNamespaceOrThrow(typeDef);

                    if (namespaceName.Length > 0)
                    {
                        mdName = MetadataTypeName.FromNamespaceAndTypeName(namespaceName, name);
                    }
                    else
                    {
                        // It is extremely difficult to hit this block because it is executed 
                        // only for types in the Global namespace and they are getting loaded 
                        // as soon as we start traversing Symbol Table, therefore, their TypeDef
                        // handle is getting cached and lookup in the cache succeeds. 
                        // Probably we can hit it if the first thing we do is to interrogate 
                        // Module/Assembly level attributes, which refer to a TypeDef in the 
                        // Global namespace.
                        mdName = MetadataTypeName.FromTypeName(name);
                    }

                    // Check if this is NoPia local type which should be substituted 
                    // with corresponding canonical type
                    if (Module.IsNoPiaLocalType(
                            typeDef,
                            out interfaceGuid,
                            out scope,
                            out identifier))
                    {
                        isNoPiaLocalType = true;

                        if (!Module.HasGenericParametersOrThrow(typeDef))
                        {
                            MetadataTypeName localTypeName = MetadataTypeName.FromNamespaceAndTypeName(mdName.NamespaceName, mdName.TypeName, forcedArity: 0);
                            result = SubstituteNoPiaLocalType(typeDef,
                                                ref localTypeName,
                                                interfaceGuid,
                                                scope,
                                                identifier);

                            if (result != null)
                            {
                                return result;
                            }
                        }
                        else
                        {
                            // Unification of generic local types is not supported 
                            result = GetUnsupportedMetadataTypeSymbol();

                            if (cache != null)
                            {
                                result = cache.GetOrAdd(typeDef, result);
                            }

                            return result;
                        }
                    }
                    else
                    {
                        isNoPiaLocalType = false;
                    }

                    result = LookupTopLevelTypeDefSymbol(ref mdName, out isNoPiaLocalType);
                    Debug.Assert(!isNoPiaLocalType);
                    return result;
                }
            }
            catch (BadImageFormatException mrEx)
            {
                return GetUnsupportedMetadataTypeSymbol(mrEx); // an exception from metadata reader.
            }
        }

        internal struct ModifierInfo
        {
            internal readonly bool IsOptional;
            internal readonly TypeSymbol Modifier;

            public ModifierInfo(bool isOptional, TypeSymbol modifer)
            {
                IsOptional = isOptional;
                Modifier = modifer;
            }
        }

        internal struct ParamInfo
        {
            internal bool IsByRef;
            internal bool HasByRefBeforeCustomModifiers;
            internal TypeSymbol Type;
            internal ParameterHandle Handle; // may be nil
            internal ImmutableArray<ModifierInfo> CustomModifiers;
        }

        internal struct LocalInfo
        {
            internal readonly bool IsPinned;
            internal readonly bool IsByRef;
            internal readonly TypeSymbol Type;
            internal readonly ImmutableArray<ModifierInfo> CustomModifiers;

            public LocalInfo(TypeSymbol type, ImmutableArray<ModifierInfo> customModifiers, bool isPinned, bool isByRef)
            {
                Type = type;
                CustomModifiers = customModifiers;
                IsPinned = isPinned;
                IsByRef = isByRef;
            }
        }

        /// <exception cref="UnsupportedSignatureContent">If the encoded type is invalid.</exception>
        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        private ImmutableArray<ModifierInfo> DecodeModifiersOrThrow(ref BlobReader signatureReader, out SignatureTypeCode typeCode)
        {
            ArrayBuilder<ModifierInfo> modifiers = null;

            for (; ;)
            {
                typeCode = signatureReader.ReadSignatureTypeCode();

                if (typeCode == SignatureTypeCode.RequiredModifier)
                {
                    throw new UnsupportedSignatureContent();
                }

                if (typeCode == SignatureTypeCode.OptionalModifier)
                {
                    Handle token = signatureReader.ReadTypeHandle();
                    ModifierInfo modifier = new ModifierInfo(true, GetTypeOfToken(token));

                    if (!IsAcceptableModOptModifier(token, modifier.Modifier))
                    {
                        throw new UnsupportedSignatureContent();
                    }

                    if (modifiers == null)
                    {
                        modifiers = ArrayBuilder<ModifierInfo>.GetInstance();
                    }

                    modifiers.Add(modifier);
                    continue;
                }

                break;
            }

            return (modifiers == null) ? default(ImmutableArray<ModifierInfo>) : modifiers.ToImmutableAndFree();
        }

        /// <summary>
        /// According to ECMA spec:
        ///  The CMOD_OPT or CMOD_REQD is followed by a metadata token that
        ///  indexes a row in the TypeDef table or the TypeRef table.
        /// i.e. No modopt in DecodeType (though it still works in DecodeModifier).
        /// </summary>
        private static bool IsAcceptableModOptModifier(Handle token, TypeSymbol modifier)
        {
            switch (token.HandleType)
            {
                case HandleType.Type:
                case HandleType.TypeReference:
                    return true;
                case HandleType.TypeSpecification:
                    // Section 23.2.7 of the CLI spec specifically says that this is not allowed (see comment on method),
                    // but, apparently, ilasm turns modopt(int32) into a TypeSpec.
                    if (modifier != null)
                    {
                        switch (modifier.SpecialType)
                        {
                            case SpecialType.System_Void:
                            case SpecialType.System_Boolean:
                            case SpecialType.System_SByte:
                            case SpecialType.System_Byte:
                            case SpecialType.System_Int16:
                            case SpecialType.System_UInt16:
                            case SpecialType.System_Int32:
                            case SpecialType.System_UInt32:
                            case SpecialType.System_Int64:
                            case SpecialType.System_UInt64:
                            case SpecialType.System_Single:
                            case SpecialType.System_Double:
                            case SpecialType.System_Char:
                            case SpecialType.System_String:
                            case SpecialType.System_Object:
                                return true;
                        }
                    }
                    return false;
                default:
                    return false;
            }
        }

        /// <exception cref="UnsupportedSignatureContent">If the encoded local variable type is invalid.</exception>
        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        internal ImmutableArray<LocalInfo> DecodeLocalSignatureOrThrow(BlobHandle signature)
        {
            if (signature.IsNil)
            {
                throw new UnsupportedSignatureContent();
            }

            byte callingConvention;
            BlobReader signatureReader = DecodeSignatureHeaderOrThrow(signature, out callingConvention);

            if (!SignatureHeader.IsLocalVarSignature(callingConvention))
            {
                throw new UnsupportedSignatureContent();
            }

            int localCount;
            int typeParameterCount;
            GetSignatureCountsOrThrow(ref signatureReader, callingConvention, out localCount, out typeParameterCount);
            Debug.Assert(typeParameterCount == 0);

            var locals = new LocalInfo[localCount];
            for (int i = 0; i < locals.Length; i++)
            {
                locals[i] = DecodeLocalVariableOrThrow(ref signatureReader);
            }

            if (signatureReader.RemainingBytes > 0)
            {
                throw new UnsupportedSignatureContent();
            }

            return ImmutableArray.Create(locals);
        }

        /// <exception cref="UnsupportedSignatureContent">If the encoded local variable type is invalid.</exception>
        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        internal LocalInfo DecodeLocalVariableOrThrow(ref BlobReader signatureReader)
        {
            SignatureTypeCode typeCode;

            var customModifiers = DecodeModifiersOrThrow(ref signatureReader, out typeCode);
            bool isByRef = false;
            bool isPinned = false;
            TypeSymbol typeSymbol;

            if (typeCode == SignatureTypeCode.Pinned)
            {
                isPinned = true;
                typeCode = signatureReader.ReadSignatureTypeCode();
            }

            if (typeCode == SignatureTypeCode.ByReference)
            {
                isByRef = true;
                typeCode = signatureReader.ReadSignatureTypeCode();
            }

            // TypedReference can't be by-ref or pinned:
            if (typeCode == SignatureTypeCode.TypedReference && (isByRef || isPinned))
            {
                typeSymbol = GetUnsupportedMetadataTypeSymbol();
            }
            else
            {
                try
                {
                    bool refersToNoPiaLocalType;
                    typeSymbol = DecodeTypeOrThrow(ref signatureReader, typeCode, out refersToNoPiaLocalType);
                }
                catch (UnsupportedSignatureContent)
                {
                    typeSymbol = GetUnsupportedMetadataTypeSymbol();
                }
            }

            return new LocalInfo(typeSymbol, customModifiers, isPinned, isByRef);
        }

        /// <exception cref="UnsupportedSignatureContent">If the encoded parameter type is invalid.</exception>
        private void DecodeParameterOrThrow(ref BlobReader signatureReader, /*out*/ ref ParamInfo info)
        {
            bool refersToNoPiaLocalType;

            SignatureTypeCode typeCode;
            info.CustomModifiers = DecodeModifiersOrThrow(ref signatureReader, out typeCode);

            if (typeCode == SignatureTypeCode.ByReference)
            {
                info.IsByRef = true;

                // The spec says that custom modifiers must precede SignatureTypeCode.ByReference, but the managed C++
                // compiler emits them in the reverse order.  In order to avoid breaking interop scenarios, we need to
                // support decoding (and later emitting) such signatures.
                // NOTE: We still don't support having SignatureTypeCode.ByReference in the middle of a list of custom modifiers.
                if (info.CustomModifiers.IsDefault)
                {
                    info.CustomModifiers = DecodeModifiersOrThrow(ref signatureReader, out typeCode);
                    info.HasByRefBeforeCustomModifiers = !info.CustomModifiers.IsDefault;

                    info.Type = DecodeTypeOrThrow(ref signatureReader, typeCode, out refersToNoPiaLocalType);
                }
                else
                {
                    info.Type = DecodeTypeOrThrow(ref signatureReader, out refersToNoPiaLocalType);
                }
            }
            else
            {
                info.Type = DecodeTypeOrThrow(ref signatureReader, typeCode, out refersToNoPiaLocalType);
            }
        }

        // MetaImport::DecodeMethodSignature
        internal ParamInfo[] GetSignatureForMethod(MethodHandle methodDef, out byte callingConvention, out BadImageFormatException metadataException, bool setParamHandles = true)
        {
            ParamInfo[] paramInfo = null;
            callingConvention = 0;

            try
            {
                BlobHandle signature = Module.GetMethodSignatureOrThrow(methodDef);
                BlobReader signatureReader = DecodeSignatureHeaderOrThrow(signature, out callingConvention);

                int typeParameterCount; //CONSIDER: expose to caller?
                paramInfo = DecodeSignatureParametersOrThrow(ref signatureReader, callingConvention, out typeParameterCount);

                if (setParamHandles)
                {
                    int paramInfoLength = paramInfo.Length;

                    // For each parameter, get corresponding row id from Param table. 
                    foreach (var param in Module.GetParametersOfMethodOrThrow(methodDef))
                    {
                        int sequenceNumber = Module.GetParameterSequenceNumberOrThrow(param);

                        // Ignore possible errors in parameter table.
                        if (sequenceNumber >= 0 && sequenceNumber < paramInfoLength && paramInfo[sequenceNumber].Handle.IsNil)
                        {
                            paramInfo[sequenceNumber].Handle = param;
                        }
                    }
                }

                metadataException = null;
            }
            catch (BadImageFormatException mrEx)
            {
                metadataException = mrEx;

                // An exception from metadata reader.
                if (paramInfo == null)
                {
                    // Pretend there are no parameters and capture error information in the return type.
                    paramInfo = new ParamInfo[1];
                    paramInfo[0].Type = GetUnsupportedMetadataTypeSymbol(mrEx);
                }
            }

            return paramInfo;
        }

        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        internal void GetSignatureCountsOrThrow(MethodHandle methodDef, out int parameterCount, out int typeParameterCount)
        {
            BlobHandle signature = Module.GetMethodSignatureOrThrow(methodDef);
            byte callingConvention;
            BlobReader signatureReader = DecodeSignatureHeaderOrThrow(signature, out callingConvention);

            GetSignatureCountsOrThrow(ref signatureReader, callingConvention, out parameterCount, out typeParameterCount);
        }

        internal ParamInfo[] GetSignatureForProperty(PropertyHandle handle, out byte callingConvention, out BadImageFormatException BadImageFormatException)
        {
            ParamInfo[] paramInfo = null;
            callingConvention = 0;

            try
            {
                var signature = Module.GetPropertySignatureOrThrow(handle);
                BlobReader signatureReader = DecodeSignatureHeaderOrThrow(signature, out callingConvention);

                int typeParameterCount; //CONSIDER: expose to caller?
                paramInfo = DecodeSignatureParametersOrThrow(ref signatureReader, callingConvention, out typeParameterCount);
                BadImageFormatException = null;
            }
            catch (BadImageFormatException mrEx)
            {
                BadImageFormatException = mrEx;

                // An exception from metadata reader.
                if (paramInfo == null)
                {
                    // Pretend there are no parameters and capture error information in the return type as well.
                    paramInfo = new ParamInfo[1];
                    paramInfo[0].Type = GetUnsupportedMetadataTypeSymbol(mrEx);
                }
            }

            return paramInfo;
        }

        internal byte GetCallingConventionForProperty(PropertyHandle handle)
        {
            try
            {
                var signature = Module.GetPropertySignatureOrThrow(handle);
                byte callingConvention;
                BlobReader signatureReader = DecodeSignatureHeaderOrThrow(signature, out callingConvention);
                return callingConvention;
            }
            catch (BadImageFormatException)
            {
                return 0;
            }
        }

        #region Custom Attributes

        /// <summary>
        /// Decodes attribute parameter type from method signature.
        /// </summary>
        /// <exception cref="UnsupportedSignatureContent">If the encoded parameter type is invalid.</exception>
        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        private void DecodeCustomAttributeParameterTypeOrThrow(ref BlobReader sigReader, out SerializationTypeCode typeCode, out TypeSymbol type, out SerializationTypeCode elementTypeCode, out TypeSymbol elementType, bool isElementType)
        {
            SignatureTypeCode paramTypeCode = sigReader.ReadSignatureTypeCode();

            if (paramTypeCode == SignatureTypeCode.SZArray)
            {
                if (isElementType)
                {
                    // nested arrays not allowed
                    throw new UnsupportedSignatureContent();
                }

                SerializationTypeCode unusedElementTypeCode;
                TypeSymbol unusedElementType;
                DecodeCustomAttributeParameterTypeOrThrow(ref sigReader, out elementTypeCode, out elementType, out unusedElementTypeCode, out unusedElementType, isElementType: true);
                type = GetSZArrayTypeSymbol(elementType, customModifiers: default(ImmutableArray<ModifierInfo>));
                typeCode = SerializationTypeCode.SZArray;
                return;
            }

            elementTypeCode = SerializationTypeCode.Invalid;
            elementType = null;

            switch (paramTypeCode)
            {
                case SignatureTypeCode.Object:
                case SignatureTypeCode.String:
                case SignatureTypeCode.Boolean:
                case SignatureTypeCode.Char:
                case SignatureTypeCode.SByte:
                case SignatureTypeCode.Byte:
                case SignatureTypeCode.Int16:
                case SignatureTypeCode.UInt16:
                case SignatureTypeCode.Int32:
                case SignatureTypeCode.UInt32:
                case SignatureTypeCode.Int64:
                case SignatureTypeCode.UInt64:
                case SignatureTypeCode.Single:
                case SignatureTypeCode.Double:
                    type = GetSpecialType(paramTypeCode.ToSpecialType());
                    typeCode = (SerializationTypeCode)paramTypeCode;
                    return;

                case SignatureTypeCode.TypeHandle:
                    // The type of the parameter can either be an enum type or System.Type.
                    bool isNoPiaLocalType;
                    type = ResolveSignatureTypeHandleOrThrow(ref sigReader, out isNoPiaLocalType);

                    var underlyingEnumType = GetEnumUnderlyingType(type);

                    // Spec: If the parameter kind is an enum -- simply store the value of the enum's underlying integer type.
                    if ((object)underlyingEnumType != null)
                    {
                        Debug.Assert(!isNoPiaLocalType);

                        // GetEnumUnderlyingType always returns a valid enum underlying type
                        typeCode = underlyingEnumType.SpecialType.ToSerializationType();
                        return;
                    }

                    if ((object)type == SystemTypeSymbol)
                    {
                        Debug.Assert(!isNoPiaLocalType);
                        typeCode = SerializationTypeCode.Type;
                        return;
                    }

                    break;
            }

            throw new UnsupportedSignatureContent();
        }

        /// <summary>
        /// Decodes attribute argument type from attribute blob (called FieldOrPropType in the spec).
        /// </summary>
        /// <exception cref="UnsupportedSignatureContent">If the encoded argument type is invalid.</exception>
        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        private void DecodeCustomAttributeFieldOrPropTypeOrThrow(ref BlobReader argReader, out SerializationTypeCode typeCode, out TypeSymbol type, out SerializationTypeCode elementTypeCode, out TypeSymbol elementType, bool isElementType)
        {
            typeCode = argReader.ReadSerializationTypeCode();

            // Spec:
            // The FieldOrPropType (typeCode) shall be exactly one of: ELEMENT_TYPE_BOOLEAN,
            // ELEMENT_TYPE_CHAR, ELEMENT_TYPE_I1, ELEMENT_TYPE_U1, ELEMENT_TYPE_I2,
            // ELEMENT_TYPE_U2, ELEMENT_TYPE_I4, ELEMENT_TYPE_U4, ELEMENT_TYPE_I8,
            // ELEMENT_TYPE_U8, ELEMENT_TYPE_R4, ELEMENT_TYPE_R8, ELEMENT_TYPE_STRING.
            // 
            // A single-dimensional, zero-based array is specified as a single byte 0x1D followed
            // by the FieldOrPropType of the element type. (See §II.23.1.16) An enum is
            // specified as a single byte 0x55 followed by a SerString.
            // 
            // tomat: The spec is missing ELEMENT_TYPE_TYPE.

            if (typeCode == SerializationTypeCode.SZArray)
            {
                if (isElementType)
                {
                    // nested array not allowed:
                    throw new UnsupportedSignatureContent();
                }

                SerializationTypeCode unusedElementTypeCode;
                TypeSymbol unusedElementType;
                DecodeCustomAttributeFieldOrPropTypeOrThrow(ref argReader, out elementTypeCode, out elementType, out unusedElementTypeCode, out unusedElementType, isElementType: true);
                type = GetSZArrayTypeSymbol(elementType, customModifiers: default(ImmutableArray<ModifierInfo>));
                return;
            }

            elementTypeCode = SerializationTypeCode.Invalid;
            elementType = null;

            switch (typeCode)
            {
                case SerializationTypeCode.TaggedObject:
                    // The spec isn't entirely clear in definition of TAGGED_OBJECT.
                    // The value decoding is the same as for OBJECT.
                    typeCode = SerializationTypeCode.Object;
                    type = GetSpecialType(SpecialType.System_Object);
                    return;

                case SerializationTypeCode.Enum:
                    string enumTypeName;
                    if (!PEModule.CrackStringInAttributeValue(out enumTypeName, ref argReader))
                    {
                        throw new UnsupportedSignatureContent();
                    }

                    type = GetTypeSymbolForSerializedType(enumTypeName);
                    var underlyingType = GetEnumUnderlyingType(type);
                    if ((object)underlyingType == null)
                    {
                        throw new UnsupportedSignatureContent();
                    }

                    // GetEnumUnderlyingType always returns a valid enum underlying type
                    typeCode = underlyingType.SpecialType.ToSerializationType();
                    return;

                case SerializationTypeCode.Type:
                    type = SystemTypeSymbol;
                    return;

                case SerializationTypeCode.String:
                case SerializationTypeCode.Boolean:
                case SerializationTypeCode.Char:
                case SerializationTypeCode.SByte:
                case SerializationTypeCode.Byte:
                case SerializationTypeCode.Int16:
                case SerializationTypeCode.UInt16:
                case SerializationTypeCode.Int32:
                case SerializationTypeCode.UInt32:
                case SerializationTypeCode.Int64:
                case SerializationTypeCode.UInt64:
                case SerializationTypeCode.Single:
                case SerializationTypeCode.Double:
                    type = GetSpecialType(((SignatureTypeCode)typeCode).ToSpecialType());
                    return;
            }

            throw new UnsupportedSignatureContent();
        }

        /// <exception cref="UnsupportedSignatureContent">If the encoded attribute argument is invalid.</exception>
        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        private TypedConstant DecodeCustomAttributeFixedArgumentOrThrow(ref BlobReader sigReader, ref BlobReader argReader)
        {
            SerializationTypeCode typeCode, elementTypeCode;
            TypeSymbol type, elementType;
            DecodeCustomAttributeParameterTypeOrThrow(ref sigReader, out typeCode, out type, out elementTypeCode, out elementType, isElementType: false);

            // arrays are allowed only on top-level:
            if (typeCode == SerializationTypeCode.SZArray)
            {
                return DecodeCustomAttributeElementArrayOrThrow(ref argReader, elementTypeCode, elementType, type);
            }

            return DecodeCustomAttributeElementOrThrow(ref argReader, typeCode, type);
        }

        /// <exception cref="UnsupportedSignatureContent">If the encoded attribute argument is invalid.</exception>
        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        private TypedConstant DecodeCustomAttributeElementOrThrow(ref BlobReader argReader, SerializationTypeCode typeCode, TypeSymbol type)
        {
            if (typeCode == SerializationTypeCode.Object)
            {
                // Spec: If the parameter kind is System.Object, the value stored represents the "boxed" instance of that value-type.
                SerializationTypeCode elementTypeCode;
                TypeSymbol elementType;
                DecodeCustomAttributeFieldOrPropTypeOrThrow(ref argReader, out typeCode, out type, out elementTypeCode, out elementType, isElementType: false);

                if (typeCode == SerializationTypeCode.SZArray)
                {
                    return DecodeCustomAttributeElementArrayOrThrow(ref argReader, elementTypeCode, elementType, type);
                }
            }

            return DecodeCustomAttributePrimitiveElementOrThrow(ref argReader, typeCode, type);
        }

        /// <exception cref="UnsupportedSignatureContent">If the encoded attribute argument is invalid.</exception>
        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        private TypedConstant DecodeCustomAttributeElementArrayOrThrow(ref BlobReader argReader, SerializationTypeCode elementTypeCode, TypeSymbol elementType, TypeSymbol arrayType)
        {
            int count = argReader.ReadInt32();
            TypedConstant[] values;

            if (count == -1)
            {
                values = null;
            }
            else if (count == 0)
            {
                values = SpecializedCollections.EmptyArray<TypedConstant>();
            }
            else
            {
                values = new TypedConstant[count];
                for (int i = 0; i < count; i++)
                {
                    values[i] = DecodeCustomAttributeElementOrThrow(ref argReader, elementTypeCode, elementType);
                }
            }

            return CreateArrayTypedConstant(arrayType, values.AsImmutableOrNull());
        }

        /// <exception cref="UnsupportedSignatureContent">If the given <paramref name="typeCode"/> is invalid.</exception>
        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        private TypedConstant DecodeCustomAttributePrimitiveElementOrThrow(ref BlobReader argReader, SerializationTypeCode typeCode, TypeSymbol type)
        {
            Debug.Assert(type != null);

            switch (typeCode)
            {
                case SerializationTypeCode.Boolean:
                    return CreateTypedConstant(type, GetPrimitiveOrEnumTypedConstantKind(type), argReader.ReadSByte() != 0);

                case SerializationTypeCode.SByte:
                    return CreateTypedConstant(type, GetPrimitiveOrEnumTypedConstantKind(type), argReader.ReadSByte());

                case SerializationTypeCode.Byte:
                    return CreateTypedConstant(type, GetPrimitiveOrEnumTypedConstantKind(type), argReader.ReadByte());

                case SerializationTypeCode.Int16:
                    return CreateTypedConstant(type, GetPrimitiveOrEnumTypedConstantKind(type), argReader.ReadInt16());

                case SerializationTypeCode.UInt16:
                    return CreateTypedConstant(type, GetPrimitiveOrEnumTypedConstantKind(type), argReader.ReadUInt16());

                case SerializationTypeCode.Int32:
                    return CreateTypedConstant(type, GetPrimitiveOrEnumTypedConstantKind(type), argReader.ReadInt32());

                case SerializationTypeCode.UInt32:
                    return CreateTypedConstant(type, GetPrimitiveOrEnumTypedConstantKind(type), argReader.ReadUInt32());

                case SerializationTypeCode.Int64:
                    return CreateTypedConstant(type, GetPrimitiveOrEnumTypedConstantKind(type), argReader.ReadInt64());

                case SerializationTypeCode.UInt64:
                    return CreateTypedConstant(type, GetPrimitiveOrEnumTypedConstantKind(type), argReader.ReadUInt64());

                case SerializationTypeCode.Single:
                    return CreateTypedConstant(type, GetPrimitiveOrEnumTypedConstantKind(type), argReader.ReadSingle());

                case SerializationTypeCode.Double:
                    return CreateTypedConstant(type, GetPrimitiveOrEnumTypedConstantKind(type), argReader.ReadDouble());

                case SerializationTypeCode.Char:
                    return CreateTypedConstant(type, GetPrimitiveOrEnumTypedConstantKind(type), argReader.ReadChar());

                case SerializationTypeCode.String:
                    string s;
                    TypedConstantKind kind = PEModule.CrackStringInAttributeValue(out s, ref argReader) ?
                        TypedConstantKind.Primitive :
                        TypedConstantKind.Error;

                    return CreateTypedConstant(type, kind, s);

                case SerializationTypeCode.Type:
                    string typeName;
                    TypeSymbol serializedType = PEModule.CrackStringInAttributeValue(out typeName, ref argReader) ?
                        (typeName != null ? GetTypeSymbolForSerializedType(typeName) : null) :
                        GetUnsupportedMetadataTypeSymbol();

                    return CreateTypedConstant(type, TypedConstantKind.Type, serializedType);

                default:
                    throw new UnsupportedSignatureContent();
            }
        }

        private static TypedConstantKind GetPrimitiveOrEnumTypedConstantKind(TypeSymbol type)
        {
            return (type.TypeKind == TypeKind.Enum) ? TypedConstantKind.Enum : TypedConstantKind.Primitive;
        }

        /// <exception cref="UnsupportedSignatureContent">If the encoded named argument is invalid.</exception>
        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        private KeyValuePair<string, TypedConstant> DecodeCustomAttributeNamedArgumentOrThrow(ref BlobReader argReader)
        {
            // Ecma-335 23.3 - A NamedArg is simply a FixedArg preceded by information to identify which field or
            // property it represents. [Note: Recall that the CLI allows fields and properties to have the same name; so
            // we require a means to disambiguate such situations. end note] FIELD is the single byte 0x53. PROPERTY is
            // the single byte 0x54.

            var kind = (CustomAttributeNamedArgumentKind)ReadCompressedInteger(ref argReader);
            if (kind != CustomAttributeNamedArgumentKind.Field && kind != CustomAttributeNamedArgumentKind.Property)
            {
                throw new UnsupportedSignatureContent();
            }

            SerializationTypeCode typeCode, elementTypeCode;
            TypeSymbol type, elementType;
            DecodeCustomAttributeFieldOrPropTypeOrThrow(ref argReader, out typeCode, out type, out elementTypeCode, out elementType, isElementType: false);

            string name;
            if (!PEModule.CrackStringInAttributeValue(out name, ref argReader))
            {
                throw new UnsupportedSignatureContent();
            }

            TypedConstant value;
            if (typeCode == SerializationTypeCode.SZArray)
            {
                value = DecodeCustomAttributeElementArrayOrThrow(ref argReader, elementTypeCode, elementType, type);
            }
            else
            {
                value = DecodeCustomAttributeElementOrThrow(ref argReader, typeCode, type);
            }

            return new KeyValuePair<string, TypedConstant>(name, value);
        }

        internal bool IsTargetAttribute(
            CustomAttributeHandle customAttribute,
            string namespaceName,
            string typeName,
            bool ignoreCase = false)
        {
            try
            {
                Handle ctor;

                return Module.IsTargetAttribute(
                    customAttribute,
                    namespaceName,
                    typeName,
                    out ctor,
                    ignoreCase);
            }
            catch (BadImageFormatException)
            {
                return false;
            }
        }

        internal int GetTargetAttributeSignatureIndex(CustomAttributeHandle customAttribute, AttributeDescription description)
        {
            try
            {
                return Module.GetTargetAttributeSignatureIndex(customAttribute, description);
            }
            catch (BadImageFormatException)
            {
                return -1;
            }
        }

        internal bool GetCustomAttribute(
            CustomAttributeHandle handle,
            out TypedConstant[] positionalArgs,
            out KeyValuePair<string, TypedConstant>[] namedArgs)
        {
            try
            {
                positionalArgs = SpecializedCollections.EmptyArray<TypedConstant>();
                namedArgs = SpecializedCollections.EmptyArray<KeyValuePair<String, TypedConstant>>();

                // We could call decoder.GetSignature and use that to decode the arguments. However, materializing the
                // constructor signature is more work. We try to decode the arguments directly from the metadata bytes.
                Handle attributeType;
                Handle ctor;

                if (Module.GetTypeAndConstructor(handle, out attributeType, out ctor))
                {
                    BlobReader argsReader = Module.GetMemoryReaderOrThrow(Module.GetCustomAttributeValueOrThrow(handle));
                    BlobReader sigReader = Module.GetMemoryReaderOrThrow(Module.GetMethodSignatureOrThrow(ctor));

                    uint prolog = argsReader.ReadUInt16();
                    if (prolog != 1)
                    {
                        return false;
                    }

                    // Read the calling convention.
                    byte callConv = sigReader.ReadByte();

                    // Get the type parameter count.
                    if (SignatureHeader.IsGeneric(callConv) && ReadCompressedInteger(ref sigReader) != 0)
                    {
                        return false;
                    }

                    // Get the parameter count
                    int paramCount = ReadCompressedInteger(ref sigReader);

                    // Get the type return type.
                    var returnTypeCode = sigReader.ReadSignatureTypeCode();
                    if (returnTypeCode != SignatureTypeCode.Void)
                    {
                        return false;
                    }

                    if (paramCount > 0)
                    {
                        positionalArgs = new TypedConstant[paramCount];

                        for (int i = 0; i < positionalArgs.Length; i++)
                        {
                            positionalArgs[i] = DecodeCustomAttributeFixedArgumentOrThrow(ref sigReader, ref argsReader);
                        }
                    }

                    short namedParamCount = argsReader.ReadInt16();

                    if (namedParamCount > 0)
                    {
                        namedArgs = new KeyValuePair<string, TypedConstant>[namedParamCount];

                        for (int i = 0; i < namedArgs.Length; i++)
                        {
                            namedArgs[i] = DecodeCustomAttributeNamedArgumentOrThrow(ref argsReader);
                        }
                    }

                    return true;
                }
            }
            catch (Exception e) if (e is UnsupportedSignatureContent || e is BadImageFormatException)
            {
                positionalArgs = SpecializedCollections.EmptyArray<TypedConstant>();
                namedArgs = SpecializedCollections.EmptyArray<KeyValuePair<String, TypedConstant>>();
            }

            return false;
        }

        internal bool GetCustomAttribute(CustomAttributeHandle handle, out TypeSymbol attributeClass, out MethodSymbol attributeCtor)
        {
            Handle attributeType;
            Handle ctor;

            try
            {
                if (!Module.GetTypeAndConstructor(handle, out attributeType, out ctor))
                {
                    attributeClass = null;
                    attributeCtor = null;
                    return false;
                }
            }
            catch (BadImageFormatException)
            {
                attributeClass = null;
                attributeCtor = null;
                return false;
            }

            attributeClass = GetTypeOfToken(attributeType);
            attributeCtor = GetMethodSymbolForMethodDefOrMemberRef(ctor, attributeClass);
            return true;
        }

        internal bool GetCustomAttributeWellKnownType(CustomAttributeHandle handle, out WellKnownType wellKnownAttribute)
        {
            wellKnownAttribute = WellKnownType.Unknown;

            try
            {
                Handle attributeType;
                Handle ctor;

                if (!Module.GetTypeAndConstructor(handle, out attributeType, out ctor))
                {
                    return false;
                }

                Handle namespaceHandle;
                StringHandle nameHandle;
                if (!Module.GetAttributeNamespaceAndName(attributeType, out namespaceHandle, out nameHandle))
                {
                    return false;
                }

                string fullName = Module.GetFullNameOrThrow(namespaceHandle, nameHandle);
                wellKnownAttribute = WellKnownTypes.GetTypeFromMetadataName(fullName);
                return true;
            }
            catch (BadImageFormatException)
            {
                return false;
            }
        }

        #endregion

        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        private TypeSymbol[] DecodeMethodSpecTypeArgumentsOrThrow(BlobHandle signature)
        {
            byte callingConvention;
            var signatureReader = DecodeSignatureHeaderOrThrow(signature, out callingConvention);
            if (!SignatureHeader.IsGenericInstanceSignature(callingConvention))
            {
                throw new BadImageFormatException();
            }

            int argumentCount = ReadCompressedInteger(ref signatureReader);
            if (argumentCount == 0)
            {
                throw new BadImageFormatException();
            }

            var result = new TypeSymbol[argumentCount];
            for (int i = 0; i < result.Length; i++)
            {
                bool refersToNoPiaLocalType;
                result[i] = DecodeTypeOrThrow(ref signatureReader, out refersToNoPiaLocalType);
            }

            return result;
        }

        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        internal BlobReader DecodeSignatureHeaderOrThrow(BlobHandle signature, out byte callingConvention)
        {
            BlobReader reader = Module.GetMemoryReaderOrThrow(signature);
            callingConvention = reader.ReadByte();
            return reader;
        }

        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        protected ParamInfo[] DecodeSignatureParametersOrThrow(ref BlobReader signatureReader, byte callingConvention, out int typeParameterCount)
        {
            int paramCount;
            GetSignatureCountsOrThrow(ref signatureReader, callingConvention, out paramCount, out typeParameterCount);

            ParamInfo[] paramInfo = new ParamInfo[paramCount + 1];

            uint paramIndex = 0;

            try
            {
                // get the return type
                DecodeParameterOrThrow(ref signatureReader, ref paramInfo[0]);
                if (paramInfo[0].HasByRefBeforeCustomModifiers)
                {
                    // We don't have a good place to record this information and it's not worthwhile to add one
                    // (it's illegal and we're not aware of any real-world code that needs it).
                    throw new UnsupportedSignatureContent();
                }

                // Get all of the parameters.
                for (paramIndex = 1; paramIndex <= paramCount; paramIndex++)
                {
                    // Figure out the type.
                    DecodeParameterOrThrow(ref signatureReader, ref paramInfo[paramIndex]);
                }

                if (signatureReader.RemainingBytes > 0)
                {
                    throw new UnsupportedSignatureContent();
                }
            }
            catch (Exception e) if (e is UnsupportedSignatureContent || e is BadImageFormatException) 
            {
                for (; paramIndex <= paramCount; paramIndex++)
                {
                    paramInfo[paramIndex].Type = GetUnsupportedMetadataTypeSymbol(e as BadImageFormatException);
                }
            }

            if (paramInfo[0].IsByRef)
            {
                paramInfo[0].IsByRef = false; // Info reflected in the error type.
                paramInfo[0].Type = GetByRefReturnTypeSymbol(paramInfo[0].Type);
            }

            return paramInfo;
        }

        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        private static void GetSignatureCountsOrThrow(ref BlobReader signatureReader, byte callingConvention, out int parameterCount, out int typeParameterCount)
        {
            // Get the type parameter count.
            typeParameterCount = SignatureHeader.IsGeneric(callingConvention) ? ReadCompressedInteger(ref signatureReader) : 0;

            // Get the parameter count
            parameterCount = ReadCompressedInteger(ref signatureReader);
        }

        internal TypeSymbol DecodeFieldSignature(FieldHandle fieldHandle, out bool isVolatile, out ImmutableArray<ModifierInfo> customModifiers)
        {
            try
            {
                BlobHandle signature = Module.GetFieldSignatureOrThrow(fieldHandle);

                byte callingConvention;
                BlobReader signatureReader = DecodeSignatureHeaderOrThrow(signature, out callingConvention);

                if (!SignatureHeader.IsFieldSignature(callingConvention))
                {
                    isVolatile = false;
                    customModifiers = default(ImmutableArray<ModifierInfo>);
                    return GetUnsupportedMetadataTypeSymbol(); // unsupported signature content
                }

                return DecodeFieldSignature(ref signatureReader, out isVolatile, out customModifiers);
            }
            catch (BadImageFormatException mrEx)
            {
                isVolatile = false;
                customModifiers = default(ImmutableArray<ModifierInfo>);
                return GetUnsupportedMetadataTypeSymbol(mrEx);
            }
        }

        // MetaImport::DecodeFieldSignature
        protected TypeSymbol DecodeFieldSignature(ref BlobReader signatureReader, out bool isVolatile, out ImmutableArray<ModifierInfo> customModifiers)
        {
            isVolatile = false;
            customModifiers = default(ImmutableArray<ModifierInfo>);

            try
            {
                // See if there is a Volatile modifier.
                SignatureTypeCode typeCode;
                ArrayBuilder<ModifierInfo> customModifierBuilder = null;

                for (; ;)
                {
                    typeCode = signatureReader.ReadSignatureTypeCode();

                    if (typeCode == SignatureTypeCode.OptionalModifier ||
                        typeCode == SignatureTypeCode.RequiredModifier)
                    {
                        Handle token = signatureReader.ReadTypeHandle();
                        ModifierInfo modifier = new ModifierInfo((typeCode == SignatureTypeCode.OptionalModifier), GetTypeOfToken(token));

                        if (!IsAcceptableModOptModifier(token, modifier.Modifier))
                        {
                            return GetUnsupportedMetadataTypeSymbol(); // unsupported signature content
                        }

                        if (IsVolatileModifierType(modifier.Modifier))
                        {
                            isVolatile = true;
                        }
                        else if (!modifier.IsOptional)
                        {
                            return GetUnsupportedMetadataTypeSymbol(); // unsupported signature content
                        }

                        if (customModifierBuilder == null)
                        {
                            customModifierBuilder = ArrayBuilder<ModifierInfo>.GetInstance();
                        }

                        customModifierBuilder.Add(modifier);

                        continue;
                    }

                    break;
                }

                if (customModifierBuilder != null)
                {
                    customModifiers = customModifierBuilder.ToImmutableAndFree();
                }

                // get the type
                bool refersToNoPiaLocalType;
                return DecodeTypeOrThrow(ref signatureReader, typeCode, out refersToNoPiaLocalType);

            }
            catch (UnsupportedSignatureContent)
            {
                return GetUnsupportedMetadataTypeSymbol(); // unsupported signature content
            }
            catch (BadImageFormatException mrEx)
            {
                return GetUnsupportedMetadataTypeSymbol(mrEx);
            }
        }

        /// <summary>
        /// Find the methods that a given method explicitly overrides.
        /// </summary>
        /// <remarks>
        /// Methods may be on class or interfaces.
        /// Containing classes/interfaces will be supertypes of the implementing type.
        /// </remarks>
        /// <param name="implementingTypeDef">TypeDef handle of the implementing type.</param>
        /// <param name="implementingMethodDef">MethodDef handle of the implementing method.</param>
        /// <param name="implementingTypeSymbol">The type symbol for the implementing type.</param>
        /// <returns>Array of implemented methods.</returns>
        internal ImmutableArray<MethodSymbol> GetExplicitlyOverriddenMethods(TypeHandle implementingTypeDef, MethodHandle implementingMethodDef, TypeSymbol implementingTypeSymbol)
        {
            ArrayBuilder<MethodSymbol> resultBuilder = ArrayBuilder<MethodSymbol>.GetInstance();

            try
            {
                foreach (var methodImpl in Module.GetMethodImplementationsOrThrow(implementingTypeDef))
                {
                    Handle methodBodyHandle;
                    Handle implementedMethodHandle;
                    Module.GetMethodImplPropsOrThrow(methodImpl, out methodBodyHandle, out implementedMethodHandle);

                    // Though it is rare in practice, the spec allows the MethodImpl table to represent
                    // methods defined in the current module as MemberRefs rather than MethodDefs.
                    if (methodBodyHandle.HandleType == HandleType.MemberReference)
                    {
                        MethodSymbol methodBodySymbol = GetMethodSymbolForMemberRef((MemberReferenceHandle)methodBodyHandle, implementingTypeSymbol);
                        if (methodBodySymbol != null)
                        {
                            // Note: this might have a nil row ID, but that won't cause a problem
                            // since it will simply fail to be equal to the implementingMethodToken.
                            methodBodyHandle = GetMethodHandle(methodBodySymbol);
                        }
                    }

                    if (methodBodyHandle == implementingMethodDef)
                    {
                        if (!implementedMethodHandle.IsNil)
                        {
                            HandleType implementedMethodTokenType = implementedMethodHandle.HandleType;

                            MethodSymbol methodSymbol = null;

                            if (implementedMethodTokenType == HandleType.Method)
                            {
                                methodSymbol = FindMethodSymbolInSuperType(implementingTypeDef, (MethodHandle)implementedMethodHandle);
                            }
                            else if (implementedMethodTokenType == HandleType.MemberReference)
                            {
                                methodSymbol = GetMethodSymbolForMemberRef((MemberReferenceHandle)implementedMethodHandle, implementingTypeSymbol);
                            }

                            if (methodSymbol != null)
                            {
                                resultBuilder.Add(methodSymbol);
                            }
                        }
                    }
                }
            }
            catch (BadImageFormatException)
            { }

            return resultBuilder.ToImmutableAndFree();
        }

        /// <summary>
        /// Search for the MethodSymbol corresponding to the a given MethodDef token.  Search amongst the supertypes
        /// (classes and interfaces) of a designated type.
        /// </summary>
        /// <remarks>
        /// Generally, the type will be a type that explicitly implements an interface and the method will be the
        /// implemented method (i.e. on the interface).
        /// </remarks>
        /// <param name="searchTypeDef">TypeDef token of the type from which the search should begin.</param>
        /// <param name="targetMethodDef">MethodDef token of the target method.</param>
        /// <returns>Corresponding MethodSymbol or null, if none is found.</returns>
        private MethodSymbol FindMethodSymbolInSuperType(TypeHandle searchTypeDef, MethodHandle targetMethodDef)
        {
            try
            {
                // We're using queues (i.e. BFS), rather than stacks (i.e. DFS), because we expect the common case
                // to be implementing a method on an immediate supertype, rather than a remote ancestor.
                // We're using more than one queue for two reasons: 1) some of our TypeDef tokens come directly from the
                // metadata tables and we'd prefer not to manipulate the correspoding symbol objects; 2) we bump TypeDefs
                // to the front of the search order (i.e. ahead of symbols) because a MethodDef can correspond to a TypeDef
                // but not to a type ref (i.e. symbol).
                Queue<TypeHandle> typeDefsToSearch = new Queue<TypeHandle>();
                Queue<TypeSymbol> typeSymbolsToSearch = new Queue<TypeSymbol>();

                // A method def represents a method defined in this module, so we can
                // just search the method defs of this module.
                EnqueueTypeDefInterfacesAndBaseTypeOrThrow(typeDefsToSearch, typeSymbolsToSearch, searchTypeDef);

                //catch both cycles and duplicate interfaces
                HashSet<TypeHandle> visitedTypeDefTokens = new HashSet<TypeHandle>();
                HashSet<TypeSymbol> visitedTypeSymbols = new HashSet<TypeSymbol>();

                bool hasMoreTypeDefs;
                while ((hasMoreTypeDefs = (typeDefsToSearch.Count > 0)) || typeSymbolsToSearch.Count > 0)
                {
                    if (hasMoreTypeDefs)
                    {
                        TypeHandle typeDef = typeDefsToSearch.Dequeue();
                        Debug.Assert(!typeDef.IsNil);

                        if (!visitedTypeDefTokens.Contains(typeDef))
                        {
                            visitedTypeDefTokens.Add(typeDef);

                            foreach (MethodHandle methodDef in Module.GetMethodsOfTypeOrThrow(typeDef))
                            {
                                if (methodDef == targetMethodDef)
                                {
                                    TypeSymbol typeSymbol = this.GetTypeOfToken(typeDef);
                                    return FindMethodSymbolInType(typeSymbol, targetMethodDef);
                                }
                            }

                            EnqueueTypeDefInterfacesAndBaseTypeOrThrow(typeDefsToSearch, typeSymbolsToSearch, typeDef);
                        }
                    }
                    else //has more type symbols
                    {
                        TypeSymbol typeSymbol = typeSymbolsToSearch.Dequeue();
                        Debug.Assert(typeSymbol != null);

                        if (!visitedTypeSymbols.Contains(typeSymbol))
                        {
                            visitedTypeSymbols.Add(typeSymbol);

                            //we're looking for a method def but we're currently on a type *ref*, so just enqueue supertypes

                            EnqueueTypeSymbolInterfacesAndBaseTypes(typeDefsToSearch, typeSymbolsToSearch, typeSymbol);
                        }
                    }
                }
            }
            catch (BadImageFormatException)
            { }

            return null;
        }

        /// <summary>
        /// Enqueue the interfaces implemented and the type extended by a given TypeDef.
        /// </summary>
        /// <param name="typeDefsToSearch">Queue of TypeDefs to search.</param>
        /// <param name="typeSymbolsToSearch">Queue of TypeSymbols (representing typeRefs to search).</param>
        /// <param name="searchTypeDef">Handle of the TypeDef for which we want to enqueue supertypes.</param>
        /// <exception cref="BadImageFormatException">An exception from metadata reader.</exception>
        private void EnqueueTypeDefInterfacesAndBaseTypeOrThrow(Queue<TypeHandle> typeDefsToSearch, Queue<TypeSymbol> typeSymbolsToSearch, TypeHandle searchTypeDef)
        {
            foreach (var interfaceImpl in Module.GetImplementedInterfacesOrThrow(searchTypeDef))
            {
                EnqueueTypeToken(typeDefsToSearch, typeSymbolsToSearch, interfaceImpl);
            }

            EnqueueTypeToken(typeDefsToSearch, typeSymbolsToSearch, Module.GetBaseTypeOfTypeOrThrow(searchTypeDef));
        }

        /// <summary>
        /// Helper method for enqueueing a type token in the right queue.
        /// Def -> typeDefsToSearch
        /// Ref -> typeSymbolsToSearch
        /// null -> neither
        /// </summary>
        private void EnqueueTypeToken(Queue<TypeHandle> typeDefsToSearch, Queue<TypeSymbol> typeSymbolsToSearch, Handle typeToken)
        {
            if (!typeToken.IsNil)
            {
                if (typeToken.HandleType == HandleType.Type)
                {
                    typeDefsToSearch.Enqueue((TypeHandle)typeToken);
                }
                else
                {
                    EnqueueTypeSymbol(typeDefsToSearch, typeSymbolsToSearch, this.GetTypeOfToken(typeToken));
                }
            }
        }

        /// <summary>
        /// Enqueue the interfaces implemented and the type extended by a given TypeDef.
        /// </summary>
        /// <param name="typeDefsToSearch">Queue of TypeDefs to search.</param>
        /// <param name="typeSymbolsToSearch">Queue of TypeSymbols (representing typeRefs to search).</param>
        /// <param name="typeSymbol">Symbol for which we want to enqueue supertypes.</param>
        protected abstract void EnqueueTypeSymbolInterfacesAndBaseTypes(Queue<TypeHandle> typeDefsToSearch, Queue<TypeSymbol> typeSymbolsToSearch, TypeSymbol typeSymbol);

        /// <summary>
        /// Enqueue the given type as either a def or a ref.
        /// </summary>
        /// <param name="typeDefsToSearch">Queue of TypeDefs to search.</param>
        /// <param name="typeSymbolsToSearch">Queue of TypeSymbols (representing typeRefs to search).</param>
        /// <param name="typeSymbol">Symbol to enqueue.</param>
        protected abstract void EnqueueTypeSymbol(Queue<TypeHandle> typeDefsToSearch, Queue<TypeSymbol> typeSymbolsToSearch, TypeSymbol typeSymbol);

        /// <summary>
        /// Search the members of a TypeSymbol to find the one that matches a given MethodDef token.
        /// </summary>
        /// <param name="type">Type to search for method.</param>
        /// <param name="methodDef">MethodDef handle of the method to find.</param>
        /// <returns>The corresponding MethodSymbol or null.</returns>
        protected abstract MethodSymbol FindMethodSymbolInType(TypeSymbol type, MethodHandle methodDef);

        /// <summary>
        /// Search the members of a TypeSymbol to find the one that matches a given FieldDef token.
        /// </summary>
        /// <param name="type">Type to search for field.</param>
        /// <param name="fieldDef">FieldDef handle of the field to find.</param>
        /// <returns>The corresponding FieldSymbol or null.</returns>
        protected abstract FieldSymbol FindFieldSymbolInType(TypeSymbol type, FieldHandle fieldDef);

        /// <summary>
        /// Given a MemberRef token for a method, we can find a corresponding MethodSymbol by
        /// searching for the name and signature.
        /// </summary>
        /// <param name="memberRef">A MemberRef token for a method.</param>
        /// <param name="implementingTypeSymbol">Scope the search to supertypes of the implementing type.</param>
        /// <param name="methodsOnly">True to only return method symbols, null if the token resolves to a field.</param>
        /// <returns>The corresponding MethodSymbol or null.</returns>
        internal abstract Symbol GetSymbolForMemberRef(MemberReferenceHandle memberRef, TypeSymbol implementingTypeSymbol = null, bool methodsOnly = false);

        internal MethodSymbol GetMethodSymbolForMemberRef(MemberReferenceHandle methodRef, TypeSymbol implementingTypeSymbol)
        {
            return (MethodSymbol)GetSymbolForMemberRef(methodRef, implementingTypeSymbol, methodsOnly: true);
        }

        internal FieldSymbol GetFieldSymbolForMemberRef(MemberReferenceHandle methodRef, TypeSymbol implementingTypeSymbol)
        {
            return (FieldSymbol)GetSymbolForMemberRef(methodRef, implementingTypeSymbol, methodsOnly: true);
        }

        /// <summary>
        /// Given a method symbol, return the MethodDef token, if it is defined in
        /// this module, or a nil token, otherwise.
        /// </summary>
        /// <param name="method">The method symbol for which to return a MethodDef token.</param>
        /// <returns>A MethodDef token or nil.</returns>
        protected abstract MethodHandle GetMethodHandle(MethodSymbol method);

        protected abstract ConcurrentDictionary<TypeHandle, TypeSymbol> GetTypeHandleToTypeMap();
        protected abstract ConcurrentDictionary<TypeReferenceHandle, TypeSymbol> GetTypeRefHandleToTypeMap();

        /// <summary>
        /// Lookup a type defined in this module.
        /// </summary>
        protected abstract TypeSymbol LookupTopLevelTypeDefSymbol(ref MetadataTypeName emittedName, out bool isNoPiaLocalType);

        protected abstract TypeSymbol SubstituteNoPiaLocalType(TypeHandle typeDef, ref MetadataTypeName name, string interfaceGuid, string scope, string identifier);

        /// <summary>
        /// Lookup a type defined in referenced assembly.
        /// </summary>
        protected abstract TypeSymbol LookupTopLevelTypeDefSymbol(int referencedAssemblyIndex, ref MetadataTypeName emittedName);
        protected abstract TypeSymbol LookupTopLevelTypeDefSymbol(string moduleName, ref MetadataTypeName emittedName, out bool isNoPiaLocalType);
        protected abstract TypeSymbol LookupNestedTypeDefSymbol(TypeSymbol container, ref MetadataTypeName emittedName);
        protected abstract TypeSymbol GetUnsupportedMetadataTypeSymbol(BadImageFormatException mrEx = null);
        protected abstract TypeSymbol GetByRefReturnTypeSymbol(TypeSymbol referencedType);

        /// <summary>
        /// Produce constructed type symbol.
        /// </summary>
        /// <param name="generic">
        /// Symbol for generic type.
        /// </param>
        /// <param name="arguments">
        /// Generic type arguments, including those for nesting types.
        /// </param>
        /// <param name="refersToNoPiaLocalType">
        /// Flags for arguments. Each item indicates whether corresponding argument refers to NoPia local types.
        /// </param>
        /// <returns></returns>
        /// <remarks></remarks>
        protected abstract TypeSymbol SubstituteTypeParameters(TypeSymbol generic, TypeSymbol[] arguments, bool[] refersToNoPiaLocalType);

        /// <summary>
        /// Produce unbound generic type symbol if the type is a generic type.
        /// </summary>
        /// <param name="type">
        /// Symbol for type.
        /// </param>
        protected abstract TypeSymbol SubstituteWithUnboundIfGeneric(TypeSymbol type);

        protected abstract TypeSymbol GetGenericTypeParamSymbol(int position);
        protected abstract TypeSymbol GetGenericMethodTypeParamSymbol(int position);
        protected abstract TypeSymbol GetSZArrayTypeSymbol(TypeSymbol elementType, ImmutableArray<ModifierInfo> customModifiers);
        protected abstract TypeSymbol GetArrayTypeSymbol(int dims, TypeSymbol elementType);
        protected abstract TypeSymbol MakePointerTypeSymbol(TypeSymbol type, ImmutableArray<ModifierInfo> customModifiers);
        protected abstract TypeSymbol GetSpecialType(SpecialType specialType);
        protected abstract TypeSymbol SystemTypeSymbol { get; }
        protected abstract TypeSymbol GetEnumUnderlyingType(TypeSymbol type);

        protected abstract bool IsVolatileModifierType(TypeSymbol type);
        protected abstract Microsoft.Cci.PrimitiveTypeCode GetPrimitiveTypeCode(TypeSymbol type);

        private TypedConstant CreateArrayTypedConstant(TypeSymbol type, ImmutableArray<TypedConstant> array)
        {
            if (type.TypeKind == TypeKind.Error)
            {
                return new TypedConstant(type, TypedConstantKind.Error, null);
            }

            Debug.Assert(type.TypeKind == TypeKind.ArrayType);
            return new TypedConstant(type, array);
        }

        private TypedConstant CreateTypedConstant(TypeSymbol type, TypedConstantKind kind, object value)
        {
            if (type.TypeKind == TypeKind.Error)
            {
                return new TypedConstant(type, TypedConstantKind.Error, null);
            }

            return new TypedConstant(type, kind, value);
        }

        private readonly static object BoxedTrue = true;
        private readonly static object BoxedFalse = false;

        private TypedConstant CreateTypedConstant(TypeSymbol type, TypedConstantKind kind, bool value)
        {
            return CreateTypedConstant(type, kind, value ? BoxedTrue : BoxedFalse);
        }

        /// <summary>
        /// Returns a symbol that given token resolves to or null of the token represents entity that isn't represented by a symbol,
        /// such as vararg MemberRef.
        /// </summary>
        internal Symbol GetSymbolForILToken(Handle token)
        {
            try
            {
                switch (token.HandleType)
                {
                    case HandleType.Type:
                    case HandleType.TypeSpecification:
                    case HandleType.TypeReference:
                        return GetTypeOfToken(token);

                    case HandleType.Method:
                        {
                            TypeHandle typeDef = Module.FindContainingTypeOrThrow((MethodHandle)token);

                            if (typeDef.IsNil)
                            {
                                // error
                                return null;
                            }

                            TypeSymbol type = GetTypeOfTypeDef(typeDef);
                            if (type == null)
                            {
                                // error
                                return null;
                            }

                            return GetMethodSymbolForMethodDefOrMemberRef(token, type);
                        }

                    case HandleType.Field:
                        {
                            TypeHandle typeDef = Module.FindContainingTypeOrThrow((FieldHandle)token);
                            if (typeDef.IsNil)
                            {
                                // error
                                return null;
                            }

                            TypeSymbol type = GetTypeOfToken(typeDef);
                            if (type == null)
                            {
                                // error
                                return null;
                            }

                            return GetFieldSymbolForFieldDefOrMemberRef(token, type);
                        }

                    case HandleType.MethodSpecification:
                        Handle method;
                        BlobHandle instantiation;
                        this.Module.GetMethodSpecificationOrThrow((MethodSpecificationHandle)token, out method, out instantiation);

                        var genericDefinition = (MethodSymbol)GetSymbolForILToken(method);
                        if (genericDefinition == null)
                        {
                            // error
                            return null;
                        }

                        var genericArguments = DecodeMethodSpecTypeArgumentsOrThrow(instantiation);
                        return (MethodSymbol)genericDefinition.Construct(genericArguments);

                    case HandleType.MemberReference:
                        return GetSymbolForMemberRef((MemberReferenceHandle)token);
                }
            }
            catch (BadImageFormatException)
            { }

            // error: unexpected token in IL
            return null;
        }

        /// <summary>
        /// Given a MemberRef token, return the TypeSymbol for its Class field.
        /// </summary>
        internal TypeSymbol GetMemberRefTypeSymbol(MemberReferenceHandle memberRef)
        {
            try
            {
                Handle container = Module.GetContainingTypeOrThrow(memberRef);

                HandleType containerType = container.HandleType;
                Debug.Assert(
                    containerType == HandleType.Method ||
                    containerType == HandleType.ModuleReference ||
                    containerType == HandleType.Type ||
                    containerType == HandleType.TypeReference ||
                    containerType == HandleType.TypeSpecification);

                if (containerType != HandleType.Type &&
                    containerType != HandleType.TypeReference &&
                    containerType != HandleType.TypeSpecification)
                {
                    // C# symbols don't support global methods
                    return null;
                }

                return this.GetTypeOfToken(container);
            }
            catch (BadImageFormatException)
            {
                return null;
            }
        }

        internal MethodSymbol GetMethodSymbolForMethodDefOrMemberRef(Handle memberToken, TypeSymbol container)
        {
            HandleType type = memberToken.HandleType;
            Debug.Assert(type == HandleType.Method ||
                            type == HandleType.MemberReference);

            if (type == HandleType.Method)
            {
                return FindMethodSymbolInType(container, (MethodHandle)memberToken);
            }
            else
            {
                return GetMethodSymbolForMemberRef((MemberReferenceHandle)memberToken, container);
            }
        }

        internal FieldSymbol GetFieldSymbolForFieldDefOrMemberRef(Handle memberToken, TypeSymbol container)
        {
            HandleType type = memberToken.HandleType;
            Debug.Assert(type == HandleType.Field ||
                            type == HandleType.MemberReference);

            if (type == HandleType.Field)
            {
                return FindFieldSymbolInType(container, (FieldHandle)memberToken);
            }
            else
            {
                return GetFieldSymbolForMemberRef((MemberReferenceHandle)memberToken, container);
            }
        }

        internal TypeSymbol GetTypeSymbolForSerializedType(string s)
        {
            if (s == null || s.IsEmpty())
            {
                return GetUnsupportedMetadataTypeSymbol();
            }

            var decoder = new MetadataHelpers.SerializedTypeDecoder();
            MetadataHelpers.AssemblyQualifiedTypeName fullName = decoder.DecodeTypeName(s);
            bool refersToNoPiaLocalType;
            return GetTypeSymbol(fullName, out refersToNoPiaLocalType);
        }

        private TypeSymbol GetTypeSymbol(MetadataHelpers.AssemblyQualifiedTypeName fullName, out bool refersToNoPiaLocalType)
        {
            //
            // Section 23.3 (Custom Attributes) of CLI Spec Partition II:
            //
            // If the parameter kind is System.Type, (also, the middle line in above diagram) its value is 
            // stored as a SerString (as defined in the previous paragraph), representing its canonical name. 
            // The canonical name is its full type name, followed optionally by the assembly where it is defined, 
            // its version, culture and public-key-token. If the assembly name is omitted, the CLI looks first 
            // in the current assembly, and then in the system library (mscorlib); in these two special cases, 
            // it is permitted to omit the assembly-name, version, culture and public-key-token.

            int referencedAssemblyIndex;
            if (fullName.AssemblyName != null)
            {
                AssemblyIdentity identity;
                if (!AssemblyIdentity.TryParseDisplayName(fullName.AssemblyName, out identity))
                {
                    refersToNoPiaLocalType = false;
                    return GetUnsupportedMetadataTypeSymbol();
                }

                // the assembly name has to be a full name:
                referencedAssemblyIndex = Module.IndexOfReferencedAssembly(identity);
                if (referencedAssemblyIndex == -1)
                {
                    // In rare cases (e.g. assemblies emitted by Reflection.Emit) the identity 
                    // might be the identity of the containing assembly. The metadata spec doesn't disallow this.
                    if (ContainingAssemblyIdentity == null || !ContainingAssemblyIdentity.Equals(identity))
                    {
                        refersToNoPiaLocalType = false;
                        return GetUnsupportedMetadataTypeSymbol();
                    }
                }
            }
            else
            {
                // Use this assembly
                referencedAssemblyIndex = -1;
            }

            MetadataTypeName mdName;

            // Find the top level type
            Debug.Assert(MetadataHelpers.IsValidMetadataIdentifier(fullName.TopLevelType));
            mdName = MetadataTypeName.FromFullName(fullName.TopLevelType);
            TypeSymbol container = LookupTopLevelTypeDefSymbol(ref mdName, referencedAssemblyIndex, out refersToNoPiaLocalType);

            // Process any nested types
            if (fullName.NestedTypes != null)
            {
                if (refersToNoPiaLocalType)
                {
                    // Types nested into local types are not supported.
                    refersToNoPiaLocalType = false;
                    return GetUnsupportedMetadataTypeSymbol();
                }

                for (int i = 0; i < fullName.NestedTypes.Length; i++)
                {
                    Debug.Assert(MetadataHelpers.IsValidMetadataIdentifier(fullName.NestedTypes[i]));
                    mdName = MetadataTypeName.FromTypeName(fullName.NestedTypes[i]);
                    // Find nested type in the container
                    container = LookupNestedTypeDefSymbol(container, ref mdName);
                }
            }

            //  Substitute type arguments if any
            if (fullName.TypeArguments != null)
            {
                bool[] argumentRefersToNoPiaLocalType;
                var typeArguments = ResolveTypeArguments(fullName.TypeArguments, out argumentRefersToNoPiaLocalType);
                container = SubstituteTypeParameters(container, typeArguments, argumentRefersToNoPiaLocalType);

                foreach (bool flag in argumentRefersToNoPiaLocalType)
                {
                    if (flag)
                    {
                        refersToNoPiaLocalType = true;
                        break;
                    }
                }
            }
            else
            {
                container = SubstituteWithUnboundIfGeneric(container);
            }

            // Process any array type ranks
            if (fullName.ArrayRanks != null)
            {
                foreach (int rank in fullName.ArrayRanks)
                {
                    Debug.Assert(rank > 0);
                    container = rank == 1 ? GetSZArrayTypeSymbol(container, default(ImmutableArray<ModifierInfo>)) : GetArrayTypeSymbol(rank, container);
                }
            }

            return container;
        }

        private TypeSymbol[] ResolveTypeArguments(MetadataHelpers.AssemblyQualifiedTypeName[] arguments, out bool[] refersToNoPiaLocalType)
        {
            int count = arguments.Length;
            TypeSymbol[] typeArguments = new TypeSymbol[count];
            refersToNoPiaLocalType = new bool[count];

            for (int i = 0; i < count; i++)
            {
                typeArguments[i] = GetTypeSymbol(arguments[i], out refersToNoPiaLocalType[i]);
            }

            return typeArguments;
        }

        private TypeSymbol LookupTopLevelTypeDefSymbol(ref MetadataTypeName emittedName, int referencedAssemblyIndex, out bool isNoPiaLocalType)
        {
            TypeSymbol container;

            if (referencedAssemblyIndex >= 0)
            {
                // Find  top level type in referenced assembly
                isNoPiaLocalType = false;
                container = LookupTopLevelTypeDefSymbol(referencedAssemblyIndex, ref emittedName);
            }
            else
            {
                // TODO : lookup in mscorlib
                // Find top level type in this assembly or mscorlib:
                container = LookupTopLevelTypeDefSymbol(ref emittedName, out isNoPiaLocalType);
            }

            return container;
        }

        /// <summary>
        /// Checks whether signatures match where the signatures are either from a property
        /// and an accessor or two accessors. When comparing a property or getter to setter, the
        /// setter signature must be the second argument and 'comparingToSetter' must be true.
        /// </summary>
        /// <param name="signature1">
        /// Signature of the property containing the accessor, or the getter (type, then parameters).
        /// </param>
        /// <param name="signature2">
        /// Signature of the accessor when comparing property and accessor,
        /// or the setter when comparing getter and setter (return type and then parameters).
        /// </param>
        /// <param name="comparingToSetter">
        /// True when comparing a property or getter to a setter, false otherwise.
        /// </param>
        /// <param name="compareParamByRef">
        /// True if differences in IsByRef for parameters should be treated as significant.
        /// </param>
        /// <param name="compareReturnType">
        /// True if differences in return type (or value parameter for setter) should be treated as significant.
        /// </param>
        /// <returns>True if the accessor signature is appropriate for the containing property.</returns>
        internal bool DoPropertySignaturesMatch(ParamInfo[] signature1, ParamInfo[] signature2, bool comparingToSetter, bool compareParamByRef, bool compareReturnType)
        {
            int additionalParamCount = (comparingToSetter ? 1 : 0);

            // Check the number of parameters.
            if ((signature2.Length - additionalParamCount) != signature1.Length)
            {
                return false;
            }

            // Check the setter has a void type.
            if (comparingToSetter &&
                (GetPrimitiveTypeCode(signature2[0].Type) != Microsoft.Cci.PrimitiveTypeCode.Void))
            {
                return false;
            }

            // Check the type of each parameter.
            for (int paramIndex1 = compareReturnType ? 0 : 1; paramIndex1 < signature1.Length; paramIndex1++)
            {
                int paramIndex2 =
                    ((paramIndex1 == 0) && comparingToSetter) ?
                    signature1.Length :
                    paramIndex1;
                var param1 = signature1[paramIndex1];
                var param2 = signature2[paramIndex2];
                if (compareParamByRef && (param2.IsByRef != param1.IsByRef))
                {
                    return false;
                }
                if (!param2.Type.Equals(param1.Type))
                {
                    return false;
                }
            }

            return true;
        }

        /// <summary>
        /// Check whether an event accessor has an appropriate signature.
        /// </summary>
        /// <param name="eventType">Type of the event containing the accessor.</param>
        /// <param name="methodParams">Signature of the accessor (return type and then parameters).</param>
        /// <returns>True if the accessor signature is appropriate for the containing event.</returns>
        internal bool DoesSignatureMatchEvent(TypeSymbol eventType, ParamInfo[] methodParams)
        {
            // Check the number of parameters.
            if (methodParams.Length != 2)
            {
                return false;
            }

            // Check the accessor has a void type.
            if (GetPrimitiveTypeCode(methodParams[0].Type) != Microsoft.Cci.PrimitiveTypeCode.Void)
            {
                return false;
            }

            var methodParam = methodParams[1];
            return !methodParam.IsByRef && methodParam.Type.Equals(eventType);
        }

        // TODO: remove, API should be provided by MetadataReader
        private static int ReadCompressedInteger(ref BlobReader reader)
        {
            uint value;
            if (!reader.TryReadCompressedUInt32(out value))
            {
                throw new BadImageFormatException("Invalid signature");
            }

            return (int)value;
        }

        // TODO: remove, API should be provided by MetadataReader
        private static bool TryReadCompressedInteger(ref BlobReader reader, out int value)
        {
            uint unsigned;
            bool result = reader.TryReadCompressedUInt32(out unsigned);
            value = (int)unsigned;
            return result;
        }
    }
}
