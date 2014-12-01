﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Reflection.Metadata;
using System.Runtime.InteropServices;

namespace Microsoft.Cci
{
    // Consider A.B aliases to C.D, and C.D aliases to E.F.
    // Then:
    // typereference(A.B).IsAlias == true && typereference(A.B).AliasForType == aliasfortype(A.B).
    // aliasfortype(A.B).AliasedType == typereference(C.D).
    // typereference(C.D).IsAlias == true && typereference(C.D).AliasForType == aliasfortype(C.D).
    // aliasfortype(C.D).AliasedType == typereference(E.F)
    // typereference(E.F).IsAlias == false
    // Also, typereference(A.B).ResolvedType == typereference(C.D).ResolvedType == typereference(E.F).ResolvedType

    /// <summary>
    /// Alias type to represent exported types and typedef.
    /// </summary>
    internal interface IAliasForType : IDefinition
    {
        /// <summary>
        /// Type reference of the type for which this is the alias
        /// </summary>
        ITypeReference AliasedType { get; }
    }

    /// <summary>
    /// This interface models the metadata representation of an array type reference.
    /// </summary>
    internal interface IArrayTypeReference : ITypeReference
    {
        /// <summary>
        /// The type of the elements of this array.
        /// </summary>
        ITypeReference GetElementType(Microsoft.CodeAnalysis.Emit.Context context);

        /// <summary>
        /// This type of array is a single dimensional array with zero lower bound for index values.
        /// </summary>
        bool IsVector
        {
            get;

            // ^ ensures result ==> Rank == 1;
        }

        /// <summary>
        /// A possibly empty list of lower bounds for dimension indices. When not explicitly specified, a lower bound defaults to zero.
        /// The first lower bound in the list corresponds to the first dimension. Dimensions cannot be skipped.
        /// </summary>
        IEnumerable<int> LowerBounds
        {
            get;

            // ^ ensures count(result) <= Rank;
        }

        /// <summary>
        /// The number of array dimensions.
        /// </summary>
        uint Rank
        {
            get;

            // ^ ensures result > 0;
        }

        /// <summary>
        /// A possible empty list of upper bounds for dimension indices.
        /// The first upper bound in the list corresponds to the first dimension. Dimensions cannot be skipped.
        /// An unspecified upper bound means that instances of this type can have an arbitrary upper bound for that dimension.
        /// </summary>
        IEnumerable<ulong> Sizes
        {
            get;

            // ^ ensures count(result) <= Rank;
        }
    }

    /// <summary>
    /// Modifies the set of allowed values for a type, or the semantics of operations allowed on those values. 
    /// Custom modifiers are not associated directly with types, but rather with typed storage locations for values.
    /// </summary>
    internal interface ICustomModifier
    {
        /// <summary>
        /// If true, a language may use the modified storage location without being aware of the meaning of the modification.
        /// </summary>
        bool IsOptional { get; }

        /// <summary>
        /// A type used as a tag that indicates which type of modification applies to the storage location.
        /// </summary>
        ITypeReference GetModifier(Microsoft.CodeAnalysis.Emit.Context context);
    }

    /// <summary>
    /// Information that describes a method or property parameter, but does not include all the information in a IParameterDefinition.
    /// </summary>
    internal interface IParameterTypeInformation : IParameterListEntry
    {
        /// <summary>
        /// The list of custom modifiers, if any, associated with the parameter. Evaluate this property only if IsModified is true.
        /// </summary>
        IEnumerable<ICustomModifier> CustomModifiers
        {
            get;

            // ^ requires this.IsModified;
        }

        /// <summary>
        /// True if the parameter is passed by reference (using a managed pointer).
        /// </summary>
        bool IsByReference { get; }

        /// <summary>
        /// This parameter has one or more custom modifiers associated with it.
        /// </summary>
        bool IsModified { get; }

        /// <summary>
        /// The CLI spec says that custom modifiers must precede the ByRef type code in the encoding of a parameter.
        /// Unfortunately, the managed C++ compiler emits them in the reverse order.  In order to avoid breaking
        /// interop scenarios, we need to support such signatures.  When this flag is set, we need to reverse the
        /// emit order.
        /// </summary>
        /// <remarks>
        /// We support before (correct) and after (incorrect, but works), but not in between.
        /// </remarks>
        bool HasByRefBeforeCustomModifiers { get; }

        /// <summary>
        /// The type of argument value that corresponds to this parameter.
        /// </summary>
        ITypeReference GetType(Microsoft.CodeAnalysis.Emit.Context context);
    }

    /// <summary>
    /// This interface models the metadata representation of a function pointer type reference.
    /// </summary>
    internal interface IFunctionPointerTypeReference : ITypeReference, ISignature
    {
        /// <summary>
        /// The types and modifiers of extra arguments that the caller will pass to the methods that are pointed to by this pointer.
        /// </summary>
        IEnumerable<IParameterTypeInformation> ExtraArgumentTypes { get; }
    }

    /// <summary>
    /// The definition of a type parameter of a generic type or method.
    /// </summary>
    internal interface IGenericParameter : IGenericParameterReference
    {
        /// <summary>
        /// A list of classes or interfaces. All type arguments matching this parameter must be derived from all of the classes and implement all of the interfaces.
        /// </summary>
        IEnumerable<ITypeReference> GetConstraints(Microsoft.CodeAnalysis.Emit.Context context);

        /// <summary>
        /// True if all type arguments matching this parameter are constrained to be reference types.
        /// </summary>
        bool MustBeReferenceType
        {
            get;

            // ^ ensures result ==> !this.MustBeValueType;
        }

        /// <summary>
        /// True if all type arguments matching this parameter are constrained to be value types.
        /// </summary>
        bool MustBeValueType
        {
            get;

            // ^ ensures result ==> !this.MustBeReferenceType;
        }

        /// <summary>
        /// True if all type arguments matching this parameter are constrained to be value types or concrete classes with visible default constructors.
        /// </summary>
        bool MustHaveDefaultConstructor { get; }

        /// <summary>
        /// Indicates if the generic type or method with this type parameter is co-, contra-, or non variant with respect to this type parameter.
        /// </summary>
        TypeParameterVariance Variance { get; }

        IGenericMethodParameter AsGenericMethodParameter { get; }
        IGenericTypeParameter AsGenericTypeParameter { get; }
    }

    /// <summary>
    /// A reference to the definition of a type parameter of a generic type or method.
    /// </summary>
    internal interface IGenericParameterReference : ITypeReference, INamedEntity, IParameterListEntry
    {
    }

    /// <summary>
    /// The definition of a type parameter of a generic method.
    /// </summary>
    internal interface IGenericMethodParameter : IGenericParameter, IGenericMethodParameterReference
    {
        /// <summary>
        /// The generic method that defines this type parameter.
        /// </summary>
        new IMethodDefinition DefiningMethod
        {
            get;

            // ^ ensures result.IsGeneric;
        }
    }

    /// <summary>
    /// A reference to a type parameter of a generic method.
    /// </summary>
    internal interface IGenericMethodParameterReference : IGenericParameterReference
    {
        /// <summary>
        /// A reference to the generic method that defines the referenced type parameter.
        /// </summary>
        IMethodReference DefiningMethod { get; }
    }

    /// <summary>
    /// A generic type instantiated with a list of type arguments
    /// </summary>
    internal interface IGenericTypeInstanceReference : ITypeReference
    {
        /// <summary>
        /// The type arguments that were used to instantiate this.GenericType in order to create this type.
        /// </summary>
        IEnumerable<ITypeReference> GetGenericArguments(Microsoft.CodeAnalysis.Emit.Context context);
        // ^ ensures result.GetEnumerator().MoveNext(); // The collection is always non empty.

        /// <summary>
        /// Returns the generic type of which this type is an instance.
        /// Equivalent to Symbol.OriginalDefinition
        /// </summary>
        INamedTypeReference GenericType
        {
            get;

            // ^ ensures result.ResolvedType.IsGeneric;
        }
    }

    /// <summary>
    /// The definition of a type parameter of a generic type.
    /// </summary>
    internal interface IGenericTypeParameter : IGenericParameter, IGenericTypeParameterReference
    {
        /// <summary>
        /// The generic type that defines this type parameter.
        /// </summary>
        new ITypeDefinition DefiningType { get; }
    }

    /// <summary>
    /// A reference to a type parameter of a generic type.
    /// </summary>
    internal interface IGenericTypeParameterReference : IGenericParameterReference
    {
        /// <summary>
        /// A reference to the generic type that defines the referenced type parameter.
        /// </summary>
        ITypeReference DefiningType { get; }
    }

    /// <summary>
    /// A reference to a named type, such as an INamespaceTypeReference or an INestedTypeReference.
    /// </summary>
    internal interface INamedTypeReference : ITypeReference, INamedEntity
    {
        /// <summary>
        /// The number of generic parameters. Zero if the type is not generic.
        /// </summary>
        ushort GenericParameterCount { get; }

        /// <summary>
        /// If true, the persisted type name is mangled by appending "`n" where n is the number of type parameters, if the number of type parameters is greater than 0.
        /// </summary>
        bool MangleName { get; }
    }

    /// <summary>
    /// A named type definition, such as an INamespaceTypeDefinition or an INestedTypeDefinition.
    /// </summary>
    internal interface INamedTypeDefinition : ITypeDefinition, INamedTypeReference
    {
    }

    /// <summary>
    /// A type definition that is a member of a namespace definition.
    /// </summary>
    internal interface INamespaceTypeDefinition : INamedTypeDefinition, INamespaceTypeReference
    {
        /// <summary>
        /// True if the type can be accessed from other assemblies.
        /// </summary>
        bool IsPublic { get; }
    }

    /// <summary>
    /// A reference to a type definition that is a member of a namespace definition.
    /// </summary>
    internal interface INamespaceTypeReference : INamedTypeReference
    {
        /// <summary>
        /// A reference to the unit that defines the referenced type.
        /// </summary>
        IUnitReference GetUnit(Microsoft.CodeAnalysis.Emit.Context context);

        /// <summary>
        /// Fully qualified name of the containing namespace.
        /// </summary>
        string NamespaceName { get; }
    }

    /// <summary>
    /// A type definition that is a member of another type definition.
    /// </summary>
    internal interface INestedTypeDefinition : INamedTypeDefinition, ITypeDefinitionMember, INestedTypeReference
    {
    }

    /// <summary>
    /// A type definition that is a member of another type definition.
    /// </summary>
    internal interface INestedTypeReference : INamedTypeReference, ITypeMemberReference
    {
    }

    /// <summary>
    /// A reference to a type definition that is a specialized nested type.
    /// </summary>
    internal interface ISpecializedNestedTypeReference : INestedTypeReference
    {
        /// <summary>
        /// A reference to the nested type that has been specialized to obtain this nested type reference. When the containing type is an instance of type which is itself a specialized member (i.e. it is a nested
        /// type of a generic type instance), then the unspecialized member refers to a member from the unspecialized containing type. (I.e. the unspecialized member always
        /// corresponds to a definition that is not obtained via specialization.)
        /// </summary>
        INestedTypeReference/*!*/ UnspecializedVersion
        {
            get;
        }
    }

    /// <summary>
    /// Models an explicit implemenation or override of a base class virtual method or an explicit implementation of an interface method.
    /// </summary>
    internal interface IMethodImplementation
    {
        /// <summary>
        /// The type that is explicitly implementing or overriding the base class virtual method or explicitly implementing an interface method.
        /// </summary>
        ITypeDefinition ContainingType { get; }

        /// <summary>
        /// Calls the visitor.Visit(T) method where T is the most derived object model node interface type implemented by the concrete type
        /// of the object implementing IDefinition. The dispatch method does not invoke Dispatch on any child objects. If child traversal
        /// is desired, the implementations of the Visit methods should do the subsequent dispatching.
        /// </summary>
        void Dispatch(MetadataVisitor visitor);

        /// <summary>
        /// A reference to the method whose implementation is being provided or overridden.
        /// </summary>
        IMethodReference ImplementedMethod { get; }

        /// <summary>
        /// A reference to the method that provides the implementation.
        /// </summary>
        IMethodReference ImplementingMethod { get; }
    }

    /// <summary>
    /// A type reference that has custom modifiers associated with it. For example a reference to the target type of a managed pointer to a constant.
    /// </summary>
    internal interface IModifiedTypeReference : ITypeReference
    {
        /// <summary>
        /// Returns the list of custom modifiers associated with the type reference. Evaluate this property only if IsModified is true.
        /// </summary>
        IEnumerable<ICustomModifier> CustomModifiers { get; }

        /// <summary>
        /// An unmodified type reference.
        /// </summary>
        ITypeReference UnmodifiedType { get; }
    }

    /// <summary>
    /// This interface models the metadata representation of a pointer to a location in unmanaged memory.
    /// </summary>
    internal interface IPointerTypeReference : ITypeReference
    {
        /// <summary>
        /// The type of value stored at the target memory location.
        /// </summary>
        ITypeReference GetTargetType(Microsoft.CodeAnalysis.Emit.Context context);
    }

    /// <summary>
    /// This interface models the metadata representation of a managed pointer.
    /// Remark: This should be only used in attributes. For other objects like Local variables etc
    /// there is explicit IsReference field that should be used.
    /// </summary>
    internal interface IManagedPointerTypeReference : ITypeReference
    {
        /// <summary>
        /// The type of value stored at the target memory location.
        /// </summary>
        ITypeReference GetTargetType(Microsoft.CodeAnalysis.Emit.Context context);
    }

    /// <summary>
    /// This interface models the metadata representation of a type.
    /// </summary>
    internal interface ITypeDefinition : IDefinition, ITypeReference
    {
        /// <summary>
        /// The byte alignment that values of the given type ought to have. Must be a power of 2. If zero, the alignment is decided at runtime.
        /// </summary>
        ushort Alignment { get; }

        /// <summary>
        /// Returns null for interfaces and System.Object.
        /// </summary>
        ITypeReference GetBaseClass(Microsoft.CodeAnalysis.Emit.Context context);
        // ^ ensures result == null || result.ResolvedType.IsClass;

        /// <summary>
        /// Zero or more events defined by this type.
        /// </summary>
        IEnumerable<IEventDefinition> Events { get; }

        /// <summary>
        /// Zero or more implementation overrides provided by the class.
        /// </summary>
        IEnumerable<IMethodImplementation> GetExplicitImplementationOverrides(Microsoft.CodeAnalysis.Emit.Context context);

        /// <summary>
        /// Zero or more fields defined by this type.
        /// </summary>
        IEnumerable<IFieldDefinition> GetFields(Microsoft.CodeAnalysis.Emit.Context context);

        /// <summary>
        /// Zero or more parameters that can be used as type annotations.
        /// </summary>
        IEnumerable<IGenericTypeParameter> GenericParameters
        {
            get;
            // ^ requires this.IsGeneric;
        }

        /// <summary>
        /// The number of generic parameters. Zero if the type is not generic.
        /// </summary>
        ushort GenericParameterCount
        { // TODO: remove this
            get;

            // ^ ensures !this.IsGeneric ==> result == 0;
            // ^ ensures this.IsGeneric ==> result > 0;
        }

        /// <summary>
        /// True if this type has a non empty collection of SecurityAttributes or the System.Security.SuppressUnmanagedCodeSecurityAttribute.
        /// </summary>
        bool HasDeclarativeSecurity { get; }

        /// <summary>
        /// Zero or more interfaces implemented by this type.
        /// </summary>
        IEnumerable<ITypeReference> Interfaces(Microsoft.CodeAnalysis.Emit.Context context);

        /// <summary>
        /// True if the type may not be instantiated.
        /// </summary>
        bool IsAbstract { get; }

        /// <summary>
        /// Is type initialized anytime before first access to static field
        /// </summary>
        bool IsBeforeFieldInit { get; }

        /// <summary>
        /// Is this imported from COM type library
        /// </summary>
        bool IsComObject { get; }

        /// <summary>
        /// True if this type is parameterized (this.GenericParameters is a non empty collection).
        /// </summary>
        bool IsGeneric { get; }

        /// <summary>
        /// True if the type is an interface.
        /// </summary>
        bool IsInterface { get; }

        /// <summary>
        /// True if this type gets special treatment from the runtime.
        /// </summary>
        bool IsRuntimeSpecial { get; }

        /// <summary>
        /// True if this type is serializable.
        /// </summary>
        bool IsSerializable { get; }

        /// <summary>
        /// True if the type has special name.
        /// </summary>
        bool IsSpecialName { get; }

        /// <summary>
        /// True if the type is a Windows runtime type.
        /// </summary>
        /// <remarks>
        /// A type can me marked as a Windows runtime type in source by applying the WindowsRuntimeImportAttribute.
        /// WindowsRuntimeImportAttribute is a pseudo custom attribute defined as an internal class in System.Runtime.InteropServices.WindowsRuntime namespace.
        /// This is needed to mark Windows runtime types which are redefined in mscorlib.dll and System.Runtime.WindowsRuntime.dll.
        /// These two assemblies are special as they implement the CLR's support for WinRT.
        /// </remarks>
        bool IsWindowsRuntimeImport { get; }

        /// <summary>
        /// True if the type may not be subtyped.
        /// </summary>
        bool IsSealed { get; }

        /// <summary>
        /// Layout of the type.
        /// </summary>
        LayoutKind Layout { get; }

        /// <summary>
        /// Zero or more methods defined by this type.
        /// </summary>
        IEnumerable<IMethodDefinition> GetMethods(Microsoft.CodeAnalysis.Emit.Context context);

        /// <summary>
        /// Zero or more nested types defined by this type.
        /// </summary>
        IEnumerable<INestedTypeDefinition> GetNestedTypes(Microsoft.CodeAnalysis.Emit.Context context);

        /// <summary>
        /// Zero or more properties defined by this type.
        /// </summary>
        IEnumerable<IPropertyDefinition> GetProperties(Microsoft.CodeAnalysis.Emit.Context context);

        /// <summary>
        /// Declarative security actions for this type. Will be empty if this.HasSecurity is false.
        /// </summary>
        IEnumerable<SecurityAttribute> SecurityAttributes { get; }

        /// <summary>
        /// Size of an object of this type. In bytes. If zero, the size is unspecified and will be determined at runtime.
        /// </summary>
        uint SizeOf { get; }

        /// <summary>
        /// Default marshalling of the Strings in this class.
        /// </summary>
        CharSet StringFormat { get; }
    }

    /// <summary>
    /// A reference to a type.
    /// </summary>
    internal interface ITypeReference : IReference
    {
        /// <summary>
        /// True if the type is an enumeration (it extends System.Enum and is sealed). Corresponds to C# enum.
        /// </summary>
        bool IsEnum { get; }

        /// <summary>
        /// True if the type is a value type. 
        /// Value types are sealed and extend System.ValueType or System.Enum.
        /// A type parameter for which MustBeValueType (the struct constraint in C#) is true also returns true for this property.
        /// </summary>
        bool IsValueType { get; }

        /// <summary>
        /// The type definition being referred to.
        /// In case this type was alias, this is also the type of the aliased type
        /// </summary>
        ITypeDefinition GetResolvedType(Microsoft.CodeAnalysis.Emit.Context context);
        // ^ ensures this.IsAlias ==> result == this.AliasForType.AliasedType.ResolvedType;
        // ^ ensures (this is ITypeDefinition) ==> result == this;

        /// <summary>
        /// Unless the value of TypeCode is PrimitiveTypeCode.NotPrimitive, the type corresponds to a "primitive" CLR type (such as System.Int32) and
        /// the type code identifies which of the primitive types it corresponds to.
        /// </summary>
        PrimitiveTypeCode TypeCode(Microsoft.CodeAnalysis.Emit.Context context);

        /// <summary>
        /// TypeDefs defined in modules linked to the assembly being emitted are listed in the ExportedTypes table.
        /// </summary>
        TypeHandle TypeDef { get; }

        IGenericMethodParameterReference AsGenericMethodParameterReference { get; }
        IGenericTypeInstanceReference AsGenericTypeInstanceReference { get; }
        IGenericTypeParameterReference AsGenericTypeParameterReference { get; }
        INamespaceTypeDefinition AsNamespaceTypeDefinition(Microsoft.CodeAnalysis.Emit.Context context);
        INamespaceTypeReference AsNamespaceTypeReference { get; }
        INestedTypeDefinition AsNestedTypeDefinition(Microsoft.CodeAnalysis.Emit.Context context);
        INestedTypeReference AsNestedTypeReference { get; }
        ISpecializedNestedTypeReference AsSpecializedNestedTypeReference { get; }
        ITypeDefinition AsTypeDefinition(Microsoft.CodeAnalysis.Emit.Context context);
    }

    /// <summary>
    /// A enumeration of all of the value types that are built into the Runtime (and thus have specialized IL instructions that manipulate them).
    /// </summary>
    internal enum PrimitiveTypeCode
    {
        /// <summary>
        /// A single bit.
        /// </summary>
        Boolean,

        /// <summary>
        /// An usigned 16 bit integer representing a Unicode UTF16 code point.
        /// </summary>
        Char,

        /// <summary>
        /// A signed 8 bit integer.
        /// </summary>
        Int8,

        /// <summary>
        /// A 32 bit IEEE floating point number.
        /// </summary>
        Float32,

        /// <summary>
        /// A 64 bit IEEE floating point number.
        /// </summary>
        Float64,

        /// <summary>
        /// A signed 16 bit integer.
        /// </summary>
        Int16,

        /// <summary>
        /// A signed 32 bit integer.
        /// </summary>
        Int32,

        /// <summary>
        /// A signed 64 bit integer.
        /// </summary>
        Int64,

        /// <summary>
        /// A signed 32 bit integer or 64 bit integer, depending on the native word size of the underlying processor.
        /// </summary>
        IntPtr,

        /// <summary>
        /// A pointer to fixed or unmanaged memory.
        /// </summary>
        Pointer,

        /// <summary>
        /// A reference to managed memory.
        /// </summary>
        Reference,

        /// <summary>
        /// A string.
        /// </summary>
        String,

        /// <summary>
        /// An unsigned 8 bit integer.
        /// </summary>
        UInt8,

        /// <summary>
        /// An unsigned 16 bit integer.
        /// </summary>
        UInt16,

        /// <summary>
        /// An unsigned 32 bit integer.
        /// </summary>
        UInt32,

        /// <summary>
        /// An unsigned 64 bit integer.
        /// </summary>
        UInt64,

        /// <summary>
        /// An unsigned 32 bit integer or 64 bit integer, depending on the native word size of the underlying processor.
        /// </summary>
        UIntPtr,

        /// <summary>
        /// A type that denotes the absense of a value.
        /// </summary>
        Void,

        /// <summary>
        /// Not a primitive type.
        /// </summary>
        NotPrimitive,

        /// <summary>
        /// Type is a dummy type.
        /// </summary>
        Invalid,
    }

    /// <summary>
    /// Enumerates the different kinds of levels of visibility a type member can have.
    /// </summary>
    internal enum TypeMemberVisibility
    {
        /// <summary>
        /// The visibility has not been specified. Use the applicable default.
        /// </summary>
        Default,

        /// <summary>
        /// The member is visible only within its own assembly.
        /// </summary>
        Assembly,

        /// <summary>
        /// The member is visible only within its own type and any subtypes.
        /// </summary>
        Family,

        /// <summary>
        /// The member is visible only within the intersection of its family (its own type and any subtypes) and assembly. 
        /// </summary>
        FamilyAndAssembly,

        /// <summary>
        /// The member is visible only within the union of its family and assembly. 
        /// </summary>
        FamilyOrAssembly,

        /// <summary>
        /// The member is visible only to the compiler producing its assembly.
        /// </summary>
        Other,

        /// <summary>
        /// The member is visible only within its own type.
        /// </summary>
        Private,

        /// <summary>
        /// The member is visible everywhere its declaring type is visible.
        /// </summary>
        Public,

        /// <summary>
        /// A mask that can be used to mask out flag bits when the latter are stored in the same memory word as this enumeration.
        /// </summary>
        Mask = 0xF
    }

    /// <summary>
    /// Enumerates the different kinds of variance a generic method or generic type parameter may have.
    /// </summary>
    internal enum TypeParameterVariance
    {
        /// <summary>
        /// Two type or method instances are compatible only if they have exactly the same type argument for this parameter.
        /// </summary>
        NonVariant,

        /// <summary>
        /// A type or method instance will match another instance if it has a type for this parameter that is the same or a subtype of the type the
        /// other instance has for this parameter.
        /// </summary>
        Covariant,

        /// <summary>
        /// A type or method instance will match another instance if it has a type for this parameter that is the same or a supertype of the type the
        /// other instance has for this parameter.
        /// </summary>
        Contravariant,

        /// <summary>
        /// A mask that can be used to mask out flag bits when the latter are stored in the same memory word as the enumeration.
        /// </summary>
        Mask = 3,
    }
}