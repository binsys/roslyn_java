﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Reflection;
using System.Runtime.CompilerServices;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;
using System.Diagnostics;

namespace Microsoft.Cci
{
    /// <summary>
    /// Specifies how the caller passes parameters to the callee and who cleans up the stack.
    /// </summary>
    [Flags]
    internal enum CallingConvention
    {
        /// <summary>
        /// C/C++ style calling convention for unmanaged methods. The call stack is cleaned up by the caller, 
        /// which makes this convention suitable for calling methods that accept extra arguments.
        /// </summary>
        C = 1,

        /// <summary>
        /// The convention for calling managed methods with a fixed number of arguments.
        /// </summary>
        Default = 0,

        /// <summary>
        /// The convention for calling managed methods that accept extra arguments.
        /// </summary>
        ExtraArguments = 5,

        /// <summary>
        /// Arguments are passed in registers when possible. This calling convention is not yet supported.
        /// </summary>
        FastCall = 4,

        /// <summary>
        /// Win32 API calling convention for calling unmanged methods via PlatformInvoke. The call stack is cleaned up by the callee.
        /// </summary>
        Standard = 2,

        /// <summary>
        /// C++ member unmanaged method (non-vararg) calling convention. The callee cleans the stack and the this pointer is pushed on the stack last.
        /// </summary>
        ThisCall = 3,

        /// <summary>
        /// The convention for calling a generic method.
        /// </summary>
        Generic = 0x10,

        /// <summary>
        /// The convention for calling an instance method with an implicit this parameter (the method does not have an explicit parameter definition for this).
        /// </summary>
        HasThis = 0x20,

        /// <summary>
        /// The convention for calling an instance method that explicitly declares its first parameter to correspond to the this instance.
        /// </summary>
        ExplicitThis = 0x40
    }

    /// <summary>
    /// An event is a member that enables an object or class to provide notifications. Clients can attach executable code for events by supplying event handlers.
    /// This interface models the metadata representation of an event.
    /// </summary>
    internal interface IEventDefinition : ITypeDefinitionMember
    {
        /// <summary>
        /// A list of methods that are associated with the event.
        /// </summary>
        IEnumerable<IMethodReference> Accessors { get; }

        /// <summary>
        /// The method used to add a handler to the event.
        /// </summary>
        IMethodReference Adder { get; }

        /// <summary>
        /// The method used to call the event handlers when the event occurs. May be null.
        /// </summary>
        IMethodReference/*?*/ Caller { get; }

        /// <summary>
        /// True if the event gets special treatment from the runtime.
        /// </summary>
        bool IsRuntimeSpecial { get; }

        /// <summary>
        /// This event is special in some way, as specified by the name.
        /// </summary>
        bool IsSpecialName { get; }

        /// <summary>
        /// The method used to add a handler to the event.
        /// </summary>
        IMethodReference Remover { get; }

        /// <summary>
        /// The (delegate) type of the handlers that will handle the event.
        /// </summary>
        ITypeReference GetType(Microsoft.CodeAnalysis.Emit.Context context);
    }

    /// <summary>
    /// A field is a member that represents a variable associated with an object or class.
    /// This interface models the metadata representation of a field.
    /// </summary>
    internal interface IFieldDefinition : ITypeDefinitionMember, IFieldReference
    {
        /// <summary>
        /// The compile time value of the field. This value should be used directly in IL, rather than a reference to the field.
        /// If the field does not have a valid compile time value, Dummy.Constant is returned.
        /// </summary>
        IMetadataConstant GetCompileTimeValue(Microsoft.CodeAnalysis.Emit.Context context);

        /// <summary>
        /// Information of the location where this field is mapped to
        /// </summary>
        ISectionBlock FieldMapping
        {
            get;

            // ^ requires this.IsMapped;
        }

        /// <summary>
        /// This field is a compile-time constant. The field has no runtime location and cannot be directly addressed from IL.
        /// </summary>
        bool IsCompileTimeConstant { get; }

        /// <summary>
        /// This field is mapped to an explicitly initialized (static) memory location.
        /// </summary>
        bool IsMapped
        {
            get;

            // ^ ensures result ==> this.IsStatic;
            // ^ ensures !result || this.IsStatic;
        }

        /// <summary>
        /// This field has associated field marshalling information.
        /// </summary>
        bool IsMarshalledExplicitly { get; }

        /// <summary>
        /// The field does not have to be serialized when its containing instance is serialized.
        /// </summary>
        bool IsNotSerialized { get; }

        /// <summary>
        /// This field can only be read. Initialization takes place in a constructor.
        /// </summary>
        bool IsReadOnly { get; }

        /// <summary>
        /// True if the field gets special treatment from the runtime.
        /// </summary>
        bool IsRuntimeSpecial { get; }

        /// <summary>
        /// This field is special in some way, as specified by the name.
        /// </summary>
        bool IsSpecialName { get; }

        /// <summary>
        /// This field is static (shared by all instances of its declaring type).
        /// </summary>
        bool IsStatic { get; }

        /// <summary>
        /// Specifies how this field is marshalled when it is accessed from unmanaged code.
        /// </summary>
        IMarshallingInformation MarshallingInformation
        {
            get;

            // ^ requires this.IsMarshalledExplicitly;
        }

        /// <summary>
        /// Checked if IsMarshalledExplicitly == true and MarshallingInformation is null
        /// </summary>
        ImmutableArray<byte> MarshallingDescriptor
        {
            get;

            // ^ requires this.IsMarshalledExplicitly;
        }

        /// <summary>
        /// Offset of the field.
        /// </summary>
        uint Offset
        {
            get;

            // ^ requires this.ContainingTypeDefinition.Layout == LayoutKind.Explicit;
        }
    }

    /// <summary>
    /// A reference to a field.
    /// </summary>
    internal interface IFieldReference : ITypeMemberReference
    { // TODO: add custom modifiers

        /// <summary>
        /// The type of value that is stored in this field.
        /// </summary>
        ITypeReference GetType(Microsoft.CodeAnalysis.Emit.Context context);

        /// <summary>
        /// The Field being referred to.
        /// </summary>
        IFieldDefinition GetResolvedField(Microsoft.CodeAnalysis.Emit.Context context);


        ISpecializedFieldReference AsSpecializedFieldReference { get; }


        /// <summary>
        /// True, if field is an IContextualNamedEntity, even if field reference implements the interface,
        /// doesn't mean it is contextual.
        /// </summary>
        bool IsContextualNamedEntity
        {
            get;
        }
    }

    /// <summary>
    /// An object that represents a local variable or constant.
    /// </summary>
    internal interface ILocalDefinition : INamedEntity
    {
        /// <summary>
        /// The compile time value of the definition, if it is a local constant.
        /// </summary>
        IMetadataConstant CompileTimeValue
        {
            get;

            // ^ requires this.IsConstant;
        }

        /// <summary>
        /// Custom modifiers associated with local variable definition.
        /// </summary>
        IEnumerable<ICustomModifier> CustomModifiers
        {
            get;

            // ^ requires this.IsModified;
        }

        /// <summary>
        /// True if this local definition is readonly and initialized with a compile time constant value.
        /// </summary>
        bool IsConstant { get; }

        /// <summary>
        /// The local variable has custom modifiers.
        /// </summary>
        bool IsModified { get; }

        /// <summary>
        /// True if the value referenced by the local must not be moved by the actions of the garbage collector.
        /// </summary>
        bool IsPinned { get; }

        /// <summary>
        /// True if the local contains a managed pointer (for example a reference to a local variable or a reference to a field of an object).
        /// </summary>
        bool IsReference { get; }

        /// <summary>
        /// True if the local variable is of type Dynamic.
        /// </summary>
        bool IsDynamic { get; }

        /// <summary>
        /// True if the local was not declared in source.
        /// </summary>
        bool IsCompilerGenerated { get; }

        /// <summary>
        /// Should return the synthesized dynamic attributes of the local definition if any. Else null.
        /// </summary>
        ImmutableArray<TypedConstant> DynamicTransformFlags { get; }

        /// <summary>
        /// The type of the local.
        /// </summary>
        ITypeReference Type { get; }

        /// <summary>
        /// Location for reporting diagnostics about the local.
        /// </summary>
        /// <remark>
        /// Use <see cref="Location.None"/> rather than null.
        /// </remark>
        Location Location { get; }
    }

    /// <summary>
    /// Represents additional info needed by async method implementation methods 
    /// (MoveNext() methods) to properly emit necessary PDB data for async debugging.
    /// </summary>
    internal class AsyncMethodBodyDebugInfo
    {
        /// <summary> Original async method transformed into MoveNext() </summary>
        public readonly IMethodDefinition KickoffMethod;

        /// <summary> IL offset of catch handler or -1 </summary>
        public readonly int CatchHandlerOffset;

        /// <summary> Set of IL offsets where await operators yield control </summary>
        public readonly ImmutableArray<int> YieldOffsets;

        /// <summary> Set of IL offsets where await operators are to be resumed </summary>
        public readonly ImmutableArray<int> ResumeOffsets;

        public AsyncMethodBodyDebugInfo(
            IMethodDefinition kickoffMethod,
            int catchHandlerOffset,
            ImmutableArray<int> yieldOffsets,
            ImmutableArray<int> resumeOffsets)
        {
            Debug.Assert(!yieldOffsets.IsDefault && !resumeOffsets.IsDefault && yieldOffsets.Length == resumeOffsets.Length);
            this.KickoffMethod = kickoffMethod;
            this.CatchHandlerOffset = catchHandlerOffset;
            this.YieldOffsets = yieldOffsets;
            this.ResumeOffsets = resumeOffsets;
        }
    }

    /// <summary>
    /// A metadata (IL) level represetation of the body of a method or of a property/event accessor.
    /// </summary>
    internal interface IMethodBody
    {
        /// <summary>
        /// Calls the visitor.Visit(T) method where T is the most derived object model node interface type implemented by the concrete type
        /// of the object implementing IDoubleDispatcher. The dispatch method does not invoke Dispatch on any child objects. If child traversal
        /// is desired, the implementations of the Visit methods should do the subsequent dispatching.
        /// </summary>
        void Dispatch(MetadataVisitor visitor);

        ///// <summary>
        ///// Returns the IL operation that is located at the given offset. If no operation exists the given offset, Dummy.Operation is returned.
        ///// The offset of the operation that follows the operation at the given offset is returned as the value of the second parameter.
        ///// If the given offset is invalid, or is the offset of the last operation in the method body, offsetOfNextOperation will be set to -1.
        ///// </summary>
        ///// <param name="offset">The offset of the operation to be returned by this method.</param>
        ///// <param name="offsetOfNextOperation">The offset of the operation that follows the one returned by this method. If no such operation exists, the value is -1.</param>
        // IOperation GetOperationAt(int offset, out int offsetOfNextOperation);
        //// ^ requires 0 <= offset;
        //// ^ ensures offsetOfNextOperation == -1 || offsetOfNextOperation > offset;

        /// <summary>
        /// A list exception data within the method body IL.
        /// </summary>
        IEnumerable<ExceptionHandlerRegion> ExceptionRegions
        {
            get;

            // ^ requires !this.MethodDefinition.IsAbstract && !this.MethodDefinition.IsExternal && this.MethodDefinition.IsCil;
        }

        /// <summary>
        /// True if the locals are initialized by zeroeing the stack upon method entry.
        /// </summary>
        bool LocalsAreZeroed { get; }

        /// <summary>
        /// The local variables of the method.
        /// </summary>
        IEnumerable<ILocalDefinition> LocalVariables { get; }

        /// <summary>
        /// The definition of the method whose body this is.
        /// If this is the body of an event or property accessor, this will hold the corresponding adder/remover/setter or getter method.
        /// </summary>
        IMethodDefinition MethodDefinition
        {
            get;
        }

        /// <summary> The additional info required by debugger  </summary>
        AsyncMethodBodyDebugInfo AsyncMethodDebugInfo { get; }

        /// <summary>
        /// The maximum number of elements on the evaluation stack during the execution of the method.
        /// </summary>
        ushort MaxStack { get; }

        byte[] IL { get; }
        bool HasAnyLocations { get; }
        bool HasAnySequencePoints { get; }
        ImmutableArray<SequencePoint> GetSequencePoints();
        ImmutableArray<SequencePoint> GetLocations();

        /// <summary>
        /// The PDB content for custom debug information is different between Visual Basic and CSharp.
        /// E.g. CS always includes a CustomMetadata Header (MD2) that contains the namespace scope counts, where 
        /// as VB only outputs namespace imports into the namespace scopes. CS defines forwards in that header, VB includes
        /// them into the scopes list.
        /// This enum is used to distinguish which style to pick while writing the PDB information.
        /// </summary>
        CustomDebugInfoKind CustomDebugInfoKind { get; }

        /// <summary>
        /// Returns true if there is atleast one dynamic local within the MethodBody
        /// </summary>
        bool HasDynamicLocalVariables { get; }

        /// <summary>
        /// Returns zero or more local (block) scopes into which the CLR IL operations in the given method body is organized.
        /// </summary>
        ImmutableArray<ILocalScope> LocalScopes { get; }

        /// <summary>
        /// Returns zero or more namespace scopes into which the namespace type containing the given method body has been nested.
        /// These scopes determine how simple names are looked up inside the method body. There is a separate scope for each dotted
        /// component in the namespace type name. For istance namespace type x.y.z will have two namespace scopes, the first is for the x and the second
        /// is for the y.
        /// </summary>
        ImmutableArray<INamespaceScope> NamespaceScopes { get; }

        /// <summary>
        /// Returns zero or more local (block) scopes, each defining an IL range in which an iterator local is defined.
        /// The scopes are returned for the MoveNext method of the object returned by the iterator method.
        /// The index of the scope corresponds to the index of the local.  Specifically local scope i corresponds
        /// to the local stored in a field named &lt;localName&gt;5__i of the class used to store the local values in
        /// between calls to MoveNext, where localName is the original name of the local variable.  For example, if
        /// the first local to be moved into the class is named "xyzzy", it will be stored in a field named
        /// "&lt;xyzzy&gt;5__1", and the ILocalScope returned from this method at index 1 (i.e. the second one) will
        /// have the scope information for where that variable is in scope.
        /// </summary>
        ImmutableArray<ILocalScope> IteratorScopes { get; }

        /// <summary>
        /// If the body was written as an iterator, returns the name of the (nested)
        /// type that implements the iterator's state machine.
        /// </summary>
        string IteratorClassName { get; }
    }

    /// <summary>
    /// This enum is used to distinguish which style to pick while writing the PDB information.
    /// </summary>
    internal enum CustomDebugInfoKind
    {
        CSharpStyle,
        VisualBasicStyle
    }

    /// <summary>
    /// This interface models the metadata representation of a method.
    /// </summary>
    internal interface IMethodDefinition : ITypeDefinitionMember, IMethodReference
    {
        /// <summary>
        /// A container for a list of IL instructions providing the implementation (if any) of this method.
        /// </summary>
        /// <remarks>
        /// When emitting metadata-only assemblies this returns null even if <see cref="M:Cci.Extensions.HasBody"/> returns true.
        /// </remarks>
        IMethodBody GetBody(Microsoft.CodeAnalysis.Emit.Context context);

        /// <summary>
        /// If the method is generic then this list contains the type parameters.
        /// </summary>
        IEnumerable<IGenericMethodParameter> GenericParameters
        {
            get;

            // ^ requires this.IsGeneric;
        }

        /// <summary>
        /// True if this method has a non empty collection of SecurityAttributes or the System.Security.SuppressUnmanagedCodeSecurityAttribute.
        /// </summary>
        bool HasDeclarativeSecurity { get; }

        /// <summary>
        /// True if the method does not provide an implementation.
        /// </summary>
        bool IsAbstract { get; }

        /// <summary>
        /// True if the method can only be overridden when it is also accessible. 
        /// </summary>
        bool IsAccessCheckedOnOverride { get; }

        /// <summary>
        /// True if the method is a constructor.
        /// </summary>
        bool IsConstructor { get; }

        /// <summary>
        /// True if the method has an external implementation (i.e. not supplied by this definition).
        /// </summary>
        /// <remarks>
        /// If the method is not external and not abstract it has to provide an IL body.
        /// </remarks>
        bool IsExternal { get; }

        /// <summary>
        /// True if this method is hidden if a derived type declares a method with the same name and signature. 
        /// If false, any method with the same name hides this method. This flag is ignored by the runtime and is only used by compilers.
        /// </summary>
        bool IsHiddenBySignature { get; }

        /// <summary>
        /// The method always gets a new slot in the virtual method table. 
        /// This means the method will hide (not override) a base type method with the same name and signature.
        /// </summary>
        bool IsNewSlot { get; }

        /// <summary>
        /// True if the method is implemented via the invocation of an underlying platform method.
        /// </summary>
        bool IsPlatformInvoke { get; }

        /// <summary>
        /// True if the method gets special treatment from the runtime. For example, it might be a constructor.
        /// </summary>
        bool IsRuntimeSpecial { get; }

        /// <summary>
        /// True if the method may not be overridden.
        /// </summary>
        bool IsSealed { get; }

        /// <summary>
        /// True if the method is special in some way for tools. For example, it might be a property getter or setter.
        /// </summary>
        bool IsSpecialName { get; }

        /// <summary>
        /// True if the method does not require an instance of its declaring type as its first argument.
        /// </summary>
        bool IsStatic { get; }

        /// <summary>
        /// True if the method may be overridden (or if it is an override).
        /// </summary>
        bool IsVirtual
        {
            get;
            // ^ ensures result ==> !this.IsStatic;
        }

        /// <summary>
        /// Implementation flags.
        /// </summary>
        MethodImplAttributes GetImplementationAttributes(Microsoft.CodeAnalysis.Emit.Context context);

        /// <summary>
        /// The parameters forming part of this signature.
        /// </summary>
        IEnumerable<IParameterDefinition> Parameters { get; }

        /// <summary>
        /// Detailed information about the PInvoke stub. Identifies which method to call, which module has the method and the calling convention among other things.
        /// </summary>
        IPlatformInvokeInformation PlatformInvokeData
        {
            get;

            // ^ requires this.IsPlatformInvoke;
        }

        /// <summary>
        /// True if the method calls another method containing security code. If this flag is set, the method
        /// should have System.Security.DynamicSecurityMethodAttribute present in its list of custom attributes.
        /// </summary>
        bool RequiresSecurityObject { get; }

        /// <summary>
        /// Custom attributes associated with the method's return value.
        /// </summary>
        IEnumerable<ICustomAttribute> ReturnValueAttributes { get; }

        /// <summary>
        /// The return value has associated marshalling information.
        /// </summary>
        bool ReturnValueIsMarshalledExplicitly { get; }

        /// <summary>
        /// Specifies how the return value is marshalled when the method is called from unmanaged code.
        /// </summary>
        IMarshallingInformation ReturnValueMarshallingInformation
        {
            get;

            // ^ requires this.ReturnValueIsMarshalledExplicitly;
        }

        /// <summary>
        /// Checked if ReturnValueIsMarshalledExplicitly == true and ReturnValueMarshallingInformation is null
        /// </summary>
        ImmutableArray<byte> ReturnValueMarshallingDescriptor
        {
            get;

            // ^ requires this.ReturnValueIsMarshalledExplicitly;
        }

        /// <summary>
        /// Declarative security actions for this method.
        /// </summary>
        IEnumerable<SecurityAttribute> SecurityAttributes { get; }
    }

    /// <summary>
    /// This interface models the metadata representation of a method or property parameter.
    /// </summary>
    internal interface IParameterDefinition : IDefinition, INamedEntity, IParameterTypeInformation
    {
        /// <summary>
        /// A compile time constant value that should be supplied as the corresponding argument value by callers that do not explicitly specify an argument value for this parameter.
        /// Null if the parameter doesn't have default value.
        /// </summary>
        IMetadataConstant GetDefaultValue(Microsoft.CodeAnalysis.Emit.Context context);

        /// <summary>
        /// True if the parameter has a default value that should be supplied as the argument value by a caller for which the argument value has not been explicitly specified.
        /// </summary>
        bool HasDefaultValue { get; }

        /// <summary>
        /// True if the argument value must be included in the marshalled arguments passed to a remote callee.
        /// </summary>
        bool IsIn { get; }

        /// <summary>
        /// This parameter has associated marshalling information.
        /// </summary>
        bool IsMarshalledExplicitly { get; }

        /// <summary>
        /// True if the argument value must be included in the marshalled arguments passed to a remote callee only if it is different from the default value (if there is one).
        /// </summary>
        bool IsOptional
        {
            get;

            // ^ result ==> this.HasDefaultValue;
        }

        /// <summary>
        /// True if the final value assigned to the parameter will be marshalled with the return values passed back from a remote callee.
        /// </summary>
        bool IsOut { get; }

        /// <summary>
        /// Specifies how this parameter is marshalled when it is accessed from unmanaged code.
        /// </summary>
        IMarshallingInformation MarshallingInformation
        {
            get;

            // ^ requires this.IsMarshalledExplicitly;
        }

        /// <summary>
        /// Checked if IsMarshalledExplicitly == true and MarshallingInformation is null
        /// </summary>
        ImmutableArray<byte> MarshallingDescriptor
        {
            get;

            // ^ requires this.IsMarshalledExplicitly;
        }
    }

    /// <summary>
    /// A property is a member that provides access to an attribute of an object or a class.
    /// This interface models the metadata representation of a property.
    /// </summary>
    internal interface IPropertyDefinition : ISignature, ITypeDefinitionMember
    {
        /// <summary>
        /// A list of methods that are associated with the property.
        /// </summary>
        IEnumerable<IMethodReference> Accessors { get; }

        /// <summary>
        /// A compile time constant value that provides the default value for the property. (Who uses this and why?)
        /// </summary>
        IMetadataConstant DefaultValue
        {
            get;

            // ^ requires this.HasDefaultValue;
        }

        /// <summary>
        /// The method used to get the value of this property. May be absent (null).
        /// </summary>
        IMethodReference/*?*/ Getter { get; }

        /// <summary>
        /// True if this property has a compile time constant associated with that serves as a default value for the property. (Who uses this and why?)
        /// </summary>
        bool HasDefaultValue { get; }

        /// <summary>
        /// True if this property gets special treatment from the runtime.
        /// </summary>
        bool IsRuntimeSpecial { get; }

        /// <summary>
        /// True if this property is special in some way, as specified by the name.
        /// </summary>
        bool IsSpecialName { get; }

        /// <summary>
        /// The parameters forming part of this signature.
        /// </summary>
        IEnumerable<IParameterDefinition> Parameters { get; }

        /// <summary>
        /// The method used to set the value of this property. May be absent (null).
        /// </summary>
        IMethodReference/*?*/ Setter { get; }
    }

    /// <summary>
    /// The parameters and return type that makes up a method or property signature.
    /// This interface models the metadata representation of a signature.
    /// </summary>
    internal interface ISignature
    {
        /// <summary>
        /// Calling convention of the signature.
        /// </summary>
        CallingConvention CallingConvention { get; }

        /// <summary>
        /// The number of required parameters of the signature.
        /// </summary>
        ushort ParameterCount { get; }

        /// <summary>
        /// The parameters forming part of this signature.
        /// </summary>
        IEnumerable<IParameterTypeInformation> GetParameters(Microsoft.CodeAnalysis.Emit.Context context);

        /// <summary>
        /// Returns the list of custom modifiers, if any, associated with the returned value. Evaluate this property only if ReturnValueIsModified is true.
        /// </summary>
        IEnumerable<ICustomModifier> ReturnValueCustomModifiers
        {
            get;

            // ^ requires this.ReturnValueIsModified;
        }

        /// <summary>
        /// True if the return value is passed by reference (using a managed pointer).
        /// </summary>
        bool ReturnValueIsByRef { get; }

        /// <summary>
        /// True if the return value has one or more custom modifiers associated with it.
        /// </summary>
        bool ReturnValueIsModified { get; }

        /// <summary>
        /// The return type of the method or type of the property.
        /// </summary>
        ITypeReference GetType(Microsoft.CodeAnalysis.Emit.Context context);
    }

    /// <summary>
    /// A member of a type definition, such as a field or a method.
    /// This interface models the metadata representation of a type member.
    /// </summary>
    internal interface ITypeDefinitionMember : ITypeMemberReference, IDefinition
    {
        /// <summary>
        /// The type definition that contains this member.
        /// </summary>
        ITypeDefinition ContainingTypeDefinition { get; }

        /// <summary>
        /// Indicates if the member is public or confined to its containing type, derived types and/or declaring assembly.
        /// </summary>
        TypeMemberVisibility Visibility { get; }
    }

    /// <summary>
    /// A reference to a member of a type, such as a field or a method.
    /// This interface models the metadata representation of a type member reference.
    /// </summary>
    internal interface ITypeMemberReference : IReference, INamedEntity
    {
        /// <summary>
        /// A reference to the containing type of the referenced type member.
        /// </summary>
        ITypeReference GetContainingType(Microsoft.CodeAnalysis.Emit.Context context);
    }

    /// <summary>
    /// Represents the specialized event definition.
    /// </summary>
    internal interface ISpecializedEventDefinition : IEventDefinition
    {
        /// <summary>
        /// The event that has been specialized to obtain this event. When the containing type is an instance of type which is itself a specialized member (i.e. it is a nested
        /// type of a generic type instance), then the unspecialized member refers to a member from the unspecialized containing type. (I.e. the unspecialized member always
        /// corresponds to a definition that is not obtained via specialization.)
        /// </summary>
        IEventDefinition/*!*/ UnspecializedVersion
        {
            get;
        }
    }

    /// <summary>
    /// Represents reference specialized field.
    /// </summary>
    internal interface ISpecializedFieldReference : IFieldReference
    {
        /// <summary>
        /// A reference to the field definition that has been specialized to obtain the field definition referred to by this field reference. 
        /// When the containing type of the referenced specialized field definition is itself a specialized nested type of a generic type instance, 
        /// then the unspecialized field reference refers to the corresponding field definition from the unspecialized containing type definition.
        /// (I.e. the unspecialized field reference always refers to a field definition that is not obtained via specialization.)
        /// </summary>
        IFieldReference UnspecializedVersion { get; }
    }

    /// <summary>
    /// Represents reference specialized method.
    /// </summary>
    internal interface ISpecializedMethodReference : IMethodReference
    {
        /// <summary>
        /// A reference to the method definition that has been specialized to obtain the method definition referred to by this method reference. 
        /// When the containing type of the referenced specialized method definition is itself a specialized nested type of a generic type instance, 
        /// then the unspecialized method reference refers to the corresponding method definition from the unspecialized containing type definition.
        /// (I.e. the unspecialized method reference always refers to a method definition that is not obtained via specialization.)
        /// </summary>
        IMethodReference UnspecializedVersion { get; }
    }

    /// <summary>
    /// Represents the specialized property definition.
    /// </summary>
    internal interface ISpecializedPropertyDefinition : IPropertyDefinition
    {
        /// <summary>
        /// The property that has been specialized to obtain this property. When the containing type is an instance of type which is itself a specialized member (i.e. it is a nested
        /// type of a generic type instance), then the unspecialized member refers to a member from the unspecialized containing type. (I.e. the unspecialized member always
        /// corresponds to a definition that is not obtained via specialization.)
        /// </summary>
        IPropertyDefinition/*!*/ UnspecializedVersion
        {
            get;
        }
    }

    /// <summary>
    /// A reference to a method.
    /// </summary>
    internal interface IMethodReference : ISignature, ITypeMemberReference
    {
        /// <summary>
        /// True if the call sites that references the method with this object supply extra arguments.
        /// </summary>
        bool AcceptsExtraArguments { get; }

        /// <summary>
        /// The number of generic parameters of the method. Zero if the referenced method is not generic.
        /// </summary>
        ushort GenericParameterCount
        {
            get;

            // ^ ensures !this.IsGeneric ==> result == 0;
            // ^ ensures this.IsGeneric ==> result > 0;
        }

        /// <summary>
        /// True if the method has generic parameters;
        /// </summary>
        bool IsGeneric { get; }

        /// <summary>
        /// The method being referred to.
        /// </summary>
        IMethodDefinition GetResolvedMethod(Microsoft.CodeAnalysis.Emit.Context context);
        // ^ ensures this is IMethodDefinition ==> result == this;

        /// <summary>
        /// Information about this types of the extra arguments supplied at the call sites that references the method with this object.
        /// </summary>
        IEnumerable<IParameterTypeInformation> ExtraParameters { get; }

        IGenericMethodInstanceReference AsGenericMethodInstanceReference { get; }
        ISpecializedMethodReference AsSpecializedMethodReference { get; }
    }

    /// <summary>
    /// A reference to generic method instantiated with a list of type arguments.
    /// </summary>
    internal interface IGenericMethodInstanceReference : IMethodReference
    {
        /// <summary>
        /// The type arguments that were used to instantiate this.GenericMethod in order to create this method.
        /// </summary>
        IEnumerable<ITypeReference> GetGenericArguments(Microsoft.CodeAnalysis.Emit.Context context);
        // ^ ensures result.GetEnumerator().MoveNext(); // The collection is always non empty.

        /// <summary>
        /// Returns the generic method of which this method is an instance.
        /// </summary>
        IMethodReference GetGenericMethod(Microsoft.CodeAnalysis.Emit.Context context);
        // ^ ensures result.ResolvedMethod.IsGeneric;
    }

    /// <summary>
    /// Represents a global field in symbol table.
    /// </summary>
    internal interface IGlobalFieldDefinition : IFieldDefinition
    {
    }

    /// <summary>
    /// Represents a global method in symbol table.
    /// </summary>
    internal interface IGlobalMethodDefinition : IMethodDefinition
    {
        /// <summary>
        /// The name of the method.
        /// </summary>
        new string Name { get; }
    }

    internal enum EncFuncCode
    {
        Delta = 0,
        AddMethod = 1,
        AddField = 2,
        AddParameter = 3,
        AddProperty = 4,
        AddEvent = 5
    }

    internal static class Extensions
    {
        internal static bool HasBody(this IMethodDefinition methodDef)
        {
            // Method definition has body if it is a non-abstract, non-extern method.
            // Additionally, methods within COM types have no body.

            return !methodDef.IsAbstract && !methodDef.IsExternal &&
                (methodDef.ContainingTypeDefinition == null || !methodDef.ContainingTypeDefinition.IsComObject);
        }
    }
}