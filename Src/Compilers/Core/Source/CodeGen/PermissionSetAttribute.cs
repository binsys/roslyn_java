﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis.Collections;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using Cci = Microsoft.Cci;

namespace Microsoft.CodeAnalysis.CodeGen
{
    /// <summary>
    /// This class represents the PermissionSetAttribute specified in source which needs fixup during codegen.
    /// </summary>
    /// <remarks>
    /// PermissionSetAttribute needs fixup when it contains an assignment to the 'File' property as a single named attribute argument.
    /// Fixup performed is ported from SecurityAttributes::FixUpPermissionSetAttribute at ndp\clr\src\vm\securityattributes.cpp.
    /// It involves following steps:
    /// 1) Verifying that the specified file name resolves to a valid path: This is done during binding.
    /// 2) Reading the contents of the file into a byte array.
    /// 3) Convert each byte in the file content into two bytes containing hexa-decimal characters (see method <see cref="ConvertToHex"/>).
    /// 4) Replacing the 'File = fileName' named argument with 'Hex = hexFileContent' argument, where hexFileContent is the converted output from step 3) above.
    /// </remarks>
    internal class PermissionSetAttributeWithFileReference : Cci.ICustomAttribute
    {
        private readonly Cci.ICustomAttribute sourceAttribute;
        private string resolvedPermissionSetFilePath;
        internal static readonly string FilePropertyName = "File";
        internal static readonly string HexPropertyName = "Hex";

        public PermissionSetAttributeWithFileReference(Cci.ICustomAttribute sourceAttribute, string resolvedPermissionSetFilePath)
        {
            Debug.Assert(!String.IsNullOrEmpty(resolvedPermissionSetFilePath));
            Debug.Assert(PathUtilities.IsAbsolute(resolvedPermissionSetFilePath));

            this.sourceAttribute = sourceAttribute;
            this.resolvedPermissionSetFilePath = resolvedPermissionSetFilePath;
        }

        /// <summary>
        /// Zero or more positional arguments for the attribute constructor.
        /// </summary>
        public IEnumerable<Cci.IMetadataExpression> GetArguments(Microsoft.CodeAnalysis.Emit.Context context)
        {
            return this.sourceAttribute.GetArguments(context);
        }

        /// <summary>
        /// A reference to the constructor that will be used to instantiate this custom attribute during execution (if the attribute is inspected via Reflection).
        /// </summary>
        public Cci.IMethodReference Constructor(Microsoft.CodeAnalysis.Emit.Context context)
        {
            return this.sourceAttribute.Constructor(context);
        }

        /// <summary>
        /// Zero or more named arguments that specify values for fields and properties of the attribute.
        /// </summary>
        public IEnumerable<Cci.IMetadataNamedArgument> GetNamedArguments(Microsoft.CodeAnalysis.Emit.Context context)
        {
            // Perform fixup 
            Cci.ITypeReference stringType = context.Module.GetPlatformType(Cci.PlatformType.SystemString, context);

#if DEBUG
            // Must have exactly 1 named argument.
            var namedArgs = this.sourceAttribute.GetNamedArguments(context);
            Debug.Assert(namedArgs.Count() == 1);

            // Named argument must be 'File' property of string type
            var fileArg = namedArgs.First();
            Debug.Assert(fileArg.ArgumentName == FilePropertyName);
            Debug.Assert(context.Module.IsPlatformType(fileArg.Type, Cci.PlatformType.SystemString));

            // Named argument value must be a non-empty string
            Debug.Assert(fileArg.ArgumentValue is Cci.IMetadataConstant);
            var fileName = (string)((Cci.IMetadataConstant)fileArg.ArgumentValue).Value;
            Debug.Assert(!String.IsNullOrEmpty(fileName));

            // PermissionSetAttribute type must have a writable public string type property member 'Hex'
            Debug.Assert(((INamedTypeSymbol)this.sourceAttribute.GetType(context)).GetMembers(HexPropertyName).Any(
                member => member.Kind == SymbolKind.Property && ((IPropertySymbol)member).Type.SpecialType == SpecialType.System_String));
#endif

            string hexFileContent;

            // Read the file contents at the resolved file path into a byte array.
            // May throw PermissionSetFileReadException, which is handled in Compilation.Emit.
            try
            {
                using (Stream stream = context.ModuleBuilder.CommonCompilation.Options.FileResolver.OpenReadChecked(resolvedPermissionSetFilePath))
                {
                    // Convert the byte array contents into a string in hexa-decimal format.
                    hexFileContent = ConvertToHex(stream);
                }
            }
            catch (IOException e)
            {
                throw new PermissionSetFileReadException(e.Message, resolvedPermissionSetFilePath);
            }

            // Synthesize a named attribute argument "Hex = hexFileContent".
            yield return new HexPropertyMetadataNamedArgument(stringType, new MetadataConstant(stringType, hexFileContent));
        }

        // internal for testing purposes.
        internal static string ConvertToHex(Stream stream)
        {
            Debug.Assert(stream != null);

            var pooledStrBuilder = PooledStringBuilder.GetInstance();
            StringBuilder stringBuilder = pooledStrBuilder.Builder;

            int b;
            while ((b = stream.ReadByte()) >= 0)
            {
                stringBuilder.Append(ConvertHexToChar((b >> 4) & 0xf));
                stringBuilder.Append(ConvertHexToChar(b & 0xf));
            }

            return pooledStrBuilder.ToStringAndFree();
        }

        private static char ConvertHexToChar(int b)
        {
            Debug.Assert(b < 0x10);
            return (char)(b < 10 ? '0' + b : 'a' + b - 10);
        }

        /// <summary>
        /// The number of positional arguments.
        /// </summary>
        public int ArgumentCount
        {
            get
            {
                return this.sourceAttribute.ArgumentCount;
            }
        }

        /// <summary>
        /// The number of named arguments.
        /// </summary>
        public ushort NamedArgumentCount
        {
            get
            {
                Debug.Assert(this.sourceAttribute.NamedArgumentCount == 1);
                return 1;
            }
        }

        /// <summary>
        /// The type of the attribute. For example System.AttributeUsageAttribute.
        /// </summary>
        public Cci.ITypeReference GetType(Microsoft.CodeAnalysis.Emit.Context context)
        {
            return this.sourceAttribute.GetType(context);
        }

        public bool AllowMultiple
        {
            get { return this.sourceAttribute.AllowMultiple; }
        }

        private struct HexPropertyMetadataNamedArgument : Cci.IMetadataNamedArgument
        {
            private readonly Cci.ITypeReference type;
            private readonly Cci.IMetadataExpression value;

            public HexPropertyMetadataNamedArgument(Cci.ITypeReference type, Cci.IMetadataExpression value)
            {
                this.type = type;
                this.value = value;
            }

            public string ArgumentName { get { return HexPropertyName; } }
            public Cci.IMetadataExpression ArgumentValue { get { return this.value; } }
            public bool IsField { get { return false; } }

            Cci.ITypeReference Cci.IMetadataExpression.Type { get { return this.type; } }

            void Cci.IMetadataExpression.Dispatch(Cci.MetadataVisitor visitor)
            {
                visitor.Visit(this);
            }
        }
    }

    /// <summary>
    /// Exception class to enable generating ERR_PermissionSetAttributeFileReadError while reading the file for PermissionSetAttribute fixup.
    /// </summary>
    internal class PermissionSetFileReadException : Exception
    {
        private readonly string file;

        public PermissionSetFileReadException(string message, string file)
            : base(message)
        {
            this.file = file;
        }

        public string FileName
        {
            get { return file; }
        }

        public string PropertyName
        {
            get { return PermissionSetAttributeWithFileReference.FilePropertyName; }
        }
    }
}