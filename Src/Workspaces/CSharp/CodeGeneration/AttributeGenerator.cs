﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeGeneration;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.CodeGeneration
{
    internal class AttributeGenerator : AbstractCSharpCodeGenerator
    {
        public static SyntaxList<AttributeListSyntax> GenerateAttributeLists(
            ImmutableArray<AttributeData> attributes,
            CodeGenerationOptions options,
            SyntaxToken? target = null)
        {
            if (options.MergeAttributes)
            {
                var attributeNodes = attributes.OrderBy(a => a.AttributeClass.Name).Select((a) => GenerateAttribute(a, options)).WhereNotNull().ToList();
                return 
                    default(SyntaxList<AttributeListSyntax>)
                    ;
            }
            else
            {
                var attributeDeclarations = attributes.OrderBy(a => a.AttributeClass.Name).Select(a => GenerateAttributeDeclaration(a, target, options)).WhereNotNull().ToList();
                return attributeDeclarations.Count == 0
                    ? default(SyntaxList<AttributeListSyntax>)
                    : SyntaxFactory.List<AttributeListSyntax>(attributeDeclarations);
            }
        }

        private static AttributeListSyntax GenerateAttributeDeclaration(
            AttributeData attribute, SyntaxToken? target, CodeGenerationOptions options)
        {
            var attributeSyntax = GenerateAttribute(attribute, options);
            return null;
        }

        private static AttributeSyntax GenerateAttribute(AttributeData attribute, CodeGenerationOptions options)
        {
            if (!options.MergeAttributes)
            {
                var reusableSyntax = GetReuseableSyntaxNodeForAttribute<AttributeSyntax>(attribute, options);
                if (reusableSyntax != null)
                {
                    return reusableSyntax;
                }
            }

            var attributeArguments = GenerateAttributeArgumentList(attribute);
            var nameSyntax = attribute.AttributeClass.GenerateTypeSyntax() as NameSyntax;
            return nameSyntax == null ? null : SyntaxFactory.Attribute(nameSyntax, attributeArguments);
        }

        private static AttributeArgumentListSyntax GenerateAttributeArgumentList(AttributeData attribute)
        {
            if (attribute.ConstructorArguments.Length == 0 && attribute.NamedArguments.Length == 0)
            {
                return null;
            }

            var arguments = new List<AttributeArgumentSyntax>();
            arguments.AddRange(attribute.ConstructorArguments.Select(c =>
                SyntaxFactory.AttributeArgument(GenerateExpression(c))));

            arguments.AddRange(attribute.NamedArguments.Select(kvp =>
                SyntaxFactory.AttributeArgument(
                    SyntaxFactory.NameEquals(SyntaxFactory.IdentifierName(kvp.Key)), null,
                    GenerateExpression(kvp.Value))));

            return SyntaxFactory.AttributeArgumentList(SyntaxFactory.SeparatedList(arguments));
        }
    }
}
