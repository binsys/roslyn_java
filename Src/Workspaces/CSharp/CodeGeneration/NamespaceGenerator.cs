﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
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
    internal class NamespaceGenerator : AbstractCSharpCodeGenerator
    {
        public static PackageDeclarationSyntax AddNamespaceTo(
            ICodeGenerationService service,
            PackageDeclarationSyntax destination,
            INamespaceSymbol @namespace,
            CodeGenerationOptions options,
            IList<bool> availableIndices)
        {
            var declaration = GenerateNamespaceDeclaration(service, @namespace, options);
            if (!(declaration is PackageDeclarationSyntax))
            {
                throw new ArgumentException(CSharpWorkspaceResources.NamespaceCanNotBeAddedIn);
            }

            var members = Insert(destination.Members, (PackageDeclarationSyntax)declaration, options, availableIndices);
            return destination.WithMembers(members);
        }

        public static CompilationUnitSyntax AddNamespaceTo(
            ICodeGenerationService service,
            CompilationUnitSyntax destination,
            INamespaceSymbol @namespace,
            CodeGenerationOptions options,
            IList<bool> availableIndices)
        {
            var declaration = GenerateNamespaceDeclaration(service, @namespace, options);
            if (!(declaration is PackageDeclarationSyntax))
            {
                throw new ArgumentException(CSharpWorkspaceResources.NamespaceCanNotBeAddedIn);
            }

            var members = Insert(destination.Members, (PackageDeclarationSyntax)declaration, options, availableIndices);
            return destination.WithMembers(members);
        }

        internal static SyntaxNode GenerateNamespaceDeclaration(
            ICodeGenerationService service,
            INamespaceSymbol @namespace,
            CodeGenerationOptions options)
        {
            options = options ?? CodeGenerationOptions.Default;

            string name;
            INamespaceSymbol innermostNamespace;
            GetNameAndInnermostNamespace(@namespace, options, out name, out innermostNamespace);

            var declaration = GetDeclarationSyntaxWithoutMembers(@namespace, innermostNamespace, name, options);

            declaration = options.GenerateMembers
                    ? service.AddMembers(declaration, innermostNamespace.GetMembers(), options)
                    : declaration;

            return AddCleanupAnnotationsTo(declaration);
        }

        public static SyntaxNode UpdateCompilationUnitOrNamespaceDeclaration(
            ICodeGenerationService service,
            SyntaxNode declaration,
            IList<ISymbol> newMembers,
            CodeGenerationOptions options,
            CancellationToken cancellationToken)
        {
            declaration = RemoveAllMembers(declaration);
            declaration = service.AddMembers(declaration, newMembers, options, cancellationToken);
            return AddCleanupAnnotationsTo(declaration);
        }

        private static SyntaxNode GenerateNamespaceDeclarationWorker(
            string name, INamespaceSymbol innermostNamespace)
        {
            var usings = GenerateUsingDirectives(innermostNamespace);

            // If they're just generating the empty namespace then make that into compilation unit.
            if (name == string.Empty)
            {
                return SyntaxFactory.CompilationUnit().WithUsings(usings);
            }

            return SyntaxFactory.PackageDeclaration(SyntaxFactory.ParseName(name)).WithUsings(usings);
        }

        private static SyntaxNode GetDeclarationSyntaxWithoutMembers(
            INamespaceSymbol @namespace,
            INamespaceSymbol innermostNamespace,
            string name,
            CodeGenerationOptions options)
        {
            var reusableSyntax = GetReuseableSyntaxNodeForSymbol<SyntaxNode>(@namespace, options);
            if (reusableSyntax == null)
            {
                return GenerateNamespaceDeclarationWorker(name, innermostNamespace);
            }

            return RemoveAllMembers(reusableSyntax);
        }

        private static SyntaxNode RemoveAllMembers(SyntaxNode declaration)
        {
            switch (declaration.CSharpKind())
            {
                case SyntaxKind.CompilationUnit:
                    return ((CompilationUnitSyntax)declaration).WithMembers(default(SyntaxList<MemberDeclarationSyntax>));

                case SyntaxKind.NamespaceDeclaration:
                    return ((PackageDeclarationSyntax)declaration).WithMembers(default(SyntaxList<MemberDeclarationSyntax>));

                default:
                    return declaration;
            }
        }

        private static SyntaxList<UsingDirectiveSyntax> GenerateUsingDirectives(INamespaceSymbol innermostNamespace)
        {
            var usingDirectives =
                CodeGenerationNamespaceInfo.GetImports(innermostNamespace)
                                           .Select(GenerateUsingDirective)
                                           .WhereNotNull()
                                           .ToList();

            return usingDirectives.ToSyntaxList();
        }

        private static UsingDirectiveSyntax GenerateUsingDirective(ISymbol symbol)
        {
            if (symbol is IAliasSymbol)
            {
                var alias = (IAliasSymbol)symbol;
                var name = GenerateName(alias.Target);
                if (name != null)
                {
                    return SyntaxFactory.UsingDirective(
                        SyntaxFactory.NameEquals(alias.Name.ToIdentifierName()),
                        name);
                }
            }
            else if (symbol is INamespaceOrTypeSymbol)
            {
                var name = GenerateName((INamespaceOrTypeSymbol)symbol);
                if (name != null)
                {
                    return SyntaxFactory.UsingDirective(name);
                }
            }

            return null;
        }

        private static NameSyntax GenerateName(INamespaceOrTypeSymbol symbol)
        {
            if (symbol is ITypeSymbol)
            {
                return ((ITypeSymbol)symbol).GenerateTypeSyntax() as NameSyntax;
            }
            else
            {
                return SyntaxFactory.ParseName(symbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
            }
        }
    }
}