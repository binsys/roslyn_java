// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading;

using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
	internal partial class LanguageParser : SyntaxParser
	{
		private struct CompilationUnitBodyBuilder
		{
			public SyntaxListBuilder<AnnotationSyntax> Annotations;
			public PackageDeclarationSyntax Package;
			public SyntaxListBuilder<ImportDeclarationSyntax> Imports;
			public SyntaxListBuilder<MemberDeclarationSyntax> Members;


			public CompilationUnitBodyBuilder(SyntaxListPool pool)
			{
				Annotations = pool.Allocate<AnnotationSyntax>();
				//Packages = pool.Allocate<PackageDeclarationSyntax>();
				Package = null;
				Imports = pool.Allocate<ImportDeclarationSyntax>();
				Members = pool.Allocate<MemberDeclarationSyntax>();
			}

			internal void Free(SyntaxListPool pool)
			{
				pool.Free(Members);
				pool.Free(Imports);
				//pool.Free(Packages);
				//Package = null;
				pool.Free(Annotations);
			}
		}

	}
}