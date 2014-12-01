﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
namespace Microsoft.CodeAnalysis.CSharp.Syntax
{
	public partial class MethodDeclarationSyntax
	{
		public int Arity
		{
			get
			{
				return this.TypeParameterList == null ? 0 : this.TypeParameterList.Parameters.Count;
			}
		}
	}
}