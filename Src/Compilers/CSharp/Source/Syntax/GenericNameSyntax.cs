﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
namespace Microsoft.CodeAnalysis.CSharp.Syntax
{
	public partial class GenericNameSyntax
	{
		public bool IsUnboundGenericName
		{
			get
			{
				return this.TypeArgumentList.Arguments.Any(SyntaxKind.OmittedTypeArgument);
			}
		}
	}
}