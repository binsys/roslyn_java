﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
namespace Microsoft.CodeAnalysis.CSharp.Syntax
{
	public partial class BaseMethodDeclarationSyntax
	{
		internal int ParameterCount
		{
			get
			{
				int count = 0;
				foreach (ParameterSyntax parameter in this.ParameterList.Parameters)
				{
					// __arglist does not affect the parameter count.
					if (!parameter.IsArgList)
					{
						count++;
					}
				}
				return count;
			}
		}
	}
}