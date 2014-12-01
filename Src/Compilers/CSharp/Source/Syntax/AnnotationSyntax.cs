// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp.Syntax
{
	partial class AnnotationSyntax
	{
		/// <summary>
		/// Return the name used in syntax for the _annotation. This is typically the class
		/// name without the "_annotation" suffix. (For certain diagnostics, the native
		/// compiler uses the _annotation name from syntax rather than the class name.)
		/// </summary>
		internal string GetErrorDisplayName()
		{
			// Dev10 uses the name from source, even if it's an alias.
			return Name.ToString();
		}

		internal AnnotationArgumentSyntax GetNamedArgumentSyntax(string namedArgName)
		{
			Debug.Assert(!String.IsNullOrEmpty(namedArgName));

			if (argumentList != null)
			{
				foreach (var argSyntax in argumentList.Arguments)
				{
					if (argSyntax.NameEquals != null && argSyntax.NameEquals.Name.Identifier.ValueText == namedArgName)
					{
						return argSyntax;
					}
				}
			}

			return null;
		}
	}
}
