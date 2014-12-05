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
		// Parses the parts of the names between Dots and ColonColons.
		private SimpleNameSyntax ParseSimpleName(NameOptions options = NameOptions.None)
		{
			var id = this.ParseIdentifierName();
			if (id.Identifier.IsMissing)
			{
				return id;
			}

			// You can pass ignore generics if you don't even want the parser to consider generics at all.
			// The name parsing will then stop at the first "<". It doesn't make sense to pass both Generic and IgnoreGeneric.

			SimpleNameSyntax name = id;
			if (this.CurrentToken.Kind == SyntaxKind.LessThanToken)
			{
				var pt = this.GetResetPoint();
				var kind = this.ScanTypeArgumentList((options & NameOptions.InExpression) != 0);
				this.Reset(ref pt);
				this.Release(ref pt);

				if (kind == ScanTypeArgumentListKind.DefiniteTypeArgumentList 
					|| (kind == ScanTypeArgumentListKind.PossibleTypeArgumentList 
					&& (options & NameOptions.InTypeList) != 0))
				{
					Debug.Assert(this.CurrentToken.Kind == SyntaxKind.LessThanToken);
					SyntaxToken open;
					var types = this._pool.AllocateSeparated<TypeSyntax>();
					SyntaxToken close;
					this.ParseTypeArgumentList(out open, types, out close);
					name = _syntaxFactory.GenericName(id.Identifier,
						_syntaxFactory.TypeArgumentList(open, types, close));
					this._pool.Free(types);
				}
			}

			return name;
		}


		private NameSyntax ParseQualifiedName(NameOptions options = NameOptions.None)
		{
			NameSyntax name = this.ParseSimpleName(options);

			while (this.IsDot())
			{
				//stop parse name with list:
				//.this
				//.super
				//.class
				//.*
				if (this.PeekToken(1).Kind != SyntaxKind.IdentifierToken)
				{
					break;
				}

				var separator = this.EatToken();
				name = ParseQualifiedNameRight(options, name, separator);
			}

			return name;
		}

		private NameSyntax ParseQualifiedNameRight(
			NameOptions options,
			NameSyntax left,
			SyntaxToken separator)
		{
			var right = this.ParseSimpleName(options);

			if (separator.Kind == SyntaxKind.DotToken)
			{
				return _syntaxFactory.QualifiedName(left, separator, right);
			}
			else
			{
				return left;
			}
		}
	}
}