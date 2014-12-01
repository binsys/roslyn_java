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

		#region TypeArgument
		// ParseInstantiation: Parses the generic argument/parameter parts of the name.
		private void ParseTypeArgumentList(out SyntaxToken open, SeparatedSyntaxListBuilder<TypeSyntax> types, out SyntaxToken close)
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.LessThanToken);
			open = this.EatToken(SyntaxKind.LessThanToken);
			open = CheckFeatureAvailability(open, MessageID.IDS_FeatureGenerics);

			if (this.IsOpenName())
			{
				// NOTE: trivia will be attached to comma, not omitted type argument
				var omittedTypeArgumentInstance = _syntaxFactory.OmittedTypeArgument(SyntaxFactory.Token(SyntaxKind.OmittedTypeArgumentToken));
				types.Add(omittedTypeArgumentInstance);
				while (this.CurrentToken.Kind == SyntaxKind.CommaToken)
				{
					types.AddSeparator(this.EatToken(SyntaxKind.CommaToken));
					types.Add(omittedTypeArgumentInstance);
				}

				close = this.EatToken(SyntaxKind.GreaterThanToken);

				return;
			}

			// first type
			types.Add(this.ParseTypeArgument());

			// remaining types & commas
			while (true)
			{
				if (this.CurrentToken.Kind == SyntaxKind.GreaterThanToken || this.IsPossibleTypeParameterConstraintClauseStart())
				{
					break;
				}
				else if (this.CurrentToken.Kind == SyntaxKind.CommaToken || this.IsPossibleType())
				{
					types.AddSeparator(this.EatToken(SyntaxKind.CommaToken));
					types.Add(this.ParseTypeArgument());
				}
				else if (this.SkipBadTypeArgumentListTokens(types, SyntaxKind.CommaToken) == PostSkipAction.Abort)
				{
					break;
				}
			}

			close = this.EatToken(SyntaxKind.GreaterThanToken);
		}

		// Parses the individual generic parameter/arguments in a name.
		private TypeSyntax ParseTypeArgument()
		{
			if (this.IsPossibleTypeParameterConstraintClauseStart())
			{
				return this.AddError(this.CreateMissingIdentifierName(), ErrorCode.ERR_TypeExpected);
			}

			var attrs = this._pool.Allocate<AnnotationSyntax>();
			try
			{
				if (this.CurrentToken.Kind == SyntaxKind.OpenBracketToken && this.PeekToken(1).Kind != SyntaxKind.CloseBracketToken)
				{
					// Here, if we see a "[" that looks like it has something in it, we parse
					// it as an _annotation and then later put an error on the whole type if
					// it turns out that attributes are not allowed. 
					// TODO: should there be another flag that controls this behavior? we have
					// "allowAttrs" but should there also be a "recognizeAttrs" that we can
					// set to false in an expression context?

					var saveTerm = this._termState;
					this._termState = TerminatorState.IsEndOfTypeArgumentList;
					this.ParseAnnotationDeclarations(attrs);
					this._termState = saveTerm;
				}

				SyntaxToken varianceToken = null;


				var result = this.ParseType(parentIsParameter: false);

				if (varianceToken != null)
				{
					result = AddLeadingSkippedSyntax(result, varianceToken);
				}

				if (attrs.Count > 0)
				{
					result = AddLeadingSkippedSyntax(result, attrs.ToListNode());
					result = this.AddError(result, ErrorCode.ERR_TypeExpected);
				}

				return result;
			}
			finally
			{
				this._pool.Free(attrs);
			}
		}
		#endregion



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

				if (kind == ScanTypeArgumentListKind.DefiniteTypeArgumentList || (kind == ScanTypeArgumentListKind.PossibleTypeArgumentList && (options & NameOptions.InTypeList) != 0))
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



		#region QualifiedName
		private NameSyntax ParseQualifiedName(NameOptions options = NameOptions.None)
		{
			NameSyntax name = this.ParseSimpleName(options);

			while (this.IsDot())
			{
				//*****.this
				if (this.PeekToken(1).Kind == SyntaxKind.ThisKeyword)
				{
					break;
				}

				//******.class
				if (this.PeekToken(1).Kind == SyntaxKind.ClassKeyword)
				{
					break;
				}

				//import
				if (this.PeekToken(1).Kind == SyntaxKind.AsteriskToken)
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

		#endregion


	}
}