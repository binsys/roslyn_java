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

		/*
		1. parameter是指函数定义中参数，而argument指的是函数调用时的实际参数。
		2. 简略描述为：parameter=形参(formal parameter)， argument=实参(actual parameter)。
		*/


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
	}
}