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


		private TypeSyntax ParseType(bool parentIsParameter)
		{
			return ParseTypeCore(parentIsParameter, isInstanceOfOrAs: false, expectSizes: false, isArrayCreation: false);
		}


		private TypeSyntax ParseTypeCore(
			bool parentIsParameter,
			bool isInstanceOfOrAs,
			bool expectSizes,
			bool isArrayCreation)
		{
			var type = this.ParseUnderlyingType(parentIsParameter);

			//if (this.CurrentToken.Kind == SyntaxKind.QuestionToken)
			//{
			//	var resetPoint = this.GetResetPoint();
			//	try
			//	{
			//		var question = this.EatToken();

			//		if (isInstanceOfOrAs && (IsTerm() || IsPredefinedType(this.CurrentToken.Kind) || SyntaxKindFacts.IsAnyUnaryExpression(this.CurrentToken.Kind)))
			//		{
			//			this.Reset(ref resetPoint);

			//			Debug.Assert(type != null);
			//			return type;
			//		}

			//		question = CheckFeatureAvailability(question, MessageID.IDS_FeatureNullable);
			//		type = _syntaxFactory.NullableType(type, question);
			//	}
			//	finally
			//	{
			//		this.Release(ref resetPoint);
			//	}
			//}


			// Now check for arrays.
			if (this.IsPossibleRankAndDimensionSpecifier())
			{
				var ranks = this._pool.Allocate<ArrayRankSpecifierSyntax>();
				try
				{
					while (this.IsPossibleRankAndDimensionSpecifier())
					{
						bool unused;
						var rank = this.ParseArrayRankSpecifier(isArrayCreation, expectSizes, out unused);
						ranks.Add(rank);
						expectSizes = false;
					}

					type = _syntaxFactory.ArrayType(type, ranks);
				}
				finally
				{
					this._pool.Free(ranks);
				}
			}

			Debug.Assert(type != null);
			return type;
		}


		private TypeSyntax ParseUnderlyingType(bool parentIsParameter)
		{
			if (IsPredefinedType(this.CurrentToken.Kind))
			{
				// This is a predefined type
				var token = this.EatToken();
				if (token.Kind == SyntaxKind.VoidKeyword && this.CurrentToken.Kind != SyntaxKind.AsteriskToken)
				{
					token = this.AddError(token, parentIsParameter ? ErrorCode.ERR_NoVoidParameter : ErrorCode.ERR_NoVoidHere);
				}

				return _syntaxFactory.PredefinedType(token);
			}
			else if (this.CurrentToken.Kind == SyntaxKind.QuestionToken)
			{
				return this.ParseJavaWildcardType();
			}
			else if (this.CurrentToken.Kind == SyntaxKind.IdentifierToken)
			{
				return this.ParseQualifiedName();
			}
			else
			{
				var name = this.CreateMissingIdentifierName();
				return this.AddError(name, ErrorCode.ERR_TypeExpected);
			}
		}


		private JavaWildcardTypeSyntax ParseJavaWildcardType()
		{
			var token = this.EatToken();
			JavaWildcardTypeBoundSyntax typeBound = default(JavaWildcardTypeBoundSyntax);
			if (this.CurrentToken.Kind == SyntaxKind.ExtendsKeyword || this.CurrentToken.Kind == SyntaxKind.SuperKeyword)
			{
				var keyword = this.EatToken();
				var type = this.ParseType(false);

				typeBound = _syntaxFactory.JavaWildcardTypeBound(keyword, type);

			}
			return _syntaxFactory.JavaWildcardType(token, typeBound);
		}
	}
}