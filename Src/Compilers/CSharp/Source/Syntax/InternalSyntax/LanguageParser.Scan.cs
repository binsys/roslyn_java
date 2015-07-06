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
		private ScanTypeArgumentListKind ScanTypeArgumentList(bool inExpression)
		{
			if (this.CurrentToken.Kind == SyntaxKind.LessThanToken)
			{
				if (inExpression)
				{
					// Scan for a type argument list. If we think it's a type argument list
					// then assume it is unless we see specific tokens following it.
					if (this.ScanPossibleTypeArgumentList())
					{
						var tokenID = this.CurrentToken.Kind;
						if (tokenID != SyntaxKind.OpenParenToken &&
							tokenID != SyntaxKind.CloseParenToken &&
							tokenID != SyntaxKind.CloseBracketToken &&
							tokenID != SyntaxKind.ColonToken &&
							tokenID != SyntaxKind.SemicolonToken &&
							tokenID != SyntaxKind.CommaToken &&
							tokenID != SyntaxKind.DotToken &&
							tokenID != SyntaxKind.QuestionToken &&
							tokenID != SyntaxKind.EqualsEqualsToken &&
							tokenID != SyntaxKind.ExclamationEqualsToken &&

							// The preceding tokens are from 7.5.4.2 Grammar Ambiguities;
							// the following tokens are not.
							tokenID != SyntaxKind.AmpersandAmpersandToken &&
							tokenID != SyntaxKind.BarBarToken &&
							tokenID != SyntaxKind.CaretToken &&
							tokenID != SyntaxKind.BarToken &&
							tokenID != SyntaxKind.CloseBraceToken &&
							tokenID != SyntaxKind.EndOfFileToken)
						{
							return ScanTypeArgumentListKind.PossibleTypeArgumentList;
						}
						else
						{
							return ScanTypeArgumentListKind.DefiniteTypeArgumentList;
						}
					}
				}
				else
				{
					return ScanTypeArgumentListKind.DefiniteTypeArgumentList;
				}
			}

			return ScanTypeArgumentListKind.NotTypeArgumentList;
		}

		private bool ScanPossibleTypeArgumentList()
		{
			SyntaxToken lastTokenOfList = null;
			return ScanPossibleTypeArgumentList(ref lastTokenOfList);
		}

		private bool ScanPossibleTypeArgumentList(ref SyntaxToken lastTokenOfList)
		{
			if (this.CurrentToken.Kind == SyntaxKind.LessThanToken)
			{
				do
				{
					lastTokenOfList = this.EatToken();

					// We currently do not have the ability to scan attributes, so if this is an open square, we early out and assume it is an _annotation
					if (this.CurrentToken.Kind == SyntaxKind.OpenBracketToken)
					{
						return true;
					}

					if (this.CurrentToken.Kind == SyntaxKind.GreaterThanToken)
					{
						lastTokenOfList = EatToken();
						return true;
					}

					var tt = this.ScanType(out lastTokenOfList);
					if (tt == ScanTypeFlags.NotType)
					{
						lastTokenOfList = null;
						return false;
					}

					//if (tt == ScanTypeFlags.JavaWildcardType)
					//{
					//	bool havaBound = false;
					//	if (this.CurrentToken.Kind == SyntaxKind.ExtendsKeyword)
					//	{
					//		lastTokenOfList = EatToken();
					//		havaBound = true;
					//	}

					//	if (this.CurrentToken.Kind == SyntaxKind.SuperKeyword)
					//	{
					//		lastTokenOfList = EatToken();
					//		havaBound = true;
					//	}

					//	if (this.IsTrueIdentifier())
					//	{
					//		this.ScanType(out lastTokenOfList);
					//	}

					//	if (this.CurrentToken.Kind == SyntaxKind.GreaterThanToken)
					//	{
					//		lastTokenOfList = EatToken();
					//		return true;
					//	}

					//	return true;
					//}

				}
				while (this.CurrentToken.Kind == SyntaxKind.CommaToken);

				if (this.CurrentToken.Kind != SyntaxKind.GreaterThanToken)
				{
					lastTokenOfList = null;
					return false;
				}


				lastTokenOfList = this.EatToken();
			}

			return true;
		}



		private ScanTypeFlags ScanType()
		{
			SyntaxToken lastTokenOfType;
			return ScanType(out lastTokenOfType);
		}

		private ScanTypeFlags ScanType(out SyntaxToken lastTokenOfType)
		{
			ScanTypeFlags result = this.ScanNonArrayType(out lastTokenOfType);

			if (result == ScanTypeFlags.NotType)
			{
				return result;
			}

			// Finally, check for array types and nullables.
			while (this.CurrentToken.Kind == SyntaxKind.OpenBracketToken)
			{
				this.EatToken();
				if (this.CurrentToken.Kind != SyntaxKind.CloseBracketToken)
				{
					while (this.CurrentToken.Kind == SyntaxKind.CommaToken)
					{
						this.EatToken();
					}

					if (this.CurrentToken.Kind != SyntaxKind.CloseBracketToken)
					{
						lastTokenOfType = null;
						return ScanTypeFlags.NotType;
					}
				}

				lastTokenOfType = this.EatToken();
				result = ScanTypeFlags.MustBeType;
			}

			return result;
		}


		private NamedTypePart ScanNamedTypePart()
		{
			SyntaxToken lastTokenOfType;
			return ScanNamedTypePart(out lastTokenOfType);
		}

		private NamedTypePart ScanNamedTypePart(out SyntaxToken lastTokenOfType)
		{
			//if (this.CurrentToken.Kind == SyntaxKind.ClassKeyword)
			//{
			//	lastTokenOfType = this.EatToken();
			//	return NamedTypePart.ClassKeywordSuffix;
			//}

			if (this.CurrentToken.Kind != SyntaxKind.IdentifierToken || !this.IsTrueIdentifier())
			{
				lastTokenOfType = this.EatToken();
				return NamedTypePart.NotName;
			}

			lastTokenOfType = this.EatToken();
			if (this.CurrentToken.Kind == SyntaxKind.LessThanToken)
			{
				if (!this.ScanPossibleTypeArgumentList(ref lastTokenOfType))
				{
					return NamedTypePart.NotName;
				}

				return NamedTypePart.GenericName;
			}
			else
			{
				return NamedTypePart.SimpleName;
			}
		}


		private ScanTypeFlags ScanNonArrayType()
		{
			SyntaxToken lastTokenOfType;
			return ScanNonArrayType(out lastTokenOfType);
		}

		private ScanTypeFlags ScanNonArrayType(out SyntaxToken lastTokenOfType)
		{
			ScanTypeFlags result;
			if (this.CurrentToken.Kind == SyntaxKind.IdentifierToken)
			{
				var partResult = this.ScanNamedTypePart(out lastTokenOfType);
				if (partResult == NamedTypePart.NotName)
				{
					return ScanTypeFlags.NotType;
				}

				result = NamedTypePartToScanTypeFlags(partResult);

				// Scan a name
				while (IsDot())
				{
					lastTokenOfType = this.EatToken();

					partResult = this.ScanNamedTypePart(out lastTokenOfType);

					if (partResult == NamedTypePart.NotName)
					{
						return ScanTypeFlags.NotType;
					}

					result = NamedTypePartToScanTypeFlags(partResult);
				}
			}
			else if (IsPredefinedType(this.CurrentToken.Kind))
			{
				// Simple type...
				lastTokenOfType = this.EatToken();
				result = ScanTypeFlags.MustBeType;
			}
			else if (this.CurrentToken.Kind == SyntaxKind.QuestionToken)
			{
				lastTokenOfType = this.EatToken();

				bool haveBound = false;
				if (this.CurrentToken.Kind == SyntaxKind.SuperKeyword)
				{
					lastTokenOfType = this.EatToken();
					haveBound = true;
				}

				if (this.CurrentToken.Kind == SyntaxKind.ExtendsKeyword)
				{
					lastTokenOfType = this.EatToken();
					haveBound = true;
				}

				if (this.IsTrueIdentifier() && haveBound)
				{
					this.ScanType(out lastTokenOfType);
				}
				result = ScanTypeFlags.JavaWildcardType;
			}
			else
			{
				// Can't be a type!
				lastTokenOfType = null;
				return ScanTypeFlags.NotType;
			}

			//if (this.CurrentToken.Kind == SyntaxKind.QuestionToken)
			//{
			//	lastTokenOfType = this.EatToken();
			//	result = ScanTypeFlags.NullableType;
			//}


			//// Now check for pointer type(s)
			//while (this.CurrentToken.Kind == SyntaxKind.AsteriskToken)
			//{
			//	lastTokenOfType = this.EatToken();
			//	if (result == ScanTypeFlags.GenericTypeOrExpression || result == ScanTypeFlags.NonGenericTypeOrExpression)
			//	{
			//		result = ScanTypeFlags.PointerOrMultiplication;
			//	}
			//}

			return result;
		}



		private bool ScanParenthesizedImplicitlyTypedLambda(uint precedence)
		{
			if (!(precedence <= LambdaPrecedence))
			{
				return false;
			}

			//  case 1:  ( x ,
			if (this.PeekToken(1).Kind == SyntaxKind.IdentifierToken
				&& this.PeekToken(2).Kind == SyntaxKind.CommaToken)
			{
				return true;
			}


			//  case 3:  ( ) =>
			if (this.PeekToken(1).Kind == SyntaxKind.CloseParenToken
				&& this.PeekToken(2).Kind == SyntaxKind.MinusGreaterThanToken)
			{
				return true;
			}

			// case 4:  ( params
			// This case is interesting in that it is not legal; this error could be caught at parse time but we would rather
			// recover from the error and let the semantic analyzer deal with it.
			//if (this.PeekToken(1).Kind == SyntaxKind.ParamsKeyword)
			//{
			//	return true;
			//}

			return false;
		}

		private bool ScanExplicitlyTypedLambda(uint precedence)
		{
			if (!(precedence <= LambdaPrecedence))
			{
				return false;
			}

			var resetPoint = this.GetResetPoint();
			try
			{
				// do we have the following:
				//   case 1: ( T x ,
				//   case 2: ( T x ) =>
				//   case 3: ( out T x,
				//   case 4: ( ref T x,
				//   case 5: ( out T x ) =>
				//   case 6: ( ref T x ) =>
				//
				// if so then parse it as a lambda

				// Advance past the open paren.
				this.EatToken();


				// NOTE: if we see "out" or ref" and part of cases 3,4,5,6 followed by EOF, we'll parse as a lambda.
				if (this.CurrentToken.Kind == SyntaxKind.EndOfFileToken)
				{
					return true;
				}

				// NOTE: advances CurrentToken
				if (this.ScanType() == ScanTypeFlags.NotType)
				{
					return false;
				}

				if (this.CurrentToken.Kind == SyntaxKind.EndOfFileToken)
				{
					return true;
				}

				if (!this.IsTrueIdentifier())
				{
					return false;
				}

				switch (this.PeekToken(1).Kind)
				{
					case SyntaxKind.EndOfFileToken:
					case SyntaxKind.CommaToken:
						return true;

					case SyntaxKind.CloseParenToken:
						switch (this.PeekToken(2).Kind)
						{
							case SyntaxKind.EndOfFileToken:
							case SyntaxKind.MinusGreaterThanToken:
								return true;

							default:
								return false;
						}
					default:
						return false;
				}
			}
			finally
			{
				this.Reset(ref resetPoint);
				this.Release(ref resetPoint);
			}
		}

		private bool ScanCast()
		{
			if (this.CurrentToken.Kind != SyntaxKind.OpenParenToken)
			{
				return false;
			}

			this.EatToken();

			var type = this.ScanType();
			if (type == ScanTypeFlags.NotType)
			{
				return false;
			}

			if (this.CurrentToken.Kind != SyntaxKind.CloseParenToken)
			{
				return false;
			}

			// If we have any of the following, we know it must be a cast:
			// 1) (Foo*)bar;
			// 2) (Foo?)bar;
			// 3) "(int)bar" or "(int[])bar"
			// 4) (G::Foo)bar
			if (type == ScanTypeFlags.PointerOrMultiplication ||
				type == ScanTypeFlags.NullableType ||
				type == ScanTypeFlags.MustBeType)
			{
				return true;
			}

			this.EatToken();

			// check for ambiguous type or expression followed by disambiguating token.  i.e.
			//
			// "(A)b" is a cast.  But "(A)+b" is not a cast.  
			return IsAnyTypeOrExpr(type) && CanFollowCast(this.CurrentToken.Kind);
		}
	}
}