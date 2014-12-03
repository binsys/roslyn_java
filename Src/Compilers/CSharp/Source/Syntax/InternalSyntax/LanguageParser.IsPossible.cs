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

		private static bool IsPossibleStartOfTypeDeclaration(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.EnumKeyword:     // enum
				case SyntaxKind.ClassKeyword:    // class
				case SyntaxKind.InterfaceKeyword:// interface
				case SyntaxKind.AtToken:         // *
				case SyntaxKind.AbstractKeyword: // abstract
				case SyntaxKind.StaticKeyword:   // static
				case SyntaxKind.NewKeyword:      // new
				case SyntaxKind.PrivateKeyword:  // private
				case SyntaxKind.ProtectedKeyword:// protected
				case SyntaxKind.PublicKeyword:   // public
					return true;
				default:
					return false;
			}
		}

		public bool IsPossiblePackageAnnotations()
		{
			//skip @interface define
			if (this.CurrentToken.Kind == SyntaxKind.AtToken && this.PeekToken(1).Kind == SyntaxKind.InterfaceKeyword)
			{
				return false;
			}
			var resetPoint = this.GetResetPoint();
			var pendingAnnotations = this._pool.Allocate<AnnotationSyntax>();
			try
			{
				this.ParseAnnotationDeclarations(pendingAnnotations);

				if (this.CurrentToken.Kind == SyntaxKind.PackageKeyword)
				{
					return true;
				}
			}
			finally
			{
				this.Reset(ref resetPoint);
				this.Release(ref resetPoint);
				_pool.Free(pendingAnnotations);
			}

			return false;
		}

		private bool IsPossiblePackageMemberDeclaration()
		{
			switch (this.CurrentToken.Kind)
			{
				case SyntaxKind.ImportKeyword:
					return true;
				default:
					return IsPossibleStartOfTypeDeclaration(this.CurrentToken.Kind);
			}
		}


		private bool IsPossibleAnnotationSyntax()
		{
			return this.CurrentToken.Kind == SyntaxKind.AtToken && this.PeekToken(1).Kind != SyntaxKind.InterfaceKeyword;
		}

		private bool IsPossibleAnnotationDeclaration()
		{
			return this.CurrentToken.Kind == SyntaxKind.AtToken && this.PeekToken(1).Kind == SyntaxKind.InterfaceKeyword;
		}

		private bool IsPossibleAttribute()
		{
			return this.IsTrueIdentifier();
		}


		private bool IsPossibleAttributeArgument()
		{
			return this.IsPossibleExpression();
		}



		private bool IsPossibleModifier()
		{
			return GetModifier(this.CurrentToken) != SyntaxModifier.None;
		}


		private bool IsPossibleMemberName()
		{
			switch (this.CurrentToken.Kind)
			{
				case SyntaxKind.IdentifierToken:
					return true;
				default:
					return false;
			}
		}


		private bool IsPossibleMemberStartOrStop()
		{
			return this.IsPossibleMemberStart() || this.CurrentToken.Kind == SyntaxKind.CloseBraceToken;
		}

		private bool IsPossibleAggregateClauseStartOrStop()
		{
			return this.CurrentToken.Kind == SyntaxKind.ColonToken
				|| this.IsPossibleTypeParameterConstraintClauseStart()
				|| this.CurrentToken.Kind == SyntaxKind.OpenBraceToken;
		}


		private bool IsPossibleTypeParameterConstraintClauseStart()
		{
			return
				this.PeekToken(1).Kind == SyntaxKind.IdentifierToken &&
				this.PeekToken(2).Kind == SyntaxKind.ColonToken;
		}

		private bool IsPossibleTypeParameterConstraint()
		{
			switch (this.CurrentToken.Kind)
			{
				case SyntaxKind.NewKeyword:
				case SyntaxKind.ClassKeyword:
					return true;
				case SyntaxKind.IdentifierToken:
					return this.IsTrueIdentifier();
				default:
					return IsPredefinedType(this.CurrentToken.Kind);
			}
		}

		private bool IsPossibleMemberStart()
		{
			return CanStartMember(this.CurrentToken.Kind);
		}

		private bool IsPossibilityAnnotationTypeDefine()
		{
			return this.CurrentToken.Kind == SyntaxKind.AtToken && this.PeekToken(1).Kind == SyntaxKind.InterfaceKeyword;
		}


		private bool IsPossibleEndOfVariableDeclaration()
		{
			switch (this.CurrentToken.Kind)
			{
				case SyntaxKind.CommaToken:
				case SyntaxKind.SemicolonToken:
					return true;
				default:
					return false;
			}
		}

		private bool IsPossibleVariableInitializer(bool allowStack)
		{
			return (allowStack)
				|| this.CurrentToken.Kind == SyntaxKind.OpenBraceToken
				|| this.IsPossibleExpression();
		}

		private bool IsPossibleJavaEnumConstant()
		{
			return this.CurrentToken.Kind == SyntaxKind.AtToken || this.IsTrueIdentifier();
		}


		private bool IsPossibleType()
		{
			var tk = this.CurrentToken.Kind;
			return IsPredefinedType(tk) || this.IsTrueIdentifier();
		}

		private bool IsPossibleName()
		{
			return this.IsTrueIdentifier();
		}


		private bool IsPossibleLabeledStatement()
		{
			return this.PeekToken(1).Kind == SyntaxKind.ColonToken && this.IsTrueIdentifier();
		}



		private bool IsPossibleLocalDeclarationStatement(bool allowAnyExpression)
		{
			// This method decides whether to parse a statement as a
			// declaration or as an expression statement. In the old
			// compiler it would simple call IsLocalDeclaration.

			var tk = this.CurrentToken.Kind;
			if ((SyntaxKindFacts.IsPredefinedType(tk) && this.PeekToken(1).Kind != SyntaxKind.DotToken) || IsDeclarationModifier(tk))
			{
				return true;
			}

			bool? typedIdentifier = IsPossibleTypedIdentifierStart(this.CurrentToken, this.PeekToken(1), allowThisKeyword: false);
			if (typedIdentifier != null)
			{
				return typedIdentifier.Value;
			}

			var resetPoint = this.GetResetPoint();
			try
			{
				ScanTypeFlags st = this.ScanType();

				// We could always return true for st == AliasQualName in addition to MustBeType on the first line, however, we want it to return false in the case where
				// CurrentToken.Kind != SyntaxKind.Identifier so that error cases, like: A::N(), are not parsed as variable declarations and instead are parsed as A.N() where we can give
				// a better error message saying "did you meant to use a '.'?"
				if (st == ScanTypeFlags.MustBeType && this.CurrentToken.Kind != SyntaxKind.DotToken)
				{
					return true;
				}

				if (st == ScanTypeFlags.NotType || this.CurrentToken.Kind != SyntaxKind.IdentifierToken)
				{
					return false;
				}

				// T? and T* might start an expression, we need to parse further to disambiguate:
				if (allowAnyExpression)
				{
					if (st == ScanTypeFlags.PointerOrMultiplication)
					{
						return false;
					}
					else if (st == ScanTypeFlags.NullableType)
					{
						return IsPossibleDeclarationStatementFollowingNullableType();
					}
				}

				return true;
			}
			finally
			{
				this.Reset(ref resetPoint);
				this.Release(ref resetPoint);
			}
		}

		// Looks ahead for a declaration of a field, property or method declaration following a nullable type T?.
		private bool IsPossibleDeclarationStatementFollowingNullableType()
		{
			if (IsFieldDeclaration(isEvent: false))
			{
				return IsPossibleFieldDeclarationFollowingNullableType();
			}

			SyntaxToken identifierOrThisOpt;
			TypeParameterListSyntax typeParameterListOpt;
			this.ParseMemberName(out identifierOrThisOpt, out typeParameterListOpt);

			if (identifierOrThisOpt == null && typeParameterListOpt == null)
			{
				return false;
			}

			// looks like a property:
			if (this.CurrentToken.Kind == SyntaxKind.OpenBraceToken)
			{
				return true;
			}

			// don't accept indexers:
			if (identifierOrThisOpt.Kind == SyntaxKind.ThisKeyword)
			{
				return false;
			}

			return IsPossibleMethodDeclarationFollowingNullableType();
		}

		// At least one variable declaration terminated by a semicolon or a comma.
		//   idf;
		//   idf,
		//   idf = <expr>;
		//   idf = <expr>, 
		private bool IsPossibleFieldDeclarationFollowingNullableType()
		{
			if (this.CurrentToken.Kind != SyntaxKind.IdentifierToken)
			{
				return false;
			}

			this.EatToken();

			if (this.CurrentToken.Kind == SyntaxKind.EqualsToken)
			{
				var saveTerm = this._termState;
				this._termState |= TerminatorState.IsEndOfFieldDeclaration;
				this.EatToken();
				this.ParseVariableInitializer(allowStackAlloc: false);
				this._termState = saveTerm;
			}

			return this.CurrentToken.Kind == SyntaxKind.CommaToken || this.CurrentToken.Kind == SyntaxKind.SemicolonToken;
		}

		private bool IsPossibleMethodDeclarationFollowingNullableType()
		{
			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsEndOfMethodSignature;

			var paramList = this.ParseParenthesizedParameterList(allowThisKeyword: true, allowDefaults: true, allowAttributes: true, allowFieldModifiers: false);

			this._termState = saveTerm;
			var separatedParameters = paramList.Parameters.GetWithSeparators();

			// parsed full signature:
			if (!paramList.CloseParenToken.IsMissing)
			{

				// disambiguates conditional expressions
				// (...) :
				if (this.CurrentToken.Kind == SyntaxKind.ColonToken)
				{
					return false;
				}
			}

			// no parameters, just an open paren followed by a token that doesn't belong to a parameter definition:
			if (separatedParameters.Count == 0)
			{
				return false;
			}

			var parameter = (ParameterSyntax)separatedParameters[0];

			// has an _annotation:
			//   ([Attr]
			if (parameter.AttributeLists.Count > 0)
			{
				return true;
			}

			// has params modifier:
			//   (params
			for (int i = 0; i < parameter.Modifiers.Count; i++)
			{
				if (parameter.Modifiers[i].Kind == SyntaxKind.ParamsKeyword)
				{
					return true;
				}
			}

			if (parameter.Type == null)
			{
				// has arglist:
				//   (__arglist
				if (parameter.Identifier.Kind == SyntaxKind.ArgListKeyword)
				{
					return true;
				}
			}
			else if (parameter.Type.Kind == SyntaxKind.NullableType)
			{
				// nullable type with modifiers
				//   (ref T?
				//   (out T?
				if (parameter.Modifiers.Count > 0)
				{
					return true;
				}

				// nullable type, identifier, and separator or closing parent
				//   (T ? idf,
				//   (T ? idf)
				if (!parameter.Identifier.IsMissing &&
					(separatedParameters.Count >= 2 && !separatedParameters[1].IsMissing ||
					 separatedParameters.Count == 1 && !paramList.CloseParenToken.IsMissing))
				{
					return true;
				}
			}
			else
			{
				// has a name and a non-nullable type:
				//   (T idf
				//   (ref T idf
				//   (out T idf
				if (!parameter.Identifier.IsMissing)
				{
					return true;
				}
			}

			return false;
		}

		private bool IsPossibleNewExpression()
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.NewKeyword);

			// skip new
			SyntaxToken nextToken = PeekToken(1);

			// new { }
			// new [ ]
			switch (nextToken.Kind)
			{
				case SyntaxKind.OpenBraceToken:
				case SyntaxKind.OpenBracketToken:
					return true;
			}

			//
			// Declaration with new modifier vs. new expression
			// Parse it as an expression if the type is not followed by an identifier or this keyword.
			//
			// Member declarations:
			//   new T Idf ...
			//   new T this ...
			//   new partial Idf    ("partial" as a type name)
			//   new partial this   ("partial" as a type name)
			//   new partial T Idf
			//   new partial T this
			//   new <modifier>
			//   new <class|interface|struct|enum>
			//   new partial <class|interface|struct|enum>
			//
			// New expressions:
			//   new T []
			//   new T { }
			//   new <non-type>
			//
			if (SyntaxKindFacts.GetBaseTypeDeclarationKind(nextToken.Kind) != SyntaxKind.None)
			{
				return false;
			}

			SyntaxModifier modifier = GetModifier(nextToken);
			if (modifier != SyntaxModifier.None)
			{
				return false;
			}

			bool? typedIdentifier = IsPossibleTypedIdentifierStart(nextToken, PeekToken(2), allowThisKeyword: true);
			if (typedIdentifier != null)
			{
				// new Idf Idf
				// new Idf .
				// new partial T
				// new partial .
				return !typedIdentifier.Value;
			}

			var resetPoint = this.GetResetPoint();
			try
			{
				// skips new keyword
				EatToken();

				ScanTypeFlags st = this.ScanType();

				return !IsPossibleMemberName() || st == ScanTypeFlags.NotType;
			}
			finally
			{
				this.Reset(ref resetPoint);
				this.Release(ref resetPoint);
			}
		}

		/// <returns>
		/// true if the current token can be the first token of a typed identifier (a type name followed by an identifier),
		/// false if it definitely can't be,
		/// null if we need to scan further to find out.
		/// </returns>
		private static bool? IsPossibleTypedIdentifierStart(SyntaxToken current, SyntaxToken next, bool allowThisKeyword)
		{
			if (current.Kind == SyntaxKind.IdentifierToken)
			{
				switch (next.Kind)
				{
					// tokens that can be in type names...
					case SyntaxKind.DotToken:
					case SyntaxKind.AsteriskToken:
					case SyntaxKind.QuestionToken:
					case SyntaxKind.OpenBracketToken:
					case SyntaxKind.LessThanToken:
					case SyntaxKind.ColonColonToken:
						return null;

					case SyntaxKind.IdentifierToken:
						return true;

					case SyntaxKind.ThisKeyword:
						return allowThisKeyword;

					default:
						return false;
				}
			}

			return null;
		}


		private bool IsPossibleStatementStartOrStop()
		{
			return this.CurrentToken.Kind == SyntaxKind.SemicolonToken
				|| this.IsPossibleStatement();
		}


		private bool IsPossibleStatement()
		{
			var tk = this.CurrentToken.Kind;
			switch (tk)
			{
				case SyntaxKind.BreakKeyword:
				case SyntaxKind.ContinueKeyword:
				case SyntaxKind.TryKeyword:
				case SyntaxKind.ConstKeyword:
				case SyntaxKind.DoKeyword:
				case SyntaxKind.ForKeyword:
				case SyntaxKind.GotoKeyword:
				case SyntaxKind.IfKeyword:
				case SyntaxKind.ReturnKeyword:
				case SyntaxKind.SwitchKeyword:
				case SyntaxKind.ThrowKeyword:
				case SyntaxKind.ImportKeyword:
				case SyntaxKind.WhileKeyword:
				case SyntaxKind.OpenBraceToken:
				case SyntaxKind.SemicolonToken:
				case SyntaxKind.StaticKeyword:
				case SyntaxKind.VolatileKeyword:
					return true;
				case SyntaxKind.IdentifierToken:
					return IsTrueIdentifier();
				case SyntaxKind.CatchKeyword:
				case SyntaxKind.FinallyKeyword:
					return !this._isInTry;
				default:
					return IsPredefinedType(tk)
					   || IsPossibleExpression();
			}
		}



		private bool IsPossibleSwitchSection()
		{
			return (this.CurrentToken.Kind == SyntaxKind.CaseKeyword) ||
				   (this.CurrentToken.Kind == SyntaxKind.DefaultKeyword && this.PeekToken(1).Kind != SyntaxKind.OpenParenToken);
		}


		private bool IsPossibleExpression()
		{
			var tk = this.CurrentToken.Kind;
			switch (tk)
			{
				case SyntaxKind.DefaultKeyword:
				case SyntaxKind.ArgListKeyword:
				case SyntaxKind.SuperKeyword:
				case SyntaxKind.FalseKeyword:
				case SyntaxKind.ThisKeyword:
				case SyntaxKind.TrueKeyword:
				case SyntaxKind.NullKeyword:
				case SyntaxKind.OpenParenToken:
				case SyntaxKind.NumericLiteralToken:
				case SyntaxKind.StringLiteralToken:
				case SyntaxKind.CharacterLiteralToken:
				case SyntaxKind.NewKeyword:
				case SyntaxKind.ColonColonToken: // bad aliased name
				case SyntaxKind.AtToken:
				case SyntaxKind.OpenBraceToken:
					return true;
				case SyntaxKind.IdentifierToken:
					// Specifically allow the from contextual keyword, because it can always be the start of an
					// expression (whether it is used as an identifier or a keyword).
					return this.IsTrueIdentifier();
				default:
					return IsExpectedPrefixUnaryOperator(tk)
						|| (IsPredefinedType(tk) && tk != SyntaxKind.VoidKeyword)
						|| SyntaxKindFacts.IsAnyUnaryExpression(tk)
						|| SyntaxKindFacts.IsBinaryExpression(tk);
			}
		}



		private bool IsPossibleIdentifierDotSuffix(SyntaxKind kind)
		{
			var tk = this.CurrentToken.Kind;
			if (tk == SyntaxKind.IdentifierName 
				&& this.PeekToken(1).Kind == SyntaxKind.DotToken 
				&& this.PeekToken(2).Kind == kind)
			{
				return true;
			}

			var resetPoint = this.GetResetPoint();
			try
			{
				SyntaxToken lastTokenOfType;
				ScanTypeFlags st = this.ScanType(out lastTokenOfType);

				if (st == ScanTypeFlags.ClassKeywordSuffix)
				{
					return true;
				}
				return false;
			}
			finally
			{
				this.Reset(ref resetPoint);
				this.Release(ref resetPoint);
			}
		}


		private bool IsPossibleClassLiteralExpression(bool contextRequiresVariable)
		{
			var tk = this.CurrentToken.Kind;
			if (tk == SyntaxKind.IdentifierName 
				&& this.PeekToken(1).Kind == SyntaxKind.DotToken 
				&& this.PeekToken(2).Kind == SyntaxKind.ClassKeyword)
			{
				return true;
			}

			var resetPoint = this.GetResetPoint();
			try
			{
				SyntaxToken lastTokenOfType;
				ScanTypeFlags st = this.ScanType(out lastTokenOfType);

				if (st == ScanTypeFlags.ClassKeywordSuffix)
				{
					return true;
				}
				return false;
			}
			finally
			{
				this.Reset(ref resetPoint);
				this.Release(ref resetPoint);
			}
		}

		private bool IsPossibleQualifiedThisExpression(bool contextRequiresVariable)
		{
			var tk = this.CurrentToken.Kind;
			if (tk == SyntaxKind.IdentifierName 
				&& this.PeekToken(1).Kind == SyntaxKind.DotToken 
				&& this.PeekToken(1).Kind == SyntaxKind.ClassKeyword)
			{
				return true;
			}

			var resetPoint = this.GetResetPoint();
			try
			{
				SyntaxToken lastTokenOfType;
				ScanTypeFlags st = this.ScanType(out lastTokenOfType);

				if (st == ScanTypeFlags.ClassKeywordSuffix)
				{
					return true;
				}
				return false;
			}
			finally
			{
				this.Reset(ref resetPoint);
				this.Release(ref resetPoint);
			}
		}

		private bool IsPossibleDeclarationExpression(bool contextRequiresVariable)
		{
			var tk = this.CurrentToken.Kind;
			if (SyntaxKindFacts.IsPredefinedType(tk) && tk != SyntaxKind.VoidKeyword && this.PeekToken(1).Kind != SyntaxKind.DotToken)
			{
				return true;
			}

			var resetPoint = this.GetResetPoint();
			try
			{
				SyntaxToken lastTokenOfType;
				ScanTypeFlags st = this.ScanType(out lastTokenOfType);

				if (st == ScanTypeFlags.NotType || !this.IsTrueIdentifier())
				{
					return false;
				}

				if (contextRequiresVariable)
				{
					// Unless we parse this as a declaration expression, 
					// we'll get binding errors later on.
					return true;
				}

				switch (st)
				{
					case ScanTypeFlags.GenericTypeOrExpression:
					case ScanTypeFlags.PointerOrMultiplication:

						int position = 0;
						SyntaxKind afterIdentifierTokenKind;

						do
						{
							position++;
							afterIdentifierTokenKind = this.PeekToken(position).Kind;
						}
						while (afterIdentifierTokenKind == SyntaxKind.CloseParenToken);

						// If we have something that looks like a pointer or generic type, followed by an identifier, followed by '[)...][+-*...]=' tokens.
						// This cannot be a valid non-declaration expression.
						return SyntaxKindFacts.IsAssignmentExpressionOperatorToken(afterIdentifierTokenKind);

					case ScanTypeFlags.NullableType:
						// This can be a part of a ConditionalExpression.
						var resetPoint2 = this.GetResetPoint();
						try
						{
							var nullCoalescingPrecedence = GetPrecedence(SyntaxKind.CoalesceExpression);
							var colonLeft = this.ParseSubExpression(nullCoalescingPrecedence - 1);

							if (colonLeft.Kind != SyntaxKind.DeclarationExpression && this.CurrentToken.Kind == SyntaxKind.ColonToken)
							{
								return false;
							}
						}
						finally
						{
							this.Reset(ref resetPoint2);
							this.Release(ref resetPoint2);
						}

						break;
				}

				// Let's specially handle some error cases.
				// For example:
				//      Colors? d = Co
				//      Colors c = Colors.Blue;
				//
				// We don't want this erroneous code to be parsed as a single statement equivalent to 
				//      Colors? d = Co Colors c = Colors.Blue;
				// Where "Co Colors" is parsed as a Declaration Expression. This would have negative
				// effect on editing experience.
				Debug.Assert(this.IsTrueIdentifier());

				if (lastTokenOfType.TrailingTrivia.Any(SyntaxKind.EndOfLineTrivia))
				{
					// We have a line break between something that looks like a type and the following identifier.
					// Can that identifier be a beginning of a local declaration?
					ScanTypeFlags st2 = this.ScanType();
					if (st2 != ScanTypeFlags.NotType && this.IsTrueIdentifier())
					{
						return false;
					}
				}

				return true;
			}
			finally
			{
				this.Reset(ref resetPoint);
				this.Release(ref resetPoint);
			}
		}

		private bool IsPossibleLambdaExpression(uint precedence)
		{
			if (precedence <= LambdaPrecedence && this.PeekToken(1).Kind == SyntaxKind.MinusGreaterThanToken)
			{
				return true;
			}


			return false;
		}



		private bool IsPossibleArgumentExpression()
		{
			switch (this.CurrentToken.Kind)
			{
				default:
					return this.IsPossibleExpression();
			}
		}


		private bool IsPossibleArrayCreationExpression()
		{
			// previous token should be NewKeyword

			var resetPoint = this.GetResetPoint();
			try
			{
				ScanTypeFlags isType = this.ScanNonArrayType();
				return isType != ScanTypeFlags.NotType && this.CurrentToken.Kind == SyntaxKind.OpenBracketToken;
			}
			finally
			{
				this.Reset(ref resetPoint);
				this.Release(ref resetPoint);
			}
		}



		private bool IsPossibleLambdaParameter()
		{
			switch (this.CurrentToken.Kind)
			{
				case SyntaxKind.ParamsKeyword:
					// params is not actually legal in a lambda, but we allow it for error
					// recovery purposes and then give an error during semantic analysis.
					return true;
				case SyntaxKind.IdentifierToken:
					return this.IsTrueIdentifier();
				default:
					return IsPredefinedType(this.CurrentToken.Kind);
			}
		}

		private bool IsPossibleParameter(bool allowThisKeyword, bool allowFieldModifiers)
		{
			switch (this.CurrentToken.Kind)
			{
				case SyntaxKind.AtToken: // _annotation
				case SyntaxKind.ParamsKeyword:
				case SyntaxKind.ArgListKeyword:
					return true;
				case SyntaxKind.ThisKeyword:
					return allowThisKeyword;
				case SyntaxKind.IdentifierToken:
					return this.IsTrueIdentifier();

				case SyntaxKind.NewKeyword:
				case SyntaxKind.PublicKeyword:
				case SyntaxKind.ProtectedKeyword:
				case SyntaxKind.PrivateKeyword:
				case SyntaxKind.StaticKeyword:
				case SyntaxKind.VolatileKeyword:
					return allowFieldModifiers;

				default:
					return IsPredefinedType(this.CurrentToken.Kind);
			}
		}

		private bool IsPossibleRankAndDimensionSpecifier()
		{
			if (this.CurrentToken.Kind == SyntaxKind.OpenBracketToken)
			{
				// When specifying rank and dimension, only commas and close square
				// brackets are valid after an open square bracket. However, we accept
				// numbers as well as the user might (mistakenly) try to specify the
				// array size here. This way, when the parser actually consumes these
				// tokens it will be able to specify an appropriate error message.
				/*
				SyntaxKind k = this.PeekToken(1).Kind;
				if (k == SyntaxKind.Comma ||
					k == SyntaxKind.CloseBracket ||
					k == SyntaxKind.NumericLiteral)
				{
					return true;
				}
				 */
				return true;
			}

			return false;
		}


	}
}