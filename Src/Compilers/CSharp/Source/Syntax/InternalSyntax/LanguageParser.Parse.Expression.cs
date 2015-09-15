// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Threading;

using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
	internal partial class LanguageParser : SyntaxParser
	{
		public ExpressionSyntax ParseExpression(bool allowDeclarationExpressionAtTheBeginning = true)
		{
			return this.ParseSubExpression(0, allowDeclarationExpressionAtTheBeginning);
		}

		private ExpressionSyntax ParseSubExpression(uint precedence, bool allowDeclarationExpressionAtTheBeginning = true, bool contextRequiresVariable = false)
		{
			ExpressionSyntax leftOperand = null;
			uint newPrecedence = 0;
			SyntaxKind opKind = SyntaxKind.None;

			// all of these are tokens that start statements and are invalid
			// to start a expression with. if we see one, then we must have
			// something like:
			//
			// return
			// if (...
			// parse out a missing name node for the expression, and keep on going
			var tk = this.CurrentToken.Kind;
			if (IsInvalidSubExpression(tk))
			{
				return this.AddError(this.CreateMissingIdentifierName(), ErrorCode.ERR_InvalidExprTerm, SyntaxKindFacts.GetText(tk));
			}

			// No left operand, so we need to parse one -- possibly preceded by a
			// unary operator.
			if (IsExpectedPrefixUnaryOperator(tk))
			{
				opKind = SyntaxKindFacts.GetPrefixUnaryExpression(tk);
				newPrecedence = GetPrecedence(opKind);
				var opToken = this.EatToken();
				var operand = this.ParseSubExpression(newPrecedence);
				leftOperand = _syntaxFactory.PrefixUnaryExpression(opKind, opToken, operand);
			}
			else
			{
				// Not a unary operator - get a primary expression.
				leftOperand = this.ParseTerm(precedence, allowDeclarationExpressionAtTheBeginning, contextRequiresVariable);
			}

			while (true)
			{
				// We either have a binary operator here, or we're finished.
				tk = this.CurrentToken.Kind;
				if (!IsExpectedBinaryOperator(tk))
				{
					break;
				}

				opKind = SyntaxKindFacts.GetBinaryExpression(tk);
				newPrecedence = GetPrecedence(opKind);

				Debug.Assert(newPrecedence > 0);      // All binary operators must have precedence > 0!

				// check for >> or >>= or >>> or >>>=
				bool doubleOp = false;
				bool doubleOpPlusOne = false;
				if (tk == SyntaxKind.GreaterThanToken
					&& (this.PeekToken(1).Kind == SyntaxKind.GreaterThanToken)
					&& (this.PeekToken(2).Kind == SyntaxKind.GreaterThanToken
						|| this.PeekToken(2).Kind == SyntaxKind.GreaterThanEqualsToken))
				{
					// check to see if they really are adjacent
					if (this.CurrentToken.GetTrailingTriviaWidth() == 0 
						&& this.PeekToken(1).GetLeadingTriviaWidth() == 0
						&& this.PeekToken(1).GetTrailingTriviaWidth() == 0
						&& this.PeekToken(2).GetLeadingTriviaWidth() == 0
						)
					{
						opKind = SyntaxKindFacts.GetBinaryExpression(this.PeekToken(2).Kind == SyntaxKind.GreaterThanToken ?
							SyntaxKind.GreaterThanGreaterThanGreaterThanToken :
							SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken);


						newPrecedence = GetPrecedence(opKind);
						doubleOp = true;
						doubleOpPlusOne = true;
					}
				}
				else if (tk == SyntaxKind.GreaterThanToken
					&& (this.PeekToken(1).Kind == SyntaxKind.GreaterThanToken
					|| this.PeekToken(1).Kind == SyntaxKind.GreaterThanEqualsToken))
				{
					// check to see if they really are adjacent
					if (this.CurrentToken.GetTrailingTriviaWidth() == 0 && this.PeekToken(1).GetLeadingTriviaWidth() == 0)
					{
						opKind = SyntaxKindFacts.GetBinaryExpression(this.PeekToken(1).Kind == SyntaxKind.GreaterThanToken ? 
							SyntaxKind.GreaterThanGreaterThanToken : 
							SyntaxKind.GreaterThanGreaterThanEqualsToken);
						newPrecedence = GetPrecedence(opKind);
						doubleOp = true;
					}
				}

				// Check the precedence to see if we should "take" this operator
				if (newPrecedence < precedence)
				{
					break;
				}

				// Same precedence, but not right-associative -- deal with this "later"
				if ((newPrecedence == precedence) && !IsRightAssociative(opKind))
				{
					break;
				}

				// Precedence is okay, so we'll "take" this operator.
				var opToken = this.EatToken();
				if (doubleOp)
				{
					// combine tokens into a single token
					var opToken2 = this.EatToken();
					if (doubleOpPlusOne)
					{
						var opToken3 = this.EatToken();
						var kind = opToken3.Kind == SyntaxKind.GreaterThanToken ? 
							SyntaxKind.GreaterThanGreaterThanGreaterThanToken : 
							SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken;

						opToken = SyntaxFactory.Token(opToken.GetLeadingTrivia(), kind, opToken3.GetTrailingTrivia());
					}
					else
					{
						var kind = opToken2.Kind == SyntaxKind.GreaterThanToken ? 
							SyntaxKind.GreaterThanGreaterThanToken : 
							SyntaxKind.GreaterThanGreaterThanEqualsToken;
						opToken = SyntaxFactory.Token(opToken.GetLeadingTrivia(), kind, opToken2.GetTrailingTrivia());
					}

				}

				if (opKind == SyntaxKind.InstanceOfExpression)
				{
					leftOperand = _syntaxFactory.BinaryExpression(opKind, leftOperand, opToken,
						this.ParseTypeCore(parentIsParameter: false, isInstanceOfOrAs: true, expectSizes: false, isArrayCreation: false));
				}
				else
				{
					leftOperand = _syntaxFactory.BinaryExpression(opKind, leftOperand, opToken, this.ParseSubExpression(newPrecedence));
				}
			}

			// From the language spec:
			//
			// conditional-expression:
			//  null-coalescing-expression
			//  null-coalescing-expression   ?   expression   :   expression
			//
			// Only take the ternary if we're at a precedence less than the null coelescing
			// expression.

			var nullCoalescingPrecedence = 2U;//GetPrecedence(SyntaxKind.CoalesceExpression);
			if (tk == SyntaxKind.QuestionToken && precedence < 2)
			{
				var questionToken = this.EatToken();

				var colonLeft = this.ParseSubExpression(nullCoalescingPrecedence - 1);
				var colon = this.EatToken(SyntaxKind.ColonToken);

				var colonRight = this.ParseSubExpression(nullCoalescingPrecedence - 1);
				leftOperand = _syntaxFactory.ConditionalExpression(leftOperand, questionToken, colonLeft, colon, colonRight);
			}

			return leftOperand;
		}

		private ExpressionSyntax ParseTerm(uint precedence, bool allowDeclarationExpression, bool contextRequiresVariable)
		{
			ExpressionSyntax expr = null;

			var tk = this.CurrentToken.Kind;
			if (SyntaxKindFacts.IsPredefinedType(tk) && allowDeclarationExpression
				&& this.PeekToken(1).Kind == SyntaxKind.DotToken 
				&& this.PeekToken(2).Kind == SyntaxKind.ClassKeyword)
			{
				expr = ParseClassLiteralExpression();
			}
			else
			{
				switch (tk)
				{
					case SyntaxKind.DefaultKeyword:
						expr = this.ParseDefaultExpression();
						break;
					case SyntaxKind.ColonColonToken:
						// misplaced ::
						// TODO: this should not be a compound name.. (disallow dots)
						expr = this.ParseQualifiedName(NameOptions.InExpression);
						break;
					case SyntaxKind.IdentifierToken:
						if (this.IsTrueIdentifier())
						{
							if (this.IsPossibleLambdaExpression(precedence))
							{
								expr = this.ParseLambdaExpression();
							}
							else if (allowDeclarationExpression && IsPossibleDeclarationExpression(contextRequiresVariable))
							{
								expr = ParseDeclarationExpression();
							}
							else if (IsPossibleIdentifierDotSuffix(SyntaxKind.ClassKeyword))
							{
								expr = ParseClassLiteralExpression();
							}
							else if (IsPossibleIdentifierDotSuffix(SyntaxKind.ThisKeyword))
							{
								expr = ParseJavaQualifiedThisExpression();
							}
							else if (IsPossibleIdentifierDotSuffix(SyntaxKind.SuperKeyword))
							{
								expr = ParseJavaQualifiedSuperExpression();
							}
							else if (IsPossibleIdentifierDotSuffix(SyntaxKind.NewKeyword))
							{
								expr = this.ParseNewExpression();
							}
							else
							{
								expr = this.ParseSimpleName(NameOptions.InExpression);
							}
						}
						else
						{
							expr = this.CreateMissingIdentifierName();
							expr = this.AddError(expr, ErrorCode.ERR_InvalidExprTerm, this.CurrentToken.Text);
						}

						break;
					case SyntaxKind.ThisKeyword:
						expr = _syntaxFactory.ThisExpression(this.EatToken());
						break;
					case SyntaxKind.SuperKeyword:
						expr = _syntaxFactory.BaseExpression(this.EatToken());
						break;
					case SyntaxKind.ArgListKeyword:
					case SyntaxKind.FalseKeyword:

					case SyntaxKind.TrueKeyword:
					case SyntaxKind.NullKeyword:
					case SyntaxKind.NumericLiteralToken:
					case SyntaxKind.StringLiteralToken:
					case SyntaxKind.CharacterLiteralToken:
						expr = _syntaxFactory.LiteralExpression(SyntaxKindFacts.GetLiteralExpression(tk), this.EatToken());
						break;
					case SyntaxKind.OpenParenToken:
						expr = this.ParseCastOrParenExpressionOrLambda(precedence, contextRequiresVariable: contextRequiresVariable);
						break;
					case SyntaxKind.NewKeyword:
						expr = this.ParseNewExpression();
						break;
					case SyntaxKind.OpenBraceToken:
						expr = this.ParseArrayInitializer();
						break;
					case SyntaxKind.AtToken:
						expr = this.ParseAnnotationCreationExpression();
						break;
					default:
						// check for intrinsic type followed by '.'
						if (IsPredefinedType(tk))
						{
							if (allowDeclarationExpression && IsPossibleDeclarationExpression(contextRequiresVariable))
							{
								expr = ParseDeclarationExpression();
							}
							else
							{
								expr = _syntaxFactory.PredefinedType(this.EatToken());

								if (this.CurrentToken.Kind != SyntaxKind.DotToken || tk == SyntaxKind.VoidKeyword)
								{
									expr = this.AddError(expr, ErrorCode.ERR_InvalidExprTerm, SyntaxKindFacts.GetText(tk));
								}
							}
						}
						else
						{
							expr = this.CreateMissingIdentifierName();

							if (tk == SyntaxKind.EndOfFileToken)
							{
								expr = this.AddError(expr, ErrorCode.ERR_ExpressionExpected);
							}
							else
							{
								expr = this.AddError(expr, ErrorCode.ERR_InvalidExprTerm, SyntaxKindFacts.GetText(tk));
							}
						}

						break;
				}
			}

			return this.ParsePostFixExpression(expr);
		}

		private ExpressionSyntax ParseDeclarationExpression()
		{


			TypeSyntax typeSyntax = ParseType(parentIsParameter: false);

			return _syntaxFactory.DeclarationExpression(typeSyntax,
													   ParseVariableDeclarator(typeSyntax,
																			   VariableFlags.Local,
																			   isFirst: true,
																			   isExpressionContext: true));
		}

		private ExpressionSyntax ParseClassLiteralExpression()
		{
			TypeSyntax typeSyntax = ParseType(parentIsParameter: false);
			return _syntaxFactory.ClassLiteralExpression(typeSyntax, 
				this.EatToken(SyntaxKind.DotToken),
				this.EatToken(SyntaxKind.ClassKeyword));
		}

		private JavaQualifiedSuperExpressionSyntax ParseJavaQualifiedSuperExpression()
		{
			TypeSyntax typeSyntax = ParseType(parentIsParameter: false);
			return _syntaxFactory.JavaQualifiedSuperExpression(typeSyntax,
				this.EatToken(SyntaxKind.DotToken),
				this.EatToken(SyntaxKind.SuperKeyword));
		}

		private JavaQualifiedThisExpressionSyntax ParseJavaQualifiedThisExpression()
		{
			TypeSyntax typeSyntax = ParseType(parentIsParameter: false);
			return _syntaxFactory.JavaQualifiedThisExpression(typeSyntax,
				this.EatToken(SyntaxKind.DotToken),
				this.EatToken(SyntaxKind.ThisKeyword));
		}

		private ExpressionSyntax ParsePostFixExpression(ExpressionSyntax expr)
		{
			Debug.Assert(expr != null);

			while (true)
			{
				SyntaxKind tk = this.CurrentToken.Kind;
				switch (tk)
				{
					case SyntaxKind.OpenParenToken:
						expr = _syntaxFactory.InvocationExpression(expr, this.ParseParenthesizedArgumentList());
						break;

					case SyntaxKind.OpenBracketToken:
						expr = _syntaxFactory.ElementAccessExpression(expr, this.ParseBracketedArgumentList());
						break;

					case SyntaxKind.PlusPlusToken:
					case SyntaxKind.MinusMinusToken:
						expr = _syntaxFactory.PostfixUnaryExpression(SyntaxKindFacts.GetPostfixUnaryExpression(tk), expr, this.EatToken());
						break;

					case SyntaxKind.ColonColonToken:
						if (this.PeekToken(1).Kind == SyntaxKind.IdentifierToken)
						{
							// replace :: with missing dot and annotate with skipped text "::" and error
							var ccToken = this.EatToken();
							ccToken = this.AddError(ccToken, ErrorCode.ERR_UnexpectedAliasedName);
							var dotToken = this.ConvertToMissingWithTrailingTrivia(ccToken, SyntaxKind.DotToken);
							expr = _syntaxFactory.MemberAccessExpression(
								expr,
								dotToken,
								this.ParseSimpleName(NameOptions.InExpression));
						}
						else
						{
							// just some random trailing :: ?
							expr = AddTrailingSkippedSyntax(expr, this.EatTokenWithPrejudice(SyntaxKind.DotToken));
						}
						break;

					//case SyntaxKind.MinusGreaterThanToken:
					//	expr = _syntaxFactory.MemberAccessExpression(SyntaxKind.PointerMemberAccessExpression, 
					//		expr, 
					//		this.EatToken(), 
					//		this.ParseSimpleName(NameOptions.InExpression));
					//	break;
					case SyntaxKind.DotToken:
						expr = _syntaxFactory.MemberAccessExpression(
							expr,
							this.EatToken(SyntaxKind.DotToken),
							this.ParseSimpleName(NameOptions.InExpression));
						break;

					default:
						return expr;
				}
			}
		}

		private ExpressionSyntax ParseCastOrParenExpressionOrLambda(uint precedence, bool contextRequiresVariable)
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.OpenParenToken);

			var resetPoint = this.GetResetPoint();
			try
			{
				if (ScanParenthesizedImplicitlyTypedLambda(precedence))
				{
					return this.ParseLambdaExpression();
				}

				// We have a decision to make -- is this a cast, or is it a parenthesized
				// expression?  Because look-ahead is cheap with our token stream, we check
				// to see if this "looks like" a cast (without constructing any parse trees)
				// to help us make the decision.
				if (this.ScanCast())
				{

					// Looks like a cast, so parse it as one.
					this.Reset(ref resetPoint);
					var openParen = this.EatToken(SyntaxKind.OpenParenToken);
					var type = this.ParseType(false);
					var closeParen = this.EatToken(SyntaxKind.CloseParenToken);
					var expr = this.ParseSubExpression(GetPrecedence(SyntaxKind.CastExpression));
					return _syntaxFactory.CastExpression(openParen, type, closeParen, expr);

				}

				this.Reset(ref resetPoint);
				if (this.ScanExplicitlyTypedLambda(precedence))
				{
					return this.ParseLambdaExpression();
				}

				// Doesn't look like a cast, so parse this as a parenthesized expression.
				{
					this.Reset(ref resetPoint);
					var openParen = this.EatToken(SyntaxKind.OpenParenToken);
					var expression = this.ParseSubExpression(0, contextRequiresVariable: contextRequiresVariable);
					var closeParen = this.EatToken(SyntaxKind.CloseParenToken);
					return _syntaxFactory.ParenthesizedExpression(openParen, expression, closeParen);
				}
			}
			finally
			{
				this.Release(ref resetPoint);
			}
		}

		private ExpressionSyntax ParseNewExpression()
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.NewKeyword || this.PeekToken(1).Kind == SyntaxKind.NewKeyword);

			if (this.IsAnonymousType())
			{
				return this.ParseAnonymousTypeExpression();
			}
			else
			{
				// assume object creation as default case
				return this.ParseArrayOrObjectCreationExpression();
			}
		}

		private ExpressionSyntax ParseArrayOrObjectCreationExpression()
		{
			SimpleNameSyntax name = this.CurrentToken.Kind == SyntaxKind.IdentifierToken ? this.ParseSimpleName(NameOptions.InExpression) : null;
			SyntaxToken @dot = (name != null) ? this.EatToken(SyntaxKind.DotToken) : null;
			SyntaxToken @new = this.EatToken(SyntaxKind.NewKeyword);
			bool isPossibleArrayCreation = this.IsPossibleArrayCreationExpression();
			var type = this.ParseTypeCore(parentIsParameter: false, isInstanceOfOrAs: false, expectSizes: isPossibleArrayCreation, isArrayCreation: isPossibleArrayCreation);

			if (type.Kind == SyntaxKind.ArrayType)
			{
				// Check for an initializer.
				InitializerExpressionSyntax initializer = null;
				if (this.CurrentToken.Kind == SyntaxKind.OpenBraceToken)
				{
					initializer = this.ParseArrayInitializer();
				}
				else if (type.Kind == SyntaxKind.ArrayType)
				{
					var rankSpec = ((ArrayTypeSyntax)type).RankSpecifiers[0];
					if (GetNumberOfNonOmittedArraySizes(rankSpec) == 0)
					{
						type = this.AddError(type, rankSpec, ErrorCode.ERR_MissingArraySize);
					}
				}

				return _syntaxFactory.ArrayCreationExpression(@new, (ArrayTypeSyntax)type, initializer);
			}
			else
			{
				ArgumentListSyntax argumentList = null;
				if (this.CurrentToken.Kind == SyntaxKind.OpenParenToken)
				{
					argumentList = this.ParseParenthesizedArgumentList();
				}

				//处理双大括号初始化 匿名类
				//List<String> blah = new ArrayList<String>(){{add("asdfa");add("bbb");}};
				//	List<Character> characters = new ArrayList<Character>() {
				//		{
				//			for (char c = 'A'; c <= 'E'; c++) add(c);
				//		}
				//	};
				//JavaAnonymousClassDeclarationSyntax


				//InitializerExpressionSyntax initializer = null;
				//if (this.CurrentToken.Kind == SyntaxKind.OpenBraceToken)
				//{
				//	initializer = this.ParseObjectOrCollectionInitializer();
				//}

				JavaAnonymousClassInitializerExpressionSyntax initializer = null;
				if (this.CurrentToken.Kind == SyntaxKind.OpenBraceToken)
				{
					initializer = this.ParseJavaAnonymousClassBody();
				}

				// we need one or the other
				if (argumentList == null && initializer == null)
				{
					argumentList = _syntaxFactory.ArgumentList(
						this.AddError(SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken), ErrorCode.ERR_BadNewExpr),
						default(SeparatedSyntaxList<ArgumentSyntax>),
						SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken));
				}

				return _syntaxFactory.ObjectCreationExpression(name, @dot, @new, type, argumentList, initializer);
			}
		}

		private ExpressionSyntax ParseElementInitializer()
		{
			if (this.CurrentToken.Kind == SyntaxKind.OpenBraceToken)
			{
				return this.ParseComplexElementInitializer();
			}
			else
			{
				return this.ParseExpression();
			}
		}


		#region {}
		private InitializerExpressionSyntax ParseObjectOrCollectionInitializer()
		{
			var openBrace = this.EatToken(SyntaxKind.OpenBraceToken);

			var initializers = this._pool.AllocateSeparated<ExpressionSyntax>();
			try
			{
				bool isObjectInitializer;
				this.ParseObjectOrCollectionInitializerMembers(ref openBrace, initializers, out isObjectInitializer);
				Debug.Assert(initializers.Count > 0 || isObjectInitializer);

				openBrace = CheckFeatureAvailability(openBrace, isObjectInitializer ? MessageID.IDS_FeatureObjectInitializer : MessageID.IDS_FeatureCollectionInitializer);

				var closeBrace = this.EatToken(SyntaxKind.CloseBraceToken);
				return _syntaxFactory.InitializerExpression(
					isObjectInitializer ?
						SyntaxKind.ObjectInitializerExpression :
						SyntaxKind.CollectionInitializerExpression,
					openBrace,
					initializers,
					closeBrace);
			}
			finally
			{
				this._pool.Free(initializers);
			}
		}

		private void ParseObjectOrCollectionInitializerMembers(ref SyntaxToken startToken, SeparatedSyntaxListBuilder<ExpressionSyntax> list, out bool isObjectInitializer)
		{
			// Empty initializer list must be parsed as an object initializer.
			isObjectInitializer = true;

			if (this.CurrentToken.Kind != SyntaxKind.CloseBraceToken)
			{
			tryAgain:
				if (this.IsInitializerMember() || this.CurrentToken.Kind == SyntaxKind.CommaToken)
				{
					// We have at least one initializer expression.
					// If at least one initializer expression is a named assignment, this is an object initializer.
					// Otherwise, this is a collection initializer.
					isObjectInitializer = false;

					// first argument
					list.Add(this.ParseObjectOrCollectionInitializerMember(ref isObjectInitializer));

					// additional arguments
					while (true)
					{
						if (this.CurrentToken.Kind == SyntaxKind.CloseBraceToken)
						{
							break;
						}
						else if (this.CurrentToken.Kind == SyntaxKind.CommaToken || this.IsInitializerMember())
						{
							list.AddSeparator(this.EatToken(SyntaxKind.CommaToken));

							// check for exit case after legal trailing comma
							if (this.CurrentToken.Kind == SyntaxKind.CloseBraceToken)
							{
								break;
							}

							list.Add(this.ParseObjectOrCollectionInitializerMember(ref isObjectInitializer));
							continue;
						}
						else if (this.SkipBadInitializerListTokens(ref startToken, list, SyntaxKind.CommaToken) == PostSkipAction.Abort)
						{
							break;
						}
					}
				}
				else if (this.SkipBadInitializerListTokens(ref startToken, list, SyntaxKind.IdentifierToken) == PostSkipAction.Continue)
				{
					goto tryAgain;
				}
			}

			// We may have invalid initializer elements. These will be reported during binding.
		}

		private ExpressionSyntax ParseObjectOrCollectionInitializerMember(ref bool isObjectInitializer)
		{
			if (this.IsComplexElementInitializer())
			{
				return this.ParseComplexElementInitializer();
			}
			else if (IsDictionaryInitializer())
			{
				isObjectInitializer = true;
				return this.ParseDictionaryInitializer();
			}
			else if (this.IsNamedAssignment())
			{
				isObjectInitializer = true;
				return this.ParseObjectInitializerNamedAssignment();
			}
			else
			{
				return this.ParseExpression();
			}
		}

		private ExpressionSyntax ParseObjectInitializerNamedAssignment()
		{
			var identifier = this.ParseIdentifierName();
			var equal = this.EatToken(SyntaxKind.EqualsToken);
			ExpressionSyntax expression;
			if (this.CurrentToken.Kind == SyntaxKind.OpenBraceToken)
			{
				expression = this.ParseObjectOrCollectionInitializer();
			}
			else
			{
				expression = this.ParseExpression();
			}

			return _syntaxFactory.BinaryExpression(SyntaxKind.SimpleAssignmentExpression, identifier, equal, expression);
		}

		private ExpressionSyntax ParseDictionaryInitializer()
		{
			var arguments = this.ParseBracketedArgumentList();
			var equal = this.EatToken(SyntaxKind.EqualsToken);
			ExpressionSyntax expression;
			if (this.CurrentToken.Kind == SyntaxKind.OpenBraceToken)
			{
				expression = this.ParseObjectOrCollectionInitializer();
			}
			else
			{
				expression = this.ParseExpression();
			}

			var elementAccess = _syntaxFactory.ImplicitElementAccess(arguments);
			return _syntaxFactory.BinaryExpression(SyntaxKind.SimpleAssignmentExpression, elementAccess, equal, expression);
		}

		#endregion


		private InitializerExpressionSyntax ParseComplexElementInitializer()
		{
			var openBrace = this.EatToken(SyntaxKind.OpenBraceToken);
			var initializers = this._pool.AllocateSeparated<ExpressionSyntax>();
			try
			{
				DiagnosticInfo closeBraceError;
				this.ParseExpressionsForComplexElementInitializer(ref openBrace, initializers, out closeBraceError);
				var closeBrace = this.EatToken(SyntaxKind.CloseBraceToken);
				if (closeBraceError != null)
				{
					closeBrace = WithAdditionalDiagnostics(closeBrace, closeBraceError);
				}
				return _syntaxFactory.InitializerExpression(SyntaxKind.ComplexElementInitializerExpression, openBrace, initializers, closeBrace);
			}
			finally
			{
				this._pool.Free(initializers);
			}
		}

		private void ParseExpressionsForComplexElementInitializer(ref SyntaxToken openBrace, SeparatedSyntaxListBuilder<ExpressionSyntax> list, out DiagnosticInfo closeBraceError)
		{
			closeBraceError = null;

			if (this.CurrentToken.Kind != SyntaxKind.CloseBraceToken)
			{
			tryAgain:
				if (this.IsPossibleExpression() || this.CurrentToken.Kind == SyntaxKind.CommaToken)
				{
					// first argument
					list.Add(this.ParseExpression());

					// additional arguments
					while (true)
					{
						if (this.CurrentToken.Kind == SyntaxKind.CloseBraceToken)
						{
							break;
						}
						else if (this.CurrentToken.Kind == SyntaxKind.CommaToken || this.IsPossibleExpression())
						{
							list.AddSeparator(this.EatToken(SyntaxKind.CommaToken));
							if (this.CurrentToken.Kind == SyntaxKind.CloseBraceToken)
							{
								closeBraceError = MakeError(this.CurrentToken, ErrorCode.ERR_ExpressionExpected);
								break;
							}
							list.Add(this.ParseExpression());
							continue;
						}
						else if (this.SkipBadInitializerListTokens(ref openBrace, list, SyntaxKind.CommaToken) == PostSkipAction.Abort)
						{
							break;
						}
					}
				}
				else if (this.SkipBadInitializerListTokens(ref openBrace, list, SyntaxKind.IdentifierToken) == PostSkipAction.Continue)
				{
					goto tryAgain;
				}
			}
		}

		private InitializerExpressionSyntax ParseArrayInitializer()
		{
			var openBrace = this.EatToken(SyntaxKind.OpenBraceToken);

			// NOTE:  This loop allows " { <initexpr>, } " but not " { , } "
			var list = _pool.AllocateSeparated<ExpressionSyntax>();
			try
			{
				if (this.CurrentToken.Kind != SyntaxKind.CloseBraceToken)
				{
				tryAgain:
					if (this.IsPossibleVariableInitializer(false) || this.CurrentToken.Kind == SyntaxKind.CommaToken)
					{
						list.Add(this.ParseVariableInitializer(false));

						while (true)
						{
							if (this.CurrentToken.Kind == SyntaxKind.CloseBraceToken)
							{
								break;
							}
							else if (this.IsPossibleVariableInitializer(false) || this.CurrentToken.Kind == SyntaxKind.CommaToken)
							{
								list.AddSeparator(this.EatToken(SyntaxKind.CommaToken));

								// check for exit case after legal trailing comma
								if (this.CurrentToken.Kind == SyntaxKind.CloseBraceToken)
								{
									break;
								}
								else if (!this.IsPossibleVariableInitializer(false))
								{
									goto tryAgain;
								}

								list.Add(this.ParseVariableInitializer(false));
								continue;
							}
							else if (SkipBadArrayInitializerTokens(ref openBrace, list, SyntaxKind.CommaToken) == PostSkipAction.Abort)
							{
								break;
							}
						}
					}
					else if (SkipBadArrayInitializerTokens(ref openBrace, list, SyntaxKind.CommaToken) == PostSkipAction.Continue)
					{
						goto tryAgain;
					}
				}

				var closeBrace = this.EatToken(SyntaxKind.CloseBraceToken);

				return _syntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression, openBrace, list, closeBrace);
			}
			finally
			{
				this._pool.Free(list);
			}
		}

		#region LambdaExpression

		private ExpressionSyntax ParseLambdaExpression()
		{
			//bool parentScopeIsInAsync = IsInAsync;

			ExpressionSyntax result;
			if (this.CurrentToken.Kind == SyntaxKind.OpenParenToken)
			{
				var paramList = this.ParseLambdaParameterList();
				var arrow = this.EatToken(SyntaxKind.MinusGreaterThanToken);
				arrow = CheckFeatureAvailability(arrow, MessageID.IDS_FeatureLambda);
				CSharpSyntaxNode body;
				if (this.CurrentToken.Kind == SyntaxKind.OpenBraceToken)
				{
					body = this.ParseBlock();
				}
				else
				{
					body = this.ParseExpression();
				}

				result = _syntaxFactory.ParenthesizedLambdaExpression(paramList, arrow, body);
			}
			else
			{
				var name = this.ParseIdentifierToken();
				var arrow = this.EatToken(SyntaxKind.MinusGreaterThanToken);
				arrow = CheckFeatureAvailability(arrow, MessageID.IDS_FeatureLambda);
				CSharpSyntaxNode body = null;
				if (this.CurrentToken.Kind == SyntaxKind.OpenBraceToken)
				{
					body = this.ParseBlock();
				}
				else
				{
					body = this.ParseExpression();
				}

				result = _syntaxFactory.SimpleLambdaExpression(
					_syntaxFactory.Parameter(default(SyntaxList<AnnotationSyntax>), default(SyntaxList<SyntaxToken>), type: null,dotDotDotToken:null, identifier: name, @default: null),
					arrow,
					body);
			}

			//IsInAsync = parentScopeIsInAsync;
			return result;
		}

		private ParameterListSyntax ParseLambdaParameterList()
		{
			var openParen = this.EatToken(SyntaxKind.OpenParenToken);
			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsEndOfParameterList;

			var nodes = this._pool.AllocateSeparated<ParameterSyntax>();
			try
			{
				bool hasTypes = false;

				if (this.CurrentToken.Kind != SyntaxKind.CloseParenToken)
				{
				tryAgain:
					if (this.IsPossibleLambdaParameter() || this.CurrentToken.Kind == SyntaxKind.CommaToken)
					{
						// first parameter
						var parameter = this.ParseLambdaParameter(isFirst: true, hasTypes: ref hasTypes);
						nodes.Add(parameter);

						// additional parameters
						while (true)
						{
							if (this.CurrentToken.Kind == SyntaxKind.CloseParenToken)
							{
								break;
							}
							else if (this.CurrentToken.Kind == SyntaxKind.CommaToken || this.IsPossibleLambdaParameter())
							{
								nodes.AddSeparator(this.EatToken(SyntaxKind.CommaToken));
								parameter = this.ParseLambdaParameter(false, ref hasTypes);
								nodes.Add(parameter);
								continue;
							}
							else if (this.SkipBadLambdaParameterListTokens(ref openParen, nodes, SyntaxKind.CommaToken, SyntaxKind.CloseParenToken) == PostSkipAction.Abort)
							{
								break;
							}
						}
					}
					else if (this.SkipBadLambdaParameterListTokens(ref openParen, nodes, SyntaxKind.IdentifierToken, SyntaxKind.CloseParenToken) == PostSkipAction.Continue)
					{
						goto tryAgain;
					}
				}

				this._termState = saveTerm;
				var closeParen = this.EatToken(SyntaxKind.CloseParenToken);

				return _syntaxFactory.ParameterList(openParen, nodes, closeParen);
			}
			finally
			{
				this._pool.Free(nodes);
			}
		}

		private ParameterSyntax ParseLambdaParameter(bool isFirst, ref bool hasTypes)
		{
			TypeSyntax paramType = null;
			SyntaxToken paramName = null;
			SyntaxToken refOrOutOrParams = null;

			// Params are actually illegal in a lambda, but we'll allow it for error recovery purposes and
			// give the "params unexpected" error at semantic analysis time.
			var pk = this.PeekToken(1).Kind;
			if ((pk != SyntaxKind.CommaToken && pk != SyntaxKind.CloseParenToken && (hasTypes || isFirst))
				|| IsPredefinedType(this.CurrentToken.Kind))
			{

				paramType = this.ParseType(true);
			}

			paramName = this.ParseIdentifierToken();

			if (isFirst)
			{
				hasTypes = paramType != null;
			}
			else if (paramType != null && !hasTypes && !paramName.IsMissing)
			{
				paramType = this.AddError(paramType, ErrorCode.ERR_InconsistentLambdaParameterUsage);
			}
			else if (paramType == null && hasTypes && !paramName.IsMissing)
			{
				paramName = this.AddError(paramName, ErrorCode.ERR_InconsistentLambdaParameterUsage);
			}

			return _syntaxFactory.Parameter(default(SyntaxList<AnnotationSyntax>), refOrOutOrParams, paramType, null,paramName, null);
		}
		#endregion

		private ExpressionSyntax ParseVariableInitializer(bool allowStackAlloc)
		{
			switch (this.CurrentToken.Kind)
			{
				case SyntaxKind.OpenBraceToken:
					return this.ParseArrayInitializer();
				default:
					return this.ParseElementInitializer();
			}
		}

		private AnonymousObjectCreationExpressionSyntax ParseAnonymousTypeExpression()
		{
			Debug.Assert(IsAnonymousType());
			var @new = this.EatToken(SyntaxKind.NewKeyword);
			@new = CheckFeatureAvailability(@new, MessageID.IDS_FeatureAnonymousTypes);

			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.OpenBraceToken);

			var openBrace = this.EatToken(SyntaxKind.OpenBraceToken);
			var expressions = this._pool.AllocateSeparated<AnonymousObjectMemberDeclaratorSyntax>();
			this.ParseAnonymousTypeMemberInitializers(ref openBrace, ref expressions);
			var closeBrace = this.EatToken(SyntaxKind.CloseBraceToken);
			var result = _syntaxFactory.AnonymousObjectCreationExpression(@new, openBrace, expressions, closeBrace);
			this._pool.Free(expressions);

			return result;
		}

		private void ParseAnonymousTypeMemberInitializers(ref SyntaxToken openBrace, ref SeparatedSyntaxListBuilder<AnonymousObjectMemberDeclaratorSyntax> list)
		{
			if (this.CurrentToken.Kind != SyntaxKind.CloseBraceToken)
			{
			tryAgain:
				if (this.IsPossibleExpression() || this.CurrentToken.Kind == SyntaxKind.CommaToken)
				{
					// first argument
					list.Add(this.ParseAnonymousTypeMemberInitializer());

					// additional arguments
					while (true)
					{
						if (this.CurrentToken.Kind == SyntaxKind.CloseBraceToken)
						{
							break;
						}
						else if (this.CurrentToken.Kind == SyntaxKind.CommaToken || this.IsPossibleExpression())
						{
							list.AddSeparator(this.EatToken(SyntaxKind.CommaToken));

							// check for exit case after legal trailing comma
							if (this.CurrentToken.Kind == SyntaxKind.CloseBraceToken)
							{
								break;
							}
							else if (!this.IsPossibleExpression())
							{
								goto tryAgain;
							}

							list.Add(this.ParseAnonymousTypeMemberInitializer());
							continue;
						}
						else if (this.SkipBadInitializerListTokens(ref openBrace, list, SyntaxKind.CommaToken) == PostSkipAction.Abort)
						{
							break;
						}
					}
				}
				else if (this.SkipBadInitializerListTokens(ref openBrace, list, SyntaxKind.IdentifierToken) == PostSkipAction.Continue)
				{
					goto tryAgain;
				}
			}
		}

		private AnonymousObjectMemberDeclaratorSyntax ParseAnonymousTypeMemberInitializer()
		{
			bool isNamedAssignment = this.IsNamedAssignment();

			NameEqualsSyntax nameEquals = null;
			if (isNamedAssignment)
			{
				nameEquals = ParseNameEquals();
			}

			var expression = this.ParseExpression();
			if (!isNamedAssignment && !IsAnonymousTypeMemberExpression(expression))
			{
				expression = this.AddError(expression, ErrorCode.ERR_InvalidAnonymousTypeMemberDeclarator);
			}

			return _syntaxFactory.AnonymousObjectMemberDeclarator(nameEquals, expression);
		}

		private DefaultExpressionSyntax ParseDefaultExpression()
		{
			var keyword = this.EatToken();
			var openParen = this.EatToken(SyntaxKind.OpenParenToken);
			var type = this.ParseType(false);
			var closeParen = this.EatToken(SyntaxKind.CloseParenToken);

			keyword = CheckFeatureAvailability(keyword, MessageID.IDS_FeatureDefault);

			return _syntaxFactory.DefaultExpression(keyword, openParen, type, closeParen);
		}

	}
}