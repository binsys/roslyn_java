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

		private TypeParameterConstraintSyntax ParseTypeParameterConstraint(bool isFirst, ref bool isStruct)
		{
			switch (this.CurrentToken.Kind)
			{
				//case SyntaxKind.NewKeyword:
				//	var newToken = this.EatToken();
				//	if (isStruct)
				//	{
				//		newToken = this.AddError(newToken, ErrorCode.ERR_NewBoundWithVal);
				//	}

				//	var open = this.EatToken(SyntaxKind.OpenParenToken);
				//	var close = this.EatToken(SyntaxKind.CloseParenToken);
				//	if (this.CurrentToken.Kind == SyntaxKind.CommaToken)
				//	{
				//		newToken = this.AddError(newToken, ErrorCode.ERR_NewBoundMustBeLast);
				//	}

				//	return _syntaxFactory.ConstructorConstraint(newToken, open, close);
				//case SyntaxKind.ClassKeyword:
				//	var token = this.EatToken();
				//	if (!isFirst)
				//	{
				//		token = this.AddError(token, ErrorCode.ERR_RefValBoundMustBeFirst);
				//	}

				//	return _syntaxFactory.ClassOrStructConstraint(isStruct ? SyntaxKind.StructConstraint : SyntaxKind.ClassConstraint, token);
				default:
					var type = this.ParseDeclarationType(true, false);
					return _syntaxFactory.TypeConstraint(type);
			}
		}

		private bool IsPossibleOnDemandImport()
		{
			//skip @interface define
			if (this.CurrentToken.Kind == SyntaxKind.AtToken && this.PeekToken(1).Kind == SyntaxKind.InterfaceKeyword)
			{
				return false;
			}

			var resetPoint = this.GetResetPoint();
			try
			{
				TypeSyntax type = this.ParseType(false);

				if (this.CurrentToken.Kind == SyntaxKind.DotToken && this.PeekToken(1).Kind == SyntaxKind.AsteriskToken)
				{
					return true;
				}
			}
			finally
			{
				this.Reset(ref resetPoint);
				this.Release(ref resetPoint);
			}

			return false;
		}





		private TypeSyntax ParseDeclarationType(bool isConstraint, bool parentIsParameter)
		{
			var type = this.ParseType(parentIsParameter);
			if (type.Kind != SyntaxKind.PredefinedType && !SyntaxKindFacts.IsName(type.Kind))
			{
				if (isConstraint)
				{
					type = this.AddError(type, ErrorCode.ERR_BadConstraintType);
				}
				else
				{
					type = this.AddError(type, ErrorCode.ERR_BadBaseType);
				}
			}

			return type;
		}


		private ArrayRankSpecifierSyntax ParseArrayRankSpecifier(bool isArrayCreation, bool expectSizes, out bool sawNonOmittedSize)
		{
			sawNonOmittedSize = false;
			bool sawOmittedSize = false;
			var open = this.EatToken(SyntaxKind.OpenBracketToken);
			var list = this._pool.AllocateSeparated<ExpressionSyntax>();
			try
			{
				var omittedArraySizeExpressionInstance = _syntaxFactory.OmittedArraySizeExpression(SyntaxFactory.Token(SyntaxKind.OmittedArraySizeExpressionToken));
				while (this.CurrentToken.Kind != SyntaxKind.CloseBracketToken)
				{
					if (this.CurrentToken.Kind == SyntaxKind.CommaToken)
					{
						// NOTE: trivia will be attached to comma, not omitted array size
						sawOmittedSize = true;
						list.Add(omittedArraySizeExpressionInstance);
						list.AddSeparator(this.EatToken());
					}
					else if (this.IsPossibleExpression())
					{
						var size = this.ParseExpression();
						sawNonOmittedSize = true;
						if (!expectSizes)
						{
							size = this.AddError(size, isArrayCreation ? ErrorCode.ERR_InvalidArray : ErrorCode.ERR_ArraySizeInDeclaration);
						}

						list.Add(size);

						if (this.CurrentToken.Kind != SyntaxKind.CloseBracketToken)
						{
							list.AddSeparator(this.EatToken(SyntaxKind.CommaToken));
						}
					}
					else if (this.SkipBadArrayRankSpecifierTokens(ref open, list, SyntaxKind.CommaToken) == PostSkipAction.Abort)
					{
						break;
					}
				}

				// Don't end on a comma.
				// If the omitted size would be the only element, then skip it unless sizes were expected.
				if (((list.Count & 1) == 0))
				{
					sawOmittedSize = true;
					list.Add(omittedArraySizeExpressionInstance);
				}

				// Never mix omitted and non-omitted array sizes.  If there were non-omitted array sizes,
				// then convert all of the omitted array sizes to missing identifiers.
				if (sawOmittedSize && sawNonOmittedSize)
				{
					for (int i = 0; i < list.Count; i++)
					{
						if (list[i].Kind == SyntaxKind.OmittedArraySizeExpression)
						{
							int width = list[i].Width;
							int offset = list[i].GetLeadingTriviaWidth();
							list[i] = this.AddError(this.CreateMissingIdentifierName(), offset, width, ErrorCode.ERR_ValueExpected);
						}
					}
				}

				// Eat the close brace and we're done.
				var close = this.EatToken(SyntaxKind.CloseBracketToken);

				return _syntaxFactory.ArrayRankSpecifier(open, list, close);
			}
			finally
			{
				this._pool.Free(list);
			}
		}

		// If "isMethodBody" is true, then this is the immediate body of a method/accessor.
		// In this case, we create a many-child list if the body is not a small single statement.
		// This then allows a "with many weak children" red node when the red node is created.
		// If "isAccessorBody" is true, then we produce a special diagnostic if the open brace is
		// missing.  Also, "isMethodBody" must be true.
		private BlockSyntax ParseBlock(bool isMethodBody = false, bool isAccessorBody = false)
		{
			// This makes logical sense, but isn't actually required.
			Debug.Assert(!isAccessorBody || isMethodBody, "An accessor body is a method body.");

			// Check again for incremental re-use, since ParseBlock is called from a bunch of places
			// other than ParseStatement()
			if (this.IsIncrementalAndFactoryContextMatches && this.CurrentNodeKind == SyntaxKind.Block)
			{
				return (BlockSyntax)this.EatNode();
			}

			// There's a special error code for a missing token after an accessor keyword
			var openBrace = isAccessorBody && this.CurrentToken.Kind != SyntaxKind.OpenBraceToken
				? this.AddError(SyntaxFactory.MissingToken(SyntaxKind.OpenBraceToken), ErrorCode.ERR_SemiOrLBraceExpected)
				: this.EatToken(SyntaxKind.OpenBraceToken);

			var statements = this._pool.Allocate<StatementSyntax>();
			try
			{
				CSharpSyntaxNode tmp = openBrace;
				this.ParseStatements(ref tmp, statements, stopOnSwitchSections: false);
				openBrace = (SyntaxToken)tmp;
				var closeBrace = this.EatToken(SyntaxKind.CloseBraceToken);

				SyntaxList<StatementSyntax> statementList;
				if (isMethodBody && IsLargeEnoughNonEmptyStatementList(statements))
				{
					// Force creation a many-children list, even if only 1, 2, or 3 elements in the statement list.
					statementList = new SyntaxList<StatementSyntax>(SyntaxList.List(((SyntaxListBuilder)statements).ToArray()));
				}
				else
				{
					statementList = statements;
				}

				return _syntaxFactory.Block(openBrace, statementList, closeBrace);
			}
			finally
			{
				this._pool.Free(statements);
			}
		}

		private ConstructorDeclarationSyntax ParseConstructorDeclaration(string typeName, SyntaxListBuilder<AnnotationSyntax> attributes, SyntaxListBuilder modifiers)
		{
			var name = this.ParseIdentifierToken();
			Debug.Assert(name.ValueText == typeName);

			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsEndOfMethodSignature;
			try
			{
				var paramList = this.ParseParenthesizedParameterList(allowThisKeyword: false, allowDefaults: true, allowAttributes: true, allowFieldModifiers: false);

				ConstructorInitializerSyntax initializer = null;
				if (this.CurrentToken.Kind == SyntaxKind.ColonToken)
				{
					bool isStatic = modifiers != null && modifiers.Any(SyntaxKind.StaticKeyword);
					initializer = this.ParseConstructorInitializer(name.ValueText, isStatic);
				}

				BlockSyntax body;
				SyntaxToken semicolon;
				this.ParseBodyOrSemicolon(out body, out semicolon);

				return _syntaxFactory.ConstructorDeclaration(attributes, modifiers.ToTokenList(), name, paramList, initializer, body, semicolon);
			}
			finally
			{
				this._termState = saveTerm;
			}
		}

		private ConstructorInitializerSyntax ParseConstructorInitializer(string name, bool isStatic)
		{
			var colon = this.EatToken(SyntaxKind.ColonToken);

			var reportError = true;
			var kind = this.CurrentToken.Kind == SyntaxKind.SuperKeyword
				? SyntaxKind.BaseConstructorInitializer
				: SyntaxKind.ThisConstructorInitializer;

			SyntaxToken token;
			if (this.CurrentToken.Kind == SyntaxKind.SuperKeyword || this.CurrentToken.Kind == SyntaxKind.ThisKeyword)
			{
				token = this.EatToken();
			}
			else
			{
				token = this.EatToken(SyntaxKind.ThisKeyword, ErrorCode.ERR_ThisOrBaseExpected);

				// No need to report further errors at this point:
				reportError = false;
			}

			ArgumentListSyntax argumentList;
			if (this.CurrentToken.Kind == SyntaxKind.OpenParenToken)
			{
				argumentList = this.ParseParenthesizedArgumentList();
			}
			else
			{
				var openToken = this.EatToken(SyntaxKind.OpenParenToken, reportError);
				var closeToken = this.EatToken(SyntaxKind.CloseParenToken, reportError);
				argumentList = _syntaxFactory.ArgumentList(openToken, default(SeparatedSyntaxList<ArgumentSyntax>), closeToken);
			}

			if (isStatic)
			{
				// Static constructor can't have any base call
				token = this.AddError(token, ErrorCode.ERR_StaticConstructorWithExplicitConstructorCall, name);
			}

			return _syntaxFactory.ConstructorInitializer(kind, colon, token, argumentList);
		}

		private DestructorDeclarationSyntax ParseDestructorDeclaration(string typeName, SyntaxListBuilder<AnnotationSyntax> attributes, SyntaxListBuilder modifiers)
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.TildeToken);
			var tilde = this.EatToken(SyntaxKind.TildeToken);

			var name = this.ParseIdentifierToken();
			if (name.ValueText != typeName)
			{
				name = this.AddError(name, ErrorCode.ERR_BadDestructorName);
			}

			var openParen = this.EatToken(SyntaxKind.OpenParenToken);
			var closeParen = this.EatToken(SyntaxKind.CloseParenToken);

			BlockSyntax body;
			SyntaxToken semicolon;
			this.ParseBodyOrSemicolon(out body, out semicolon);

			var parameterList = _syntaxFactory.ParameterList(openParen, default(SeparatedSyntaxList<ParameterSyntax>), closeParen);
			return _syntaxFactory.DestructorDeclaration(attributes, modifiers.ToTokenList(), tilde, name, parameterList, body, semicolon);
		}

		private void ParseBodyOrSemicolon(out BlockSyntax body, out SyntaxToken semicolon)
		{
			if (this.CurrentToken.Kind == SyntaxKind.OpenBraceToken)
			{
				body = this.ParseBlock(isMethodBody: true);

				semicolon = null;
				if (this.CurrentToken.Kind == SyntaxKind.SemicolonToken)
				{
					semicolon = this.EatTokenWithPrejudice(ErrorCode.ERR_UnexpectedSemicolon);
				}
			}
			else
			{
				semicolon = this.EatToken(SyntaxKind.SemicolonToken);
				body = null;
			}
		}



		private NameEqualsSyntax ParseNameEquals(bool warnOnGlobal = false)
		{
			Debug.Assert(this.IsNamedAssignment());

			var id = this.ParseIdentifierToken();
			var equals = this.EatToken(SyntaxKind.EqualsToken);


			return _syntaxFactory.NameEquals(_syntaxFactory.IdentifierName(id), equals);
		}


		private VariableDeclaratorSyntax ParseVariableDeclarator(TypeSyntax parentType, VariableFlags flags, bool isFirst, bool isExpressionContext = false)
		{
			if (this.IsIncrementalAndFactoryContextMatches && CanReuseVariableDeclarator(this.CurrentNode as CSharp.Syntax.VariableDeclaratorSyntax, flags, isFirst))
			{
				return (VariableDeclaratorSyntax)this.EatNode();
			}

			if (!isExpressionContext)
			{
				// Check for the common pattern of:
				//
				// C                    //<-- here
				// Console.WriteLine();
				//
				// Standard greedy parsing will assume that this should be parsed as a variable
				// declaration: "C Console".  We want to avoid that as it can confused parts of the
				// system further up.  So, if we see certain things following the identifier, then we can
				// assume it's not the actual name.  
				// 
				// So, if we're after a newline and we see a name followed by the list below, then we
				// assume that we're accidently consuming too far into the next statement.
				//
				// <dot>, <arrow>, any binary operator (except =), <question>.  None of these characters
				// are allowed in a normal variable declaration.  This also provides a more useful error
				// message to the user.  Instead of telling them that a semicolon is expected after the
				// following token, then instead get a useful message about an identifier being missing.
				// The above list prevents:
				//
				// C                    //<-- here
				// Console.WriteLine();
				//
				// C                    //<-- here 
				// Console->WriteLine();
				//
				// C 
				// A + B; // etc.
				//
				// C 
				// A ? B : D;
				var resetPoint = this.GetResetPoint();
				try
				{
					var currentTokenKind = this.CurrentToken.Kind;
					if (currentTokenKind == SyntaxKind.IdentifierToken && !parentType.IsMissing)
					{
						var isAfterNewLine = parentType.GetLastToken().TrailingTrivia.Any(SyntaxKind.EndOfLineTrivia);
						if (isAfterNewLine)
						{
							int offset, width;
							this.GetDiagnosticSpanForMissingToken(out offset, out width);

							this.EatToken();
							currentTokenKind = this.CurrentToken.Kind;

							var isNonEqualsBinaryToken =
								currentTokenKind != SyntaxKind.EqualsToken &&
								SyntaxKindFacts.IsBinaryExpressionOperatorToken(currentTokenKind);

							if (currentTokenKind == SyntaxKind.DotToken ||
								currentTokenKind == SyntaxKind.MinusGreaterThanToken ||
								isNonEqualsBinaryToken)
							{
								var missingIdentifier = CreateMissingIdentifierToken();
								missingIdentifier = this.AddError(missingIdentifier, offset, width, ErrorCode.ERR_IdentifierExpected);

								return _syntaxFactory.VariableDeclarator(missingIdentifier, null, null);
							}
						}
					}
				}
				finally
				{
					this.Reset(ref resetPoint);
					this.Release(ref resetPoint);
				}
			}

			// NOTE: Diverges from Dev10.
			//
			// When we see parse an identifier and we see the partial contextual keyword, we check
			// to see whether it is already attached to a partial class or partial method
			// declaration.  However, in the specific case of variable declarators, Dev10
			// specifically treats it as a variable name, even if it could be interpreted as a
			// keyword.
			var name = this.ParseIdentifierToken();
			BracketedArgumentListSyntax argumentList = null;
			EqualsValueClauseSyntax initializer = null;
			TerminatorState saveTerm = this._termState;
			bool isFixed = (flags & VariableFlags.Fixed) != 0;
			bool isConst = (flags & VariableFlags.Const) != 0;
			bool isLocal = (flags & VariableFlags.Local) != 0;

			// Give better error message in the case where the user did something like:
			//
			// X x = 1, Y y = 2; 
			// using (X x = expr1, Y y = expr2) ...
			//
			// The superfluous type name is treated as variable (it is an identifier) and a missing ',' is injected after it.
			if (!isFirst && this.IsTrueIdentifier())
			{
				name = this.AddError(name, ErrorCode.ERR_MultiTypeInDeclaration);
			}

			switch (this.CurrentToken.Kind)
			{
				case SyntaxKind.EqualsToken:
					if (isFixed)
					{
						goto default;
					}

					var equals = this.EatToken();
					var init = this.ParseVariableInitializer(isLocal && !isConst);
					initializer = _syntaxFactory.EqualsValueClause(equals, init);
					break;

				case SyntaxKind.OpenParenToken:
					// Special case for accidental use of C-style constructors
					// Fake up something to hold the arguments.
					this._termState |= TerminatorState.IsPossibleEndOfVariableDeclaration;
					argumentList = this.ParseBracketedArgumentList();
					this._termState = saveTerm;
					argumentList = this.AddError(argumentList, ErrorCode.ERR_BadVarDecl);
					break;

				case SyntaxKind.OpenBracketToken:
					bool sawNonOmittedSize;
					this._termState |= TerminatorState.IsPossibleEndOfVariableDeclaration;
					var specifier = this.ParseArrayRankSpecifier(isArrayCreation: false, expectSizes: flags == VariableFlags.Fixed, sawNonOmittedSize: out sawNonOmittedSize);
					this._termState = saveTerm;
					var open = specifier.OpenBracketToken;
					var sizes = specifier.Sizes;
					var close = specifier.CloseBracketToken;
					if (isFixed && !sawNonOmittedSize)
					{
						close = this.AddError(close, ErrorCode.ERR_ValueExpected);
					}

					var args = this._pool.AllocateSeparated<ArgumentSyntax>();
					try
					{
						var withSeps = sizes.GetWithSeparators();
						foreach (var item in withSeps)
						{
							var expression = item as ExpressionSyntax;
							if (expression != null)
							{
								args.Add(_syntaxFactory.Argument(null, expression));
							}
							else
							{
								args.AddSeparator((SyntaxToken)item);
							}
						}

						argumentList = _syntaxFactory.BracketedArgumentList(open, args, close);
						if (!isFixed)
						{
							argumentList = this.AddError(argumentList, ErrorCode.ERR_CStyleArray);
							// If we have "int x[] = new int[10];" then parse the initializer.
							if (this.CurrentToken.Kind == SyntaxKind.EqualsToken)
							{
								goto case SyntaxKind.EqualsToken;
							}
						}
					}
					finally
					{
						this._pool.Free(args);
					}

					break;

				default:
					if (isConst)
					{
						name = this.AddError(name, ErrorCode.ERR_ConstValueRequired);  // Error here for missing constant initializers
					}
					else if (isFixed)
					{
						if (parentType.Kind == SyntaxKind.ArrayType)
						{
							// They accidentally put the array before the identifier
							name = this.AddError(name, ErrorCode.ERR_FixedDimsRequired);
						}
						else
						{
							goto case SyntaxKind.OpenBracketToken;
						}
					}

					break;
			}

			return _syntaxFactory.VariableDeclarator(name, argumentList, initializer);
		}



		// This is public and parses open types. You probably don't want to use it.
		public NameSyntax ParseName()
		{
			return this.ParseQualifiedName();
		}


		private IdentifierNameSyntax ParseIdentifierName()
		{
			if (this.IsIncrementalAndFactoryContextMatches && this.CurrentNodeKind == SyntaxKind.IdentifierName)
			{
				if (!SyntaxKindFacts.IsContextualKeyword(((CSharp.Syntax.IdentifierNameSyntax)this.CurrentNode).Identifier.CSharpKind()))
				{
					return (IdentifierNameSyntax)this.EatNode();
				}
			}

			var tk = ParseIdentifierToken();
			return SyntaxFactory.IdentifierName(tk);
		}

		private SyntaxToken ParseIdentifierToken()
		{
			var ctk = this.CurrentToken.Kind;
			if (ctk == SyntaxKind.IdentifierToken)
			{
				// Error tolerance for IntelliSense. Consider the following case: [EditorBrowsable( partial class Foo {
				// } Because we're parsing an _annotation argument we'll end up consuming the "partial" identifier and
				// we'll eventually end up in an pretty confused state.  Because of that it becomes very difficult to
				// show the correct parameter help in this case.  So, when we see "partial" we check if it's being used
				// as an identifier or as a contextual keyword.  If it's the latter then we bail out.  See
				// Bug: vswhidbey/542125


				SyntaxToken identifierToken = this.EatToken();


				return identifierToken;
			}
			else
			{
				var name = CreateMissingIdentifierToken();
				name = this.AddError(name, ErrorCode.ERR_IdentifierExpected);
				return name;
			}
		}



	}
}