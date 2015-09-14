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



		// If "isMethodBody" is true, then this is the immediate body of a method/accessor.
		// In this case, we create a many-child list if the body is not a small single statement.
		// This then allows a "with many weak children" red node when the red node is created.
		// If "isAccessorBody" is true, then we produce a special diagnostic if the open brace is
		// missing.  Also, "isMethodBody" must be true.
		private BlockSyntax ParseBlock(bool isMethodBody = false)
		{
			// Check again for incremental re-use, since ParseBlock is called from a bunch of places
			// other than ParseStatement()
			if (this.IsIncrementalAndFactoryContextMatches && this.CurrentNodeKind == SyntaxKind.Block)
			{
				return (BlockSyntax)this.EatNode();
			}

			// There's a special error code for a missing token after an accessor keyword
			var openBrace = this.EatToken(SyntaxKind.OpenBraceToken);

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


		private void ParseStatements(ref CSharpSyntaxNode previousNode, SyntaxListBuilder<StatementSyntax> statements, bool stopOnSwitchSections)
		{
			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsPossibleStatementStartOrStop; // partial statements can abort if a new statement starts
			if (stopOnSwitchSections)
			{
				this._termState |= TerminatorState.IsSwitchSectionStart;
			}

			while (this.CurrentToken.Kind != SyntaxKind.CloseBraceToken
				&& this.CurrentToken.Kind != SyntaxKind.EndOfFileToken
				&& !(stopOnSwitchSections && this.IsPossibleSwitchSection()))
			{
				if (this.IsPossibleStatement())
				{
					var statement = this.ParseStatement();
					statements.Add(statement);
				}
				else
				{
					CSharpSyntaxNode trailingTrivia;
					var action = this.SkipBadStatementListTokens(statements, SyntaxKind.CloseBraceToken, out trailingTrivia);
					if (trailingTrivia != null)
					{
						previousNode = AddTrailingSkippedSyntax(previousNode, trailingTrivia);
					}
					if (action == PostSkipAction.Abort)
					{
						break;
					}
				}
			}

			this._termState = saveTerm;
		}



		public StatementSyntax ParseStatement()
		{
			if (this.IsIncrementalAndFactoryContextMatches && this.CurrentNode is CSharp.Syntax.StatementSyntax)
			{
				return (StatementSyntax)this.EatNode();
			}

			// First, try to parse as a non-declaration statement. 
			// If the statement is a single expression then we only allow legal expression statements. 
			// (That is, "new C();",  "C();", "x = y;" and so on.)

			StatementSyntax result = ParseStatementNoDeclaration(allowAnyExpression: false);
			if (result != null)
			{
				return result;
			}

			// We could not successfully parse the statement as a non-declaration. Try to parse
			// it as either a declaration or as an "await X();" statement that is in a non-async
			// method. 

			return ParsePossibleBadAwaitStatement();
		}

		/// <summary>
		/// Parses any statement but a declaration statement. Returns null if the lookahead looks like a declaration.
		/// </summary>
		/// <remarks>
		/// Variable declarations in global code are parsed as field declarations so we need to fallback if we encounter a declaration statement.
		/// </remarks>
		private StatementSyntax ParseStatementNoDeclaration(bool allowAnyExpression)
		{
			switch (this.CurrentToken.Kind)
			{
				case SyntaxKind.BreakKeyword:
					return this.ParseBreakStatement();
				case SyntaxKind.ContinueKeyword:
					return this.ParseContinueStatement();
				case SyntaxKind.TryKeyword:
				case SyntaxKind.CatchKeyword:
				case SyntaxKind.FinallyKeyword:
					return this.ParseTryStatement();
				case SyntaxKind.ConstKeyword:
					return null;
				case SyntaxKind.DoKeyword:
					return this.ParseDoStatement();
				case SyntaxKind.ForKeyword:
					return this.ParseForOrForEachStatement();
				case SyntaxKind.IfKeyword:
					return this.ParseIfStatement();
				case SyntaxKind.ReturnKeyword:
					return this.ParseReturnStatement();
				case SyntaxKind.SwitchKeyword:
					return this.ParseSwitchStatement();
				case SyntaxKind.ThrowKeyword:
					return this.ParseThrowStatement();
				case SyntaxKind.ImportKeyword:
					return this.ParseUsingStatement();
				case SyntaxKind.SynchronizedKeyword:
					return this.ParseJavaSynchronizedStatement();
				case SyntaxKind.WhileKeyword:
					return this.ParseWhileStatement();
				case SyntaxKind.OpenBraceToken:
					return this.ParseBlock();
				case SyntaxKind.AssertKeyword:
					return this.ParseJavaAssertStatement();
				case SyntaxKind.SemicolonToken:
					return _syntaxFactory.EmptyStatement(this.EatToken());
				case SyntaxKind.IdentifierToken:
					if (this.IsPossibleLabeledStatement())
					{
						return this.ParseLabeledStatement();
					}
					else
					{
						goto default;
					}

				default:
					if (this.IsPossibleLocalDeclarationStatement(allowAnyExpression))
					{
						return null;
					}
					else
					{
						return this.ParseExpressionStatement();
					}
			}
		}


		private StatementSyntax ParsePossibleBadAwaitStatement()
		{
			ResetPoint resetPointBeforeStatement = this.GetResetPoint();

			//StatementSyntax result = ParsePossibleBadAwaitStatement(ref resetPointBeforeStatement);
			// Precondition: We have already attempted to parse the statement as a non-declaration and failed.
			//
			// That means that we are in one of the following cases:
			//
			// 1) This is a perfectly mundane and correct local declaration statement like "int x;"
			// 2) This is a perfectly mundane but erroneous local declaration statement, like "int X();"
			// 3) We are in the rare case of the code containing "await x;" and the intention is that
			//    "await" is the type of "x".  This only works in a non-async method.
			// 4) We have what would be a legal await statement, like "await X();", but we are not in
			//    an async method, so the parse failed. (Had we been in an async method then the parse
			//    attempt done by our caller would have succeeded.)
			// 5) The statement begins with "await" but is not a legal local declaration and not a legal
			//    await expression regardless of whether the method is marked as "async".

			StatementSyntax result = ParseLocalDeclarationStatement();

			this.Release(ref resetPointBeforeStatement);
			return result;
		}


		private StatementSyntax ParseEmbeddedStatement(bool complexCheck)
		{
			StatementSyntax statement;

			if (this.CurrentToken.Kind == SyntaxKind.SemicolonToken && (!complexCheck || this.PeekToken(1).Kind == SyntaxKind.OpenBraceToken))
			{
				statement = this.ParseStatement();
				statement = this.AddError(statement, ErrorCode.WRN_PossibleMistakenNullStatement);
			}
			else
			{
				statement = this.ParseStatement();
			}

			// An "embedded" statement is simply a statement that is not a labelled
			// statement or a declaration statement.  Parse a normal statement and post-
			// check for the error case.
			if (statement != null && (statement.Kind == SyntaxKind.LabeledStatement || statement.Kind == SyntaxKind.LocalDeclarationStatement))
			{
				statement = this.AddError(statement, ErrorCode.ERR_BadEmbeddedStmt);
			}

			return statement;
		}

		private TypeSyntax ParseClassType()
		{
			var type = this.ParseType(false);
			if (!SyntaxKindFacts.IsName(type.Kind))
			{
				type = this.AddError(type, ErrorCode.ERR_ClassTypeExpected);
			}
			return type;
		}

		private LabeledStatementSyntax ParseLabeledStatement()
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.IdentifierToken);

			// We have an identifier followed by a colon. But if the identifier is a contextual keyword in a query context,
			// ParseIdentifier will result in a missing name and Eat(Colon) will fail. We won't make forward progress.
			Debug.Assert(this.IsTrueIdentifier());

			var label = this.ParseIdentifierToken();
			var colon = this.EatToken(SyntaxKind.ColonToken);
			Debug.Assert(!colon.IsMissing);
			var statement = this.ParseStatement();
			return _syntaxFactory.LabeledStatement(label, colon, statement);
		}



		private ExpressionStatementSyntax ParseExpressionStatement()
		{
			return ParseExpressionStatement(this.ParseExpression(allowDeclarationExpressionAtTheBeginning: false));
		}

		private ExpressionStatementSyntax ParseExpressionStatement(ExpressionSyntax expression)
		{
			SyntaxToken semicolon;

			// Do not report an error if the expression is not a statement expression.
			// The error is reported in semantic analysis.
			semicolon = this.EatToken(SyntaxKind.SemicolonToken);
			return _syntaxFactory.ExpressionStatement(expression, semicolon);
		}



		#region LocalDeclaration
		private LocalDeclarationStatementSyntax ParseLocalDeclarationStatement()
		{
			TypeSyntax type;
			var mods = this._pool.Allocate();
			var variables = this._pool.AllocateSeparated<VariableDeclaratorSyntax>();
			try
			{
				this.ParseDeclarationModifiers(mods);
				this.ParseDeclaration(mods.Any(SyntaxKind.FinalKeyword), out type, variables);
				var semicolon = this.EatToken(SyntaxKind.SemicolonToken);
				return _syntaxFactory.LocalDeclarationStatement(
					mods.ToTokenList(),
					_syntaxFactory.VariableDeclaration(type, variables),
					semicolon);
			}
			finally
			{
				this._pool.Free(variables);
				this._pool.Free(mods);
			}
		}


		private void ParseDeclarationModifiers(SyntaxListBuilder list)
		{
			SyntaxKind k;
			while (IsDeclarationModifier(k = this.CurrentToken.Kind))
			{
				var mod = this.EatToken();
				if (k == SyntaxKind.StaticKeyword || k == SyntaxKind.VolatileKeyword || k == SyntaxKind.ConstKeyword)
				{
					mod = this.AddError(mod, ErrorCode.ERR_BadMemberFlag, mod.Text);
				}

				list.Add(mod);
			}
		}



		private void ParseDeclaration(bool isFinal, out TypeSyntax type, SeparatedSyntaxListBuilder<VariableDeclaratorSyntax> variables)
		{
			type = this.ParseType(false);

			VariableFlags flags = VariableFlags.Local;
			if (isFinal)
			{
				flags |= VariableFlags.Final;
			}

			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsEndOfDeclarationClause;
			this.ParseVariableDeclarators(type, flags, variables, variableDeclarationsExpected: true);
			this._termState = saveTerm;
		}
		#endregion


		#region assert
		private JavaAssertStatementSyntax ParseJavaAssertStatement()
		{
			//assert  expression1;
			//assert  expression1:expression2;
			//expression1表示一个boolean表达式 如果它的值为false，该语句强抛出一个AssertionError对象
			//	如果assertion语句包括expression2参数，程序将计算出 expression2的结果，
			//	然后将这个结果作为AssertionError的构造函数的参数，来创建AssertionError对象，
			//	并抛出该对 象；如果expression1值为true，expression2将不被计算。 


			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.AssertKeyword);
			var assertKeyword = this.EatToken(SyntaxKind.AssertKeyword);

			SyntaxToken tk = default(SyntaxToken);
			ExpressionSyntax exp2 = default(ExpressionSyntax);
			var condition = this.ParseExpression();

			if (this.CurrentToken.Kind == SyntaxKind.ColonToken)
			{
				tk = this.EatToken(SyntaxKind.ColonToken);

				exp2 = this.ParseExpression();
			}

			return _syntaxFactory.JavaAssertStatement(assertKeyword, condition, tk, exp2,
				this.EatToken(SyntaxKind.SemicolonToken));

		}
		#endregion

		#region synchronized
		private JavaSynchronizedStatementSyntax ParseJavaSynchronizedStatement()
		{
			var synchronizedKeyword = this.EatToken(SyntaxKind.SynchronizedKeyword);
			var openParen = this.EatToken(SyntaxKind.OpenParenToken);
			ExpressionSyntax expression = this.ParseExpression();
			var closeParen = this.EatToken(SyntaxKind.CloseParenToken);
			var statement = this.ParseEmbeddedStatement(false);
			return _syntaxFactory.JavaSynchronizedStatement(synchronizedKeyword, openParen, expression, closeParen, statement);
		}
		#endregion

		#region break
		private BreakStatementSyntax ParseBreakStatement()
		{
			var breakKeyword = this.EatToken(SyntaxKind.BreakKeyword);
			var semicolon = this.EatToken(SyntaxKind.SemicolonToken);
			return _syntaxFactory.BreakStatement(breakKeyword, semicolon);
		}
		#endregion

		#region continue
		private ContinueStatementSyntax ParseContinueStatement()
		{
			var continueKeyword = this.EatToken(SyntaxKind.ContinueKeyword);
			var id = this.EatToken(SyntaxKind.IdentifierToken);
			var semicolon = this.EatToken(SyntaxKind.SemicolonToken);
			
			return _syntaxFactory.ContinueStatement(continueKeyword,id, semicolon);
		}
		#endregion

		#region try
		private TryStatementSyntax ParseTryStatement()
		{
			var isInTry = this._isInTry;
			this._isInTry = true;

			var @try = this.EatToken(SyntaxKind.TryKeyword);

			BlockSyntax block;
			if (@try.IsMissing)
			{
				block = _syntaxFactory.Block(this.EatToken(SyntaxKind.OpenBraceToken), default(SyntaxList<StatementSyntax>), this.EatToken(SyntaxKind.CloseBraceToken));
			}
			else
			{
				var saveTerm = this._termState;
				this._termState |= TerminatorState.IsEndOfTryBlock;
				block = this.ParseBlock();
				this._termState = saveTerm;
			}

			var catches = default(SyntaxListBuilder<CatchClauseSyntax>);
			FinallyClauseSyntax @finally = null;
			try
			{
				bool hasEnd = false;
				bool hasCatchAll = false;

				if (this.CurrentToken.Kind == SyntaxKind.CatchKeyword)
				{
					hasEnd = true;
					catches = this._pool.Allocate<CatchClauseSyntax>();
					while (this.CurrentToken.Kind == SyntaxKind.CatchKeyword)
					{
						var clause = this.ParseCatchClause(hasCatchAll);
						hasCatchAll |= clause.Declaration == null && clause.Filter == null;
						catches.Add(clause);
					}
				}

				if (this.CurrentToken.Kind == SyntaxKind.FinallyKeyword)
				{
					hasEnd = true;
					var fin = this.EatToken();
					var finBlock = this.ParseBlock();
					@finally = _syntaxFactory.FinallyClause(fin, finBlock);
				}

				if (!hasEnd)
				{
					block = this.AddErrorToLastToken(block, ErrorCode.ERR_ExpectedEndTry);

					// synthesize missing tokens for "finally { }":
					@finally = _syntaxFactory.FinallyClause(
						SyntaxToken.CreateMissing(SyntaxKind.FinallyKeyword, null, null),
						_syntaxFactory.Block(
							SyntaxToken.CreateMissing(SyntaxKind.OpenBraceToken, null, null),
							default(SyntaxList<StatementSyntax>),
							SyntaxToken.CreateMissing(SyntaxKind.CloseBraceToken, null, null)));
				}

				this._isInTry = isInTry;

				return _syntaxFactory.TryStatement(@try, block, catches, @finally);
			}
			finally
			{
				if (!catches.IsNull)
				{
					this._pool.Free(catches);
				}
			}
		}
		#endregion

		#region catch
		private CatchClauseSyntax ParseCatchClause(bool hasCatchAll)
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.CatchKeyword);

			var @catch = this.EatToken();

			// Check for the error of catch clause following empty catch here.
			if (hasCatchAll)
			{
				@catch = this.AddError(@catch, ErrorCode.ERR_TooManyCatches);
			}

			CatchDeclarationSyntax decl = null;
			var saveTerm = this._termState;

			if (this.CurrentToken.Kind == SyntaxKind.OpenParenToken)
			{
				var openParen = this.EatToken();
				this._termState |= TerminatorState.IsEndOfCatchClause;
				var type = this.ParseClassType();
				SyntaxToken name = null;

				if (this.IsTrueIdentifier())
				{
					name = this.ParseIdentifierToken();
				}

				this._termState = saveTerm;
				var closeParen = this.EatToken(SyntaxKind.CloseParenToken);
				decl = _syntaxFactory.CatchDeclaration(openParen, type, name, closeParen);
			}

			CatchFilterClauseSyntax filter = null;

			if (this.CurrentToken.Kind == SyntaxKind.IfKeyword)
			{
				var ifKeyword = CheckFeatureAvailability(this.EatToken(), MessageID.IDS_FeatureExceptionFilter);
				this._termState |= TerminatorState.IsEndOfilterClause;
				var openParen = this.EatToken(SyntaxKind.OpenParenToken);
				var filterExpression = this.ParseExpression();

				this._termState = saveTerm;
				var closeParen = this.EatToken(SyntaxKind.CloseParenToken);
				filter = _syntaxFactory.CatchFilterClause(ifKeyword, openParen, filterExpression, closeParen);
			}

			this._termState |= TerminatorState.IsEndOfCatchBlock;
			var block = this.ParseBlock();
			this._termState = saveTerm;

			return _syntaxFactory.CatchClause(@catch, decl, filter, block);
		}
		#endregion

		#region do
		private DoStatementSyntax ParseDoStatement()
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.DoKeyword);
			var @do = this.EatToken(SyntaxKind.DoKeyword);
			var statement = this.ParseEmbeddedStatement(false);
			var @while = this.EatToken(SyntaxKind.WhileKeyword);
			var openParen = this.EatToken(SyntaxKind.OpenParenToken);
			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsEndOfDoWhileExpression;
			var expression = this.ParseExpression();
			this._termState = saveTerm;
			var closeParen = this.EatToken(SyntaxKind.CloseParenToken);
			var semicolon = this.EatToken(SyntaxKind.SemicolonToken);
			return _syntaxFactory.DoStatement(@do, statement, @while, openParen, expression, closeParen, semicolon);
		}
		#endregion

		#region for

		private StatementSyntax ParseForOrForEachStatement()
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.ForKeyword);

			// Check if the user wrote the following accidently:
			//
			// for (SomeType t in
			//
			// instead of
			//
			// foreach (SomeType t in
			//
			// In that case, parse it as a foreach, but given the appropriate message that a
			// 'foreach' keyword was expected.
			var resetPoint = this.GetResetPoint();
			try
			{
				if (this.CurrentToken.Kind == SyntaxKind.ForKeyword)
				{
					this.EatToken();
					if (this.EatToken().Kind == SyntaxKind.OpenParenToken &&
						this.ScanType() != ScanTypeFlags.NotType &&
						this.EatToken().Kind == SyntaxKind.IdentifierToken &&
						this.EatToken().Kind == SyntaxKind.ColonToken)
					{
						// Looks like a foreach statement.  Parse it that way instead
						this.Reset(ref resetPoint);
						return this.ParseJavaEnhancedForStatement();
					}
					else
					{
						// Normal for statement.
						this.Reset(ref resetPoint);
						return this.ParseForStatement();
					}
				}
				else
				{
					return this.ParseJavaEnhancedForStatement();
				}
			}
			finally
			{
				this.Release(ref resetPoint);
			}
		}

		private JavaEnhancedForStatementSyntax ParseJavaEnhancedForStatement()
		{
			// Can be a 'for' keyword if the user typed: 'for (SomeType t :'
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.ForKeyword);

			// Syntax for foreach is:
			//  for ( <type> <identifier> : <expr> ) <embedded-statement>

			SyntaxToken forKeyword = this.EatToken(SyntaxKind.ForKeyword);
			

			var openParen = this.EatToken(SyntaxKind.OpenParenToken);
			var type = this.ParseType(false);
			SyntaxToken name;
			if (this.CurrentToken.Kind == SyntaxKind.ColonToken)
			{
				name = this.ParseIdentifierToken();
				name = this.AddError(name, ErrorCode.ERR_BadForeachDecl);
			}
			else
			{
				name = this.ParseIdentifierToken();
			}

			var @in = this.EatToken(SyntaxKind.ColonToken, ErrorCode.ERR_InExpected);
			var expression = this.ParseExpression();
			var closeParen = this.EatToken(SyntaxKind.CloseParenToken);
			var statement = this.ParseEmbeddedStatement(true);
			return _syntaxFactory.JavaEnhancedForStatement(forKeyword, openParen, type, name, @in, expression, closeParen, statement);
		}

		private ForStatementSyntax ParseForStatement()
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.ForKeyword);

			var @for = this.EatToken(SyntaxKind.ForKeyword);
			var openParen = this.EatToken(SyntaxKind.OpenParenToken);

			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsEndOfForStatementArgument;

			var resetPoint = this.GetResetPoint();
			var initializers = this._pool.AllocateSeparated<ExpressionSyntax>();
			var incrementors = this._pool.AllocateSeparated<ExpressionSyntax>();
			try
			{
				// Here can be either a declaration or an expression statement list.  Scan
				// for a declaration first.
				ScanTypeFlags st;

				st = this.ScanType();


				VariableDeclarationSyntax decl = null;

				if (st != ScanTypeFlags.NotType && this.IsTrueIdentifier())
				{
					this.Reset(ref resetPoint);
					TypeSyntax type;
					var variables = this._pool.AllocateSeparated<VariableDeclaratorSyntax>();
					this.ParseDeclaration(false, out type, variables);
					decl = _syntaxFactory.VariableDeclaration(type, variables);
					this._pool.Free(variables);
				}
				else
				{
					// Not a type followed by an identifier, so it must be an expression list.
					this.Reset(ref resetPoint);
					if (this.CurrentToken.Kind != SyntaxKind.SemicolonToken)
					{
						this.ParseForStatementExpressionList(ref openParen, initializers);
					}
				}

				var semi = this.EatToken(SyntaxKind.SemicolonToken);

				ExpressionSyntax condition = null;
				if (this.CurrentToken.Kind != SyntaxKind.SemicolonToken)
				{
					condition = this.ParseExpression();
				}

				var semi2 = this.EatToken(SyntaxKind.SemicolonToken);

				if (this.CurrentToken.Kind != SyntaxKind.CloseParenToken)
				{
					this.ParseForStatementExpressionList(ref semi2, incrementors);
				}

				var closeParen = this.EatToken(SyntaxKind.CloseParenToken);
				var statement = ParseEmbeddedStatement(true);

				return _syntaxFactory.ForStatement(@for, openParen, decl, initializers, semi, condition, semi2, incrementors, closeParen, statement);
			}
			finally
			{
				this._termState = saveTerm;
				this.Release(ref resetPoint);
				this._pool.Free(incrementors);
				this._pool.Free(initializers);
			}
		}

		private void ParseForStatementExpressionList(ref SyntaxToken startToken, SeparatedSyntaxListBuilder<ExpressionSyntax> list)
		{
			if (this.CurrentToken.Kind != SyntaxKind.CloseParenToken && this.CurrentToken.Kind != SyntaxKind.SemicolonToken)
			{
			tryAgain:
				if (this.IsPossibleExpression() || this.CurrentToken.Kind == SyntaxKind.CommaToken)
				{
					// first argument
					list.Add(this.ParseExpression());

					// additional arguments
					while (true)
					{
						if (this.CurrentToken.Kind == SyntaxKind.CloseParenToken || this.CurrentToken.Kind == SyntaxKind.SemicolonToken)
						{
							break;
						}
						else if (this.CurrentToken.Kind == SyntaxKind.CommaToken || this.IsPossibleExpression())
						{
							list.AddSeparator(this.EatToken(SyntaxKind.CommaToken));
							list.Add(this.ParseExpression());
							continue;
						}
						else if (this.SkipBadForStatementExpressionListTokens(ref startToken, list, SyntaxKind.CommaToken) == PostSkipAction.Abort)
						{
							break;
						}
					}
				}
				else if (this.SkipBadForStatementExpressionListTokens(ref startToken, list, SyntaxKind.IdentifierToken) == PostSkipAction.Continue)
				{
					goto tryAgain;
				}
			}
		}
		#endregion


		#region if
		private IfStatementSyntax ParseIfStatement()
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.IfKeyword);
			var @if = this.EatToken(SyntaxKind.IfKeyword);
			var openParen = this.EatToken(SyntaxKind.OpenParenToken);
			var condition = this.ParseExpression();
			var closeParen = this.EatToken(SyntaxKind.CloseParenToken);
			var statement = this.ParseEmbeddedStatement(false);
			ElseClauseSyntax @else = null;
			if (this.CurrentToken.Kind == SyntaxKind.ElseKeyword)
			{
				var elseToken = this.EatToken(SyntaxKind.ElseKeyword);
				var elseStatement = this.ParseEmbeddedStatement(false);
				@else = _syntaxFactory.ElseClause(elseToken, elseStatement);
			}

			return _syntaxFactory.IfStatement(@if, openParen, condition, closeParen, statement, @else);
		}
		#endregion

		#region return
		private ReturnStatementSyntax ParseReturnStatement()
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.ReturnKeyword);
			var @return = this.EatToken(SyntaxKind.ReturnKeyword);
			ExpressionSyntax arg = null;
			if (this.CurrentToken.Kind != SyntaxKind.SemicolonToken)
			{
				arg = this.ParseExpression();
			}

			var semicolon = this.EatToken(SyntaxKind.SemicolonToken);
			return _syntaxFactory.ReturnStatement(@return, arg, semicolon);
		}
		#endregion

		#region switch
		private SwitchStatementSyntax ParseSwitchStatement()
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.SwitchKeyword);
			var @switch = this.EatToken(SyntaxKind.SwitchKeyword);
			var openParen = this.EatToken(SyntaxKind.OpenParenToken);
			var expression = this.ParseExpression();
			var closeParen = this.EatToken(SyntaxKind.CloseParenToken);
			var openBrace = this.EatToken(SyntaxKind.OpenBraceToken);

			if (this.CurrentToken.Kind == SyntaxKind.CloseBraceToken)
			{
				openBrace = this.AddError(openBrace, ErrorCode.WRN_EmptySwitch);
			}

			var sections = this._pool.Allocate<SwitchSectionSyntax>();
			try
			{
				while (this.IsPossibleSwitchSection())
				{
					var swcase = this.ParseSwitchSection();
					sections.Add(swcase);
				}

				var closeBrace = this.EatToken(SyntaxKind.CloseBraceToken);
				return _syntaxFactory.SwitchStatement(@switch, openParen, expression, closeParen, openBrace, sections, closeBrace);
			}
			finally
			{
				this._pool.Free(sections);
			}
		}
		
		private SwitchSectionSyntax ParseSwitchSection()
		{
			Debug.Assert(this.IsPossibleSwitchSection());

			// First, parse case label(s)
			var labels = this._pool.Allocate<SwitchLabelSyntax>();
			var statements = this._pool.Allocate<StatementSyntax>();
			try
			{
				do
				{
					SyntaxToken specifier;
					ExpressionSyntax expression;
					SyntaxKind kind;
					if (this.CurrentToken.Kind == SyntaxKind.CaseKeyword)
					{
						kind = SyntaxKind.CaseSwitchLabel;
						specifier = this.EatToken();
						if (this.CurrentToken.Kind == SyntaxKind.ColonToken)
						{
							expression = this.CreateMissingIdentifierName();
							expression = this.AddError(expression, ErrorCode.ERR_ConstantExpected);
						}
						else
						{
							expression = this.ParseExpression();
						}
					}
					else
					{
						kind = SyntaxKind.DefaultSwitchLabel;
						Debug.Assert(this.CurrentToken.Kind == SyntaxKind.DefaultKeyword);
						specifier = this.EatToken(SyntaxKind.DefaultKeyword);
						expression = null;
					}

					var colon = this.EatToken(SyntaxKind.ColonToken);
					var caseLabel = _syntaxFactory.SwitchLabel(kind, specifier, expression, colon);
					labels.Add(caseLabel);
				}
				while (IsPossibleSwitchSection());

				// Next, parse statement list stopping for new sections
				CSharpSyntaxNode tmp = labels[labels.Count - 1];
				this.ParseStatements(ref tmp, statements, true);
				labels[labels.Count - 1] = (SwitchLabelSyntax)tmp;

				return _syntaxFactory.SwitchSection(labels, statements);
			}
			finally
			{
				this._pool.Free(statements);
				this._pool.Free(labels);
			}
		}
		#endregion

		#region throw
		private ThrowStatementSyntax ParseThrowStatement()
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.ThrowKeyword);
			var @throw = this.EatToken(SyntaxKind.ThrowKeyword);
			ExpressionSyntax arg = null;
			if (this.CurrentToken.Kind != SyntaxKind.SemicolonToken)
			{
				arg = this.ParseExpression();
			}

			var semi = this.EatToken(SyntaxKind.SemicolonToken);
			return _syntaxFactory.ThrowStatement(@throw, arg, semi);
		}
		#endregion

		#region using
		private UsingStatementSyntax ParseUsingStatement()
		{
			var @using = this.EatToken(SyntaxKind.ImportKeyword);
			var openParen = this.EatToken(SyntaxKind.OpenParenToken);

			VariableDeclarationSyntax declaration = null;
			ExpressionSyntax expression = null;

			var resetPoint = this.GetResetPoint();
			ParseUsingExpression(ref declaration, ref expression, ref resetPoint);
			this.Release(ref resetPoint);

			var closeParen = this.EatToken(SyntaxKind.CloseParenToken);
			var statement = this.ParseEmbeddedStatement(false);

			return _syntaxFactory.UsingStatement(@using, openParen, declaration, expression, closeParen, statement);
		}

		private void ParseUsingExpression(ref VariableDeclarationSyntax declaration, ref ExpressionSyntax expression, ref ResetPoint resetPoint)
		{
			TypeSyntax type;

			// Now, this can be either an expression or a decl list

			ScanTypeFlags st;


			st = this.ScanType();


			if (st == ScanTypeFlags.NullableType)
			{
				// We need to handle:
				// * using (f ? x = a : x = b)
				// * using (f ? x = a)
				// * using (f ? x, y)

				if (this.CurrentToken.Kind != SyntaxKind.IdentifierToken)
				{
					this.Reset(ref resetPoint);
					expression = this.ParseExpression();
				}
				else
				{
					SeparatedSyntaxListBuilder<VariableDeclaratorSyntax> variables;

					switch (this.PeekToken(1).Kind)
					{
						default:
							this.Reset(ref resetPoint);
							expression = this.ParseExpression();
							break;

						case SyntaxKind.CommaToken:
						case SyntaxKind.CloseParenToken:
							this.Reset(ref resetPoint);
							variables = this._pool.AllocateSeparated<VariableDeclaratorSyntax>();
							this.ParseDeclaration(false, out type, variables);
							declaration = _syntaxFactory.VariableDeclaration(type, variables.ToList());
							this._pool.Free(variables);
							break;

						case SyntaxKind.EqualsToken:
							// Parse it as a decl. If the next token is a : and only one variable was parsed,
							// convert the whole thing to ?: expression.
							this.Reset(ref resetPoint);
							variables = this._pool.AllocateSeparated<VariableDeclaratorSyntax>();
							this.ParseDeclaration(false, out type, variables);

							//// We may have non-nullable types in error scenarios.
							//if (this.CurrentToken.Kind == SyntaxKind.ColonToken &&
							//	type.Kind == SyntaxKind.NullableType &&
							//	SyntaxKindFacts.IsName(((NullableTypeSyntax)type).ElementType.Kind) &&
							//	variables.Count == 1)
							//{
							//	// We have "name? id = expr :" so need to convert to a ?: expression.
							//	this.Reset(ref resetPoint);
							//	expression = this.ParseExpression();
							//}
							//else
							//{
								declaration = _syntaxFactory.VariableDeclaration(type, variables.ToList());
							//}

							this._pool.Free(variables);
							break;
					}
				}
			}
			else if (IsUsingStatementVariableDeclaration(st))
			{
				this.Reset(ref resetPoint);
				var variables = this._pool.AllocateSeparated<VariableDeclaratorSyntax>();
				this.ParseDeclaration(false, out type, variables);
				declaration = _syntaxFactory.VariableDeclaration(type, variables);
				this._pool.Free(variables);
			}
			else
			{
				// Must be an expression statement
				this.Reset(ref resetPoint);
				expression = this.ParseExpression();
			}
		}

		#endregion


		#region while
		private WhileStatementSyntax ParseWhileStatement()
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.WhileKeyword);
			var @while = this.EatToken(SyntaxKind.WhileKeyword);
			var openParen = this.EatToken(SyntaxKind.OpenParenToken);
			var condition = this.ParseExpression();
			var closeParen = this.EatToken(SyntaxKind.CloseParenToken);
			var statement = this.ParseEmbeddedStatement(true);
			return _syntaxFactory.WhileStatement(@while, openParen, condition, closeParen, statement);
		}
		#endregion

		


	}
}