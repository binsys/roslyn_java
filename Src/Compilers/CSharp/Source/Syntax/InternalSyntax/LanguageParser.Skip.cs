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

		private PostSkipAction SkipBadAttributeArgumentTokens(ref SyntaxToken openParen, SeparatedSyntaxListBuilder<AnnotationArgumentSyntax> list, SyntaxKind expected)
		{
			return this.SkipBadSeparatedListTokensWithExpectedKind(ref openParen, list,
				p => p.CurrentToken.Kind != SyntaxKind.CommaToken && !p.IsPossibleAttributeArgument(),
				p => p.CurrentToken.Kind == SyntaxKind.CloseParenToken || p.IsTerminator(),
				expected);
		}

		private void SkipBadMemberListTokens(ref SyntaxToken openBrace, SyntaxListBuilder members)
		{
			if (members.Count > 0)
			{
				CSharpSyntaxNode tmp = members[members.Count - 1];
				this.SkipBadMemberListTokens(ref tmp);
				members[members.Count - 1] = tmp;
			}
			else
			{
				CSharpSyntaxNode tmp = openBrace;
				this.SkipBadMemberListTokens(ref tmp);
				openBrace = (SyntaxToken)tmp;
			}
		}

		private void SkipBadMemberListTokens(ref CSharpSyntaxNode previousNode)
		{
			int curlyCount = 0;
			var tokens = this._pool.Allocate();
			try
			{
				bool done = false;

				while (!done)
				{
					SyntaxKind kind = this.CurrentToken.Kind;

					// If this token can start a member, we're done
					if (CanStartMember(kind) &&
						!((this.PeekToken(1).Kind == SyntaxKind.OpenBraceToken || this.PeekToken(1).Kind == SyntaxKind.OpenParenToken)))
					{
						done = true;
						continue;
					}

					// <UNDONE>  UNDONE: Seems like this makes sense, 
					// but if this token can start a namespace element, but not a member, then
					// perhaps we should bail back up to parsing a namespace body somehow...</UNDONE>

					// Watch curlies and look for end of file/close curly
					switch (kind)
					{
						case SyntaxKind.OpenBraceToken:
							curlyCount++;
							break;

						case SyntaxKind.CloseBraceToken:
							if (curlyCount-- == 0)
							{
								done = true;
								continue;
							}

							break;

						case SyntaxKind.EndOfFileToken:
							done = true;
							continue;

						default:
							break;
					}

					var token = this.EatToken();
					if (tokens.Count == 0)
					{
						token = this.AddError(token, ErrorCode.ERR_InvalidMemberDecl, token.Text);
					}

					tokens.Add(token);
				}

				previousNode = AddTrailingSkippedSyntax(previousNode, tokens.ToListNode());
			}
			finally
			{
				this._pool.Free(tokens);
			}
		}



		private PostSkipAction SkipBadBaseListTokens(ref SyntaxToken colon, SeparatedSyntaxListBuilder<TypeSyntax> list, SyntaxKind expected)
		{
			return this.SkipBadSeparatedListTokensWithExpectedKind(ref colon, list,
				p => p.CurrentToken.Kind != SyntaxKind.CommaToken && !p.IsPossibleAttribute(),
				p => p.CurrentToken.Kind == SyntaxKind.OpenBraceToken || p.IsPossibleTypeParameterConstraintClauseStart() || p.IsTerminator(),
				expected);
		}

		private PostSkipAction SkipBadTypeParameterConstraintTokens(SeparatedSyntaxListBuilder<TypeParameterConstraintSyntax> list, SyntaxKind expected)
		{
			CSharpSyntaxNode tmp = null;
			Debug.Assert(list.Count > 0);
			return this.SkipBadSeparatedListTokensWithExpectedKind(ref tmp, list,
				p => this.CurrentToken.Kind != SyntaxKind.CommaToken && !this.IsPossibleTypeParameterConstraint(),
				p => this.CurrentToken.Kind == SyntaxKind.OpenBraceToken || this.IsPossibleTypeParameterConstraintClauseStart() || this.IsTerminator(),
				expected);
		}


		private PostSkipAction SkipBadSeparatedListTokensWithExpectedKind<T, TNode>(
			ref T startToken,
			SeparatedSyntaxListBuilder<TNode> list,
			Func<LanguageParser, bool> isNotExpectedFunction,
			Func<LanguageParser, bool> abortFunction,
			SyntaxKind expected)
			where T : CSharpSyntaxNode
			where TNode : CSharpSyntaxNode
		{
			// We're going to cheat here and pass the underlying SyntaxListBuilder of "list" to the helper method so that
			// it can append skipped trivia to the last element, regardless of whether that element is a node or a token.
			CSharpSyntaxNode trailingTrivia;
			var action = this.SkipBadListTokensWithExpectedKindHelper(list.UnderlyingBuilder, isNotExpectedFunction, abortFunction, expected, out trailingTrivia);
			if (trailingTrivia != null)
			{
				startToken = AddTrailingSkippedSyntax(startToken, trailingTrivia);
			}
			return action;
		}

		private PostSkipAction SkipBadListTokensWithErrorCode<T, TNode>(
			ref T startToken,
			SyntaxListBuilder<TNode> list,
			Func<LanguageParser, bool> isNotExpectedFunction,
			Func<LanguageParser, bool> abortFunction,
			ErrorCode error)
			where T : CSharpSyntaxNode
			where TNode : CSharpSyntaxNode
		{
			CSharpSyntaxNode trailingTrivia;
			var action = this.SkipBadListTokensWithErrorCodeHelper(list, isNotExpectedFunction, abortFunction, error, out trailingTrivia);
			if (trailingTrivia != null)
			{
				startToken = AddTrailingSkippedSyntax(startToken, trailingTrivia);
			}
			return action;
		}

		/// <remarks>
		/// WARNING: it is possible that "list" is really the underlying builder of a SeparateSyntaxListBuilder,
		/// so it is important that we not add anything to the list.
		/// </remarks>
		private PostSkipAction SkipBadListTokensWithExpectedKindHelper(
			SyntaxListBuilder list,
			Func<LanguageParser, bool> isNotExpectedFunction,
			Func<LanguageParser, bool> abortFunction,
			SyntaxKind expected,
			out CSharpSyntaxNode trailingTrivia)
		{
			if (list.Count == 0)
			{
				return SkipBadTokensWithExpectedKind(isNotExpectedFunction, abortFunction, expected, out trailingTrivia);
			}
			else
			{
				CSharpSyntaxNode lastItemTrailingTrivia;
				var action = SkipBadTokensWithExpectedKind(isNotExpectedFunction, abortFunction, expected, out lastItemTrailingTrivia);
				if (lastItemTrailingTrivia != null)
				{
					list[list.Count - 1] = AddTrailingSkippedSyntax(list[list.Count - 1], lastItemTrailingTrivia);
				}
				trailingTrivia = null;
				return action;
			}
		}

		private PostSkipAction SkipBadListTokensWithErrorCodeHelper<TNode>(
			SyntaxListBuilder<TNode> list,
			Func<LanguageParser, bool> isNotExpectedFunction,
			Func<LanguageParser, bool> abortFunction,
			ErrorCode error,
			out CSharpSyntaxNode trailingTrivia) where TNode : CSharpSyntaxNode
		{
			if (list.Count == 0)
			{
				return SkipBadTokensWithErrorCode(isNotExpectedFunction, abortFunction, error, out trailingTrivia);
			}
			else
			{
				CSharpSyntaxNode lastItemTrailingTrivia;
				var action = SkipBadTokensWithErrorCode(isNotExpectedFunction, abortFunction, error, out lastItemTrailingTrivia);
				if (lastItemTrailingTrivia != null)
				{
					list[list.Count - 1] = AddTrailingSkippedSyntax(list[list.Count - 1], lastItemTrailingTrivia);
				}
				trailingTrivia = null;
				return action;
			}
		}

		private PostSkipAction SkipBadTokensWithExpectedKind(
			Func<LanguageParser, bool> isNotExpectedFunction,
			Func<LanguageParser, bool> abortFunction,
			SyntaxKind expected,
			out CSharpSyntaxNode trailingTrivia)
		{
			var nodes = this._pool.Allocate();
			try
			{
				bool first = true;
				var action = PostSkipAction.Continue;
				while (isNotExpectedFunction(this))
				{
					if (abortFunction(this))
					{
						action = PostSkipAction.Abort;
						break;
					}

					var token = (first && !this.CurrentToken.ContainsDiagnostics) ? this.EatTokenWithPrejudice(expected) : this.EatToken();
					first = false;
					nodes.Add(token);
				}

				trailingTrivia = (nodes.Count > 0) ? nodes.ToListNode() : null;
				return action;
			}
			finally
			{
				this._pool.Free(nodes);
			}
		}

		private PostSkipAction SkipBadTokensWithErrorCode(
			Func<LanguageParser, bool> isNotExpectedFunction,
			Func<LanguageParser, bool> abortFunction,
			ErrorCode errorCode,
			out CSharpSyntaxNode trailingTrivia)
		{
			var nodes = this._pool.Allocate();
			try
			{
				bool first = true;
				var action = PostSkipAction.Continue;
				while (isNotExpectedFunction(this))
				{
					if (abortFunction(this))
					{
						action = PostSkipAction.Abort;
						break;
					}

					var token = (first && !this.CurrentToken.ContainsDiagnostics) ? this.EatTokenWithPrejudice(errorCode) : this.EatToken();
					first = false;
					nodes.Add(token);
				}

				trailingTrivia = (nodes.Count > 0) ? nodes.ToListNode() : null;
				return action;
			}
			finally
			{
				this._pool.Free(nodes);
			}
		}

		private PostSkipAction SkipBadVariableListTokens(SeparatedSyntaxListBuilder<VariableDeclaratorSyntax> list, SyntaxKind expected)
		{
			CSharpSyntaxNode tmp = null;
			Debug.Assert(list.Count > 0);
			return this.SkipBadSeparatedListTokensWithExpectedKind(ref tmp, list,
				p => this.CurrentToken.Kind != SyntaxKind.CommaToken,
				p => this.CurrentToken.Kind == SyntaxKind.SemicolonToken || this.IsTerminator(),
				expected);
		}


		private PostSkipAction SkipBadJavaEnumConstantListTokens(ref SyntaxToken openBrace, SeparatedSyntaxListBuilder<JavaEnumConstantSyntax> list, SyntaxKind expected)
		{
			return this.SkipBadSeparatedListTokensWithExpectedKind(ref openBrace, list,
				p => p.CurrentToken.Kind != SyntaxKind.CommaToken && p.CurrentToken.Kind != SyntaxKind.SemicolonToken && !p.IsPossibleJavaEnumConstant(),
				p => p.CurrentToken.Kind == SyntaxKind.CloseBraceToken || p.IsTerminator(),
				expected);
		}

		private PostSkipAction SkipBadTypeParameterListTokens(SeparatedSyntaxListBuilder<TypeParameterSyntax> list, SyntaxKind expected)
		{
			CSharpSyntaxNode tmp = null;
			Debug.Assert(list.Count > 0);
			return this.SkipBadSeparatedListTokensWithExpectedKind(ref tmp, list,
				p => this.CurrentToken.Kind != SyntaxKind.CommaToken,
				p => this.CurrentToken.Kind == SyntaxKind.GreaterThanToken || this.IsTerminator(),
				expected);
		}


		private PostSkipAction SkipBadTypeArgumentListTokens(SeparatedSyntaxListBuilder<TypeSyntax> list, SyntaxKind expected)
		{
			CSharpSyntaxNode tmp = null;
			Debug.Assert(list.Count > 0);
			return this.SkipBadSeparatedListTokensWithExpectedKind(ref tmp, list,
				p => this.CurrentToken.Kind != SyntaxKind.CommaToken && !this.IsPossibleType(),
				p => this.CurrentToken.Kind == SyntaxKind.GreaterThanToken || this.IsTerminator(),
				expected);
		}


		private PostSkipAction SkipBadArrayRankSpecifierTokens(ref SyntaxToken openBracket, SeparatedSyntaxListBuilder<ExpressionSyntax> list, SyntaxKind expected)
		{
			return this.SkipBadSeparatedListTokensWithExpectedKind(ref openBracket, list,
				p => p.CurrentToken.Kind != SyntaxKind.CommaToken && !p.IsPossibleExpression(),
				p => p.CurrentToken.Kind == SyntaxKind.CloseBracketToken || p.IsTerminator(),
				expected);
		}

		private PostSkipAction SkipBadStatementListTokens(SyntaxListBuilder<StatementSyntax> statements, SyntaxKind expected, out CSharpSyntaxNode trailingTrivia)
		{
			return this.SkipBadListTokensWithExpectedKindHelper(
				statements,
				p => !p.IsPossibleStatement(),
				p => p.CurrentToken.Kind == SyntaxKind.CloseBraceToken || p.IsTerminator(),
				expected,
				out trailingTrivia
			);
		}


		private PostSkipAction SkipBadForStatementExpressionListTokens(ref SyntaxToken startToken, SeparatedSyntaxListBuilder<ExpressionSyntax> list, SyntaxKind expected)
		{
			return this.SkipBadSeparatedListTokensWithExpectedKind(ref startToken, list,
				p => p.CurrentToken.Kind != SyntaxKind.CommaToken && !p.IsPossibleExpression(),
				p => p.CurrentToken.Kind == SyntaxKind.CloseParenToken || p.CurrentToken.Kind == SyntaxKind.SemicolonToken || p.IsTerminator(),
				expected);
		}



		private PostSkipAction SkipBadArgumentListTokens(ref SyntaxToken open, SeparatedSyntaxListBuilder<ArgumentSyntax> list, SyntaxKind expected, SyntaxKind closeKind)
		{
			return this.SkipBadSeparatedListTokensWithExpectedKind(ref open, list,
				p => p.CurrentToken.Kind != SyntaxKind.CommaToken && !p.IsPossibleArgumentExpression(),
				p => p.CurrentToken.Kind == closeKind || p.CurrentToken.Kind == SyntaxKind.SemicolonToken || p.IsTerminator(),
				expected);
		}
		private PostSkipAction SkipBadInitializerListTokens<T>(ref SyntaxToken startToken, SeparatedSyntaxListBuilder<T> list, SyntaxKind expected)
			where T : CSharpSyntaxNode
		{
			return this.SkipBadSeparatedListTokensWithExpectedKind(ref startToken, list,
				p => p.CurrentToken.Kind != SyntaxKind.CommaToken && !p.IsPossibleExpression(),
				p => p.CurrentToken.Kind == SyntaxKind.CloseBraceToken || p.IsTerminator(),
				expected);
		}
		private PostSkipAction SkipBadArrayInitializerTokens(ref SyntaxToken openBrace, SeparatedSyntaxListBuilder<ExpressionSyntax> list, SyntaxKind expected)
		{
			return this.SkipBadSeparatedListTokensWithExpectedKind(ref openBrace, list,
				p => p.CurrentToken.Kind != SyntaxKind.CommaToken && !p.IsPossibleVariableInitializer(false),
				p => this.CurrentToken.Kind == SyntaxKind.CloseBraceToken || this.IsTerminator(),
				expected);
		}


		private PostSkipAction SkipBadLambdaParameterListTokens(ref SyntaxToken openParen, SeparatedSyntaxListBuilder<ParameterSyntax> list, SyntaxKind expected, SyntaxKind closeKind)
		{
			return this.SkipBadSeparatedListTokensWithExpectedKind(ref openParen, list,
				p => p.CurrentToken.Kind != SyntaxKind.CommaToken && !p.IsPossibleLambdaParameter(),
				p => p.CurrentToken.Kind == closeKind || p.IsTerminator(),
				expected);
		}


		private PostSkipAction SkipBadParameterListTokens(ref SyntaxToken open, SeparatedSyntaxListBuilder<ParameterSyntax> list, SyntaxKind expected, SyntaxKind closeKind, bool allowThisKeyword, bool allowFieldModifiers)
		{
			return this.SkipBadSeparatedListTokensWithExpectedKind(ref open, list,
				p => p.CurrentToken.Kind != SyntaxKind.CommaToken && !p.IsPossibleParameter(allowThisKeyword, allowFieldModifiers),
				p => p.CurrentToken.Kind == closeKind || p.IsTerminator(),
				expected);
		}


	}
}