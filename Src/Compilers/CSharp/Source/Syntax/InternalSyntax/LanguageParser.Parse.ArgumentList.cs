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
		#region ArgumentList 实参列表
		private ArgumentSyntax ParseArgumentExpression(bool isIndexer)
		{
			NameColonSyntax nameColon = null;
			if (this.CurrentToken.Kind == SyntaxKind.IdentifierToken && this.PeekToken(1).Kind == SyntaxKind.ColonToken)
			{
				var name = this.ParseIdentifierName();
				var colon = this.EatToken(SyntaxKind.ColonToken);
				nameColon = _syntaxFactory.NameColon(name, colon);
				nameColon = CheckFeatureAvailability(nameColon, MessageID.IDS_FeatureNamedArgument);
			}

			SyntaxToken refOrOutKeyword = null;


			ExpressionSyntax expression;

			if (isIndexer && (this.CurrentToken.Kind == SyntaxKind.CommaToken || this.CurrentToken.Kind == SyntaxKind.CloseBracketToken))
			{
				expression = this.AddError(this.CreateMissingIdentifierName(), ErrorCode.ERR_ValueExpected);
			}
			else if (this.CurrentToken.Kind == SyntaxKind.CommaToken)
			{
				expression = this.AddError(this.CreateMissingIdentifierName(), ErrorCode.ERR_MissingArgument);
			}
			else
			{
				expression = this.ParseSubExpression(0, contextRequiresVariable: refOrOutKeyword != null);
			}

			return _syntaxFactory.Argument(nameColon, expression);
		}


		private void ParseArgumentList(
			out SyntaxToken openToken,
			out SeparatedSyntaxList<ArgumentSyntax> arguments,
			out SyntaxToken closeToken,
			SyntaxKind openKind,
			SyntaxKind closeKind)
		{
			bool isIndexer = openKind == SyntaxKind.OpenBracketToken;
			var open = this.EatToken(openKind);
			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsEndOfArgumentList;

			SeparatedSyntaxListBuilder<ArgumentSyntax> list = default(SeparatedSyntaxListBuilder<ArgumentSyntax>);
			try
			{
				if (this.CurrentToken.Kind != closeKind && this.CurrentToken.Kind != SyntaxKind.SemicolonToken)
				{
				tryAgain:
					if (list.IsNull)
					{
						list = this._pool.AllocateSeparated<ArgumentSyntax>();
					}

					if (this.IsPossibleArgumentExpression() || this.CurrentToken.Kind == SyntaxKind.CommaToken)
					{
						// first argument
						list.Add(this.ParseArgumentExpression(isIndexer));

						// additional arguments
						while (true)
						{
							if (this.CurrentToken.Kind == closeKind || this.CurrentToken.Kind == SyntaxKind.SemicolonToken)
							{
								break;
							}
							else if (this.CurrentToken.Kind == SyntaxKind.CommaToken || this.IsPossibleArgumentExpression())
							{
								list.AddSeparator(this.EatToken(SyntaxKind.CommaToken));
								list.Add(this.ParseArgumentExpression(isIndexer));
								continue;
							}
							else if (this.SkipBadArgumentListTokens(ref open, list, SyntaxKind.CommaToken, closeKind) == PostSkipAction.Abort)
							{
								break;
							}
						}
					}
					else if (this.SkipBadArgumentListTokens(ref open, list, SyntaxKind.IdentifierToken, closeKind) == PostSkipAction.Continue)
					{
						goto tryAgain;
					}
				}
				else if (isIndexer && this.CurrentToken.Kind == closeKind)
				{
					// An indexer always expects at least one value. And so we need to give an error
					// for the case where we see only "[]". ParseArgumentExpression gives it.

					if (list.IsNull)
					{
						list = this._pool.AllocateSeparated<ArgumentSyntax>();
					}

					list.Add(this.ParseArgumentExpression(isIndexer));
				}

				this._termState = saveTerm;

				openToken = open;
				closeToken = this.EatToken(closeKind);
				arguments = list.ToList();
			}
			finally
			{
				if (!list.IsNull)
				{
					this._pool.Free(list);
				}
			}
		}


		/// <summary>
		/// 小括号参数列表 ()
		/// </summary>
		/// <returns></returns>
		internal ArgumentListSyntax ParseParenthesizedArgumentList()
		{
			if (this.IsIncrementalAndFactoryContextMatches && this.CurrentNodeKind == SyntaxKind.ArgumentList)
			{
				return (ArgumentListSyntax)this.EatNode();
			}

			SyntaxToken openToken, closeToken;
			SeparatedSyntaxList<ArgumentSyntax> arguments;
			ParseArgumentList(out openToken, out arguments, out closeToken, SyntaxKind.OpenParenToken, SyntaxKind.CloseParenToken);

			return _syntaxFactory.ArgumentList(openToken, arguments, closeToken);
		}

		/// <summary>
		/// 中括号参数列表[]
		/// </summary>
		/// <returns></returns>
		internal BracketedArgumentListSyntax ParseBracketedArgumentList()
		{
			if (this.IsIncrementalAndFactoryContextMatches && this.CurrentNodeKind == SyntaxKind.BracketedArgumentList)
			{
				return (BracketedArgumentListSyntax)this.EatNode();
			}

			SyntaxToken openToken, closeToken;
			SeparatedSyntaxList<ArgumentSyntax> arguments;
			ParseArgumentList(out openToken, out arguments, out closeToken, SyntaxKind.OpenBracketToken, SyntaxKind.CloseBracketToken);

			return _syntaxFactory.BracketedArgumentList(openToken, arguments, closeToken);
		}

		#endregion
	}
}