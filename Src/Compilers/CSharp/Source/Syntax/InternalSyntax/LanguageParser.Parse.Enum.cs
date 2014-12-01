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
		private EnumDeclarationSyntax ParseEnumDeclaration(SyntaxListBuilder<AnnotationSyntax> attributes, SyntaxListBuilder modifiers)
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.EnumKeyword);

			var enumToken = this.EatToken(SyntaxKind.EnumKeyword);
			var name = this.ParseIdentifierToken();


			var implementsClauseList = this.ParseImplementsListClause(false);


			var members = default(SeparatedSyntaxList<EnumMemberDeclarationSyntax>);
			var openBrace = this.EatToken(SyntaxKind.OpenBraceToken);

			if ( !openBrace.IsMissing)
			{
				var builder = this._pool.AllocateSeparated<EnumMemberDeclarationSyntax>();
				try
				{
					this.ParseEnumMemberDeclarations(ref openBrace, builder,name);
					members = builder.ToList();
				}
				finally
				{
					this._pool.Free(builder);
				}
			}

			var closeBrace = this.EatToken(SyntaxKind.CloseBraceToken);

			SyntaxToken semicolon = null;
			if (this.CurrentToken.Kind == SyntaxKind.SemicolonToken)
			{
				semicolon = this.EatToken();
			}

			return _syntaxFactory.EnumDeclaration(
				this.CreateJavaMemberModifierSyntax(attributes, modifiers),
				enumToken,
				name,
				implementsClauseList,
				openBrace,
				members,
				closeBrace,
				semicolon);
		}


		private void ParseEnumMemberDeclarations(
			ref SyntaxToken openBrace,
			SeparatedSyntaxListBuilder<EnumMemberDeclarationSyntax> members,SyntaxToken name)
		{
			if (this.CurrentToken.Kind != SyntaxKind.SemicolonToken && this.CurrentToken.Kind != SyntaxKind.CloseBraceToken)
			{
			tryAgain:

				if (this.IsPossibleEnumMemberDeclaration() 
					|| this.CurrentToken.Kind == SyntaxKind.CommaToken 
					|| this.CurrentToken.Kind == SyntaxKind.SemicolonToken
					)
				{
					// first member
					members.Add(this.ParseJavaEnumConstant());

					// additional members
					while (true)
					{
						if (this.CurrentToken.Kind == SyntaxKind.CloseBraceToken)
						{
							break;
						}
						else if (this.CurrentToken.Kind == SyntaxKind.CommaToken || this.CurrentToken.Kind == SyntaxKind.SemicolonToken || this.IsPossibleEnumMemberDeclaration())
						{
							if (this.CurrentToken.Kind == SyntaxKind.SemicolonToken)
							{
								// semicolon instead of comma.. consume it with error and act as if it were a comma.
								members.AddSeparator(this.EatTokenWithPrejudice(SyntaxKind.CommaToken));
							}
							else
							{
								members.AddSeparator(this.EatToken(SyntaxKind.CommaToken));
							}

							// check for exit case after legal trailing comma
							if (this.CurrentToken.Kind == SyntaxKind.CloseBraceToken)
							{
								break;
							}
							else if (this.CurrentToken.Kind == SyntaxKind.SemicolonToken)
							{
								break;
							}
							else if (!this.IsPossibleEnumMemberDeclaration())
							{
								goto tryAgain;
							}

							members.Add(this.ParseJavaEnumConstant());
							continue;
						}
						else if (this.SkipBadEnumMemberListTokens(ref openBrace, members, SyntaxKind.CommaToken) == PostSkipAction.Abort)
						{
							break;
						}
					}
				}
				else if (this.SkipBadEnumMemberListTokens(ref openBrace, members, SyntaxKind.IdentifierToken) == PostSkipAction.Continue)
				{
					goto tryAgain;
				}
			}


			if (this.CurrentToken.Kind == SyntaxKind.SemicolonToken)
			{
				var semToken = this.EatToken(SyntaxKind.SemicolonToken);

				SyntaxListBuilder<MemberDeclarationSyntax> classBodyMembers = default(SyntaxListBuilder<MemberDeclarationSyntax>);
				try
				{
					classBodyMembers = this._pool.Allocate<MemberDeclarationSyntax>();

					while (true)
					{
						SyntaxKind kind = this.CurrentToken.Kind;

						if (CanStartMember(kind))
						{
							// This token can start a member -- go parse it
							var saveTerm2 = this._termState;
							this._termState |= TerminatorState.IsPossibleMemberStartOrStop;

							var memberOrStatement = this.ParseMemberDeclaration(kind, name.ValueText);
							if (memberOrStatement != null)
							{
								// statements are accepted here, a semantic error will be reported later
								classBodyMembers.Add(memberOrStatement);
							}
							else
							{
								// we get here if we couldn't parse the lookahead as a statement or a declaration (we haven't consumed any tokens):
								this.SkipBadMemberListTokens(ref openBrace, classBodyMembers);
							}

							this._termState = saveTerm2;
						}
						else if (kind == SyntaxKind.CloseBraceToken || kind == SyntaxKind.EndOfFileToken || this.IsTerminator())
						{
							// This marks the end of members of this class
							break;
						}
						else
						{
							// Error -- try to sync up with intended reality
							this.SkipBadMemberListTokens(ref openBrace, classBodyMembers);
						}
					}



				}
				finally
				{

					var enumClassBody = _syntaxFactory.JavaEnumClassBody(semToken, classBodyMembers.Count == 0 ? null : classBodyMembers.ToList());

					members.Add(enumClassBody);


					if (!classBodyMembers.IsNull)
					{
						this._pool.Free(classBodyMembers);
					}
				}

			}
		}

		private JavaEnumConstantSyntax ParseJavaEnumConstant()
		{
			if (this.IsIncrementalAndFactoryContextMatches && this.CurrentNodeKind == SyntaxKind.JavaEnumConstant)
			{
				return (JavaEnumConstantSyntax)this.EatNode();
			}

			var memberAttrs = this._pool.Allocate<AnnotationSyntax>();

			ArgumentListSyntax args = default(ArgumentListSyntax);
			try
			{
				this.ParseAnnotationDeclarations(memberAttrs);
				var memberName = this.ParseIdentifierToken();
				EqualsValueClauseSyntax equalsValue = null;

				if (this.CurrentToken.Kind == SyntaxKind.OpenParenToken)
				{
					args = this.ParseParenthesizedArgumentList();
				}

				//if (this.CurrentToken.Kind == SyntaxKind.EqualsToken)
				//{
				//	var equals = this.EatToken(SyntaxKind.EqualsToken);
				//	ExpressionSyntax value;
				//	if (this.CurrentToken.Kind == SyntaxKind.CommaToken || this.CurrentToken.Kind == SyntaxKind.CloseBraceToken)
				//	{
				//		value = this.CreateMissingIdentifierName(); //an identifier is a valid expression
				//		value = this.AddErrorToFirstToken(value, ErrorCode.ERR_ConstantExpected);
				//	}
				//	else
				//	{
				//		value = this.ParseExpression();
				//	}

				//	equalsValue = _syntaxFactory.EqualsValueClause(equals, value);
				//}

				//this.ParseParenthesizedArgumentList();

				return _syntaxFactory.JavaEnumConstant(memberAttrs, memberName, args);

				//return _syntaxFactory.EnumMemberDeclaration(memberAttrs, memberName, equalsValue);
			}
			finally
			{
				this._pool.Free(memberAttrs);
			}
		}


		private void ParseClassBody()
		{

		}
	}
}