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
		private JavaEnumDeclarationSyntax ParseJavaEnumDeclaration(SyntaxListBuilder<AnnotationSyntax> attributes,
			SyntaxListBuilder modifiers)
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.EnumKeyword);

			var enumToken = this.EatToken(SyntaxKind.EnumKeyword);
			var name = this.ParseIdentifierToken();
			var implementsClauseList = this.ParseImplementsListClause(false);
			var body = this.ParseJavaEnumBody(name);
			return _syntaxFactory.JavaEnumDeclaration(this.CreateJavaMemberModifierSyntax(attributes, modifiers),
				enumToken,
				name,
				implementsClauseList, body);
		}

		private JavaEnumBodySyntax ParseJavaEnumBody(SyntaxToken name)
		{
			var openBrace = this.EatToken(SyntaxKind.OpenBraceToken);

			var consts = default(SeparatedSyntaxList<JavaEnumConstantSyntax>);
			var bodyDecl = default(JavaEnumBodyDeclarationsSyntax);

			if (!openBrace.IsMissing)
			{
				var constsBuilder = this._pool.AllocateSeparated<JavaEnumConstantSyntax>();
				try
				{
					this.ParseJavaEnumConstants(ref openBrace, constsBuilder, name);
					consts = constsBuilder.ToList();
				}
				finally
				{
					this._pool.Free(constsBuilder);
				}

				bodyDecl = this.ParseJavaEnumBodyDeclarations(ref openBrace, name);
			}

			var closeBrace = this.EatToken(SyntaxKind.CloseBraceToken);

			SyntaxToken semicolon = null;
			if (this.CurrentToken.Kind == SyntaxKind.SemicolonToken)
			{
				semicolon = this.EatToken();
			}


			return _syntaxFactory.JavaEnumBody(openBrace, consts, bodyDecl, closeBrace, semicolon);
		}


		private void ParseJavaEnumConstants(
			ref SyntaxToken openBrace,
			SeparatedSyntaxListBuilder<JavaEnumConstantSyntax> members, SyntaxToken name)
		{
			if (this.CurrentToken.Kind != SyntaxKind.SemicolonToken && this.CurrentToken.Kind != SyntaxKind.CloseBraceToken)
			{
			tryAgain:

				if (this.IsPossibleJavaEnumConstant() 
					|| this.CurrentToken.Kind == SyntaxKind.CommaToken 
					//|| this.CurrentToken.Kind == SyntaxKind.SemicolonToken
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
						else if (this.CurrentToken.Kind == SyntaxKind.CommaToken 
							|| this.CurrentToken.Kind == SyntaxKind.SemicolonToken 
							|| this.IsPossibleJavaEnumConstant())
						{
							if (this.CurrentToken.Kind == SyntaxKind.SemicolonToken)
							{
								//// semicolon instead of comma.. consume it with error and act as if it were a comma.
								//members.AddSeparator(this.EatTokenWithPrejudice(SyntaxKind.CommaToken));
								break;
							}
							//else
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
							else if (!this.IsPossibleJavaEnumConstant())
							{
								goto tryAgain;
							}

							members.Add(this.ParseJavaEnumConstant());
							continue;
						}
						else if (this.SkipBadJavaEnumConstantListTokens(ref openBrace, members, SyntaxKind.CommaToken) == PostSkipAction.Abort)
						{
							break;
						}
					}
				}
				else if (this.SkipBadJavaEnumConstantListTokens(ref openBrace, members, SyntaxKind.IdentifierToken) == PostSkipAction.Continue)
				{
					goto tryAgain;
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

				if (this.CurrentToken.Kind == SyntaxKind.OpenParenToken)
				{
					args = this.ParseParenthesizedArgumentList();
				}

				return _syntaxFactory.JavaEnumConstant(memberAttrs, memberName, args);
			}
			finally
			{
				this._pool.Free(memberAttrs);
			}
		}


		private JavaEnumBodyDeclarationsSyntax ParseJavaEnumBodyDeclarations(ref SyntaxToken openBrace, SyntaxToken name)
		{
			if (this.IsIncrementalAndFactoryContextMatches && this.CurrentNodeKind == SyntaxKind.JavaEnumBodyDeclarations)
			{
				return (JavaEnumBodyDeclarationsSyntax)this.EatNode();
			}

			if (this.CurrentToken.Kind == SyntaxKind.SemicolonToken)
			{
				var semToken = this.EatToken(SyntaxKind.SemicolonToken);
				SyntaxListBuilder<MemberDeclarationSyntax> classBodyMembers = default(SyntaxListBuilder<MemberDeclarationSyntax>);
				try
				{
					classBodyMembers = this._pool.Allocate<MemberDeclarationSyntax>();
					this.ParseJavaNormalClassMembers(ref classBodyMembers, ref openBrace, name);
					var enumClassBody = _syntaxFactory.JavaEnumBodyDeclarations(semToken, classBodyMembers.Count == 0 ? null : classBodyMembers.ToList());
					return enumClassBody;
				}
				finally
				{
					if (!classBodyMembers.IsNull)
					{
						this._pool.Free(classBodyMembers);
					}
				}

			}
			return null;
		}
	}
}