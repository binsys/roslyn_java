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
		private JavaNormalClassDeclarationSyntax ParseJavaNormalClassDeclaration(SyntaxListBuilder<AnnotationSyntax> attributes, SyntaxListBuilder modifiers)
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.ClassKeyword);

			var classKeyword = this.EatToken(SyntaxKind.ClassKeyword);
			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsPossibleAggregateClauseStartOrStop;
			var name = this.ParseIdentifierToken();
			var typeParameters = this.ParseTypeParameterList();

			this._termState = saveTerm;

			var extendsClause = this.ParseExtendsClause();
			var implementsClauseList = this.ParseImplementsListClause(false);

			var body = this.ParseJavaClassBody(name);

			return _syntaxFactory.JavaNormalClassDeclaration(
				this.CreateJavaMemberModifierSyntax(attributes, modifiers),
				classKeyword, 
				name, 
				typeParameters, 
				extendsClause, 
				implementsClauseList, body);
		}



		private JavaClassBodySyntax ParseJavaClassBody(SyntaxToken name)
		{
			// Parse class body
			bool parseMembers = true;
			SyntaxListBuilder<MemberDeclarationSyntax> members = default(SyntaxListBuilder<MemberDeclarationSyntax>);
			try
			{
				var openBrace = this.EatToken(SyntaxKind.OpenBraceToken);

				// ignore members if missing type name or missing open curly
				if (name.IsMissing || openBrace.IsMissing)
				{
					parseMembers = false;
				}

				// even if we saw a { or think we should parse members bail out early since
				// we know namespaces can't be nested inside types
				if (parseMembers)
				{
					members = this._pool.Allocate<MemberDeclarationSyntax>();
					this.ParseJavaNormalClassMembers(ref members, ref openBrace, name);
				}

				var closeBrace = this.EatToken(SyntaxKind.CloseBraceToken);
				SyntaxToken semicolon = null;
				if (this.CurrentToken.Kind == SyntaxKind.SemicolonToken)
				{
					semicolon = this.EatToken();
				}

				return _syntaxFactory.JavaClassBody(openBrace, members.ToList(), closeBrace, semicolon);
			}
			finally
			{
				if (!members.IsNull)
				{
					this._pool.Free(members);
				}

			}
		}


		public void ParseJavaNormalClassMembers(ref SyntaxListBuilder<MemberDeclarationSyntax> members, ref SyntaxToken openBrace, SyntaxToken name)
		{
			while (true)
			{
				SyntaxKind kind = this.CurrentToken.Kind;

				if (CanStartMember(kind))
				{
					// This token can start a member -- go parse it
					var saveTerm2 = this._termState;
					this._termState |= TerminatorState.IsPossibleMemberStartOrStop;

					var memberOrStatement = this.ParseMemberDeclaration(kind, name == null ? null : name.ValueText);
					if (memberOrStatement != null)
					{
						// statements are accepted here, a semantic error will be reported later
						members.Add(memberOrStatement);
					}
					else
					{
						// we get here if we couldn't parse the lookahead as a statement or a declaration (we haven't consumed any tokens):
						this.SkipBadMemberListTokens(ref openBrace, members);
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
					this.SkipBadMemberListTokens(ref openBrace, members);
				}
			}
		}



		public JavaMemberModifierSyntax CreateJavaMemberModifierSyntax(SyntaxListBuilder<AnnotationSyntax> attributes,
			SyntaxListBuilder modifiers)
		{
			JavaMemberModifierSyntax modifier = default(JavaMemberModifierSyntax);


			if (attributes.Count != 0 || modifiers.Count != 0)
			{
				modifier = _syntaxFactory.JavaMemberModifier(attributes, modifiers.ToTokenList());
			}

			return modifier;
		}


		private JavaAnonymousClassInitializerExpressionSyntax ParseJavaAnonymousClassBody()
		{
			bool parseMembers = true;
			SyntaxListBuilder<MemberDeclarationSyntax> members = default(SyntaxListBuilder<MemberDeclarationSyntax>);
			try
			{
				var openBrace = this.EatToken(SyntaxKind.OpenBraceToken);

				// ignore members if missing type name or missing open curly
				if (openBrace.IsMissing)
				{
					parseMembers = false;
				}

				// even if we saw a { or think we should parse members bail out early since
				// we know namespaces can't be nested inside types
				if (parseMembers)
				{
					members = this._pool.Allocate<MemberDeclarationSyntax>();
					this.ParseJavaNormalClassMembers(ref members, ref openBrace, null);
				}

				var closeBrace = this.EatToken(SyntaxKind.CloseBraceToken);
				return _syntaxFactory.JavaAnonymousClassInitializerExpression(openBrace, members.ToList(), closeBrace);

			}
			finally
			{
				if (!members.IsNull)
				{
					this._pool.Free(members);
				}

			}
		}
	}
}