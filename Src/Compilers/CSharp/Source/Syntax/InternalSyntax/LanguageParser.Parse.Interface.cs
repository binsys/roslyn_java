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
		private JavaNormalInterfaceDeclarationSyntax ParseJavaNormalInterfaceDeclaration(SyntaxListBuilder<AnnotationSyntax> attributes, SyntaxListBuilder modifiers)
		{
			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.InterfaceKeyword);

			var interfaceKeyword = this.EatToken();

			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsPossibleAggregateClauseStartOrStop;
			var name = this.ParseIdentifierToken();
			var typeParameters = this.ParseTypeParameterList();

			this._termState = saveTerm;
			var implementsList = this.ParseExtendsListClause(false);

			// Parse class body
			bool parseMembers = true;
			SyntaxListBuilder<MemberDeclarationSyntax> members = default(SyntaxListBuilder<MemberDeclarationSyntax>);
			var constraints = default(SyntaxListBuilder<TypeBoundSyntax>);
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

				var closeBrace = this.EatToken(SyntaxKind.CloseBraceToken);
				SyntaxToken semicolon = null;
				if (this.CurrentToken.Kind == SyntaxKind.SemicolonToken)
				{
					semicolon = this.EatToken();
				}


				var ret = _syntaxFactory.JavaNormalInterfaceDeclaration(
					this.CreateJavaMemberModifierSyntax(attributes, modifiers),
					interfaceKeyword,
					name,
					typeParameters,
					implementsList, 
					openBrace,
					members,
					closeBrace,
					semicolon);

				return ret;
			}
			finally
			{
				if (!members.IsNull)
				{
					this._pool.Free(members);
				}

				if (!constraints.IsNull)
				{
					this._pool.Free(constraints);
				}
			}
		}
	
	
	
	}


}