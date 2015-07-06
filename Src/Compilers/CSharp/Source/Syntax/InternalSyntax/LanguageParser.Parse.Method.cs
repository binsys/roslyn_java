// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading;

using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using System.Linq;
using System.Linq.Expressions;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
	internal partial class LanguageParser : SyntaxParser
	{



		private ConstructorDeclarationSyntax ParseConstructorDeclaration(string typeName, SyntaxListBuilder<AnnotationSyntax> attributes, SyntaxListBuilder modifiers, TypeParameterListSyntax typeParameterList)
		{
			var name = this.ParseIdentifierToken();
			Debug.Assert(name.ValueText == typeName);

			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsEndOfMethodSignature;
			try
			{
				var paramList = this.ParseParenthesizedParameterList(allowThisKeyword: false, allowDefaults: true, allowAttributes: true, allowFieldModifiers: false);


				JavaThrowsListClauseSyntax throws = this.ParseJavaThrowsListClause(true);

				ConstructorInitializerSyntax initializer = null;
				if (this.CurrentToken.Kind == SyntaxKind.ColonToken)
				{
					bool isStatic = modifiers != null && modifiers.Any(SyntaxKind.StaticKeyword);
					initializer = this.ParseConstructorInitializer(name.ValueText, isStatic);
				}

				BlockSyntax body;
				SyntaxToken semicolon;
				this.ParseBodyOrSemicolon(out body, out semicolon);

				return _syntaxFactory.ConstructorDeclaration(attributes, modifiers.ToTokenList(),typeParameterList, name, paramList, throws, initializer, body, semicolon);
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

		//private DestructorDeclarationSyntax ParseDestructorDeclaration(string typeName, SyntaxListBuilder<AnnotationSyntax> attributes, SyntaxListBuilder modifiers)
		//{
		//	Debug.Assert(this.CurrentToken.Kind == SyntaxKind.TildeToken);
		//	var tilde = this.EatToken(SyntaxKind.TildeToken);

		//	var name = this.ParseIdentifierToken();
		//	if (name.ValueText != typeName)
		//	{
		//		name = this.AddError(name, ErrorCode.ERR_BadDestructorName);
		//	}

		//	var openParen = this.EatToken(SyntaxKind.OpenParenToken);
		//	var closeParen = this.EatToken(SyntaxKind.CloseParenToken);

		//	BlockSyntax body;
		//	SyntaxToken semicolon;
		//	this.ParseBodyOrSemicolon(out body, out semicolon);

		//	var parameterList = _syntaxFactory.ParameterList(openParen, default(SeparatedSyntaxList<ParameterSyntax>), closeParen);
		//	return _syntaxFactory.DestructorDeclaration(attributes, modifiers.ToTokenList(), tilde, name, parameterList, body, semicolon);
		//}

		private void ParseBodyOrSemicolon(out BlockSyntax body, out SyntaxToken semicolon)
		{
			if (this.CurrentToken.Kind == SyntaxKind.OpenBraceToken)
			{
				body = this.ParseBlock(isMethodBody: true);

				semicolon = null;
				if (this.CurrentToken.Kind == SyntaxKind.SemicolonToken)
				{
					semicolon = this.EatToken(SyntaxKind.SemicolonToken);
					//semicolon = this.EatTokenWithPrejudice(ErrorCode.ERR_UnexpectedSemicolon);
				}
			}
			else
			{
				semicolon = this.EatToken(SyntaxKind.SemicolonToken);
				body = null;
			}
		}

		private JavaInitializerMethodDeclarationSyntax ParseJavaInitializerMethodDeclaration(SyntaxListBuilder modifiers)
		{
			//InstanceInitializer:
			//	Block
			//StaticInitializer:
			//	static Block

			SyntaxToken staticKeyword = default(SyntaxToken);

			var modifiersTokens = modifiers.ToTokenList();


			//if (modifiersTokens.Count > 0)
			//{
			//	var token = modifiersTokens[0];
			//	if (token.Kind != SyntaxKind.StaticKeyword)
			//	{
			//		token = this.AddError(token, ErrorCode.ERR_BadModifierLocation);
			//		//token = this.ConvertToMissingWithTrailingTrivia(token, SyntaxKind.StaticKeyword);
			//	}

			//	staticKeyword = token;
			//}

			bool haveStatic = modifiersTokens.Any(SyntaxKind.StaticKeyword);

			//有static 且 修饰符就一个
			if (haveStatic && modifiersTokens.Count == 1)
			{
				staticKeyword = modifiersTokens[0];
			}
			else if (haveStatic && modifiersTokens.Count != 1)
			{
				int staticIndex = -1;

				for (int i = 0; i < modifiersTokens.Count; i++)
				{
					if (modifiersTokens[i].Kind == SyntaxKind.StaticKeyword)
					{
						staticIndex = i;
						break;
					}
				}
				staticKeyword = modifiersTokens[staticIndex];

				for (int i = 0; i < modifiersTokens.Count; i++)
				{
					if (i != staticIndex)
					{
						if (i < staticIndex)
						{
							staticKeyword = this.AddLeadingSkippedSyntax(staticKeyword, this.AddErrorToFirstToken(modifiersTokens[i], ErrorCode.ERR_BadModifierLocation));
						}
						else if (i > staticIndex)
						{
							staticKeyword = this.AddTrailingSkippedSyntax(staticKeyword, this.AddErrorToFirstToken(modifiersTokens[i], ErrorCode.ERR_BadModifierLocation));
						}
					}
				}

			}
			else if (!haveStatic && modifiersTokens.Count != 0)
			{
				staticKeyword = Syntax.InternalSyntax.SyntaxFactory.MissingToken(SyntaxKind.StaticKeyword);
				foreach (SyntaxToken t in modifiersTokens)
				{
					staticKeyword = this.AddLeadingSkippedSyntax(staticKeyword, this.AddErrorToFirstToken(t, ErrorCode.ERR_BadModifierLocation));
				}
			}

			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsEndOfMethodSignature;
			try
			{
				if (this.CurrentToken.Kind == SyntaxKind.OpenBraceToken)
				{
					BlockSyntax body = this.ParseBlock(isMethodBody: true);

					return _syntaxFactory.JavaInitializerMethodDeclaration(staticKeyword, body);
				}
			}
			finally
			{
				this._termState = saveTerm;
			}
			return null;
		}

		private MethodDeclarationSyntax ParseMethodDeclaration(
			SyntaxListBuilder<AnnotationSyntax> attributes,
			SyntaxListBuilder modifiers,
			TypeSyntax type,
			SyntaxToken identifier,
			TypeParameterListSyntax typeParameterList,bool isDtor)
		{
			// Parse the name (it could be qualified)
			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsEndOfMethodSignature;

			var paramList = this.ParseParenthesizedParameterList(allowThisKeyword: true, allowDefaults: true,
				allowAttributes: true, allowFieldModifiers: true);

			JavaThrowsListClauseSyntax throws = this.ParseJavaThrowsListClause(true);


			if (this.CurrentToken.Kind == SyntaxKind.ColonToken)
			{
				// Use else if, rather than if, because if we see both a constructor initializer and a constraint clause, we're too lost to recover.
				var colonToken = this.CurrentToken;
				// Set isStatic to false because pretending we're in a static constructor will just result in more errors.
				ConstructorInitializerSyntax initializer = this.ParseConstructorInitializer(identifier.ValueText, isStatic: false);
				initializer = this.AddErrorToFirstToken(initializer, ErrorCode.ERR_UnexpectedCharacter, colonToken.Text);
					//CONSIDER: better error code?
				paramList = AddTrailingSkippedSyntax(paramList, initializer);

				// CONSIDER: Parsing an invalid constructor initializer could, conceivably, get us way
				// off track.  If this becomes a problem, an alternative approach would be to generalize
				// EatTokenWithPrejudice in such a way that we can just skip everything until we recognize
				// our context again (perhaps an open brace).
			}


			this._termState = saveTerm;

			BlockSyntax body;
			SyntaxToken semicolon;

			this.ParseBodyOrSemicolon(out body, out semicolon);


			return
				_syntaxFactory.MethodDeclaration(isDtor ? SyntaxKind.DestructorDeclaration : SyntaxKind.MethodDeclaration,
					attributes,
					modifiers.ToTokenList(),
					typeParameterList,
					type,
					identifier,
					paramList,
					throws,
					body,
					semicolon);

		}

		private JavaThrowsListClauseSyntax ParseJavaThrowsListClause(bool allowArguments)
		{
			if (this.CurrentToken.Kind != SyntaxKind.ThrowsKeyword)
			{
				return null;
			}

			var implementsKeyword = this.EatToken(SyntaxKind.ThrowsKeyword);
			var list = _pool.AllocateSeparated<TypeSyntax>();
			try
			{
				// first type
				if (this.IsPossibleTypeParameterConstraintClauseStart())
				{
					list.Add(this.AddError(this.CreateMissingIdentifierName(), ErrorCode.ERR_TypeExpected));
				}
				else
				{
					TypeSyntax firstType = this.ParseDeclarationType(isConstraint: false, parentIsParameter: false);

					if (allowArguments && this.CurrentToken.Kind == SyntaxKind.OpenParenToken)
					{
						firstType = _syntaxFactory.BaseClassWithArguments(firstType, this.ParseParenthesizedArgumentList());
					}

					list.Add(firstType);

					// any additional types
					while (true)
					{
						if (this.CurrentToken.Kind == SyntaxKind.OpenBraceToken
							|| this.IsPossibleTypeParameterConstraintClauseStart())
						{
							break;
						}
						else if (this.CurrentToken.Kind == SyntaxKind.CommaToken || this.IsPossibleType())
						{
							list.AddSeparator(this.EatToken(SyntaxKind.CommaToken));
							if (this.IsPossibleTypeParameterConstraintClauseStart())
							{
								list.Add(this.AddError(this.CreateMissingIdentifierName(), ErrorCode.ERR_TypeExpected));
							}
							else
							{
								list.Add(this.ParseDeclarationType(isConstraint: false, parentIsParameter: false));
							}

							continue;
						}
						else if (this.SkipBadBaseListTokens(ref implementsKeyword, list, SyntaxKind.CommaToken) == PostSkipAction.Abort)
						{
							break;
						}
					}
				}

				return _syntaxFactory.JavaThrowsListClause(implementsKeyword, list);
			}
			finally
			{
				this._pool.Free(list);
			}
		}

	}
}