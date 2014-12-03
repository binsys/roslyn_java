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
		private void ParseAnnotationDeclarations(SyntaxListBuilder list, bool allowAttributes = true)
		{
			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsAttributeDeclarationTerminator;
			while (this.IsPossibleAnnotationSyntax())
			{
				var section = this.ParseAnnotationDeclaration();
				if (!allowAttributes)
				{
					section = this.AddError(section, ErrorCode.ERR_AttributesNotAllowed);
				}

				list.Add(section);
			}

			this._termState = saveTerm;
		}

		private AnnotationSyntax ParseAnnotationDeclaration()
		{
			if (this.IsIncrementalAndFactoryContextMatches && this.CurrentNodeKind == SyntaxKind.Annotation)
			{
				return (AnnotationSyntax)this.EatNode();
			}

			var atToken = this.EatToken(SyntaxKind.AtToken);
			var name = this.ParseQualifiedName();
			var argList = this.ParseAnnotationArgumentList();
			return _syntaxFactory.Annotation(atToken, name, argList);

		}

		internal AnnotationArgumentListSyntax ParseAnnotationArgumentList()
		{
			if (this.IsIncrementalAndFactoryContextMatches && this.CurrentNodeKind == SyntaxKind.AnnotationArgumentList)
			{
				return (AnnotationArgumentListSyntax)this.EatNode();
			}

			AnnotationArgumentListSyntax argList = null;
			if (this.CurrentToken.Kind == SyntaxKind.OpenParenToken)
			{
				var openParen = this.EatToken(SyntaxKind.OpenParenToken);
				var argNodes = this._pool.AllocateSeparated<AnnotationArgumentSyntax>();
				try
				{
					bool shouldHaveName = false;
				tryAgain:
					if (this.CurrentToken.Kind != SyntaxKind.CloseParenToken)
					{
						if (this.IsPossibleAttributeArgument() || this.CurrentToken.Kind == SyntaxKind.CommaToken)
						{
							// first argument
							argNodes.Add(this.ParseAnnotationArgument(ref shouldHaveName));

							// comma + argument or end?
							while (true)
							{
								if (this.CurrentToken.Kind == SyntaxKind.CloseParenToken)
								{
									break;
								}
								else if (this.CurrentToken.Kind == SyntaxKind.CommaToken || this.IsPossibleAttributeArgument())
								{
									argNodes.AddSeparator(this.EatToken(SyntaxKind.CommaToken));
									argNodes.Add(this.ParseAnnotationArgument(ref shouldHaveName));
								}
								else if (this.SkipBadAttributeArgumentTokens(ref openParen, argNodes, SyntaxKind.CommaToken) == PostSkipAction.Abort)
								{
									break;
								}
							}
						}
						else if (this.SkipBadAttributeArgumentTokens(ref openParen, argNodes, SyntaxKind.IdentifierToken) == PostSkipAction.Continue)
						{
							goto tryAgain;
						}
					}

					var closeParen = this.EatToken(SyntaxKind.CloseParenToken);
					argList = _syntaxFactory.AnnotationArgumentList(openParen, argNodes, closeParen);
				}
				finally
				{
					this._pool.Free(argNodes);
				}
			}

			return argList;
		}

		private AnnotationArgumentSyntax ParseAnnotationArgument(ref bool shouldHaveName)
		{
			// Need to parse both "real" named arguments and _annotation-style named arguments.
			// We track _annotation-style named arguments only with fShouldHaveName.

			NameEqualsSyntax nameEquals = null;
			NameColonSyntax nameColon = null;
			if (this.CurrentToken.Kind == SyntaxKind.IdentifierToken)
			{
				SyntaxKind nextTokenKind = this.PeekToken(1).Kind;
				switch (nextTokenKind)
				{
					case SyntaxKind.EqualsToken:
						{
							var name = this.ParseIdentifierToken();
							var equals = this.EatToken(SyntaxKind.EqualsToken);
							nameEquals = _syntaxFactory.NameEquals(_syntaxFactory.IdentifierName(name), equals);
							shouldHaveName = true;
						}

						break;
					case SyntaxKind.ColonToken:
						{
							var name = this.ParseIdentifierName();
							var colonToken = this.EatToken(SyntaxKind.ColonToken);
							nameColon = _syntaxFactory.NameColon(name, colonToken);
							nameColon = CheckFeatureAvailability(nameColon, MessageID.IDS_FeatureNamedArgument);
						}

						break;
				}
			}

			var expr = this.ParseExpression();

			// Not named -- give an error if it's supposed to be
			if (shouldHaveName && nameEquals == null)
			{
				expr = this.AddError(expr, ErrorCode.ERR_NamedArgumentExpected);
			}

			return _syntaxFactory.AnnotationArgument(nameEquals, nameColon, expr);
		}

		private AnnotationCreationExpressionSyntax ParseAnnotationCreationExpression()
		{
			var anno = this.ParseAnnotationDeclaration();
			return _syntaxFactory.AnnotationCreationExpression(anno);
		}
	}
}