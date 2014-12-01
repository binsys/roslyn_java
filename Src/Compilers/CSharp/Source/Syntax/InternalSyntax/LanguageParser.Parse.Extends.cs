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
		private ExtendsClauseSyntax ParseExtendsClause()
		{
			if (this.CurrentToken.Kind != SyntaxKind.ExtendsKeyword)
			{
				return null;
			}

			var extends = this.EatToken(SyntaxKind.ExtendsKeyword);

			TypeSyntax firstType = this.ParseDeclarationType(isConstraint: false, parentIsParameter: false);


			return _syntaxFactory.ExtendsClause(extends, firstType);
		}


		private ExtendsListClauseSyntax ParseExtendsListClause(bool allowArguments)
		{

			if (this.CurrentToken.Kind != SyntaxKind.ExtendsKeyword)
			{
				return null;
			}

			var extendsKeywordToken = this.EatToken(SyntaxKind.ExtendsKeyword);
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
						else if (this.SkipBadBaseListTokens(ref extendsKeywordToken, list, SyntaxKind.CommaToken) == PostSkipAction.Abort)
						{
							break;
						}
					}
				}


				return _syntaxFactory.ExtendsListClause(extendsKeywordToken, list);
			}
			finally
			{
				this._pool.Free(list);
			}
		}

	}
}