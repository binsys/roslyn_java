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
		private ImplementsListClauseSyntax ParseImplementsListClause(bool allowArguments)
		{
			if (this.CurrentToken.Kind != SyntaxKind.ImplementsKeyword)
			{
				return null;
			}

			var implementsKeyword = this.EatToken(SyntaxKind.ImplementsKeyword);
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

				return _syntaxFactory.ImplementsListClause(implementsKeyword, list);
			}
			finally
			{
				this._pool.Free(list);
			}
		}
	}
}