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

		// Parsing rule terminating conditions.  This is how we know if it is 
		// okay to abort the current parsing rule when unexpected tokens occur.
		[Flags]
		internal enum TerminatorState
		{
			EndOfFile = 0,
			IsPackageMemberStartOrStop = 1 << 0,
			IsAttributeDeclarationTerminator = 1 << 1,
			IsPossibleAggregateClauseStartOrStop = 1 << 2,
			IsPossibleMemberStartOrStop = 1 << 3,
			IsEndOfReturnType = 1 << 4,
			IsEndOfParameterList = 1 << 5,
			IsEndOfFieldDeclaration = 1 << 6,
			IsPossibleEndOfVariableDeclaration = 1 << 7,
			IsEndOfTypeArgumentList = 1 << 8,
			IsPossibleStatementStartOrStop = 1 << 9,
			//IsEndOfFixedStatement = 1 << 10,
			IsEndOfTryBlock = 1 << 11,
			IsEndOfCatchClause = 1 << 12,
			IsEndOfilterClause = 1 << 13,
			IsEndOfCatchBlock = 1 << 14,
			IsEndOfDoWhileExpression = 1 << 15,
			IsEndOfForStatementArgument = 1 << 16,
			IsEndOfDeclarationClause = 1 << 17,
			IsEndOfArgumentList = 1 << 18,
			IsSwitchSectionStart = 1 << 19,
			IsEndOfTypeParameterList = 1 << 20,
			IsEndOfMethodSignature = 1 << 21,
			IsEndOfNameInExplicitInterface = 1 << 22,
		}

		private const int LastTerminatorState = (int)TerminatorState.IsEndOfNameInExplicitInterface;

		private bool IsTerminator()
		{
			if (this.CurrentToken.Kind == SyntaxKind.EndOfFileToken)
			{
				return true;
			}

			for (int i = 1; i <= LastTerminatorState; i <<= 1)
			{
				TerminatorState isolated = _termState & (TerminatorState)i;
				if (isolated != 0)
				{
					switch (isolated)
					{
						case TerminatorState.IsPackageMemberStartOrStop:
							if (this.IsNamespaceMemberStartOrStop())
							{
								return true;
							}

							break;
						case TerminatorState.IsAttributeDeclarationTerminator:
							if (this.IsAttributeDeclarationTerminator())
							{
								return true;
							}

							break;
						case TerminatorState.IsPossibleAggregateClauseStartOrStop:
							if (this.IsPossibleAggregateClauseStartOrStop())
							{
								return true;
							}

							break;
						case TerminatorState.IsPossibleMemberStartOrStop:
							if (this.IsPossibleMemberStartOrStop())
							{
								return true;
							}

							break;
						case TerminatorState.IsEndOfReturnType:
							if (this.IsEndOfReturnType())
							{
								return true;
							}

							break;
						case TerminatorState.IsEndOfParameterList:
							if (this.IsEndOfParameterList())
							{
								return true;
							}

							break;
						case TerminatorState.IsEndOfFieldDeclaration:
							if (this.IsEndOfFieldDeclaration())
							{
								return true;
							}

							break;
						case TerminatorState.IsPossibleEndOfVariableDeclaration:
							if (this.IsPossibleEndOfVariableDeclaration())
							{
								return true;
							}

							break;
						case TerminatorState.IsEndOfTypeArgumentList:
							if (this.IsEndOfTypeArgumentList())
							{
								return true;
							}

							break;
						case TerminatorState.IsPossibleStatementStartOrStop:
							if (this.IsPossibleStatementStartOrStop())
							{
								return true;
							}

							break;
						case TerminatorState.IsEndOfTryBlock:
							if (this.IsEndOfTryBlock())
							{
								return true;
							}

							break;
						case TerminatorState.IsEndOfCatchClause:
							if (this.IsEndOfCatchClause())
							{
								return true;
							}

							break;
						case TerminatorState.IsEndOfilterClause:
							if (this.IsEndOfFilterClause())
							{
								return true;
							}

							break;
						case TerminatorState.IsEndOfCatchBlock:
							if (this.IsEndOfCatchBlock())
							{
								return true;
							}

							break;
						case TerminatorState.IsEndOfDoWhileExpression:
							if (this.IsEndOfDoWhileExpression())
							{
								return true;
							}

							break;
						case TerminatorState.IsEndOfForStatementArgument:
							if (this.IsEndOfForStatementArgument())
							{
								return true;
							}

							break;
						case TerminatorState.IsEndOfDeclarationClause:
							if (this.IsEndOfDeclarationClause())
							{
								return true;
							}

							break;
						case TerminatorState.IsEndOfArgumentList:
							if (this.IsEndOfArgumentList())
							{
								return true;
							}

							break;
						case TerminatorState.IsSwitchSectionStart:
							if (this.IsPossibleSwitchSection())
							{
								return true;
							}

							break;

						case TerminatorState.IsEndOfTypeParameterList:
							if (this.IsEndOfTypeParameterList())
							{
								return true;
							}

							break;

						case TerminatorState.IsEndOfMethodSignature:
							if (this.IsEndOfMethodSignature())
							{
								return true;
							}

							break;

						case TerminatorState.IsEndOfNameInExplicitInterface:
							if (this.IsEndOfNameInExplicitInterface())
							{
								return true;
							}

							break;
					}
				}
			}

			return false;
		}
	}
}