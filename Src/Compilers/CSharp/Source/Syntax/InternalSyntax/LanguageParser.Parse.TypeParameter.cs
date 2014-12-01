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
		#region TypeParameter

		private TypeParameterListSyntax ParseTypeParameterList()
		{
			if (this.CurrentToken.Kind != SyntaxKind.LessThanToken)
			{
				return null;
			}

			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsEndOfTypeParameterList;
			try
			{
				var parameters = this._pool.AllocateSeparated<TypeParameterSyntax>();
				var open = this.EatToken(SyntaxKind.LessThanToken);
				open = CheckFeatureAvailability(open, MessageID.IDS_FeatureGenerics);

				// first parameter
				parameters.Add(this.ParseTypeParameter());

				// remaining parameter & commas
				while (true)
				{
					if (this.CurrentToken.Kind == SyntaxKind.GreaterThanToken || this.IsPossibleTypeParameterConstraintClauseStart())
					{
						break;
					}
					else if (this.CurrentToken.Kind == SyntaxKind.CommaToken)
					{
						parameters.AddSeparator(this.EatToken(SyntaxKind.CommaToken));
						parameters.Add(this.ParseTypeParameter());
					}
					else if (this.SkipBadTypeParameterListTokens(parameters, SyntaxKind.CommaToken) == PostSkipAction.Abort)
					{
						break;
					}
				}

				var close = this.EatToken(SyntaxKind.GreaterThanToken);

				return _syntaxFactory.TypeParameterList(open, parameters, close);
			}
			finally
			{
				this._termState = saveTerm;
			}
		}



		private TypeParameterSyntax ParseTypeParameter()
		{
			//if (this.IsPossibleTypeParameterConstraintClauseStart())
			//{
			//	return _syntaxFactory.TypeParameter(
			//		default(SyntaxList<AnnotationSyntax>),
			//		this.AddError(CreateMissingIdentifierToken(),ErrorCode.ERR_IdentifierExpected));
			//}

			var attrs = this._pool.Allocate<AnnotationSyntax>();
			var additionalBound = this._pool.Allocate<AdditionalTypeConstraintSyntax>();
			try
			{
				if (this.IsPossibleAnnotationSyntax())
				{
					var saveTerm = this._termState;
					this._termState = TerminatorState.IsEndOfTypeArgumentList;
					this.ParseAnnotationDeclarations(attrs);
					this._termState = saveTerm;
				}

				var identity = this.ParseIdentifierToken();

				TypeBoundSyntax typeBound = default(TypeBoundSyntax);

				if (this.CurrentToken.Kind == SyntaxKind.ExtendsKeyword)
				{
					var extendsKeyword = this.EatToken(SyntaxKind.ExtendsKeyword);
					var boundedTypeType = this.ParseDeclarationType(true, false);
					var boundedType = _syntaxFactory.TypeConstraint(boundedTypeType);

					while (true)
					{
						if (this.CurrentToken.Kind != SyntaxKind.AmpersandToken)
						{
							break;
						}
						var ampersandToken = this.EatToken(SyntaxKind.AmpersandToken);

						var additionalTypeType = this.ParseDeclarationType(true, false);
						additionalBound.Add(_syntaxFactory.AdditionalTypeConstraint(ampersandToken, additionalTypeType));
					}
					typeBound = _syntaxFactory.TypeBound(extendsKeyword, boundedType, additionalBound);

				}


				return
					_syntaxFactory.TypeParameter(
						attrs.Count == 0 ? null : _syntaxFactory.JavaTypeParameterModifier(attrs),
						identity, typeBound);
			}
			finally
			{
				this._pool.Free(attrs);
				this._pool.Free(additionalBound);
			}
		}

		#endregion
	}
}