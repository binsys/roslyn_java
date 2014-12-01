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
		#region ParameterList 形参列表


		internal ParameterListSyntax ParseParenthesizedParameterList(bool allowThisKeyword, bool allowDefaults, bool allowAttributes, bool allowFieldModifiers)
		{
			if (this.IsIncrementalAndFactoryContextMatches && CanReuseParameterList(this.CurrentNode as CSharp.Syntax.ParameterListSyntax))
			{
				return (ParameterListSyntax)this.EatNode();
			}

			var parameters = this._pool.AllocateSeparated<ParameterSyntax>();

			try
			{
				var openKind = SyntaxKind.OpenParenToken;
				var closeKind = SyntaxKind.CloseParenToken;

				SyntaxToken open;
				SyntaxToken close;
				this.ParseParameterList(out open, parameters, out close, openKind, closeKind, allowThisKeyword, allowDefaults, allowAttributes, allowFieldModifiers);
				return _syntaxFactory.ParameterList(open, parameters, close);
			}
			finally
			{
				this._pool.Free(parameters);
			}
		}

		internal BracketedParameterListSyntax ParseBracketedParameterList(bool allowDefaults = true)
		{
			if (this.IsIncrementalAndFactoryContextMatches && CanReuseBracketedParameterList(this.CurrentNode as CSharp.Syntax.BracketedParameterListSyntax))
			{
				return (BracketedParameterListSyntax)this.EatNode();
			}

			var parameters = this._pool.AllocateSeparated<ParameterSyntax>();

			try
			{
				var openKind = SyntaxKind.OpenBracketToken;
				var closeKind = SyntaxKind.CloseBracketToken;

				SyntaxToken open;
				SyntaxToken close;
				this.ParseParameterList(out open, parameters, out close, openKind, closeKind, allowThisKeyword: false, allowDefaults: allowDefaults, allowAttributes: true, allowFieldModifiers: false);
				return _syntaxFactory.BracketedParameterList(open, parameters, close);
			}
			finally
			{
				this._pool.Free(parameters);
			}
		}

		private void ParseParameterList(
			out SyntaxToken open,
			SeparatedSyntaxListBuilder<ParameterSyntax> nodes,
			out SyntaxToken close,
			SyntaxKind openKind,
			SyntaxKind closeKind,
			bool allowThisKeyword,
			bool allowDefaults,
			bool allowAttributes,
			bool allowFieldModifiers)
		{
			open = this.EatToken(openKind);

			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsEndOfParameterList;

			var attributes = this._pool.Allocate<AnnotationSyntax>();
			var modifiers = this._pool.Allocate();
			try
			{
				if (this.CurrentToken.Kind != closeKind)
				{
				tryAgain:
					int mustBeLastIndex = -1;
					bool mustBeLastHadParams = false;
					bool hasParams = false;
					bool hasArgList = false;

					if (this.IsPossibleParameter(allowThisKeyword, allowFieldModifiers) || this.CurrentToken.Kind == SyntaxKind.CommaToken)
					{
						// first parameter
						attributes.Clear();
						modifiers.Clear();
						var parameter = this.ParseParameter(attributes, modifiers, allowThisKeyword, allowDefaults, allowAttributes, allowFieldModifiers);
						nodes.Add(parameter);
						hasParams = modifiers.Any(SyntaxKind.ParamsKeyword);
						hasArgList = parameter.Identifier.Kind == SyntaxKind.ArgListKeyword;
						bool mustBeLast = hasParams || hasArgList;
						if (mustBeLast && mustBeLastIndex == -1)
						{
							mustBeLastIndex = nodes.Count - 1;
							mustBeLastHadParams = hasParams;
						}

						// additional parameters
						while (true)
						{
							if (this.CurrentToken.Kind == closeKind)
							{
								break;
							}
							else if (this.CurrentToken.Kind == SyntaxKind.CommaToken || this.IsPossibleParameter(allowThisKeyword, allowFieldModifiers))
							{
								nodes.AddSeparator(this.EatToken(SyntaxKind.CommaToken));
								attributes.Clear();
								modifiers.Clear();
								parameter = this.ParseParameter(attributes, modifiers, allowThisKeyword, allowDefaults, allowAttributes, allowFieldModifiers);
								nodes.Add(parameter);
								hasParams = modifiers.Any(SyntaxKind.ParamsKeyword);
								hasArgList = parameter.Identifier.Kind == SyntaxKind.ArgListKeyword;
								mustBeLast = hasParams || hasArgList;
								if (mustBeLast && mustBeLastIndex == -1)
								{
									mustBeLastIndex = nodes.Count - 1;
									mustBeLastHadParams = hasParams;
								}

								continue;
							}
							else if (this.SkipBadParameterListTokens(ref open, nodes, SyntaxKind.CommaToken, closeKind, allowThisKeyword, allowFieldModifiers) == PostSkipAction.Abort)
							{
								break;
							}
						}
					}
					else if (this.SkipBadParameterListTokens(ref open, nodes, SyntaxKind.IdentifierToken, closeKind, allowThisKeyword, allowFieldModifiers) == PostSkipAction.Continue)
					{
						goto tryAgain;
					}

					if (mustBeLastIndex >= 0 && mustBeLastIndex < nodes.Count - 1)
					{
						nodes[mustBeLastIndex] = this.AddError(nodes[mustBeLastIndex], mustBeLastHadParams ? ErrorCode.ERR_ParamsLast : ErrorCode.ERR_VarargsLast);
					}
				}

				this._termState = saveTerm;
				close = this.EatToken(closeKind);
			}
			finally
			{
				this._pool.Free(modifiers);
				this._pool.Free(attributes);
			}
		}

		private ParameterSyntax ParseParameter(
			SyntaxListBuilder<AnnotationSyntax> attributes,
			SyntaxListBuilder modifiers,
			bool allowThisKeyword,
			bool allowDefaults,
			bool allowAttributes,
			bool allowFieldModifiers)
		{
			if (this.IsIncrementalAndFactoryContextMatches && CanReuseParameter(this.CurrentNode as CSharp.Syntax.ParameterSyntax, attributes, modifiers))
			{
				return (ParameterSyntax)this.EatNode();
			}

			this.ParseAnnotationDeclarations(attributes, allowAttributes);
			this.ParseParameterModifiers(modifiers, allowThisKeyword, allowFieldModifiers);

			var hasArgList = this.CurrentToken.Kind == SyntaxKind.ArgListKeyword;

			TypeSyntax type = null;
			if (!hasArgList)
			{
				type = this.ParseType(true);
			}
			else if (this.IsPossibleType())
			{
				type = this.ParseType(true);
				type = WithAdditionalDiagnostics(type, this.GetExpectedTokenError(SyntaxKind.CloseParenToken, SyntaxKind.IdentifierToken, 0, type.Width));
			}

			SyntaxToken name = null;
			if (!hasArgList)
			{
				name = this.ParseIdentifierToken();

				// When the user type "int foo[]", give them a useful error
				if (this.CurrentToken.Kind == SyntaxKind.OpenBracketToken && this.PeekToken(1).Kind == SyntaxKind.CloseBracketToken)
				{
					var open = this.EatToken();
					var close = this.EatToken();
					open = this.AddError(open, ErrorCode.ERR_BadArraySyntax);
					name = AddTrailingSkippedSyntax(name, SyntaxList.List(open, close));
				}
			}
			else if (this.IsPossibleName())
			{
				// Current token is an identifier token, we expected a CloseParenToken.
				// Get the expected token error for the missing token with correct diagnostic
				// span and then parse the identifier token.

				SyntaxDiagnosticInfo diag = this.GetExpectedTokenError(SyntaxKind.CloseParenToken, SyntaxKind.IdentifierToken);
				name = this.ParseIdentifierToken();
				name = WithAdditionalDiagnostics(name, diag);
			}
			else
			{
				// name is not optional on ParameterSyntax
				name = this.EatToken(SyntaxKind.ArgListKeyword);
			}

			EqualsValueClauseSyntax def = null;
			if (this.CurrentToken.Kind == SyntaxKind.EqualsToken)
			{
				var equals = this.EatToken(SyntaxKind.EqualsToken);
				var expr = this.ParseExpression();
				def = _syntaxFactory.EqualsValueClause(equals, expr);

				if (!allowDefaults)
				{
					def = this.AddError(def, equals, ErrorCode.ERR_DefaultValueNotAllowed);
				}
				else
				{
					def = CheckFeatureAvailability(def, MessageID.IDS_FeatureOptionalParameter);
				}
			}

			return _syntaxFactory.Parameter(attributes, modifiers.ToTokenList(), type, name, def);
		}



		private void ParseParameterModifiers(SyntaxListBuilder modifiers, bool allowThisKeyword, bool allowFieldModifiers)
		{
			var flags = ParamFlags.None;
			SyntaxModifier fieldMods = 0;
			bool seenNoDuplicates = true;
			bool seenNoAccessibilityDuplicates = true;

			while (true)
			{
				if (IsParameterModifier(this.CurrentToken.Kind, allowThisKeyword))
				{
					var mod = this.EatToken();

					if (mod.Kind == SyntaxKind.ThisKeyword ||
						mod.Kind == SyntaxKind.ParamsKeyword)
					{
						if (mod.Kind == SyntaxKind.ThisKeyword)
						{
							mod = CheckFeatureAvailability(mod, MessageID.IDS_FeatureExtensionMethod);

							if ((flags & ParamFlags.This) != 0)
							{
								mod = this.AddError(mod, ErrorCode.ERR_DupParamMod, SyntaxKindFacts.GetText(SyntaxKind.ThisKeyword));
							}
							else if ((flags & ParamFlags.Out) != 0)
							{
								mod = this.AddError(mod, ErrorCode.ERR_BadOutWithThis);
							}
							else if ((flags & ParamFlags.Ref) != 0)
							{
								mod = this.AddError(mod, ErrorCode.ERR_BadRefWithThis);
							}
							else if ((flags & ParamFlags.Params) != 0)
							{
								mod = this.AddError(mod, ErrorCode.ERR_BadParamModThis);
							}
							else
							{
								flags |= ParamFlags.This;
							}
						}
						else if (mod.Kind == SyntaxKind.ParamsKeyword)
						{
							if ((flags & ParamFlags.Params) != 0)
							{
								mod = this.AddError(mod, ErrorCode.ERR_DupParamMod, SyntaxKindFacts.GetText(SyntaxKind.ParamsKeyword));
							}
							else if ((flags & ParamFlags.This) != 0)
							{
								mod = this.AddError(mod, ErrorCode.ERR_BadParamModThis);
							}
							else if ((flags & (ParamFlags.Ref | ParamFlags.Out | ParamFlags.This)) != 0)
							{
								mod = this.AddError(mod, ErrorCode.ERR_MultiParamMod);
							}
							else
							{
								flags |= ParamFlags.Params;
							}
						}
					}

					modifiers.Add(mod);
					continue;
				}

				if (allowFieldModifiers)
				{
					var newFieldMod = GetFieldModifier(this.CurrentToken);
					if (newFieldMod != SyntaxModifier.None)
					{
						var modTok = this.EatToken();

						if (newFieldMod == SyntaxModifier.Static)
						{
							if ((fieldMods & SyntaxModifier.Static) == 0)
							{
								modTok = this.AddError(modTok, ErrorCode.ERR_StaticParamMod);
							}
						}
						else
						{
							ReportDuplicateModifiers(ref modTok, newFieldMod, fieldMods, ref seenNoDuplicates, ref seenNoAccessibilityDuplicates);
						}

						fieldMods |= newFieldMod;

						modifiers.Add(modTok);
						continue;
					}
				}

				break;
			}

			if (fieldMods != 0 && fieldMods != SyntaxModifier.Static && ((fieldMods & AccessModifiers) == 0 || (flags & (ParamFlags.Ref | ParamFlags.Out)) != 0))
			{
				for (int i = 0; i < modifiers.Count; i++)
				{
					var mod = GetFieldModifier((SyntaxToken)modifiers[i]);

					if (mod != SyntaxModifier.None && mod != SyntaxModifier.Static)
					{
						Debug.Assert((fieldMods & AccessModifiers) == 0 || (flags & (ParamFlags.Ref | ParamFlags.Out)) != 0);

						if ((flags & (ParamFlags.Ref | ParamFlags.Out)) != 0)
						{
							modifiers[i] = this.AddError(modifiers[i], ErrorCode.ERR_RefOutParameterWithFieldModifier);
						}
						else if ((fieldMods & AccessModifiers) == 0)
						{
							modifiers[i] = this.AddError(modifiers[i], ErrorCode.ERR_ParamMissingAccessMod);
						}

						break;
					}
				}
			}
		}

		#endregion
	}
}