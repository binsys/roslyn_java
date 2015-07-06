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


		// Returns null if we can't parse anything (even partially).
		private MemberDeclarationSyntax ParseMemberDeclaration(SyntaxKind parentKind, string typeName = null)
		{

			CancellationToken.ThrowIfCancellationRequested();

			// don't reuse members if they were previously declared under a different type keyword kind
			// don't reuse existing constructors & destructors because they have to match typename errors
			// don't reuse methods whose name matches the new type name (they now match as possible constructors)
			if (this.IsIncrementalAndFactoryContextMatches)
			{
				var member = this.CurrentNode as CSharp.Syntax.MemberDeclarationSyntax;
				if (CanReuseMemberDeclaration(member, typeName) || CanReuseTypeDeclaration(member))
				{
					return (MemberDeclarationSyntax)this.EatNode();
				}
			}

			var annotations = this._pool.Allocate<AnnotationSyntax>();
			var modifiers = this._pool.Allocate();

			var saveTermState = this._termState;

			try
			{
				
				this.ParseAnnotationDeclarations(annotations);

				// All modifiers that might start an expression are processed above.
				this.ParseModifiers(modifiers);

				TypeParameterListSyntax typeParameterListOpt = default(TypeParameterListSyntax);

				if (this.CurrentToken.Kind == SyntaxKind.LessThanToken)
				{
					typeParameterListOpt = this.ParseTypeParameterList();
				}

				// Check for constructor form
				if (this.CurrentToken.Kind == SyntaxKind.IdentifierToken && this.PeekToken(1).Kind == SyntaxKind.OpenParenToken)
				{
					// Script: 
					// Constructor definitions are not allowed. We parse them as method calls with semicolon missing error:
					//
					// Script(...) { ... } 
					//            ^
					//            missing ';'
					if (this.CurrentToken.ValueText == typeName)
					{
						return this.ParseConstructorDeclaration(typeName, annotations, modifiers, typeParameterListOpt);
					}

					// Script: 
					// Unless there modifiers or attributes are present this is more likely to be a method call than a method definition.

					var token = SyntaxFactory.MissingToken(SyntaxKind.VoidKeyword);
					token = this.AddError(token, ErrorCode.ERR_MemberNeedsType);
					var voidType = _syntaxFactory.PredefinedType(token);

					var identifier = this.EatToken();

					return this.ParseMethodDeclaration(annotations, modifiers, voidType, identifier: identifier, typeParameterList: typeParameterListOpt,isDtor:false);
				}
				else if (this.CurrentToken.Kind == SyntaxKind.OpenBraceToken && annotations.Count == 0)
				{
					return this.ParseJavaInitializerMethodDeclaration(modifiers);
				}

				//// Check for destructor form
				//// TODO: better error messages for script
				//if (this.CurrentToken.Kind == SyntaxKind.TildeToken)
				//{
				//	return this.ParseDestructorDeclaration(typeName, annotations, modifiers);
				//}

				

				// Check for constant (prefers const field over const local variable decl)
				//if (this.CurrentToken.Kind == SyntaxKind.ConstKeyword)
				//{
				//	return this.ParseConstantFieldDeclaration(annotations, modifiers, parentKind);
				//}


				// It's valid to have a type declaration here -- check for those
				if (CanStartTypeDeclaration(this.CurrentToken.Kind))
				{
					return this.ParseTypeDeclaration(annotations, modifiers);
				}




				// Everything that's left -- methods, fields, properties, 
				// indexers, and non-conversion operators -- starts with a type 
				// (possibly void).  Parse one.
				var type = this.ParseReturnType();

				// <UNDONE> UNDONE: should disallow non-methods with void type here</UNDONE>

				// Check for misplaced modifiers.  if we see any, then consider this member
				// terminated and restart parsing.
				if (GetModifier(this.CurrentToken) != SyntaxModifier.None &&
					IsComplete(type))
				{
					var misplacedModifier = this.CurrentToken;
					type = this.AddError(
						type,
						type.FullWidth + misplacedModifier.GetLeadingTriviaWidth(),
						misplacedModifier.Width,
						ErrorCode.ERR_BadModifierLocation,
						misplacedModifier.Text);

					return _syntaxFactory.IncompleteMember(annotations,typeParameterListOpt, modifiers.ToTokenList(), type);
				}


				if (IsFieldDeclaration(isEvent: false))
				{

					return this.ParseNormalFieldDeclaration(annotations, modifiers, type, parentKind);
				}

				// At this point we can either have indexers, methods, or 
				// properties (or something unknown).  Try to break apart
				// the following name and determine what to do from there.
	
				SyntaxToken identifierOrThisOpt;
				
				//this.ParseMemberName( out identifierOrThisOpt, out typeParameterListOpt);
				this.ParseMemberName( out identifierOrThisOpt);

				// First, check if we got absolutely nothing.  If so, then 
				// We need to consume a bad member and try again.
				if (identifierOrThisOpt == null)
				{
					if (annotations.Count == 0 && modifiers.Count == 0 && type.IsMissing)
					{
						// we haven't advanced, the caller needs to consume the tokens ahead
						return null;
					}

					var incompleteMember = _syntaxFactory.IncompleteMember(annotations,typeParameterListOpt, modifiers.ToTokenList(), type.IsMissing ? null : type);
					if (incompleteMember.ContainsDiagnostics)
					{
						return incompleteMember;
					}
					else if (parentKind == SyntaxKind.CompilationUnit)
					{
						return this.AddErrorToLastToken(incompleteMember, ErrorCode.ERR_NamespaceUnexpected);
					}
					else
					{
						//the error position should indicate CurrentToken
						return this.AddError(
							incompleteMember,
							incompleteMember.FullWidth + this.CurrentToken.GetLeadingTriviaWidth(),
							this.CurrentToken.Width,
							ErrorCode.ERR_InvalidMemberDecl,
							this.CurrentToken.Text);
					}
				}

				Debug.Assert(identifierOrThisOpt != null);

				var dtor = identifierOrThisOpt.ValueText == "finalize" && type is PredefinedTypeSyntax && ((PredefinedTypeSyntax)type).Keyword.Kind == SyntaxKind.VoidKeyword;



				// treat anything else as a method.
				return this.ParseMethodDeclaration(annotations, modifiers, type, identifierOrThisOpt, typeParameterListOpt, isDtor: dtor);

			}
			finally
			{
				this._pool.Free(modifiers);
				this._pool.Free(annotations);
				this._termState = saveTermState;
			}
		}


		private MemberDeclarationSyntax ParseTypeDeclaration(SyntaxListBuilder<AnnotationSyntax> attributes, SyntaxListBuilder modifiers)
		{
			CancellationToken.ThrowIfCancellationRequested();

			switch (this.CurrentToken.Kind)
			{
				case SyntaxKind.ClassKeyword:
					return this.ParseJavaNormalClassDeclaration(attributes, modifiers);
				case SyntaxKind.EnumKeyword:
					return this.ParseJavaEnumDeclaration(attributes, modifiers);
				case SyntaxKind.InterfaceKeyword:
					return this.ParseJavaNormalInterfaceDeclaration(attributes, modifiers);
				case SyntaxKind.AtToken:
					return this.ParseJavaAnnotationTypeDeclaration(attributes, modifiers);
				default:
					throw ExceptionUtilities.UnexpectedValue(this.CurrentToken.Kind);
			}
		}


		private void ParseModifiers(SyntaxListBuilder tokens)
		{
			SyntaxModifier mods = 0;
			bool seenNoDuplicates = true;
			bool seenNoAccessibilityDuplicates = true;

			while (true)
			{
				var newMod = GetModifier(this.CurrentToken);
				if (newMod == SyntaxModifier.None)
				{
					break;
				}

				SyntaxToken modTok = this.EatToken();

				ReportDuplicateModifiers(ref modTok, newMod, mods, ref seenNoDuplicates, ref seenNoAccessibilityDuplicates);
				mods |= newMod;
				tokens.Add(modTok);
			}
		}




		private TypeSyntax ParseReturnType()
		{
			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsEndOfReturnType;
			var type = this.ParseTypeOrVoid();
			this._termState = saveTerm;
			return type;
		}
		private TypeSyntax ParseTypeOrVoid()
		{
			if (this.CurrentToken.Kind == SyntaxKind.VoidKeyword && this.PeekToken(1).Kind != SyntaxKind.AsteriskToken)
			{
				// Must be 'void' type, so create such a type node and return it.
				return _syntaxFactory.PredefinedType(this.EatToken());
			}

			return this.ParseType(parentIsParameter: false);
		}


		private FieldDeclarationSyntax ParseNormalFieldDeclaration(
			SyntaxListBuilder<AnnotationSyntax> attributes,
			SyntaxListBuilder modifiers,
			TypeSyntax type,
			SyntaxKind parentKind)
		{
			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsEndOfFieldDeclaration;
			var variables = this._pool.AllocateSeparated<VariableDeclaratorSyntax>();
			try
			{
				this.ParseVariableDeclarators(type, flags: 0, variables: variables, parentKind: parentKind);

				var semicolon = this.EatToken(SyntaxKind.SemicolonToken);
				return _syntaxFactory.FieldDeclaration(
					attributes,
					modifiers.ToTokenList(),
					_syntaxFactory.VariableDeclaration(type, variables),
					semicolon);
			}
			finally
			{
				this._termState = saveTerm;
				this._pool.Free(variables);
			}
		}


		//private FieldDeclarationSyntax ParseConstantFieldDeclaration(SyntaxListBuilder<AnnotationSyntax> attributes, SyntaxListBuilder modifiers, SyntaxKind parentKind)
		//{
		//	var constToken = this.EatToken(SyntaxKind.ConstKeyword);
		//	modifiers.Add(constToken);

		//	var type = this.ParseType(false);

		//	var variables = this._pool.AllocateSeparated<VariableDeclaratorSyntax>();
		//	try
		//	{
		//		this.ParseVariableDeclarators(type, VariableFlags.Const, variables, parentKind);
		//		var semicolon = this.EatToken(SyntaxKind.SemicolonToken);
		//		return _syntaxFactory.FieldDeclaration(
		//			attributes,
		//			modifiers.ToTokenList(),
		//			_syntaxFactory.VariableDeclaration(type, variables),
		//			semicolon);
		//	}
		//	finally
		//	{
		//		this._pool.Free(variables);
		//	}
		//}



		private void ParseVariableDeclarators(TypeSyntax type, VariableFlags flags, SeparatedSyntaxListBuilder<VariableDeclaratorSyntax> variables, SyntaxKind parentKind)
		{
			// Although we try parse variable declarations in contexts where they are not allowed (non-interactive top-level or a namespace) 
			// the reported errors should take into consideration whether or not one expects them in the current context.
			bool variableDeclarationsExpected = (parentKind != SyntaxKind.CompilationUnit);

			ParseVariableDeclarators(type, flags, variables, variableDeclarationsExpected);
		}

		private void ParseVariableDeclarators(TypeSyntax type, VariableFlags flags, SeparatedSyntaxListBuilder<VariableDeclaratorSyntax> variables, bool variableDeclarationsExpected)
		{
			variables.Add(this.ParseVariableDeclarator(type, flags, isFirst: true));

			while (true)
			{
				if (this.CurrentToken.Kind == SyntaxKind.SemicolonToken)
				{
					break;
				}
				else if (this.CurrentToken.Kind == SyntaxKind.CommaToken)
				{
					variables.AddSeparator(this.EatToken(SyntaxKind.CommaToken));
					variables.Add(this.ParseVariableDeclarator(type, flags, isFirst: false));
				}
				else if (!variableDeclarationsExpected || this.SkipBadVariableListTokens(variables, SyntaxKind.CommaToken) == PostSkipAction.Abort)
				{
					break;
				}
			}
		}

		private void ParseMemberName(
			out SyntaxToken identifierOrThisOpt/*,
			out TypeParameterListSyntax typeParameterListOpt*/)
		{
			identifierOrThisOpt = null;
			//typeParameterListOpt = null;

			if (!IsPossibleMemberName())
			{
				// No clue what this is.  Just bail.  Our caller will have to
				// move forward and try again.
				return;
			}

			NameSyntax explicitInterfaceName = null;
			SyntaxToken separator = null;

			ResetPoint beforeIdentifierPoint = default(ResetPoint);
			bool beforeIdentifierPointSet = false;

			try
			{
				while (true)
				{
					//// Check if we got 'this'.  If so, then we have an indexer.
					//// Note: we parse out type parameters here as well so that
					//// we can give a useful error about illegal generic indexers.
					//if (this.CurrentToken.Kind == SyntaxKind.ThisKeyword)
					//{
					//	beforeIdentifierPoint = GetResetPoint();
					//	beforeIdentifierPointSet = true;
					//	identifierOrThisOpt = this.EatToken();
					//	typeParameterListOpt = this.ParseTypeParameterList();
					//	break;
					//}

					// now, scan past the next name.  if it's followed by a dot then
					// it's part of the explicit name we're building up.  Otherwise,
					// it's the name of the member.
					var point = GetResetPoint();
					bool isMemberName;
					try
					{
						ScanNamedTypePart();
						isMemberName = !IsDotOrColonColon();
					}
					finally
					{
						this.Reset(ref point);
						this.Release(ref point);
					}

					if (isMemberName)
					{
						// We're past any explicit interface portion and We've 
						// gotten to the member name.  
						beforeIdentifierPoint = GetResetPoint();
						beforeIdentifierPointSet = true;

						if (separator != null && separator.Kind == SyntaxKind.ColonColonToken)
						{
							separator = this.AddError(separator, ErrorCode.ERR_AliasQualAsExpression);
							separator = this.ConvertToMissingWithTrailingTrivia(separator, SyntaxKind.DotToken);
						}

						identifierOrThisOpt = this.ParseIdentifierToken();
						//typeParameterListOpt = this.ParseTypeParameterList();
						break;
					}
					else
					{
						// If we saw a . or :: then we must have something explicit.
						// first parse the upcoming name portion.

						var saveTerm = _termState;
						_termState |= TerminatorState.IsEndOfNameInExplicitInterface;

						if (explicitInterfaceName == null)
						{
							// If this is the first time, then just get the next simple
							// name and store it as the explicit interface name.
							explicitInterfaceName = this.ParseSimpleName(NameOptions.InTypeList);

							// Now, get the next separator.
							separator = this.CurrentToken.Kind == SyntaxKind.ColonColonToken
								? this.EatToken() // fine after the first identifier
								: this.EatToken(SyntaxKind.DotToken);
						}
						else
						{
							// Parse out the next part and combine it with the 
							// current explicit name to form the new explicit name.
							var tmp = this.ParseQualifiedNameRight(NameOptions.InTypeList, explicitInterfaceName, separator);
							Debug.Assert(!ReferenceEquals(tmp, explicitInterfaceName), "We should have consumed something and updated explicitInterfaceName");
							explicitInterfaceName = tmp;

							// Now, get the next separator.
							separator = this.CurrentToken.Kind == SyntaxKind.ColonColonToken
								? this.ConvertToMissingWithTrailingTrivia(this.EatToken(), SyntaxKind.DotToken)
								: this.EatToken(SyntaxKind.DotToken);
						}

						_termState = saveTerm;
					}
				}
			}
			finally
			{
				if (beforeIdentifierPointSet)
				{
					Release(ref beforeIdentifierPoint);
				}
			}
		}

	}
}