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
		private bool IsFieldDeclaration(bool isEvent)
		{
			if (this.CurrentToken.Kind != SyntaxKind.IdentifierToken)
			{
				return false;
			}

			// Treat this as a field, unless we have anything following that
			// makes us:
			//   a) explicit
			//   b) generic
			//   c) a property
			//   d) a method (unless we already know we're parsing an event)
			var kind = this.PeekToken(1).Kind;
			switch (kind)
			{
				case SyntaxKind.DotToken:           // Foo.     explicit
				case SyntaxKind.ColonColonToken:    // Foo::    explicit
				case SyntaxKind.LessThanToken:      // Foo<     explicit or generic method
				case SyntaxKind.OpenBraceToken:     // Foo {    property
					return false;
				case SyntaxKind.OpenParenToken:     // Foo(     method
					return isEvent;
				default:
					return true;
			}
		}

		private bool IsEndOfTypeParameterList()
		{
			if (this.CurrentToken.Kind == SyntaxKind.OpenParenToken)
			{
				// void Foo<T (
				return true;
			}

			if (this.CurrentToken.Kind == SyntaxKind.ColonToken)
			{
				// class C<T :
				return true;
			}

			if (this.CurrentToken.Kind == SyntaxKind.OpenBraceToken)
			{
				// class C<T {
				return true;
			}

			if (IsPossibleTypeParameterConstraintClauseStart())
			{
				// class C<T where T :
				return true;
			}

			return false;
		}

		private bool IsEndOfMethodSignature()
		{
			return this.CurrentToken.Kind == SyntaxKind.SemicolonToken || this.CurrentToken.Kind == SyntaxKind.OpenBraceToken;
		}

		private bool IsEndOfNameInExplicitInterface()
		{
			return this.CurrentToken.Kind == SyntaxKind.DotToken || this.CurrentToken.Kind == SyntaxKind.ColonColonToken;
		}

		private bool IsEndOfReturnType()
		{
			switch (this.CurrentToken.Kind)
			{
				case SyntaxKind.OpenParenToken:
				case SyntaxKind.OpenBraceToken:
				case SyntaxKind.SemicolonToken:
					return true;
				default:
					return false;
			}
		}

		private bool IsEndOfParameterList()
		{
			return this.CurrentToken.Kind == SyntaxKind.CloseParenToken
				|| this.CurrentToken.Kind == SyntaxKind.CloseBracketToken;
		}
		private bool IsEndOfFieldDeclaration()
		{
			return this.CurrentToken.Kind == SyntaxKind.SemicolonToken;
		}

		private bool IsEndOfTypeArgumentList()
		{
			return this.CurrentToken.Kind == SyntaxKind.GreaterThanToken;
		}

		private bool IsEndOfTryBlock()
		{
			return this.CurrentToken.Kind == SyntaxKind.CloseBraceToken
				|| this.CurrentToken.Kind == SyntaxKind.CatchKeyword
				|| this.CurrentToken.Kind == SyntaxKind.FinallyKeyword;
		}

		private bool IsEndOfCatchClause()
		{
			return this.CurrentToken.Kind == SyntaxKind.CloseParenToken
				|| this.CurrentToken.Kind == SyntaxKind.OpenBraceToken
				|| this.CurrentToken.Kind == SyntaxKind.CloseBraceToken
				|| this.CurrentToken.Kind == SyntaxKind.CatchKeyword
				|| this.CurrentToken.Kind == SyntaxKind.FinallyKeyword;
		}

		private bool IsEndOfFilterClause()
		{
			return this.CurrentToken.Kind == SyntaxKind.CloseParenToken
				|| this.CurrentToken.Kind == SyntaxKind.OpenBraceToken
				|| this.CurrentToken.Kind == SyntaxKind.CloseBraceToken
				|| this.CurrentToken.Kind == SyntaxKind.CatchKeyword
				|| this.CurrentToken.Kind == SyntaxKind.FinallyKeyword;
		}

		private bool IsEndOfCatchBlock()
		{
			return this.CurrentToken.Kind == SyntaxKind.CloseBraceToken
				|| this.CurrentToken.Kind == SyntaxKind.CatchKeyword
				|| this.CurrentToken.Kind == SyntaxKind.FinallyKeyword;
		}

		private bool IsEndOfDoWhileExpression()
		{
			return this.CurrentToken.Kind == SyntaxKind.CloseParenToken
				|| this.CurrentToken.Kind == SyntaxKind.SemicolonToken;
		}

		private bool IsEndOfForStatementArgument()
		{
			return this.CurrentToken.Kind == SyntaxKind.SemicolonToken
				|| this.CurrentToken.Kind == SyntaxKind.CloseParenToken
				|| this.CurrentToken.Kind == SyntaxKind.OpenBraceToken;
		}

		private bool IsEndOfDeclarationClause()
		{
			switch (this.CurrentToken.Kind)
			{
				case SyntaxKind.SemicolonToken:
				case SyntaxKind.CloseParenToken:
				case SyntaxKind.ColonToken:
					return true;
				default:
					return false;
			}
		}

		private bool IsEndOfArgumentList()
		{
			return this.CurrentToken.Kind == SyntaxKind.CloseParenToken
				|| this.CurrentToken.Kind == SyntaxKind.CloseBracketToken;
		}
	}
}