﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
	internal class SyntaxFirstTokenReplacer : CSharpSyntaxRewriter
	{
		private readonly SyntaxToken oldToken;
		private readonly SyntaxToken newToken;
		private readonly int diagnosticOffsetDelta;
		private bool foundOldToken;

		private SyntaxFirstTokenReplacer(SyntaxToken oldToken, SyntaxToken newToken, int diagnosticOffsetDelta)
		{
			this.oldToken = oldToken;
			this.newToken = newToken;
			this.diagnosticOffsetDelta = diagnosticOffsetDelta;
			this.foundOldToken = false;
		}

		internal static TRoot Replace<TRoot>(TRoot root, SyntaxToken oldToken, SyntaxToken newToken, int diagnosticOffsetDelta)
			where TRoot : CSharpSyntaxNode
		{
			var replacer = new SyntaxFirstTokenReplacer(oldToken, newToken, diagnosticOffsetDelta);
			var newRoot = (TRoot)replacer.Visit(root);
			Debug.Assert(replacer.foundOldToken);
			return newRoot;
		}

		public override CSharpSyntaxNode Visit(CSharpSyntaxNode node)
		{
			if (node != null)
			{
				if (!this.foundOldToken)
				{
					var token = node as SyntaxToken;
					if (token != null)
					{
						Debug.Assert(token == oldToken);
						this.foundOldToken = true;
						return newToken; // NB: diagnostic offsets have already been updated (by SyntaxParser.AddSkippedSyntax)
					}

					return UpdateDiagnosticOffset(base.Visit(node), this.diagnosticOffsetDelta);
				}
			}

			return node;
		}

		private static TSyntax UpdateDiagnosticOffset<TSyntax>(TSyntax node, int diagnosticOffsetDelta) where TSyntax : CSharpSyntaxNode
		{
			DiagnosticInfo[] oldDiagnostics = node.GetDiagnostics();
			if (oldDiagnostics == null || oldDiagnostics.Length == 0)
			{
				return node;
			}

			var numDiagnostics = oldDiagnostics.Length;
			DiagnosticInfo[] newDiagnostics = new DiagnosticInfo[numDiagnostics];
			for (int i = 0; i < numDiagnostics; i++)
			{
				DiagnosticInfo oldDiagnostic = oldDiagnostics[i];
				SyntaxDiagnosticInfo oldSyntaxDiagnostic = oldDiagnostic as SyntaxDiagnosticInfo;
				newDiagnostics[i] = oldSyntaxDiagnostic == null ?
					oldDiagnostic :
					new SyntaxDiagnosticInfo(
						oldSyntaxDiagnostic.Offset + diagnosticOffsetDelta,
						oldSyntaxDiagnostic.Width,
						(ErrorCode)oldSyntaxDiagnostic.Code,
						oldSyntaxDiagnostic.Arguments);
			}
			return node.WithDiagnosticsGreen(newDiagnostics);
		}
	}
}
