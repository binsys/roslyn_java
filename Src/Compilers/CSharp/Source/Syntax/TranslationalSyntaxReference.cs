﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using System.Diagnostics;

namespace Microsoft.CodeAnalysis.CSharp
{
	/// <summary>
	/// this is a SyntaxReference implementation that lazily translates the result (CSharpSyntaxNode) of the original syntax reference
	/// to other one.
	/// </summary>
	internal class TranslationSyntaxReference : SyntaxReference
	{
		private readonly SyntaxReference _reference;
		private readonly Func<SyntaxReference, SyntaxNode> _nodeGetter;

		public TranslationSyntaxReference(SyntaxReference reference, Func<SyntaxReference, SyntaxNode> nodeGetter)
		{
			this._reference = reference;
			this._nodeGetter = nodeGetter;
		}

		public override SyntaxNode GetSyntax(CancellationToken cancellationToken)
		{
			var node = _nodeGetter(this._reference);

			Debug.Assert(node.SyntaxTree == this._reference.SyntaxTree);

			return node;
		}

		public override TextSpan Span
		{
			get { return this._reference.Span; }
		}

		public override SyntaxTree SyntaxTree
		{
			get { return this._reference.SyntaxTree; }
		}
	}
}