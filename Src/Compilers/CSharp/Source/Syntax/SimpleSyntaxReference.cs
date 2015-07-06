﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp
{
	/// <summary>
	/// this is a basic do-nothing implementation of a syntax reference
	/// </summary>
	internal class SimpleSyntaxReference : SyntaxReference
	{
		private readonly SyntaxNode _node;

		internal SimpleSyntaxReference(SyntaxNode node)
		{
			this._node = node;
		}

		public override SyntaxTree SyntaxTree
		{
			get
			{
				return this._node.SyntaxTree;
			}
		}

		public override TextSpan Span
		{
			get
			{
				return this._node.Span;
			}
		}

		public override SyntaxNode GetSyntax(CancellationToken cancellationToken = default(CancellationToken))
		{
			return this._node;
		}
	}
}
