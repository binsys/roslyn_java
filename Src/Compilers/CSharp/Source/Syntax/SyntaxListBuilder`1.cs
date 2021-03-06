﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
namespace Microsoft.CodeAnalysis.CSharp.Syntax
{
	internal struct SyntaxListBuilder<TNode> where TNode : SyntaxNode
	{
		private readonly SyntaxListBuilder builder;

		public SyntaxListBuilder(int size)
			: this(new SyntaxListBuilder(size))
		{
		}

		public static SyntaxListBuilder<TNode> Create()
		{
			return new SyntaxListBuilder<TNode>(8);
		}

		internal SyntaxListBuilder(SyntaxListBuilder builder)
		{
			this.builder = builder;
		}

		public bool IsNull
		{
			get
			{
				return this.builder == null;
			}
		}

		public int Count
		{
			get
			{
				return this.builder.Count;
			}
		}

		public void Clear()
		{
			this.builder.Clear();
		}

		public SyntaxListBuilder<TNode> Add(TNode node)
		{
			this.builder.Add(node);
			return this;
		}

		public void AddRange(TNode[] items, int offset, int length)
		{
			this.builder.AddRange(items, offset, length);
		}

		public void AddRange(SyntaxList<TNode> nodes)
		{
			this.builder.AddRange(nodes);
		}

		public void AddRange(SyntaxList<TNode> nodes, int offset, int length)
		{
			this.builder.AddRange(nodes, offset, length);
		}

		public bool Any(SyntaxKind kind)
		{
			return this.builder.Any(kind);
		}

		public SyntaxList<TNode> ToList()
		{
			return this.builder.ToList();
		}

		public static implicit operator SyntaxListBuilder(SyntaxListBuilder<TNode> builder)
		{
			return builder.builder;
		}

		public static implicit operator SyntaxList<TNode>(SyntaxListBuilder<TNode> builder)
		{
			if (builder.builder != null)
			{
				return builder.ToList();
			}

			return default(SyntaxList<TNode>);
		}
	}
}