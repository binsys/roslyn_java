// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp.Syntax
{
	internal struct SeparatedSyntaxListBuilder<TNode> where TNode : SyntaxNode
	{
		private readonly SyntaxListBuilder _builder;
		private bool _expectedSeparator;

		public SeparatedSyntaxListBuilder(int size)
			: this(new SyntaxListBuilder(size))
		{
		}

		public static SeparatedSyntaxListBuilder<TNode> Create()
		{
			return new SeparatedSyntaxListBuilder<TNode>(8);
		}

		internal SeparatedSyntaxListBuilder(SyntaxListBuilder builder)
		{
			this._builder = builder;
			this._expectedSeparator = false;
		}

		public bool IsNull
		{
			get
			{
				return this._builder == null;
			}
		}

		public int Count
		{
			get
			{
				return this._builder.Count;
			}
		}

		public void Clear()
		{
			this._builder.Clear();
		}

		private void CheckExpectedElement()
		{
			if (this._expectedSeparator)
			{
				throw new InvalidOperationException(CSharpResources.SeparatorIsExpected);
			}
		}

		private void CheckExpectedSeparator()
		{
			if (!this._expectedSeparator)
			{
				throw new InvalidOperationException(CSharpResources.ElementIsExpected);
			}
		}

		public SeparatedSyntaxListBuilder<TNode> Add(TNode node)
		{
			CheckExpectedElement();
			this._expectedSeparator = true;
			this._builder.Add(node);
			return this;
		}

		public SeparatedSyntaxListBuilder<TNode> AddSeparator(SyntaxToken separatorToken)
		{
			CheckExpectedSeparator();
			this._expectedSeparator = false;
			this._builder.AddInternal(separatorToken.Node);
			return this;
		}

		public SeparatedSyntaxListBuilder<TNode> AddRange(SeparatedSyntaxList<TNode> nodes)
		{
			CheckExpectedElement();
			SyntaxNodeOrTokenList list = nodes.GetWithSeparators();
			this._builder.AddRange(list);
			this._expectedSeparator = ((this._builder.Count & 1) != 0);
			return this;
		}

		public SeparatedSyntaxListBuilder<TNode> AddRange(SeparatedSyntaxList<TNode> nodes, int count)
		{
			CheckExpectedElement();
			SyntaxNodeOrTokenList list = nodes.GetWithSeparators();
			this._builder.AddRange(list, this.Count, Math.Min(count << 1, list.Count));
			this._expectedSeparator = ((this._builder.Count & 1) != 0);
			return this;
		}

		public SeparatedSyntaxList<TNode> ToList()
		{
			if (this._builder == null)
			{
				return new SeparatedSyntaxList<TNode>();
			}

			return this._builder.ToSeparatedList<TNode>();
		}

		public SeparatedSyntaxList<TDerived> ToList<TDerived>() where TDerived : TNode
		{
			if (this._builder == null)
			{
				return new SeparatedSyntaxList<TDerived>();
			}

			return this._builder.ToSeparatedList<TDerived>();
		}

		public static implicit operator SyntaxListBuilder(SeparatedSyntaxListBuilder<TNode> builder)
		{
			return builder._builder;
		}

		public static implicit operator SeparatedSyntaxList<TNode>(SeparatedSyntaxListBuilder<TNode> builder)
		{
			if (builder._builder != null)
			{
				return builder.ToList();
			}

			return default(SeparatedSyntaxList<TNode>);
		}
	}
}