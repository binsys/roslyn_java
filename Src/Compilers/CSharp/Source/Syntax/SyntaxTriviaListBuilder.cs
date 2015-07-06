// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp.Syntax
{
	internal class SyntaxTriviaListBuilder
	{
		private SyntaxTrivia[] _nodes;
		private int _count;
		private SyntaxTrivia[] _previous;

		public SyntaxTriviaListBuilder(int size)
		{
			this._nodes = new SyntaxTrivia[size];
		}

		public static SyntaxTriviaListBuilder Create()
		{
			return new SyntaxTriviaListBuilder(4);
		}

		public int Count
		{
			get { return _count; }
		}

		public void Clear()
		{
			this._count = 0;
		}

		public SyntaxTrivia this[int index]
		{
			get
			{
				if (index < 0 || index > this._count)
				{
					throw new IndexOutOfRangeException();
				}

				return this._nodes[index];
			}
		}

		public SyntaxTriviaListBuilder Add(SyntaxTrivia item)
		{
			if (_nodes == null || _count >= _nodes.Length)
			{
				this.Grow(_count == 0 ? 8 : _nodes.Length * 2);
			}

			_nodes[_count++] = item;
			return this;
		}

		public void Add(SyntaxTrivia[] items)
		{
			this.Add(items, 0, items.Length);
		}

		public void Add(SyntaxTrivia[] items, int offset, int length)
		{
			if (_nodes == null || _count + length > _nodes.Length)
			{
				this.Grow(_count + length);
			}

			Array.Copy(items, offset, _nodes, _count, length);
			_count += length;
		}

		public void Add(SyntaxTriviaList list)
		{
			this.Add(list, 0, list.Count);
		}

		public void Add(SyntaxTriviaList list, int offset, int length)
		{
			if (_nodes == null || _count + length > _nodes.Length)
			{
				this.Grow(_count + length);
			}

			list.CopyTo(offset, _nodes, _count, length);
			_count += length;
		}

		private void Grow(int size)
		{
			var tmp = new SyntaxTrivia[size];
			if (_previous != null)
			{
				Array.Copy(_previous, tmp, this._count);
				this._previous = null;
			}
			else
			{
				Array.Copy(_nodes, tmp, _nodes.Length);
			}

			this._nodes = tmp;
		}

		public static implicit operator SyntaxTriviaList(SyntaxTriviaListBuilder builder)
		{
			return builder.ToList();
		}

		public SyntaxTriviaList ToList()
		{
			if (this._count > 0)
			{
				if (this._previous != null)
				{
					this.Grow(this._count);
				}

				switch (this._count)
				{
					case 1:
						return new SyntaxTriviaList(default(SyntaxToken), _nodes[0].UnderlyingNode, position: 0, index: 0);
					case 2:
						return new SyntaxTriviaList(default(SyntaxToken), Syntax.InternalSyntax.SyntaxList.List(
							(Syntax.InternalSyntax.CSharpSyntaxNode)_nodes[0].UnderlyingNode,
							(Syntax.InternalSyntax.CSharpSyntaxNode)_nodes[1].UnderlyingNode), position: 0, index: 0);
					case 3:
						return new SyntaxTriviaList(default(SyntaxToken),
							Syntax.InternalSyntax.SyntaxList.List(
								(Syntax.InternalSyntax.CSharpSyntaxNode)_nodes[0].UnderlyingNode,
								(Syntax.InternalSyntax.CSharpSyntaxNode)_nodes[1].UnderlyingNode,
								(Syntax.InternalSyntax.CSharpSyntaxNode)_nodes[2].UnderlyingNode),
							position: 0, index: 0);
					default:
						{
							var tmp = new ArrayElement<Syntax.InternalSyntax.CSharpSyntaxNode>[_count];
							for (int i = 0; i < this._count; i++)
							{
								tmp[i].Value = (Syntax.InternalSyntax.CSharpSyntaxNode)this._nodes[i].UnderlyingNode;
							}

							return new SyntaxTriviaList(default(SyntaxToken), Syntax.InternalSyntax.SyntaxList.List(tmp), position: 0, index: 0);
						}
				}
			}
			else
			{
				return default(SyntaxTriviaList);
			}
		}
	}
}