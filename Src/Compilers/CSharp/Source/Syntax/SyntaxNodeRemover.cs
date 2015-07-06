﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using System.Diagnostics;

namespace Microsoft.CodeAnalysis.CSharp.Syntax
{
	internal static class SyntaxNodeRemover
	{
		internal static TRoot RemoveNodes<TRoot>(TRoot root,
				IEnumerable<SyntaxNode> nodes,
				SyntaxRemoveOptions options)
			where TRoot : SyntaxNode
		{
			if (nodes == null)
			{
				return root;
			}

			var nodeArray = nodes.ToArray();

			if (nodeArray.Length == 0)
			{
				return root;
			}

			var remover = new SyntaxRemover(nodes.ToArray(), options);
			var result = remover.Visit(root);

			var residualTrivia = remover.ResidualTrivia;

			if (residualTrivia.Count > 0)
			{
				result = result.WithTrailingTrivia(result.GetTrailingTrivia().Concat(residualTrivia));
			}

			return (TRoot)result;
		}

		private class SyntaxRemover : CSharpSyntaxRewriter
		{
			private readonly HashSet<SyntaxNode> _nodesToRemove;
			private readonly SyntaxRemoveOptions _options;
			private readonly TextSpan _searchSpan;
			private readonly SyntaxTriviaListBuilder _residualTrivia;
			//private HashSet<SyntaxNode> directivesToKeep;

			public SyntaxRemover(
				SyntaxNode[] nodesToRemove,
				SyntaxRemoveOptions options)
				: base(nodesToRemove.Any(n => n.IsPartOfStructuredTrivia()))
			{
				this._nodesToRemove = new HashSet<SyntaxNode>(nodesToRemove);
				this._options = options;
				this._searchSpan = ComputeTotalSpan(nodesToRemove);
				this._residualTrivia = SyntaxTriviaListBuilder.Create();
			}

			private static TextSpan ComputeTotalSpan(SyntaxNode[] nodes)
			{
				var span0 = nodes[0].FullSpan;
				int start = span0.Start;
				int end = span0.End;

				for (int i = 1; i < nodes.Length; i++)
				{
					var span = nodes[i].FullSpan;
					start = Math.Min(start, span.Start);
					end = Math.Max(end, span.End);
				}

				return new TextSpan(start, end - start);
			}

			internal SyntaxTriviaList ResidualTrivia
			{
				get
				{
					if (this._residualTrivia != null)
					{
						return this._residualTrivia.ToList();
					}
					else
					{
						return default(SyntaxTriviaList);
					}
				}
			}

			private void AddResidualTrivia(SyntaxTriviaList trivia, bool requiresNewLine = false)
			{
				if (requiresNewLine)
				{
					this.AddEndOfLine();
				}

				this._residualTrivia.Add(trivia);
			}

			private void AddEndOfLine()
			{
				if (this._residualTrivia.Count == 0 || !IsEndOfLine(this._residualTrivia[this._residualTrivia.Count - 1]))
				{
					this._residualTrivia.Add(SyntaxFactory.CarriageReturnLineFeed);
				}
			}

			private static bool IsEndOfLine(SyntaxTrivia trivia)
			{
				return trivia.CSharpKind() == SyntaxKind.EndOfLineTrivia
					|| trivia.CSharpKind() == SyntaxKind.SingleLineCommentTrivia
					|| trivia.IsDirective;
			}

			private static bool HasEndOfLine(SyntaxTriviaList list)
			{
				return list.Any(t => IsEndOfLine(t));
			}

			private bool IsForRemoval(SyntaxNode node)
			{
				return this._nodesToRemove.Contains(node);
			}

			private bool ShouldVisit(SyntaxNode node)
			{
				return node.FullSpan.IntersectsWith(this._searchSpan) 
					|| (this._residualTrivia != null && this._residualTrivia.Count > 0);
			}

			public override SyntaxNode Visit(SyntaxNode node)
			{
				SyntaxNode result = node;

				if (node != null)
				{
					if (this.IsForRemoval(node))
					{
						this.AddTrivia(node);
						result = null;
					}
					else if (this.ShouldVisit(node))
					{
						result = base.Visit(node);
					}
				}

				return result;
			}

			public override SyntaxToken VisitToken(SyntaxToken token)
			{
				SyntaxToken result = token;

				// only bother visiting trivia if we are removing a node in structured trivia
				if (this.VisitIntoStructuredTrivia)
				{
					result = base.VisitToken(token);
				}

				// the next token gets the accrued trivia.
				if (result.CSharpKind() != SyntaxKind.None && this._residualTrivia != null && this._residualTrivia.Count > 0)
				{
					this._residualTrivia.Add(result.LeadingTrivia);
					result = result.WithLeadingTrivia(this._residualTrivia.ToList());
					this._residualTrivia.Clear();
				}

				return result;
			}

			// deal with separated lists and removal of associated separators
			public override SeparatedSyntaxList<TNode> VisitList<TNode>(SeparatedSyntaxList<TNode> list)
			{
				var withSeps = list.GetWithSeparators();
				bool removeNextSeparator = false;

				SyntaxNodeOrTokenListBuilder alternate = null;
				for (int i = 0, n = withSeps.Count; i < n; i++)
				{
					var item = withSeps[i];
					SyntaxNodeOrToken visited;

					if (item.IsToken) // separator
					{
						if (removeNextSeparator)
						{
							removeNextSeparator = false;
							visited = default(SyntaxNodeOrToken);
						}
						else
						{
							visited = this.VisitListSeparator(item.AsToken());
						}
					}
					else
					{
						var node = (TNode)(SyntaxNode)item.AsNode();

						if (this.IsForRemoval(node))
						{
							if (alternate == null)
							{
								alternate = new SyntaxNodeOrTokenListBuilder(n);
								alternate.Add(withSeps, 0, i);
							}

							if (alternate.Count > 0 && alternate[alternate.Count - 1].IsToken)
							{
								// remove preceding separator if any
								var separator = alternate[alternate.Count - 1].AsToken();
								this.AddTrivia(separator, node);
								alternate.RemoveLast();
							}
							else if (i + 1 < n && withSeps[i + 1].IsToken)
							{
								// otherwise remove following separator if any
								var separator = withSeps[i + 1].AsToken();
								this.AddTrivia(node, separator);
								removeNextSeparator = true;
							}
							else
							{
								this.AddTrivia(node);
							}

							visited = default(SyntaxNodeOrToken);
						}
						else
						{
							visited = this.VisitListElement((TNode)(SyntaxNode)item.AsNode());
						}
					}

					if (item != visited && alternate == null)
					{
						alternate = new SyntaxNodeOrTokenListBuilder(n);
						alternate.Add(withSeps, 0, i);
					}

					if (alternate != null && visited.CSharpKind() != SyntaxKind.None)
					{
						alternate.Add(visited);
					}
				}

				if (alternate != null)
				{
					return alternate.ToList().AsSeparatedList<TNode>();
				}

				return list;
			}

			private void AddTrivia(SyntaxNode node)
			{
				if ((this._options & SyntaxRemoveOptions.KeepLeadingTrivia) != 0)
				{
					this.AddResidualTrivia(node.GetLeadingTrivia());
				}
				else if ((this._options & SyntaxRemoveOptions.KeepEndOfLine) != 0
					&& HasEndOfLine(node.GetLeadingTrivia()))
				{
					this.AddEndOfLine();
				}

				if ((this._options & (SyntaxRemoveOptions.KeepDirectives | SyntaxRemoveOptions.KeepUnbalancedDirectives)) != 0)
				{
					//this.AddDirectives(node, GetRemovedSpan(node.Span, node.FullSpan));
					throw new NotImplementedException();
				}

				if ((this._options & SyntaxRemoveOptions.KeepTrailingTrivia) != 0)
				{
					this.AddResidualTrivia(node.GetTrailingTrivia());
				}
				else if ((this._options & SyntaxRemoveOptions.KeepEndOfLine) != 0
					&& HasEndOfLine(node.GetTrailingTrivia()))
				{
					this.AddEndOfLine();
				}
			}

			private void AddTrivia(SyntaxToken token, SyntaxNode node)
			{
				if ((this._options & SyntaxRemoveOptions.KeepLeadingTrivia) != 0)
				{
					this.AddResidualTrivia(token.LeadingTrivia);
					this.AddResidualTrivia(token.TrailingTrivia);
					this.AddResidualTrivia(node.GetLeadingTrivia());
				}
				else if ((this._options & SyntaxRemoveOptions.KeepEndOfLine) != 0
					&& (HasEndOfLine(token.LeadingTrivia) ||
						HasEndOfLine(token.TrailingTrivia) ||
						HasEndOfLine(node.GetLeadingTrivia())))
				{
					this.AddEndOfLine();
				}

				if ((this._options & (SyntaxRemoveOptions.KeepDirectives | SyntaxRemoveOptions.KeepUnbalancedDirectives)) != 0)
				{
					var span = TextSpan.FromBounds(token.Span.Start, node.Span.End);
					var fullSpan = TextSpan.FromBounds(token.FullSpan.Start, node.FullSpan.End);
					//this.AddDirectives(node.Parent, GetRemovedSpan(span, fullSpan));
					throw new NotImplementedException();
				}

				if ((this._options & SyntaxRemoveOptions.KeepTrailingTrivia) != 0)
				{
					this.AddResidualTrivia(node.GetTrailingTrivia());
				}
				else if ((this._options & SyntaxRemoveOptions.KeepEndOfLine) != 0
					&& HasEndOfLine(node.GetTrailingTrivia()))
				{
					this.AddEndOfLine();
				}
			}

			private void AddTrivia(SyntaxNode node, SyntaxToken token)
			{
				if ((this._options & SyntaxRemoveOptions.KeepLeadingTrivia) != 0)
				{
					this.AddResidualTrivia(node.GetLeadingTrivia());
				}
				else if ((this._options & SyntaxRemoveOptions.KeepEndOfLine) != 0
					&& HasEndOfLine(node.GetLeadingTrivia()))
				{
					this.AddEndOfLine();
				}

				if ((this._options & SyntaxRemoveOptions.KeepTrailingTrivia) != 0)
				{
					this.AddResidualTrivia(node.GetTrailingTrivia());
					this.AddResidualTrivia(token.LeadingTrivia);
					this.AddResidualTrivia(token.TrailingTrivia);
				}
				else if ((this._options & SyntaxRemoveOptions.KeepEndOfLine) != 0
					&& (HasEndOfLine(node.GetTrailingTrivia()) ||
						HasEndOfLine(token.LeadingTrivia) ||
						HasEndOfLine(token.TrailingTrivia)))
				{
					this.AddEndOfLine();
				}
			}

			private TextSpan GetRemovedSpan(TextSpan span, TextSpan fullSpan)
			{
				var removedSpan = fullSpan;

				if ((this._options & SyntaxRemoveOptions.KeepLeadingTrivia) != 0)
				{
					removedSpan = TextSpan.FromBounds(span.Start, removedSpan.End);
				}

				if ((this._options & SyntaxRemoveOptions.KeepTrailingTrivia) != 0)
				{
					removedSpan = TextSpan.FromBounds(removedSpan.Start, span.End);
				}

				return removedSpan;
			}


		}
	}
}
