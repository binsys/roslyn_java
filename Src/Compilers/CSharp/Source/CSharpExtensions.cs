// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{
	public static class CSharpExtensions
	{
		public static bool IsKind(this SyntaxToken token, SyntaxKind kind)
		{
			return token.CSharpKind() == kind;
		}

		public static bool IsContextualKind(this SyntaxToken token, SyntaxKind kind)
		{
			return token.CSharpContextualKind() == kind;
		}

		public static bool IsKind(this SyntaxTrivia trivia, SyntaxKind kind)
		{
			return trivia.CSharpKind() == kind;
		}

		public static bool IsKind(this SyntaxNode node, SyntaxKind kind)
		{
			return node != null
				&& node.CSharpKind() == kind;
		}

		public static bool IsKind(this SyntaxNodeOrToken nodeOrToken, SyntaxKind kind)
		{
			return nodeOrToken.CSharpKind() == kind;
		}

		public static SyntaxKind CSharpKind(this SyntaxToken token)
		{
			return (object)token.Language == (object)LanguageNames.CSharp ? (SyntaxKind)token.RawKind : SyntaxKind.None;
		}

		public static SyntaxKind CSharpContextualKind(this SyntaxToken token)
		{
			return (object)token.Language == (object)LanguageNames.CSharp ? (SyntaxKind)token.RawContextualKind : SyntaxKind.None;
		}

		public static SyntaxKind CSharpKind(this SyntaxTrivia trivia)
		{
			return (object)trivia.Language == (object)LanguageNames.CSharp ? (SyntaxKind)trivia.RawKind : SyntaxKind.None;
		}

		public static SyntaxKind CSharpKind(this SyntaxNode node)
		{
			return (object)node.Language == (object)LanguageNames.CSharp ? (SyntaxKind)node.RawKind : SyntaxKind.None;
		}

		public static SyntaxKind CSharpKind(this SyntaxNodeOrToken nodeOrToken)
		{
			return (object)nodeOrToken.Language == (object)LanguageNames.CSharp ? (SyntaxKind)nodeOrToken.RawKind : SyntaxKind.None;
		}

		/// <summary>
		/// True if the list has at least one node of the specified kind.
		/// </summary>
		public static bool Any<TNode>(this SyntaxList<TNode> list, SyntaxKind kind) where TNode : SyntaxNode
		{
			foreach (var element in list)
			{
				if (element.IsKind(kind))
				{
					return true;
				}
			}

			return false;
		}

		public static bool Any<TNode>(this SeparatedSyntaxList<TNode> list, SyntaxKind kind) where TNode : SyntaxNode
		{
			for (int i = 0; i < list.Count; i++)
			{
				if (list[i].IsKind(kind))
				{
					return true;
				}
			}

			return false;
		}

		public static bool Any(this SyntaxTriviaList list, SyntaxKind kind)
		{
			foreach (var trivia in list)
			{
				if (trivia.IsKind(kind))
				{
					return true;
				}
			}

			return false;
		}

		/// <summary>
		/// Tests whether a list contains tokens of a particular kind.
		/// </summary>
		/// <param name="list"></param>
		/// <param name="kind">The <see cref="CSharp.SyntaxKind"/> to test for.</param>
		/// <returns>Returns true if the list contains a token which matches <paramref name="kind"/></returns>
		public static bool Any(this SyntaxTokenList list, SyntaxKind kind)
		{
			if (list.Count > 0)
			{
				foreach (var token in list)
				{
					if (token.IsKind(kind))
					{
						return true;
					}
				}
			}

			return false;
		}

		internal static SyntaxToken FirstOrDefault(this SyntaxTokenList list, SyntaxKind kind)
		{
			if (list.Count > 0)
			{
				foreach (var token in list)
				{
					if (token.IsKind(kind))
					{
						return token;
					}
				}
			}

			return default(SyntaxToken);
		}
	}
}

namespace Microsoft.CodeAnalysis.CSharp
{
	public static class CSharpExtensions
	{
		public static bool IsKeyword(this SyntaxToken token)
		{
			return SyntaxKindFacts.IsKeywordKind(token.CSharpKind());
		}

		//public static bool IsContextualKeyword(this SyntaxToken token)
		//{
		//	return SyntaxKindFacts.IsContextualKeyword(token.CSharpKind());
		//}

		public static bool IsReservedKeyword(this SyntaxToken token)
		{
			return SyntaxKindFacts.IsReservedKeyword(token.CSharpKind());
		}

		public static bool IsVerbatimStringLiteral(this SyntaxToken token)
		{
			return token.IsKind(SyntaxKind.StringLiteralToken) && token.Text.Length > 0 && token.Text[0] == '@';
		}

		public static bool IsVerbatimIdentifier(this SyntaxToken token)
		{
			return token.IsKind(SyntaxKind.IdentifierToken) && token.Text.Length > 0 && token.Text[0] == '@';
		}

		public static VarianceKind VarianceKindFromToken(this SyntaxToken node)
		{
			switch (node.CSharpKind())
			{
				default: return VarianceKind.None;
			}
		}

		/// <summary>
		/// Insert one or more tokens in the list at the specified index.
		/// </summary>
		/// <returns>A new list with the tokens inserted.</returns>
		public static SyntaxTokenList Insert(this SyntaxTokenList list, int index, params SyntaxToken[] items)
		{
			if (index < 0 || index > list.Count)
			{
				throw new ArgumentOutOfRangeException("index");
			}

			if (items == null)
			{
				throw new ArgumentNullException("items");
			}

			if (list.Count == 0)
			{
				return SyntaxFactory.TokenList(items);
			}
			else
			{
				var builder = new Syntax.SyntaxTokenListBuilder(list.Count + items.Length);
				if (index > 0)
				{
					builder.Add(list, 0, index);
				}

				builder.Add(items);

				if (index < list.Count)
				{
					builder.Add(list, index, list.Count - index);
				}

				return builder.ToList();
			}
		}

		/// <summary>
		/// Creates a new token with the specified old trivia replaced with computed new trivia.
		/// </summary>
		/// <param name="token"></param>
		/// <param name="trivia">The trivia to be replaced; descendants of the root token.</param>
		/// <param name="computeReplacementTrivia">A function that computes a replacement trivia for
		/// the argument trivia. The first argument is the original trivia. The second argument is
		/// the same trivia rewritten with replaced structure.</param>
		public static SyntaxToken ReplaceTrivia(this SyntaxToken token, IEnumerable<SyntaxTrivia> trivia, Func<SyntaxTrivia, SyntaxTrivia, SyntaxTrivia> computeReplacementTrivia)
		{
			return Syntax.SyntaxReplacer.Replace(token, trivia: trivia, computeReplacementTrivia: computeReplacementTrivia);
		}

		/// <summary>
		/// Creates a new token with the specified old trivia replaced with a new trivia. The old trivia may appear in
		/// the token's leading or trailing trivia.
		/// </summary>
		/// <param name="token"></param>
		/// <param name="oldTrivia">The trivia to be replaced.</param>
		/// <param name="newTrivia">The new trivia to use in the new tree in place of the old
		/// trivia.</param>
		public static SyntaxToken ReplaceTrivia(this SyntaxToken token, SyntaxTrivia oldTrivia, SyntaxTrivia newTrivia)
		{
			return Syntax.SyntaxReplacer.Replace(token, trivia: new[] { oldTrivia }, computeReplacementTrivia: (o, r) => newTrivia);
		}


		/// <summary>
		/// Returns this list as a <see cref="Microsoft.CodeAnalysis.SeparatedSyntaxList&lt;TNode&gt;"/>.
		/// </summary>
		/// <typeparam name="TOther">The type of the list elements in the separated list.</typeparam>
		/// <returns></returns>
		internal static SeparatedSyntaxList<TOther> AsSeparatedList<TOther>(this SyntaxNodeOrTokenList list) where TOther : SyntaxNode
		{
			var builder = SeparatedSyntaxListBuilder<TOther>.Create();
			foreach (var i in list)
			{
				var node = i.AsNode();
				if (node != null)
				{
					builder.Add((TOther)node);
				}
				else
				{
					builder.AddSeparator(i.AsToken());
				}
			}

			return builder.ToList();
		}


		#region SyntaxTree
		public static CompilationUnitSyntax GetCompilationUnitRoot(this SyntaxTree tree, CancellationToken cancellationToken = default(CancellationToken))
		{
			return (CompilationUnitSyntax)tree.GetRoot(cancellationToken);
		}



		#endregion

	}
}
