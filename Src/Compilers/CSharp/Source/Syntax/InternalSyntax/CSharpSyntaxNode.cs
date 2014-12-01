﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Collections;
using Roslyn.Utilities;
using System.Diagnostics;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
	[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
	internal abstract class CSharpSyntaxNode : GreenNode
	{
		internal CSharpSyntaxNode(SyntaxKind kind)
			: base((ushort)kind)
		{
			GreenStats.NoteGreen(this);
		}

		internal CSharpSyntaxNode(SyntaxKind kind, int fullWidth)
			: base((ushort)kind, fullWidth)
		{
			GreenStats.NoteGreen(this);
		}

		internal CSharpSyntaxNode(SyntaxKind kind, DiagnosticInfo[] diagnostics)
			: base((ushort)kind, diagnostics)
		{
			GreenStats.NoteGreen(this);
		}

		internal CSharpSyntaxNode(SyntaxKind kind, DiagnosticInfo[] diagnostics, int fullWidth)
			: base((ushort)kind, diagnostics, fullWidth)
		{
			GreenStats.NoteGreen(this);
		}

		internal CSharpSyntaxNode(SyntaxKind kind, DiagnosticInfo[] diagnostics, SyntaxAnnotation[] annotations)
			: base((ushort)kind, diagnostics, annotations)
		{
			GreenStats.NoteGreen(this);
		}

		internal CSharpSyntaxNode(SyntaxKind kind, DiagnosticInfo[] diagnostics, SyntaxAnnotation[] annotations, int fullWidth)
			: base((ushort)kind, diagnostics, annotations, fullWidth)
		{
			GreenStats.NoteGreen(this);
		}

		internal CSharpSyntaxNode(ObjectReader reader)
			: base(reader)
		{
		}

		public override string Language
		{
			get { return LanguageNames.CSharp; }
		}

		public SyntaxKind Kind
		{
			get { return (SyntaxKind)this.RawKind; }
		}

		public override string KindText
		{
			get
			{
				return this.Kind.ToString();
			}
		}

		public override int RawContextualKind
		{
			get
			{
				return this.RawKind;
			}
		}

		public override bool IsStructuredTrivia
		{
			get
			{
				return this is StructuredTriviaSyntax;
			}
		}


		public override int GetSlotOffset(int index)
		{
			// This implementation should not support arbitrary
			// length lists since the implementation is O(n).
			System.Diagnostics.Debug.Assert(index < 11); // Max. slots 11 (TypeDeclarationSyntax)

			int offset = 0;
			for (int i = 0; i < index; i++)
			{
				var child = this.GetSlot(i);
				if (child != null)
				{
					offset += child.FullWidth;
				}
			}

			return offset;
		}

		internal ChildSyntaxList ChildNodesAndTokens()
		{
			return new ChildSyntaxList(this);
		}

		/// <summary>
		/// Enumerates all nodes of the tree rooted by this node (including this node).
		/// </summary>
		internal IEnumerable<GreenNode> EnumerateNodes()
		{
			yield return this;

			var stack = new Stack<ChildSyntaxList.Enumerator>(24);
			stack.Push(this.ChildNodesAndTokens().GetEnumerator());

			while (stack.Count > 0)
			{
				var en = stack.Pop();
				if (!en.MoveNext())
				{
					// no more down this branch
					continue;
				}

				var current = en.Current;
				stack.Push(en); // put it back on stack (struct enumerator)

				yield return current;

				if (!(current is SyntaxToken))
				{
					// not token, so consider children
					stack.Push(((CSharpSyntaxNode)current).ChildNodesAndTokens().GetEnumerator());
					continue;
				}
			}
		}

		public SyntaxToken GetFirstToken()
		{
			return (SyntaxToken)this.GetFirstTerminal();
		}

		public SyntaxToken GetLastToken()
		{
			return (SyntaxToken)this.GetLastTerminal();
		}

		public SyntaxToken GetLastNonmissingToken()
		{
			return (SyntaxToken)this.GetLastNonmissingTerminal();
		}

		public virtual CSharpSyntaxNode GetLeadingTrivia()
		{
			return null;
		}

		public override GreenNode GetLeadingTriviaCore()
		{
			return this.GetLeadingTrivia();
		}

		public virtual CSharpSyntaxNode GetTrailingTrivia()
		{
			return null;
		}

		public override GreenNode GetTrailingTriviaCore()
		{
			return this.GetTrailingTrivia();
		}

		public override string ToString()
		{
			var sb = PooledStringBuilder.GetInstance();
			var writer = new System.IO.StringWriter(sb.Builder, System.Globalization.CultureInfo.InvariantCulture);
			this.WriteTo(writer, leading: false, trailing: false);
			return sb.ToStringAndFree();
		}

		public override string ToFullString()
		{
			var sb = PooledStringBuilder.GetInstance();
			var writer = new System.IO.StringWriter(sb.Builder, System.Globalization.CultureInfo.InvariantCulture);
			this.WriteTo(writer, leading: true, trailing: true);
			return sb.ToStringAndFree();
		}

		public abstract TResult Accept<TResult>(CSharpSyntaxVisitor<TResult> visitor);

		public abstract void Accept(CSharpSyntaxVisitor visitor);

		/// <summary>
		/// Should only be called during construction.
		/// </summary>
		/// <remarks>
		/// This should probably be an extra constructor parameter, but we don't need more constructor overloads.
		/// </remarks>
		protected void SetFactoryContext(SyntaxFactoryContext context)
		{

		}

		internal static NodeFlags SetFactoryContext(NodeFlags flags, SyntaxFactoryContext context)
		{
			return flags;
		}

		public override AbstractSyntaxNavigator Navigator
		{
			get
			{
				return SyntaxNavigator.Instance;
			}
		}

		public override GreenNode CreateList(IEnumerable<GreenNode> nodes, bool alwaysCreateListNode)
		{
			if (nodes == null)
			{
				return null;
			}

			var list = nodes.Select(n => (CSharpSyntaxNode)n).ToArray();

			switch (list.Length)
			{
				case 0:
					return null;
				case 1:
					if (alwaysCreateListNode)
					{
						goto default;
					}
					else
					{
						return list[0];
					}
				case 2:
					return SyntaxList.List(list[0], list[1]);
				case 3:
					return SyntaxList.List(list[0], list[1], list[2]);
				default:
					return SyntaxList.List(list);
			}
		}

		public override Microsoft.CodeAnalysis.SyntaxToken CreateSeparator<TNode>(SyntaxNode element)
		{
			return Microsoft.CodeAnalysis.CSharp.SyntaxFactory.Token(SyntaxKind.CommaToken);
		}

		public override bool IsTriviaWithEndOfLine()
		{
			return this.Kind == SyntaxKind.EndOfLineTrivia
				|| this.Kind == SyntaxKind.SingleLineCommentTrivia;
		}

		// Use conditional weak table so we always return same identity for structured trivia
		private static readonly ConditionalWeakTable<SyntaxNode, Dictionary<Microsoft.CodeAnalysis.SyntaxTrivia, SyntaxNode>> structuresTable
			= new ConditionalWeakTable<SyntaxNode, Dictionary<Microsoft.CodeAnalysis.SyntaxTrivia, SyntaxNode>>();

		/// <summary>
		/// Gets the syntax node represented the structure of this trivia, if any. The HasStructure property can be used to 
		/// determine if this trivia has structure.
		/// </summary>
		/// <returns>
		/// A CSharpSyntaxNode derived from StructuredTriviaSyntax, with the structured view of this trivia node. 
		/// If this trivia node does not have structure, returns null.
		/// </returns>
		/// <remarks>
		/// Some types of trivia have structure that can be accessed as additional syntax nodes.
		/// These forms of trivia include: 
		///   directives, where the structure describes the structure of the directive.
		///   documentation comments, where the structure describes the XML structure of the comment.
		///   skipped tokens, where the structure describes the tokens that were skipped by the parser.
		/// </remarks>

		public override SyntaxNode GetStructure(Microsoft.CodeAnalysis.SyntaxTrivia trivia)
		{
			if (trivia.HasStructure)
			{
				var parent = trivia.Token.Parent;
				if (parent != null)
				{
					DebuggerUtilities.CallBeforeAcquiringLock(); //see method comment

					SyntaxNode structure;
					var structsInParent = structuresTable.GetOrCreateValue(parent);
					lock (structsInParent)
					{
						if (!structsInParent.TryGetValue(trivia, out structure))
						{
							structure = CSharp.Syntax.StructuredTriviaSyntax.Create(trivia);
							structsInParent.Add(trivia, structure);
						}
					}

					return structure;
				}
				else
				{
					return CSharp.Syntax.StructuredTriviaSyntax.Create(trivia);
				}
			}

			return null;
		}
	}
}