// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{
    using BoxedEnumerator = StrongBox<ChildSyntaxList.Enumerator>;

    /// <summary>
    /// Represents a non-terminal node in the syntax tree. This is the language agnostic equivalent of <see
    /// cref="T:Microsoft.CodeAnalysis.CSharp.SyntaxNode"/> and <see cref="T:Microsoft.CodeAnalysis.VisualBasic.SyntaxNode"/>.
    /// </summary>
    [DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
    public abstract class SyntaxNode
    {
        private readonly GreenNode green;
        private readonly SyntaxNode parent;
        internal SyntaxTree _syntaxTree;
        private readonly int position;

        internal SyntaxNode(GreenNode green, SyntaxNode parent, int position)
        {
            Debug.Assert(position >= 0, "position cannot be negative");
            Debug.Assert(parent == null || !parent.Green.IsList, "list cannot be a parent");

            this.position = position;
            this.green = green;
            this.parent = parent;
        }

        /// <summary>
        /// Used by structured trivia which has "parent == null", and therefore must know its
        /// SyntaxTree explicitly when created.
        /// </summary>
        internal SyntaxNode(GreenNode green, int position, SyntaxTree syntaxTree)
            : this(green, null, position)
        {
            this._syntaxTree = syntaxTree;
        }

        internal abstract AbstractSyntaxNavigator Navigator { get; }

        private string GetDebuggerDisplay()
        {
            return GetType().Name + " " + KindText + " " + ToString();
        }

        /// <summary>
        /// An integer representing the language specific kind of this node.
        /// </summary>
        public int RawKind
        {
            get { return green.RawKind; }
        }

        protected abstract string KindText { get; }

        /// <summary>
        /// The language name that this node is syntax of.
        /// </summary>
        public abstract string Language { get; }

        internal GreenNode Green
        {
            get { return this.green; }
        }

        internal int Position
        {
            get { return this.position; }
        }

        internal int EndPosition
        {
            get { return this.position + this.green.FullWidth; }
        }

        /// <summary>
        /// Returns SyntaxTree that owns the node or null if node does not belong to a
        /// SyntaxTree
        /// </summary>
        public SyntaxTree SyntaxTree
        {
            get
            {
                return this.SyntaxTreeCore;
            }
        }

        internal bool IsList
        {
            get
            {
                return this.Green.IsList;
            }
        }

        /// <summary>
        /// The absolute span of this node in characters, including its leading and trailing trivia.
        /// </summary>
        public TextSpan FullSpan
        {
            get
            {
                return new TextSpan(this.Position, this.Green.FullWidth);
            }
        }

        internal int SlotCount
        {
            get
            {
                return this.Green.SlotCount;
            }
        }

        /// <summary>
        /// The absolute span of this node in characters, not including its leading and trailing trivia.
        /// </summary>
        public TextSpan Span
        {
            get
            {
                // Start with the full span.
                var start = Position;
                var width = this.Green.FullWidth;

                // adjust for preceding trivia (avoid calling this twice, do not call Green.Width)
                var precedingWidth = this.Green.GetLeadingTriviaWidth();
                start += precedingWidth;
                width -= precedingWidth;

                // adjust for following trivia width
                width -= this.Green.GetTrailingTriviaWidth();

                Debug.Assert(width >= 0);
                return new TextSpan(start, width);
            }
        }

        /// <summary>
        /// Same as accessing <see cref="TextSpan.Start"/> on <see cref="Span"/>.
        /// </summary>
        /// <remarks>
        /// Slight performance improvement.
        /// </remarks>
        public int SpanStart
        {
            get
            {
                return Position + Green.GetLeadingTriviaWidth();
            }
        }

        /// <summary>
        /// The width of the node in characters, not including leading and trailing trivia.
        /// </summary>
        /// <remarks>
        /// The Width property returns the same value as Span.Length, but is somewhat more efficient.
        /// </remarks>
        internal int Width
        {
            get
            {
                return this.Green.Width;
            }
        }

        /// <summary>
        /// The complete width of the node in characters, including leading and trailing trivia.
        /// </summary>
        /// <remarks>The FullWidth property returns the same value as FullSpan.Length, but is
        /// somewhat more efficient.</remarks>
        internal int FullWidth
        {
            get
            {
                return this.Green.FullWidth;
            }
        }

        // this is used in cases where we know that a child is a node of particular type.
        internal SyntaxNode GetRed(ref SyntaxNode field, int slot)
        {
            var result = field;

            if (result == null)
            {
                var green = this.Green.GetSlot(slot);
                if (green != null)
                {
                    result = green.CreateRed(this, this.GetChildPosition(slot));
                    result = Interlocked.CompareExchange(ref field, result, null) ?? result;
                }
            }

            return result;
        }

        // special case of above function where slot = 0, does not need GetChildPosition 
        internal SyntaxNode GetRedAtZero(ref SyntaxNode field)
        {
            var result = field;

            if (result == null)
            {
                var green = this.Green.GetSlot(0);
                if (green != null)
                {
                    result = green.CreateRed(this, this.Position);
                    result = Interlocked.CompareExchange(ref field, result, null) ?? result;
                }
            }

            return result;
        }

        protected T GetRed<T>(ref T field, int slot) where T : SyntaxNode
        {
            var result = field;

            if (result == null)
            {
                var green = this.Green.GetSlot(slot);
                if (green != null)
                {
                    result = (T)green.CreateRed(this, this.GetChildPosition(slot));
                    result = Interlocked.CompareExchange(ref field, result, null) ?? result;
                }
            }

            return result;
        }

        // special case of above function where slot = 0, does not need GetChildPosition 
        protected T GetRedAtZero<T>(ref T field) where T : SyntaxNode
        {
            var result = field;

            if (result == null)
            {
                var green = this.Green.GetSlot(0);
                if (green != null)
                {
                    result = (T)green.CreateRed(this, this.Position);
                    result = Interlocked.CompareExchange(ref field, result, null) ?? result;
                }
            }

            return result;
        }

        /// <summary>
        /// This works the same as GetRed, but intended to be used in lists
        /// The only difference is that the public parent of the node is not the list, 
        /// but the list's parent. (element's grand parent).
        /// </summary>
        internal SyntaxNode GetRedElement(ref SyntaxNode element, int slot)
        {
            Debug.Assert(this.IsList);

            var result = element;

            if (result == null)
            {
                var green = this.Green.GetSlot(slot);
                result = green.CreateRed(this.Parent, this.GetChildPosition(slot)); // <- passing list's parent
                if (Interlocked.CompareExchange(ref element, result, null) != null)
                {
                    result = element;
                }
            }

            return result;
        }

        /// <summary>
        /// special cased helper for 2 and 3 children lists where child #1 may map to a token
        /// </summary>
        internal SyntaxNode GetRedElementIfNotToken(ref SyntaxNode element)
        {
            Debug.Assert(this.IsList);

            var result = element;

            if (result == null)
            {
                var green = this.Green.GetSlot(1);
                if (!green.IsToken)
                {
                    result = green.CreateRed(this.Parent, this.GetChildPosition(1)); // <- passing list's parent
                    if (Interlocked.CompareExchange(ref element, result, null) != null)
                    {
                        result = element;
                    }
                }
            }

            return result;
        }

        internal SyntaxNode GetWeakRedElement(ref WeakReference<SyntaxNode> slot, int index)
        {
            SyntaxNode value;
            WeakReference<SyntaxNode> weak = slot;
            if (weak != null && weak.TryGetTarget(out value))
            {
                return value;
            }

            return CreateWeakItem(ref slot, index);
        }

        // handle a miss
        private SyntaxNode CreateWeakItem(ref WeakReference<SyntaxNode> slot, int index)
        {
            var greenChild = this.Green.GetSlot(index);
            var newNode = greenChild.CreateRed(this.Parent, GetChildPosition(index));
            var newWeakReference = new WeakReference<SyntaxNode>(newNode);

            while (true)
            {
                SyntaxNode previousNode;
                WeakReference<SyntaxNode> previousWeakReference = slot;
                if (previousWeakReference != null && previousWeakReference.TryGetTarget(out previousNode))
                {
                    return previousNode;
                }

                if (Interlocked.CompareExchange(ref slot, newWeakReference, previousWeakReference) == previousWeakReference)
                {
                    return newNode;
                }
            }
        }


        /// <summary>
        /// Returns the string representation of this node, not including its leading and trailing trivia.
        /// </summary>
        /// <returns>The string representation of this node, not including its leading and trailing trivia.</returns>
        /// <remarks>The length of the returned string is always the same as Span.Length</remarks>
        public abstract override string ToString();

        /// <summary>
        /// Returns full string representation of this node including its leading and trailing trivia.
        /// </summary>
        /// <returns>The full string representation of this node including its leading and trailing trivia.</returns>
        /// <remarks>The length of the returned string is always the same as FullSpan.Length</remarks>
        public abstract string ToFullString();

        /// <summary>
        /// Writes the full text of this node to the specified TextWriter.
        /// </summary>
        public abstract void WriteTo(System.IO.TextWriter writer);

        /// <summary>
        /// Gets the full text of this node as an new SourceText instance.
        /// </summary>
        /// <returns></returns>
        public SourceText GetText()
        {
            return TextUtilities.Create(this.WriteTo);
        }

        /// <summary>
        /// Determine whether this node is structurally equivalent to another.
        /// </summary>
        public bool IsEquivalentTo(SyntaxNode other)
        {
            return EquivalentToCore(other);
        }

        /// <summary>
        /// Determines whether the node represents a language construct that was actually parsed
        /// from the source code. Missing nodes are generated by the parser in error scenarios to
        /// represent constructs that should have been present in the source code in order to
        /// compile successfully but were actually missing.
        /// </summary>
        public bool IsMissing
        {
            get
            {
                return this.Green.IsMissing;
            }
        }

        /// <summary>
        /// Determines whether this node is a descendant of a structured trivia.
        /// </summary>
        public bool IsPartOfStructuredTrivia()
        {
            for (var node = this; node != null; node = node.Parent)
            {
                if (node.IsStructuredTrivia)
                    return true;
            }

            return false;
        }

        /// <summary>
        /// Determines whether this node represents a structured trivia.
        /// </summary>
        public bool IsStructuredTrivia
        {
            get
            {
                return this.Green.IsStructuredTrivia;
            }
        }

        /// <summary>
        /// Determines whether a descendant trivia of this node is structured.
        /// </summary>
        public bool HasStructuredTrivia
        {
            get
            {
                return this.Green.ContainsStructuredTrivia && !this.Green.IsStructuredTrivia;
            }
        }

        /// <summary>
        /// Determines whether this node has any descendant skipped text.
        /// </summary>
        public bool ContainsSkippedText
        {
            get
            {
                return this.Green.ContainsSkippedText;
            }
        }

        /// <summary>
        /// Determines whether this node has any descendant preprocessor directives.
        /// </summary>
        public bool ContainsDirectives
        {
            get
            {
                return this.Green.ContainsDirectives;
            }
        }

        /// <summary>
        /// Determines whether this node or any of its descendant nodes, tokens or trivia have any diagnostics on them. 
        /// </summary>
        public bool ContainsDiagnostics
        {
            get
            {
                return this.Green.ContainsDiagnostics;
            }
        }

        /// <summary>
        /// Determines whether this node has any leading trivia.
        /// </summary>
        public bool HasLeadingTrivia
        {
            get
            {
                return this.Green.HasLeadingTrivia;
            }
        }

        /// <summary>
        /// Determines whether this node has any trailing trivia.
        /// </summary>
        public bool HasTrailingTrivia
        {
            get
            {
                return this.Green.HasTrailingTrivia;
            }
        }

        /// <summary>
        /// Gets a node at given node index without forcing its creation.
        /// If node was not created it would return null.
        /// </summary>
        internal abstract SyntaxNode GetCachedSlot(int index);

        internal int GetChildIndex(int slot)
        {
            int index = 0;
            for (int i = 0; i < slot; i++)
            {
                var item = this.Green.GetSlot(i);
                if (item != null)
                {
                    if (item.IsList)
                    {
                        index += item.SlotCount;
                    }
                    else
                    {
                        index++;
                    }
                }
            }

            return index;
        }

        /// <summary>
        /// This function calculates the offset of a child at given position. It is very common that
        /// some children to the left of the given index already know their positions so we first
        /// check if that is the case. In a worst case the cost is O(n), but it is not generally an
        /// issue because number of children in regular nodes is fixed and small. In a case where
        /// the number of children could be large (lists) this function is overridden with more
        /// efficient implementations.
        /// </summary>
        internal virtual int GetChildPosition(int index)
        {
            int offset = 0;
            var green = this.Green;
            while (index > 0)
            {
                index--;
                var prevSibling = this.GetCachedSlot(index);
                if (prevSibling != null)
                {
                    return prevSibling.EndPosition + offset;
                }
                var greenChild = green.GetSlot(index);
                if (greenChild != null)
                {
                    offset += greenChild.FullWidth;
                }
            }

            return this.Position + offset;
        }

        public Location GetLocation()
        {
            return this.SyntaxTree.GetLocation(this.Span);
        }

        /// <summary>
        /// Gets a list of all the diagnostics in the sub tree that has this node as its root.
        /// This method does not filter diagnostics based on #pragmas and compiler options
        /// like nowarn, warnaserror etc.
        /// </summary>
        public IEnumerable<Diagnostic> GetDiagnostics()
        {
            return this.SyntaxTree.GetDiagnostics(this);
        }

        /// <summary>
        /// Gets an SyntaxReference for this syntax node. CommonSyntaxReferences can be used to
        /// regain access to a syntax node without keeping the entire tree and source text in
        /// memory.
        /// </summary>
        public SyntaxReference GetReference()
        {
            return this.SyntaxTree.GetReference(this);
        }

        #region Node Lookup

        /// <summary>
        /// The node that contains this node in its Children collection.
        /// </summary>
        public SyntaxNode Parent
        {
            get
            {
                return this.parent;
            }
        }

        public virtual SyntaxTrivia ParentTrivia
        {
            get
            {
                return default(SyntaxTrivia);
            }
        }

        internal SyntaxNode ParentOrStructuredTriviaParent
        {
            get
            {
                return GetParent(this, ascendOutOfTrivia: true);
            }
        }

        /// <summary>
        /// The list of child nodes and tokens of this node, where each element is a SyntaxNodeOrToken instance.
        /// </summary>
        public ChildSyntaxList ChildNodesAndTokens()
        {
            return new ChildSyntaxList(this);
        }

        public abstract SyntaxNodeOrToken ChildThatContainsPosition(int position);

        /// <summary>
        /// Gets node at given node index. 
        /// This WILL force node creation if node has not yet been created.
        /// </summary>
        internal abstract SyntaxNode GetNodeSlot(int slot);

        /// <summary>
        /// Gets a list of the child nodes in prefix document order.
        /// </summary>
        public IEnumerable<SyntaxNode> ChildNodes()
        {
            foreach (var nodeOrToken in this.ChildNodesAndTokens())
            {
                if (nodeOrToken.IsNode)
                {
                    yield return nodeOrToken.AsNode();
                }
            }
        }

        /// <summary>
        /// Gets a list of ancestor nodes
        /// </summary>
        public IEnumerable<SyntaxNode> Ancestors(bool ascendOutOfTrivia = true)
        {
            return this.Parent != null
                ? this.Parent.AncestorsAndSelf(ascendOutOfTrivia)
                : SpecializedCollections.EmptyEnumerable<SyntaxNode>();
        }

        /// <summary>
        /// Gets a list of ancestor nodes (including this node) 
        /// </summary>
        public IEnumerable<SyntaxNode> AncestorsAndSelf(bool ascendOutOfTrivia = true)
        {
            for (var node = this; node != null; node = GetParent(node, ascendOutOfTrivia))
            {
                yield return node;
            }
        }

        private static SyntaxNode GetParent(SyntaxNode node, bool ascendOutOfTrivia)
        {
            var parent = node.Parent;
            if (parent == null && ascendOutOfTrivia)
            {
                var structuredTrivia = node as IStructuredTriviaSyntax;
                if (structuredTrivia != null)
                {
                    parent = structuredTrivia.ParentTrivia.Token.Parent;
                }
            }

            return parent;
        }

        /// <summary>
        /// Gets the first node of type TNode that matches the predicate.
        /// </summary>
        public TNode FirstAncestorOrSelf<TNode>(Func<TNode, bool> predicate = null, bool ascendOutOfTrivia = true)
            where TNode : SyntaxNode
        {
            for (var node = this; node != null; node = GetParent(node, ascendOutOfTrivia))
            {
                var tnode = node as TNode;
                if (tnode != null && (predicate == null || predicate(tnode)))
                {
                    return tnode;
                }
            }

            return default(TNode);
        }

        /// <summary>
        /// Gets a list of descendant nodes in prefix document order.
        /// </summary>
        /// <param name="descendIntoChildren">An optional function that determines if the search descends into the argument node's children.</param>
        /// <param name="descendIntoTrivia">Determines if nodes that are part of structured trivia are included in the list.</param>
        public IEnumerable<SyntaxNode> DescendantNodes(Func<SyntaxNode, bool> descendIntoChildren = null, bool descendIntoTrivia = false)
        {
            return DescendantNodesImpl(this.FullSpan, descendIntoChildren, descendIntoTrivia, includeSelf: false);
        }

        /// <summary>
        /// Gets a list of descendant nodes in prefix document order.
        /// </summary>
        /// <param name="span">The span the node's full span must intersect.</param>
        /// <param name="descendIntoChildren">An optional function that determines if the search descends into the argument node's children.</param>
        /// <param name="descendIntoTrivia">Determines if nodes that are part of structured trivia are included in the list.</param>
        public IEnumerable<SyntaxNode> DescendantNodes(TextSpan span, Func<SyntaxNode, bool> descendIntoChildren = null, bool descendIntoTrivia = false)
        {
            return DescendantNodesImpl(span, descendIntoChildren, descendIntoTrivia, includeSelf: false);
        }

        /// <summary>
        /// Gets a list of descendant nodes (including this node) in prefix document order.
        /// </summary>
        /// <param name="descendIntoChildren">An optional function that determines if the search descends into the argument node's children.</param>
        /// <param name="descendIntoTrivia">Determines if nodes that are part of structured trivia are included in the list.</param>
        public IEnumerable<SyntaxNode> DescendantNodesAndSelf(Func<SyntaxNode, bool> descendIntoChildren = null, bool descendIntoTrivia = false)
        {
            return DescendantNodesImpl(this.FullSpan, descendIntoChildren, descendIntoTrivia, includeSelf: true);
        }

        /// <summary>
        /// Gets a list of descendant nodes (including this node) in prefix document order.
        /// </summary>
        /// <param name="span">The span the node's full span must intersect.</param>
        /// <param name="descendIntoChildren">An optional function that determines if the search descends into the argument node's children.</param>
        /// <param name="descendIntoTrivia">Determines if nodes that are part of structured trivia are included in the list.</param>
        public IEnumerable<SyntaxNode> DescendantNodesAndSelf(TextSpan span, Func<SyntaxNode, bool> descendIntoChildren = null, bool descendIntoTrivia = false)
        {
            return DescendantNodesImpl(span, descendIntoChildren, descendIntoTrivia, includeSelf: true);
        }

        /// <summary>
        /// Gets a list of descendant nodes and tokens in prefix document order.
        /// </summary>
        /// <param name="descendIntoChildren">An optional function that determines if the search descends into the argument node's children.</param>
        /// <param name="descendIntoTrivia">Determines if nodes that are part of structured trivia are included in the list.</param>
        public IEnumerable<SyntaxNodeOrToken> DescendantNodesAndTokens(Func<SyntaxNode, bool> descendIntoChildren = null, bool descendIntoTrivia = false)
        {
            return DescendantNodesAndTokensImpl(this.FullSpan, descendIntoChildren, descendIntoTrivia, includeSelf: false);
        }

        /// <summary>
        /// Gets a list of the descendant nodes and tokens in prefix document order.
        /// </summary>
        /// <param name="span">The span the node's full span must intersect.</param>
        /// <param name="descendIntoChildren">An optional function that determines if the search descends into the argument node's children.</param>
        /// <param name="descendIntoTrivia">Determines if nodes that are part of structured trivia are included in the list.</param>
        public IEnumerable<SyntaxNodeOrToken> DescendantNodesAndTokens(TextSpan span, Func<SyntaxNode, bool> descendIntoChildren = null, bool descendIntoTrivia = false)
        {
            return DescendantNodesAndTokensImpl(span, descendIntoChildren, descendIntoTrivia, includeSelf: false);
        }

        /// <summary>
        /// Gets a list of descendant nodes and tokens (including this node) in prefix document order.
        /// </summary>
        /// <param name="descendIntoChildren">An optional function that determines if the search descends into the argument node's children.</param>
        /// <param name="descendIntoTrivia">Determines if nodes that are part of structured trivia are included in the list.</param>
        public IEnumerable<SyntaxNodeOrToken> DescendantNodesAndTokensAndSelf(Func<SyntaxNode, bool> descendIntoChildren = null, bool descendIntoTrivia = false)
        {
            return DescendantNodesAndTokensImpl(this.FullSpan, descendIntoChildren, descendIntoTrivia, includeSelf: true);
        }

        /// <summary>
        /// Gets a list of the descendant nodes and tokens (including this node) in prefix document order.
        /// </summary>
        /// <param name="span">The span the node's full span must intersect.</param>
        /// <param name="descendIntoChildren">An optional function that determines if the search descends into the argument node's children.</param>
        /// <param name="descendIntoTrivia">Determines if nodes that are part of structured trivia are included in the list.</param>
        public IEnumerable<SyntaxNodeOrToken> DescendantNodesAndTokensAndSelf(TextSpan span, Func<SyntaxNode, bool> descendIntoChildren = null, bool descendIntoTrivia = false)
        {
            return DescendantNodesAndTokensImpl(span, descendIntoChildren, descendIntoTrivia, includeSelf: true);
        }

        private IEnumerable<SyntaxNode> DescendantNodesImpl(TextSpan span, Func<SyntaxNode, bool> descendIntoChildren, bool descendIntoTrivia, bool includeSelf)
        {
            return descendIntoTrivia
                ? DescendantNodesAndTokensImpl(span, descendIntoChildren, descendIntoTrivia, includeSelf).Where(e => e.IsNode).Select(e => e.AsNode())
                : DescendantNodesOnly(span, descendIntoChildren, includeSelf);
        }

        private IEnumerable<SyntaxNodeOrToken> DescendantNodesAndTokensImpl(TextSpan span, Func<SyntaxNode, bool> descendIntoChildren, bool descendIntoTrivia, bool includeSelf)
        {
            return descendIntoTrivia
                ? DescendantNodesAndTokensIntoTrivia(span, descendIntoChildren, includeSelf)
                : DescendantNodesAndTokensOnly(span, descendIntoChildren, includeSelf);
        }

        private static bool IsInSpan(TextSpan span, TextSpan childSpan)
        {
            return childSpan.OverlapsWith(span)
                // special case for zero-width tokens (OverlapsWith never returns true for these)
                || (childSpan.Length == 0 && childSpan.IntersectsWith(span));
        }

        private IEnumerable<SyntaxNode> DescendantNodesOnly(TextSpan span, Func<SyntaxNode, bool> descendIntoChildren, bool includeSelf)
        {
            if (includeSelf && IsInSpan(span, this.FullSpan))
            {
                yield return this;
            }

            if (descendIntoChildren != null && !descendIntoChildren(this))
            {
                yield break;
            }

            // Keep around the strongboxes we use so that don't have to allocate unnecessarily.  Use
            // strong boxes so we can mutate the enumerators (which are structs) without having to
            // pop them off the stack and push them back on.  This saves about 10% of the time when
            // enumerating the tokens in a tree.
            var boxes = new Queue<BoxedEnumerator>();
            var stack = new Stack<BoxedEnumerator>();
            stack.Push(new BoxedEnumerator(this.ChildNodesAndTokens().GetEnumerator()));

            while (stack.Count > 0)
            {
                var enumerator = stack.Peek();
                if (!enumerator.Value.MoveNext())
                {
                    var box = stack.Pop();
                    box.Value = default(ChildSyntaxList.Enumerator);
                    boxes.Enqueue(box);
                }
                else
                {
                    var nodeValue = enumerator.Value.Current.AsNode();
                    if (nodeValue != null && IsInSpan(span, nodeValue.FullSpan))
                    {
                        yield return nodeValue;

                        if (descendIntoChildren == null || descendIntoChildren(nodeValue))
                        {
                            var box = boxes.Count == 0
                                ? new BoxedEnumerator()
                                : boxes.Dequeue();
                            box.Value = nodeValue.ChildNodesAndTokens().GetEnumerator();
                            stack.Push(box);
                        }
                    }
                }
            }
        }

        private IEnumerable<SyntaxNodeOrToken> DescendantNodesAndTokensOnly(TextSpan span, Func<SyntaxNode, bool> descendIntoChildren, bool includeSelf)
        {
            if (includeSelf && IsInSpan(span, this.FullSpan))
            {
                yield return this;
            }

            if (descendIntoChildren != null && !descendIntoChildren(this))
            {
                yield break;
            }

            // Keep around the strongboxes we use so that don't have to allocate unnecessarily.  Use
            // strong boxes so we can mutate the enumerators (which are structs) without having to
            // pop them off the stack and push them back on.  This saves about 10% of the time when
            // enumerating the tokens in a tree.
            var boxes = new Queue<BoxedEnumerator>();
            var stack = new Stack<BoxedEnumerator>();
            stack.Push(new BoxedEnumerator(this.ChildNodesAndTokens().GetEnumerator()));

            while (stack.Count > 0)
            {
                var enumerator = stack.Peek();
                if (!enumerator.Value.MoveNext())
                {
                    var box = stack.Pop();
                    box.Value = default(ChildSyntaxList.Enumerator);
                    boxes.Enqueue(box);
                }
                else
                {
                    var value = enumerator.Value.Current;

                    if (IsInSpan(span, value.FullSpan))
                    {
                        yield return value;

                        var nodeValue = value.AsNode();
                        if (nodeValue != null)
                        {
                            if (descendIntoChildren == null || descendIntoChildren(nodeValue))
                            {
                                var box = boxes.Count == 0
                                    ? new BoxedEnumerator()
                                    : boxes.Dequeue();
                                box.Value = value.AsNode().ChildNodesAndTokens().GetEnumerator();
                                stack.Push(box);
                            }
                        }
                    }
                }
            }
        }

        private IEnumerable<SyntaxNodeOrToken> DescendantNodesAndTokensIntoTrivia(TextSpan span, Func<SyntaxNode, bool> descendIntoChildren, bool includeSelf)
        {
            if (includeSelf && IsInSpan(span, this.FullSpan))
            {
                yield return this;
            }

            if (descendIntoChildren != null && !descendIntoChildren(this))
            {
                yield break;
            }

            var stack = new Stack<Union<ChildSyntaxList.Enumerator, SyntaxTriviaList.Enumerator, SyntaxNodeOrToken>>();
            stack.Push(this.ChildNodesAndTokens().GetEnumerator());

            while (stack.Count > 0)
            {
                var union = stack.Pop();

                switch (union.Discriminator)
                {
                    case 0: // child nodes & tokens
                        {
                            var enumerator = union.Value0;
                            if (enumerator.MoveNext())
                            {
                                var value = enumerator.Current;
                                stack.Push(enumerator);

                                if (IsInSpan(span, value.FullSpan))
                                {
                                    if (value.IsNode)
                                    {
                                        // parent nodes come before children (prefix document order)
                                        yield return value;

                                        var nodeValue = value.AsNode();

                                        if (descendIntoChildren == null || descendIntoChildren(nodeValue))
                                        {
                                            stack.Push(value.AsNode().ChildNodesAndTokens().GetEnumerator());
                                        }
                                    }
                                    else if (value.IsToken)
                                    {
                                        var token = value.AsToken();

                                        // only look through trivia if this node has structured trivia
                                        if (token.HasStructuredTrivia)
                                        {
                                            // trailing trivia comes last
                                            if (token.HasTrailingTrivia)
                                            {
                                                stack.Push(token.TrailingTrivia.GetEnumerator());
                                            }

                                            // tokens come between leading and trailing trivia
                                            stack.Push(value);

                                            // leading trivia comes first
                                            if (token.HasLeadingTrivia)
                                            {
                                                stack.Push(token.LeadingTrivia.GetEnumerator());
                                            }
                                        }
                                        else
                                        {
                                            // no structure trivia, so just yield this token now
                                            yield return value;
                                        }
                                    }
                                }
                            }
                            break;
                        }

                    case 1: // trivia
                        {
                            // yield structure nodes and enumerate their children
                            var enumerator = union.Value1;

                            if (enumerator.MoveNext())
                            {
                                var trivia = enumerator.Current;
                                stack.Push(enumerator);

                                if (trivia.HasStructure && IsInSpan(span, trivia.FullSpan))
                                {
                                    var structureNode = trivia.GetStructure();

                                    // parent nodes come before children (prefix document order)
                                    yield return structureNode;

                                    if (descendIntoChildren == null || descendIntoChildren(structureNode))
                                    {
                                        stack.Push(structureNode.ChildNodesAndTokens().GetEnumerator());
                                    }
                                }
                            }
                            break;
                        }

                    case 2: // single node or token
                        yield return union.Value2;
                        break;
                }
            }
        }

        /// <summary>
        /// Finds the node with the smallest <see cref="FullSpan"/> that contains <paramref name="span"/>.
        /// <paramref name="getInnermostNodeForTie"/> is used to determine the behavior in case of a tie (i.e. a node having the same span as its parent).
        /// If <paramref name="getInnermostNodeForTie"/> is true, then it returns lowest descending node encompassing the given <paramref name="span"/>.
        /// Otherwise, it returns the outermost node encompassing the given <paramref name="span"/>.
        /// </summary>
        /// <remarks>
        /// TODO: This should probably be reimplemented with <see cref="ChildThatContainsPosition"/>
        /// </remarks>
        /// <exception cref="ArgumentOutOfRangeException">This exception is thrown if <see cref="FullSpan"/> doesn't contain the given span.</exception>
        public SyntaxNode FindNode(TextSpan span, bool findInsideTrivia = false, bool getInnermostNodeForTie = false)
        {
            if (!this.FullSpan.Contains(span))
            {
                throw new ArgumentOutOfRangeException("span");
            }

            var node = FindToken(span.Start, findInsideTrivia)
                .Parent
                .FirstAncestorOrSelf<SyntaxNode>(a => a.FullSpan.Contains(span));

            // Tie-breaking.
            if (!getInnermostNodeForTie)
            {
                while (true)
                {
                    var parent = node.Parent;
                    // NOTE: We care about FullSpan equality, but FullWidth is cheaper and equivalent.
                    if (parent == null || parent.FullWidth != node.FullWidth) break;
                    node = parent;
                }
            }

            return node;
        }

        #endregion

        #region Token Lookup
        /// <summary>
        /// Finds a descendant token of this node whose span includes the supplied position. 
        /// </summary>
        /// <param name="position">The character position of the token relative to the beginning of the file.</param>
        /// <param name="findInsideTrivia">
        /// True to return tokens that are part of trivia. If false finds the token whose full span (including trivia)
        /// includes the position.
        /// </param>
        public SyntaxToken FindToken(int position, bool findInsideTrivia = false)
        {
            return FindTokenCore(position, findInsideTrivia);
        }

        /// <summary>
        /// Gets the first token of the tree rooted by this node. Skips zero-width tokens.
        /// </summary>
        /// <returns>The first token or <c>default(SyntaxToken)</c> if it doesn't exist.</returns>
        public SyntaxToken GetFirstToken(bool includeZeroWidth = false, bool includeSkipped = false, bool includeDirectives = false, bool includeDocumentationComments = false)
        {
            return this.Navigator.GetFirstToken(this, includeZeroWidth, includeSkipped, includeDirectives, includeDocumentationComments);
        }

        /// <summary>
        /// Gets the last token of the tree rooted by this node. Skips zero-width tokens.
        /// </summary>
        /// <returns>The last token or <c>default(SyntaxToken)</c> if it doesn't exist.</returns>
        public SyntaxToken GetLastToken(bool includeZeroWidth = false, bool includeSkipped = false, bool includeDirectives = false, bool includeDocumentationComments = false)
        {
            return this.Navigator.GetLastToken(this, includeZeroWidth, includeSkipped, includeDirectives, includeDocumentationComments);
        }

        /// <summary>
        /// Gets a list of the direct child tokens of this node.
        /// </summary>
        public IEnumerable<SyntaxToken> ChildTokens()
        {
            foreach (var nodeOrToken in this.ChildNodesAndTokens())
            {
                if (nodeOrToken.IsToken)
                {
                    yield return nodeOrToken.AsToken();
                }
            }
        }

        /// <summary>
        /// Gets a list of all the tokens in the span of this node.
        /// </summary>
        public IEnumerable<SyntaxToken> DescendantTokens(Func<SyntaxNode, bool> descendIntoChildren = null, bool descendIntoTrivia = false)
        {
            return this.DescendantNodesAndTokens(descendIntoChildren, descendIntoTrivia).Where(sn => sn.IsToken).Select(sn => sn.AsToken());
        }

        /// <summary>
        /// Gets a list of all the tokens in the full span of this node.
        /// </summary>
        public IEnumerable<SyntaxToken> DescendantTokens(TextSpan span, Func<SyntaxNode, bool> descendIntoChildren = null, bool descendIntoTrivia = false)
        {
            return this.DescendantNodesAndTokens(span, descendIntoChildren, descendIntoTrivia).Where(sn => sn.IsToken).Select(sn => sn.AsToken());
        }

        #endregion

        #region Trivia Lookup
        /// <summary>
        /// The list of trivia that appears before this node in the source code and are attached to a token that is a
        /// descendant of this node.
        /// </summary>
        public SyntaxTriviaList GetLeadingTrivia()
        {
            return GetFirstToken(includeZeroWidth: true).LeadingTrivia;
        }

        /// <summary>
        /// The list of trivia that appears after this node in the source code and are attached to a token that is a
        /// descendant of this node.
        /// </summary>
        public SyntaxTriviaList GetTrailingTrivia()
        {
            return GetLastToken(includeZeroWidth: true).TrailingTrivia;
        }

        /// <summary>
        /// Finds a descendant trivia of this node whose span includes the supplied position.
        /// </summary>
        /// <param name="position">The character position of the trivia relative to the beginning of the file.</param>
        /// <param name="findInsideTrivia">
        /// True to return tokens that are part of trivia. If false finds the token whose full span (including trivia)
        /// includes the position.
        /// </param>
        public SyntaxTrivia FindTrivia(int position, bool findInsideTrivia = false)
        {
            return FindTriviaCore(position, findInsideTrivia);
        }

        /// <summary>
        /// Get a list of all the trivia associated with the descendant nodes and tokens.
        /// </summary>
        public IEnumerable<SyntaxTrivia> DescendantTrivia(Func<SyntaxNode, bool> descendIntoChildren = null, bool descendIntoTrivia = false)
        {
            return DescendantTrivia(this.FullSpan, descendIntoChildren, descendIntoTrivia);
        }

        /// <summary>
        /// Get a list of all the trivia associated with the descendant nodes and tokens.
        /// </summary>
        public IEnumerable<SyntaxTrivia> DescendantTrivia(TextSpan span, Func<SyntaxNode, bool> descendIntoChildren = null, bool descendIntoTrivia = false)
        {
            if (descendIntoTrivia)
            {
                return DescendantTriviaIntoTrivia(span, descendIntoChildren);
            }
            else
            {
                return DescendantTriviaOnly(span, descendIntoChildren);
            }
        }

        private IEnumerable<SyntaxTrivia> DescendantTriviaOnly(TextSpan span, Func<SyntaxNode, bool> descendIntoChildren)
        {
            if (descendIntoChildren != null && !descendIntoChildren(this))
            {
                yield break;
            }

            var stack = new Stack<ChildSyntaxList.Enumerator>();
            stack.Push(this.ChildNodesAndTokens().GetEnumerator());

            while (stack.Count > 0)
            {
                var enumerator = stack.Pop();
                if (enumerator.MoveNext())
                {
                    var value = enumerator.Current;
                    stack.Push(enumerator);

                    if (IsInSpan(span, value.FullSpan))
                    {
                        if (value.IsNode)
                        {
                            var nodeValue = value.AsNode();

                            if (descendIntoChildren == null || descendIntoChildren(nodeValue))
                            {
                                stack.Push(value.AsNode().ChildNodesAndTokens().GetEnumerator());
                            }
                        }
                        else if (value.IsToken)
                        {
                            var token = value.AsToken();

                            foreach (var trivia in token.LeadingTrivia)
                            {
                                if (IsInSpan(span, trivia.FullSpan))
                                {
                                    yield return trivia;
                                }
                            }

                            foreach (var trivia in token.TrailingTrivia)
                            {
                                if (IsInSpan(span, trivia.FullSpan))
                                {
                                    yield return trivia;
                                }
                            }
                        }
                    }
                }
            }
        }

        private IEnumerable<SyntaxTrivia> DescendantTriviaIntoTrivia(TextSpan span, Func<SyntaxNode, bool> descendIntoChildren)
        {
            if (descendIntoChildren != null && !descendIntoChildren(this))
            {
                yield break;
            }

            var stack = new Stack<Union<ChildSyntaxList.Enumerator, SyntaxTriviaList.Enumerator>>();
            stack.Push(this.ChildNodesAndTokens().GetEnumerator());

            while (stack.Count > 0)
            {
                var union = stack.Pop();

                switch (union.Discriminator)
                {
                    case 0: // child nodes & tokens
                        {
                            var enumerator = union.Value0;
                            if (enumerator.MoveNext())
                            {
                                var value = enumerator.Current;
                                stack.Push(enumerator);

                                if (IsInSpan(span, value.FullSpan))
                                {
                                    if (value.IsNode)
                                    {
                                        var nodeValue = value.AsNode();

                                        if (descendIntoChildren == null || descendIntoChildren(nodeValue))
                                        {
                                            stack.Push(value.AsNode().ChildNodesAndTokens().GetEnumerator());
                                        }
                                    }
                                    else if (value.IsToken)
                                    {
                                        var token = value.AsToken();

                                        if (token.HasTrailingTrivia)
                                        {
                                            stack.Push(token.TrailingTrivia.GetEnumerator());
                                        }

                                        if (token.HasLeadingTrivia)
                                        {
                                            stack.Push(token.LeadingTrivia.GetEnumerator());
                                        }
                                    }
                                }
                            }
                            break;
                        }

                    case 1: // trivia
                        {
                            // yield structure nodes and enumerate their children
                            var enumerator = union.Value1;

                            if (enumerator.MoveNext())
                            {
                                var trivia = enumerator.Current;
                                stack.Push(enumerator);

                                if (IsInSpan(span, trivia.FullSpan))
                                {
                                    yield return trivia;
                                }

                                if (trivia.HasStructure)
                                {
                                    var structureNode = trivia.GetStructure();

                                    if (descendIntoChildren == null || descendIntoChildren(structureNode))
                                    {
                                        stack.Push(structureNode.ChildNodesAndTokens().GetEnumerator());
                                    }
                                }
                            }
                            break;
                        }
                }
            }
        }


        #endregion

        #region Annotations

        /// <summary>
        /// Determines whether this node or any sub node, token or trivia has annotations.
        /// </summary>
        public bool ContainsAnnotations
        {
            get { return this.Green.ContainsAnnotations; }
        }

        /// <summary>
        /// Determines whether this node has any annotations with the specific annotation kind.
        /// </summary>
        public bool HasAnnotations(string annotationKind)
        {
            return this.Green.HasAnnotations(annotationKind);
        }

        /// <summary>
        /// Determines whether this node has any annotations with any of the specific annotation kinds.
        /// </summary>
        public bool HasAnnotations(IEnumerable<string> annotationKinds)
        {
            return this.Green.HasAnnotations(annotationKinds);
        }

        /// <summary>
        /// Determines whether this node has the specific annotation.
        /// </summary>
        public bool HasAnnotation(SyntaxAnnotation annotation)
        {
            return this.Green.HasAnnotation(annotation);
        }

        /// <summary>
        /// Gets all the annotations with the specified annotation kind. 
        /// </summary>
        public IEnumerable<SyntaxAnnotation> GetAnnotations(string annotationKind)
        {
            return this.Green.GetAnnotations(annotationKind);
        }

        /// <summary>
        /// Gets all the annotations with the specified annotation kinds. 
        /// </summary>
        public IEnumerable<SyntaxAnnotation> GetAnnotations(IEnumerable<string> annotationKinds)
        {
            return this.Green.GetAnnotations(annotationKinds);
        }

        internal SyntaxAnnotation[] GetAnnotations()
        {
            return this.Green.GetAnnotations();
        }

        /// <summary>
        /// Gets all nodes and tokens with an annotation of the specified annotation kind.
        /// </summary>
        public IEnumerable<SyntaxNodeOrToken> GetAnnotatedNodesAndTokens(string annotationKind)
        {
            return this.DescendantNodesAndTokensAndSelf(n => n.ContainsAnnotations, descendIntoTrivia: true)
                .Where(t => t.HasAnnotations(annotationKind));
        }

        /// <summary>
        /// Gets all nodes and tokens with an annotation of the specified annotation kinds.
        /// </summary>
        public IEnumerable<SyntaxNodeOrToken> GetAnnotatedNodesAndTokens(params string[] annotationKinds)
        {
            return this.DescendantNodesAndTokensAndSelf(n => n.ContainsAnnotations, descendIntoTrivia: true)
                .Where(t => t.HasAnnotations(annotationKinds));
        }

        /// <summary>
        /// Gets all nodes and tokens with the specified annotation.
        /// </summary>
        public IEnumerable<SyntaxNodeOrToken> GetAnnotatedNodesAndTokens(SyntaxAnnotation annotation)
        {
            return this.DescendantNodesAndTokensAndSelf(n => n.ContainsAnnotations, descendIntoTrivia: true)
                .Where(t => t.HasAnnotation(annotation));
        }

        /// <summary>
        /// Gets all nodes with the specified annotation.
        /// </summary>
        public IEnumerable<SyntaxNode> GetAnnotatedNodes(SyntaxAnnotation syntaxAnnotation)
        {
            return this.GetAnnotatedNodesAndTokens(syntaxAnnotation).Where(n => n.IsNode).Select(n => n.AsNode());
        }

        /// <summary>
        /// Gets all nodes with the specified annotation kind.
        /// </summary>
        /// <param name="annotationKind"></param>
        /// <returns></returns>
        public IEnumerable<SyntaxNode> GetAnnotatedNodes(string annotationKind)
        {
            return this.GetAnnotatedNodesAndTokens(annotationKind).Where(n => n.IsNode).Select(n => n.AsNode());
        }

        /// <summary>
        /// Gets all tokens with the specified annotation.
        /// </summary>
        public IEnumerable<SyntaxToken> GetAnnotatedTokens(SyntaxAnnotation syntaxAnnotation)
        {
            return this.GetAnnotatedNodesAndTokens(syntaxAnnotation).Where(n => n.IsToken).Select(n => n.AsToken());
        }

        /// <summary>
        /// Gets all tokens with the specified annotation kind.
        /// </summary>
        public IEnumerable<SyntaxToken> GetAnnotatedTokens(string annotationKind)
        {
            return this.GetAnnotatedNodesAndTokens(annotationKind).Where(n => n.IsToken).Select(n => n.AsToken());
        }

        /// <summary>
        /// Gets all trivia with an annotation of the specified annotation kind.
        /// </summary>
        public IEnumerable<SyntaxTrivia> GetAnnotatedTrivia(string annotationKind)
        {
            return this.DescendantTrivia(n => n.ContainsAnnotations, descendIntoTrivia: true)
                       .Where(tr => tr.HasAnnotations(annotationKind));
        }

        /// <summary>
        /// Gets all trivia with an annotation of the specified annotation kinds.
        /// </summary>
        public IEnumerable<SyntaxTrivia> GetAnnotatedTrivia(params string[] annotationKinds)
        {
            return this.DescendantTrivia(n => n.ContainsAnnotations, descendIntoTrivia: true)
                       .Where(tr => tr.HasAnnotations(annotationKinds));
        }

        /// <summary>
        /// Gets all trivia with the specified annotation.
        /// </summary>
        public IEnumerable<SyntaxTrivia> GetAnnotatedTrivia(SyntaxAnnotation annotation)
        {
            return this.DescendantTrivia(n => n.ContainsAnnotations, descendIntoTrivia: true)
                       .Where(tr => tr.HasAnnotation(annotation));
        }

        internal SyntaxNode WithAdditionalAnnotationsInternal(IEnumerable<SyntaxAnnotation> annotations)
        {
            return this.Green.WithAdditionalAnnotationsGreen(annotations).CreateRed();
        }

        internal SyntaxNode GetNodeWithoutAnnotations(IEnumerable<SyntaxAnnotation> annotations)
        {
            return this.Green.WithoutAnnotationsGreen(annotations).CreateRed();
        }

        /// <summary>
        /// Copies all SyntaxAnnotations, if any, from this SyntaxNode instance and attaches them to a new instance based on <paramref name="node" />.
        /// </summary>
        /// <remarks>
        /// <para>
        /// If no annotations are copied, just returns <paramref name="node" />.
        /// </para>
        /// <para>
        /// It can also be used manually to preserve annotations in a more complex tree
        /// modification, even if the type of a node changes.
        /// </para>
        /// </remarks>
        public T CopyAnnotationsTo<T>(T node) where T : SyntaxNode
        {
            if (node == null)
            {
                return default(T);
            }

            var annotations = this.Green.GetAnnotations();
            if (annotations == null || annotations.Length == 0)
            {
                return node;
            }

            return (T)(node.Green.WithAdditionalAnnotationsGreen(annotations)).CreateRed();
        }

        #endregion

        /// <summary>
        /// Determines if two nodes are the same, disregarding trivia differences.
        /// </summary>
        /// <param name="node">The node to compare against.</param>
        /// <param name="topLevel"> If true then the nodes are equivalent if the contained nodes and
        /// tokens declaring metadata visible symbolic information are equivalent, ignoring any
        /// differences of nodes inside method bodies or initializer expressions, otherwise all
        /// nodes and tokens must be equivalent. 
        /// </param>
        public bool IsEquivalentTo(SyntaxNode node, bool topLevel = false)
        {
            return IsEquivalentToCore(node, topLevel);
        }

        public abstract void SerializeTo(Stream stream, CancellationToken cancellationToken = default(CancellationToken));

        #region Core Methods

        /// <summary>
        /// Determine if this node is structurally equivalent to another.
        /// </summary>
        protected abstract bool EquivalentToCore(SyntaxNode other);

        /// <summary>
        /// Returns SyntaxTree that owns the node or null if node does not belong to a
        /// SyntaxTree
        /// </summary>
        protected abstract SyntaxTree SyntaxTreeCore { get; }

        /// <summary>
        /// Finds a descendant token of this node whose span includes the supplied position. 
        /// </summary>
        /// <param name="position">The character position of the token relative to the beginning of the file.</param>
        /// <param name="findInsideTrivia">
        /// True to return tokens that are part of trivia.
        /// If false finds the token whose full span (including trivia) includes the position.
        /// </param>
        protected abstract SyntaxToken FindTokenCore(int position, bool findInsideTrivia);

        /// <summary>
        /// Finds a descendant token of this node whose span includes the supplied position. 
        /// </summary>
        /// <param name="position">The character position of the token relative to the beginning of the file.</param>
        /// <param name="stepInto">
        /// Applied on every structured trivia. Return false if the tokens included in the trivia should be skipped. 
        /// Pass null to skip all structured trivia.
        /// </param>
        protected abstract SyntaxToken FindTokenCore(int position, Func<SyntaxTrivia, bool> stepInto);

        /// <summary>
        /// Finds a descendant trivia of this node whose span includes the supplied position.
        /// </summary>
        /// <param name="position">The character position of the trivia relative to the beginning of the file.</param>
        /// <param name="findInsideTrivia">Whether to search inside structured trivia.</param>
        protected abstract SyntaxTrivia FindTriviaCore(int position, bool findInsideTrivia);

        /// <summary>
        /// Creates a new tree of nodes with the specified nodes, tokens or trivia replaced.
        /// </summary>
        protected internal abstract SyntaxNode ReplaceCore<TNode>(
            IEnumerable<TNode> nodes = null,
            Func<TNode, TNode, SyntaxNode> computeReplacementNode = null,
            IEnumerable<SyntaxToken> tokens = null,
            Func<SyntaxToken, SyntaxToken, SyntaxToken> computeReplacementToken = null,
            IEnumerable<SyntaxTrivia> trivia = null,
            Func<SyntaxTrivia, SyntaxTrivia, SyntaxTrivia> computeReplacementTrivia = null)
            where TNode : SyntaxNode;

        protected internal abstract SyntaxNode ReplaceNodeInListCore(SyntaxNode originalNode, IEnumerable<SyntaxNode> replacementNodes);
        protected internal abstract SyntaxNode InsertNodesInListCore(SyntaxNode nodeInList, IEnumerable<SyntaxNode> nodesToInsert, bool insertBefore);
        protected internal abstract SyntaxNode ReplaceTokenInListCore(SyntaxToken originalToken, IEnumerable<SyntaxToken> newTokens);
        protected internal abstract SyntaxNode InsertTokensInListCore(SyntaxToken originalToken, IEnumerable<SyntaxToken> newTokens, bool insertBefore);
        protected internal abstract SyntaxNode ReplaceTriviaInListCore(SyntaxTrivia originalTrivia, IEnumerable<SyntaxTrivia> newTrivia);
        protected internal abstract SyntaxNode InsertTriviaInListCore(SyntaxTrivia originalTrivia, IEnumerable<SyntaxTrivia> newTrivia, bool insertBefore);

        /// <summary>
        /// Creates a new tree of nodes with the specified node removed.
        /// </summary>
        protected internal abstract SyntaxNode RemoveNodesCore(
            IEnumerable<SyntaxNode> nodes,
            SyntaxRemoveOptions options);

        protected internal abstract SyntaxNode NormalizeWhitespaceCore(string indentation, bool elasticTrivia);

        /// <summary>
        /// Determines if two nodes are the same, disregarding trivia differences.
        /// </summary>
        /// <param name="node">The node to compare against.</param>
        /// <param name="topLevel"> If true then the nodes are equivalent if the contained nodes and
        /// tokens declaring metadata visible symbolic information are equivalent, ignoring any
        /// differences of nodes inside method bodies or initializer expressions, otherwise all
        /// nodes and tokens must be equivalent. 
        /// </param>
        protected abstract bool IsEquivalentToCore(SyntaxNode node, bool topLevel = false);

        #endregion
    }
}