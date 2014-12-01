﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.LanguageServices;
using Microsoft.CodeAnalysis.Options;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Simplification
{
    /// <summary>
    /// Expands and Reduces subtrees.
    /// 
    /// Expansion:
    ///      1) Replaces names with fully qualified dotted names.
    ///      2) Adds parentheses around expressions
    ///      3) Adds explicit casts/conversions where implicit conversions exist
    ///      4) Adds escaping to identifiers
    ///      5) Rewrites extension method invocations with explicit calls on the class containing the extension method.
    ///      
    /// Reduction:
    ///     1) Shortens dotted names to their minimally qualified form
    ///     2) Removes unnecessary parentheses
    ///     3) Removes unnecessary casts/conversions
    ///     4) Removes unnecessary escaping
    ///     5) Rewrites explicit calls to extension methods to use dot notation
    /// </summary>
    public static partial class Simplifier
    {
        /// <summary>
        /// The annotation the reducer uses to identify sub trees to be reduced.
        /// The Expand operations add this annotation to nodes so that the Reduce operations later find them.
        /// </summary>
        public static readonly SyntaxAnnotation Annotation = new SyntaxAnnotation();

        /// <summary>
        /// This is the annotation used by the simplifier and expander to identify Predefined type and preserving
        /// them from over simplification
        /// </summary>
        public static readonly SyntaxAnnotation SpecialTypeAnnotation = new SyntaxAnnotation();

        /// <summary>
        /// Expand qualifying parts of the specified subtree, annotating the parts using the Simplifier.Annotation annotation.
        /// </summary>
        public static async Task<TNode> ExpandAsync<TNode>(TNode node, Document document, Func<SyntaxNode, bool> expandInsideNode = null, bool expandParameter = false, CancellationToken cancellationToken = default(CancellationToken)) where TNode : SyntaxNode
        {
            var semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
            return Expand(node, semanticModel, document.Project.Solution.Workspace, expandInsideNode, expandParameter, cancellationToken);
        }

        /// <summary>
        /// Expand qualifying parts of the specified subtree, annotating the parts using the Simplifier.Annotation annotation.
        /// </summary>
        public static TNode Expand<TNode>(TNode node, SemanticModel semanticModel, Workspace workspace, Func<SyntaxNode, bool> expandInsideNode = null, bool expandParameter = false, CancellationToken cancellationToken = default(CancellationToken)) where TNode : SyntaxNode
        {
            var result = LanguageService
                .GetService<ISimplificationService>(workspace, node.Language)
                .Expand(node, semanticModel, annotationForReplacedAliasIdentifier: null, expandInsideNode: expandInsideNode, expandParameter: expandParameter, cancellationToken: cancellationToken);

            return (TNode)result;
        }

        /// <summary>
        /// Expand qualifying parts of the specified subtree, annotating the parts using the Simplifier.Annotation annotation.
        /// </summary>
        public static async Task<SyntaxToken> ExpandAsync(SyntaxToken token, Document document, Func<SyntaxNode, bool> expandInsideNode = null, CancellationToken cancellationToken = default(CancellationToken))
        {
            var semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
            return Expand(token, semanticModel, document.Project.Solution.Workspace, expandInsideNode, cancellationToken);
        }

        /// <summary>
        /// Expand qualifying parts of the specified subtree, annotating the parts using the Simplifier.Annotation annotation.
        /// </summary>
        public static SyntaxToken Expand(SyntaxToken token, SemanticModel semanticModel, Workspace workspace, Func<SyntaxNode, bool> expandInsideNode = null, CancellationToken cancellationToken = default(CancellationToken))
        {
            return LanguageService
                .GetService<ISimplificationService>(workspace, token.Language)
                .Expand(token, semanticModel, expandInsideNode, cancellationToken);
        }

        /// <summary>
        /// Reduce all subtrees annotated with Simplifier.Annotation found within the document.
        /// </summary>
        public static async Task<Document> ReduceAsync(Document document, OptionSet optionSet = null, CancellationToken cancellationToken = default(CancellationToken))
        {
            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            return await ReduceAsync(document, root.FullSpan, optionSet, cancellationToken).ConfigureAwait(false);
        }

        /// <summary>
        /// Reduce the sub trees annotated with Simplifier.Annotation found within the subtrees identified with the specified annotation.
        /// </summary>
        public static async Task<Document> ReduceAsync(Document document, SyntaxAnnotation annotation, OptionSet optionSet = null, CancellationToken cancellationToken = default(CancellationToken))
        {
            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            return await ReduceAsync(document, root.GetAnnotatedNodesAndTokens(annotation).Select(t => t.FullSpan), optionSet, cancellationToken).ConfigureAwait(false);
        }

        /// <summary>
        /// Reduce the sub trees annotated with Simplifier.Annotation found within the specified span.
        /// </summary>
        public static Task<Document> ReduceAsync(Document document, TextSpan span, OptionSet optionSet = null, CancellationToken cancellationToken = default(CancellationToken))
        {
            return ReduceAsync(document, SpecializedCollections.SingletonEnumerable(span), optionSet, cancellationToken);
        }

        /// <summary>
        /// Reduce the sub trees annotated with Simplifier.Annotation found within the specified spans.
        /// </summary>
        public static Task<Document> ReduceAsync(Document document, IEnumerable<TextSpan> spans, OptionSet optionSet = null, CancellationToken cancellationToken = default(CancellationToken))
        {
            return LanguageService.GetService<ISimplificationService>(document).ReduceAsync(document, spans, optionSet, cancellationToken: cancellationToken);
        }

        internal static async Task<Document> ReduceAsync(Document document, IEnumerable<AbstractReducer> reducers, OptionSet optionSet = null, CancellationToken cancellationToken = default(CancellationToken))
        {
            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            return await LanguageService.GetService<ISimplificationService>(document)
                .ReduceAsync(document, SpecializedCollections.SingletonEnumerable(root.FullSpan), optionSet, reducers, cancellationToken).ConfigureAwait(false);
        }
    }
}