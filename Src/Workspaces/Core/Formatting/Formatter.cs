﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Formatting.Rules;
using Microsoft.CodeAnalysis.LanguageServices;
using Microsoft.CodeAnalysis.Options;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.WorkspaceServices;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Formatting
{
    /// <summary>
    /// Formats whitespace in documents or syntax trees.
    /// </summary>
    public static class Formatter
    {
        /// <summary>
        /// The annotation used to mark portions of a syntax tree to be formatted.
        /// </summary>
        public static readonly SyntaxAnnotation Annotation = new SyntaxAnnotation();

        /// <summary>
        /// Gets the formatting rules that would be applied if left unspecified.
        /// </summary>
        internal static IEnumerable<IFormattingRule> GetDefaultFormattingRules(Document document)
        {
            if (document == null)
            {
                throw new ArgumentNullException("document");
            }

            var service = LanguageService.GetService<ISyntaxFormattingService>(document);
            if (service != null)
            {
                return service.GetDefaultFormattingRules();
            }
            else
            {
                return SpecializedCollections.EmptyEnumerable<IFormattingRule>();
            }
        }

        /// <summary>
        /// Gets the formatting rules that would be applied if left unspecified.
        /// </summary>
        internal static IEnumerable<IFormattingRule> GetDefaultFormattingRules(Workspace workspace, string language)
        {
            if (workspace == null)
            {
                throw new ArgumentNullException("workspace");
            }

            if (language == null)
            {
                throw new ArgumentNullException("language");
            }

            var service = LanguageService.GetService<ISyntaxFormattingService>(workspace, language);
            if (service != null)
            {
                return service.GetDefaultFormattingRules();
            }
            else
            {
                return SpecializedCollections.EmptyEnumerable<IFormattingRule>();
            }
        }

        /// <summary>
        /// Formats the whitespace in a document.
        /// </summary>
        /// <param name="document">The document to format.</param>
        /// <param name="options">An optional set of formatting options. If these options are not supplied the current set of options from the document's workspace will be used.</param>
        /// <param name="cancellationToken">An optional cancellation token.</param>
        /// <returns>The formatted document.</returns>
        public static async Task<Document> FormatAsync(Document document, OptionSet options = null, CancellationToken cancellationToken = default(CancellationToken))
        {
            if (document == null)
            {
                throw new ArgumentNullException("document");
            }

            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            return document.WithSyntaxRoot(Format(root, document.Project.Solution.Workspace, options, cancellationToken));
        }

        /// <summary>
        /// Formats the whitespace in an area of a document corresponding to a text span.
        /// </summary>
        /// <param name="document">The document to format.</param>
        /// <param name="span">The span of the document's text to format.</param>
        /// <param name="options">An optional set of formatting options. If these options are not supplied the current set of options from the document's workspace will be used.</param>
        /// <param name="cancellationToken">An optional cancellation token.</param>
        /// <returns>The formatted document.</returns>
        public static async Task<Document> FormatAsync(Document document, TextSpan span, OptionSet options = null, CancellationToken cancellationToken = default(CancellationToken))
        {
            if (document == null)
            {
                throw new ArgumentNullException("document");
            }

            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            return document.WithSyntaxRoot(Format(root, span, document.Project.Solution.Workspace, options, cancellationToken));
        }

        /// <summary>
        /// Formats the whitespace in areas of a document corresponding to multiple non-overlapping spans.
        /// </summary>
        /// <param name="document">The document to format.</param>
        /// <param name="spans">The spans of the document's text to format.</param>
        /// <param name="options">An optional set of formatting options. If these options are not supplied the current set of options from the document's workspace will be used.</param>
        /// <param name="cancellationToken">An optional cancellation token.</param>
        /// <returns>The formatted document.</returns>
        public static Task<Document> FormatAsync(Document document, IEnumerable<TextSpan> spans, OptionSet options = null, CancellationToken cancellationToken = default(CancellationToken))
        {
            return FormatAsync(document, spans, options, rules: null, cancellationToken: cancellationToken);
        }

        internal static async Task<Document> FormatAsync(Document document, IEnumerable<TextSpan> spans, OptionSet options, IEnumerable<IFormattingRule> rules, CancellationToken cancellationToken)
        {
            if (document == null)
            {
                throw new ArgumentNullException("document");
            }

            if (spans == null)
            {
                throw new ArgumentNullException("spans");
            }

            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            return document.WithSyntaxRoot(Format(root, spans, document.Project.Solution.Workspace, options, rules, cancellationToken));
        }

        /// <summary>
        /// Formats the whitespace in areas of a document corresponding to annotated nodes.
        /// </summary>
        /// <param name="document">The document to format.</param>
        /// <param name="annotation">The annotation used to find on nodes to identify spans to format.</param>
        /// <param name="options">An optional set of formatting options. If these options are not supplied the current set of options from the document's workspace will be used.</param>
        /// <param name="cancellationToken">An optional cancellation token.</param>
        /// <returns>The formatted document.</returns>
        public static Task<Document> FormatAsync(Document document, SyntaxAnnotation annotation, OptionSet options = null, CancellationToken cancellationToken = default(CancellationToken))
        {
            return FormatAsync(document, annotation, options, rules: null, cancellationToken: cancellationToken);
        }

        internal static async Task<Document> FormatAsync(Document document, SyntaxAnnotation annotation, OptionSet options, IEnumerable<IFormattingRule> rules, CancellationToken cancellationToken)
        {
            if (document == null)
            {
                throw new ArgumentNullException("document");
            }

            if (annotation == null)
            {
                throw new ArgumentNullException("annotation");
            }

            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            return document.WithSyntaxRoot(Format(root, annotation, document.Project.Solution.Workspace, options, rules, cancellationToken));
        }

        /// <summary>
        /// Formats the whitespace in areas of a syntax tree corresponding to annotated nodes.
        /// </summary>
        /// <param name="node">The root node of a syntax tree to format.</param>
        /// <param name="annotation">The annotation used to find nodes to identify spans to format.</param>
        /// <param name="workspace">A workspace used to give the formatting context.</param>
        /// <param name="options">An optional set of formatting options. If these options are not supplied the current set of options from the workspace will be used.</param>
        /// <param name="cancellationToken">An optional cancellation token.</param>
        /// <returns>The formatted tree's root node.</returns>
        public static SyntaxNode Format(SyntaxNode node, SyntaxAnnotation annotation, Workspace workspace, OptionSet options = null, CancellationToken cancellationToken = default(CancellationToken))
        {
            return Format(node, annotation, workspace, options, rules: null, cancellationToken: cancellationToken);
        }

        internal static SyntaxNode Format(SyntaxNode node, SyntaxAnnotation annotation, Workspace workspace, OptionSet options, IEnumerable<IFormattingRule> rules, CancellationToken cancellationToken)
        {
            if (workspace == null)
            {
                throw new ArgumentNullException("workspace");
            }

            if (node == null)
            {
                throw new ArgumentNullException("node");
            }

            if (annotation == null)
            {
                throw new ArgumentNullException("annotation");
            }

            return Format(node, GetAnnotatedSpans(node, annotation), workspace, options, rules, cancellationToken);
        }

        /// <summary>
        /// Formats the whitespace of a syntax tree.
        /// </summary>
        /// <param name="node">The root node of a syntax tree to format.</param>
        /// <param name="workspace">A workspace used to give the formatting context.</param>
        /// <param name="options">An optional set of formatting options. If these options are not supplied the current set of options from the workspace will be used.</param>
        /// <param name="cancellationToken">An optional cancellation token.</param>
        /// <returns>The formatted tree's root node.</returns>
        public static SyntaxNode Format(SyntaxNode node, Workspace workspace, OptionSet options = null, CancellationToken cancellationToken = default(CancellationToken))
        {
            return Format(node, workspace, options, rules: null, cancellationToken: cancellationToken);
        }

        internal static SyntaxNode Format(SyntaxNode node, Workspace workspace, OptionSet options, IEnumerable<IFormattingRule> rules, CancellationToken cancellationToken)
        {
            if (workspace == null)
            {
                throw new ArgumentNullException("workspace");
            }

            if (node == null)
            {
                throw new ArgumentNullException("node");
            }

            return Format(node, new TextSpan[] { node.FullSpan }, workspace, options, rules, cancellationToken);
        }

        /// <summary>
        /// Formats the whitespace in areas of a syntax tree identified by a span.
        /// </summary>
        /// <param name="node">The root node of a syntax tree to format.</param>
        /// <param name="span">The span within the node's full span to format.</param>
        /// <param name="workspace">A workspace used to give the formatting context.</param>
        /// <param name="options">An optional set of formatting options. If these options are not supplied the current set of options from the workspace will be used.</param>
        /// <param name="cancellationToken">An optional cancellation token.</param>
        /// <returns>The formatted tree's root node.</returns>
        public static SyntaxNode Format(SyntaxNode node, TextSpan span, Workspace workspace, OptionSet options = null, CancellationToken cancellationToken = default(CancellationToken))
        {
            if (workspace == null)
            {
                throw new ArgumentNullException("workspace");
            }

            if (node == null)
            {
                throw new ArgumentNullException("node");
            }

            return Format(node, new TextSpan[] { span }, workspace, options, cancellationToken);
        }

        /// <summary>
        /// Formats the whitespace in areas of a syntax tree identified by multiple non-overlapping spans.
        /// </summary>
        /// <param name="node">The root node of a syntax tree to format.</param>
        /// <param name="spans">The spans within the node's full span to format.</param>
        /// <param name="workspace">A workspace used to give the formatting context.</param>
        /// <param name="options">An optional set of formatting options. If these options are not supplied the current set of options from the workspace will be used.</param>
        /// <param name="cancellationToken">An optional cancellation token.</param>
        /// <returns>The formatted tree's root node.</returns>
        public static SyntaxNode Format(SyntaxNode node, IEnumerable<TextSpan> spans, Workspace workspace, OptionSet options = null, CancellationToken cancellationToken = default(CancellationToken))
        {
            return Format(node, spans, workspace, options, rules: null, cancellationToken: cancellationToken);
        }

        internal static SyntaxNode Format(SyntaxNode node, IEnumerable<TextSpan> spans, Workspace workspace, OptionSet options, IEnumerable<IFormattingRule> rules, CancellationToken cancellationToken)
        {
            if (workspace == null)
            {
                throw new ArgumentNullException("workspace");
            }

            if (node == null)
            {
                throw new ArgumentNullException("node");
            }

            if (spans == null)
            {
                throw new ArgumentNullException("spans");
            }

            var languageFormatter = LanguageService.GetService<ISyntaxFormattingService>(workspace, node.Language);
            if (languageFormatter != null)
            {
                options = options ?? WorkspaceService.GetService<IOptionService>(workspace).GetOptions();
                rules = rules ?? GetDefaultFormattingRules(workspace, node.Language);
                return languageFormatter.Format(node, spans, options, rules, cancellationToken).GetFormattedRoot(cancellationToken);
            }
            else
            {
                return node;
            }
        }

        internal static IList<TextChange> GetFormattedTextChanges(SyntaxNode node, Workspace workspace, OptionSet options = null, IEnumerable<IFormattingRule> rules = null, CancellationToken cancellationToken = default(CancellationToken))
        {
            return GetFormattedTextChanges(node, new TextSpan[] { node.FullSpan }, workspace, options, rules, cancellationToken);
        }

        internal static IList<TextChange> GetFormattedTextChanges(SyntaxNode node, TextSpan span, Workspace workspace, OptionSet options = null, IEnumerable<IFormattingRule> rules = null, CancellationToken cancellationToken = default(CancellationToken))
        {
            return GetFormattedTextChanges(node, new TextSpan[] { span }, workspace, options, rules, cancellationToken);
        }

        internal static IList<TextChange> GetFormattedTextChanges(SyntaxNode node, IEnumerable<TextSpan> spans, Workspace workspace, OptionSet options = null, IEnumerable<IFormattingRule> rules = null, CancellationToken cancellationToken = default(CancellationToken))
        {
            if (workspace == null)
            {
                throw new ArgumentNullException("workspace");
            }

            if (node == null)
            {
                throw new ArgumentNullException("node");
            }

            var languageFormatter = LanguageService.GetService<ISyntaxFormattingService>(workspace, node.Language);
            if (languageFormatter != null)
            {
                options = options ?? WorkspaceService.GetService<IOptionService>(workspace).GetOptions();
                rules = rules ?? GetDefaultFormattingRules(workspace, node.Language);
                return languageFormatter.Format(node, spans, options, rules, cancellationToken).GetTextChanges(cancellationToken);
            }
            else
            {
                return SpecializedCollections.EmptyList<TextChange>();
            }
        }

        private static IEnumerable<TextSpan> GetAnnotatedSpans(SyntaxNode node, SyntaxAnnotation annotation)
        {
            foreach (var nodeOrToken in node.GetAnnotatedNodesAndTokens(annotation))
            {
                var firstToken = nodeOrToken.IsNode ? nodeOrToken.AsNode().GetFirstToken(includeZeroWidth: true) : nodeOrToken.AsToken();
                var lastToken = nodeOrToken.IsNode ? nodeOrToken.AsNode().GetLastToken(includeZeroWidth: true) : nodeOrToken.AsToken();

                var previousToken = firstToken.GetPreviousToken();
                var nextToken = lastToken.GetNextToken();

                if (previousToken.RawKind != 0)
                {
                    firstToken = previousToken;
                }

                if (nextToken.RawKind != 0)
                {
                    lastToken = nextToken;
                }

                yield return TextSpan.FromBounds(firstToken.SpanStart, lastToken.Span.End);
            }
        }
    }
}