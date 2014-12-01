﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.Extensions
{
    internal static class SyntaxTriviaExtensions
    {
        public static bool MatchesKind(this SyntaxTrivia trivia, params SyntaxKind[] kinds)
        {
            return kinds.Any(k => k == trivia.CSharpKind());
        }

        public static bool IsRegularComment(this SyntaxTrivia trivia)
        {
            return trivia.IsSingleLineComment() || trivia.IsMultiLineComment();
        }

        public static bool IsRegularOrDocComment(this SyntaxTrivia trivia)
        {
            return trivia.IsSingleLineComment() || trivia.IsMultiLineComment() || trivia.IsDocComment();
        }

        public static bool IsSingleLineComment(this SyntaxTrivia trivia)
        {
            return trivia.CSharpKind() == SyntaxKind.SingleLineCommentTrivia;
        }

        public static bool IsMultiLineComment(this SyntaxTrivia trivia)
        {
            return trivia.CSharpKind() == SyntaxKind.MultiLineCommentTrivia;
        }

        public static bool IsCompleteMultiLineComment(this SyntaxTrivia trivia)
        {
            if (trivia.CSharpKind() != SyntaxKind.MultiLineCommentTrivia)
            {
                return false;
            }

            var text = trivia.ToFullString();
            return text.Length >= 4
                && text[text.Length - 1] == '/'
                && text[text.Length - 2] == '*';
        }

        public static bool IsDocComment(this SyntaxTrivia trivia)
        {
            return trivia.IsSingleLineDocComment() || trivia.IsMultiLineDocComment();
        }

        public static bool IsSingleLineDocComment(this SyntaxTrivia trivia)
        {
            return trivia.CSharpKind() == SyntaxKind.SingleLineDocumentationCommentTrivia;
        }

        public static bool IsMultiLineDocComment(this SyntaxTrivia trivia)
        {
            return trivia.CSharpKind() == SyntaxKind.MultiLineDocumentationCommentTrivia;
        }

        public static string GetCommentText(this SyntaxTrivia trivia)
        {
            var commentText = trivia.ToString();
            if (trivia.CSharpKind() == SyntaxKind.SingleLineCommentTrivia)
            {
                if (commentText.StartsWith("//"))
                {
                    commentText = commentText.Substring(2);
                }

                return commentText.TrimStart();
            }
            else if (trivia.CSharpKind() == SyntaxKind.MultiLineCommentTrivia)
            {
                var textBuilder = new StringBuilder();

                if (commentText.EndsWith("*/"))
                {
                    commentText = commentText.Substring(0, commentText.Length - 2);
                }

                if (commentText.StartsWith("/*"))
                {
                    commentText = commentText.Substring(2);
                }

                commentText = commentText.Trim();

                var newLine = Environment.NewLine;
                var lines = commentText.Split(new[] { newLine }, StringSplitOptions.None);
                foreach (var line in lines)
                {
                    var trimmedLine = line.Trim();

                    // Note: we trim leading '*' characters in multi-line comments.
                    // If the '*' was intentional, sorry, it's gone.
                    if (trimmedLine.StartsWith("*"))
                    {
                        trimmedLine = trimmedLine.TrimStart('*');
                        trimmedLine = trimmedLine.TrimStart();
                    }

                    textBuilder.AppendLine(trimmedLine);
                }

                // remove last line break
                textBuilder.Remove(textBuilder.Length - newLine.Length, newLine.Length);

                return textBuilder.ToString();
            }
            else
            {
                throw new InvalidOperationException();
            }
        }

        public static string AsString(this IEnumerable<SyntaxTrivia> trivia)
        {
            Contract.ThrowIfNull(trivia);

            if (trivia.Any())
            {
                var sb = new StringBuilder();
                trivia.Select(t => t.ToFullString()).Do((s) => sb.Append(s));
                return sb.ToString();
            }
            else
            {
                return string.Empty;
            }
        }

        public static int GetFullWidth(this IEnumerable<SyntaxTrivia> trivia)
        {
            Contract.ThrowIfNull(trivia);
            return trivia.Sum(t => t.FullWidth());
        }

        public static SyntaxTriviaList AsTrivia(this string s)
        {
            return SyntaxFactory.ParseLeadingTrivia(s ?? string.Empty);
        }

        public static bool IsWhitespaceOrEndOfLine(this SyntaxTrivia trivia)
        {
            return trivia.CSharpKind() == SyntaxKind.WhitespaceTrivia || trivia.CSharpKind() == SyntaxKind.EndOfLineTrivia;
        }

        public static SyntaxTrivia GetPreviousTrivia(
            this SyntaxTrivia trivia, SyntaxTree syntaxTree, CancellationToken cancellationToken, bool findInsideTrivia = false)
        {
            var span = trivia.FullSpan;
            if (span.Start == 0)
            {
                return default(SyntaxTrivia);
            }

            return syntaxTree.GetRoot(cancellationToken).FindTrivia(span.Start - 1, findInsideTrivia);
        }

#if false
        public static int Width(this SyntaxTrivia trivia)
        {
            return trivia.Span.Length;
        }

        public static int FullWidth(this SyntaxTrivia trivia)
        {
            return trivia.FullSpan.Length;
        }
#endif
    }
}
