﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.Options;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Formatting.Rules
{
    internal class BaseIndentationFormattingRule : AbstractFormattingRule
    {
        private readonly IFormattingRule vbHelperFormattingRule;
        private readonly int baseIndentation;
        private readonly SyntaxToken token1;
        private readonly SyntaxToken token2;
        private readonly SyntaxNode commonNode;
        private readonly TextSpan span;

        public BaseIndentationFormattingRule(SyntaxNode root, TextSpan span, int baseIndentation, IFormattingRule vbHelperFormattingRule = null)
        {
            this.span = span;
            SetInnermostNodeForSpan(root, ref this.span, out token1, out token2, out commonNode);

            this.baseIndentation = baseIndentation;
            this.vbHelperFormattingRule = vbHelperFormattingRule;
        }

        public override void AddIndentBlockOperations(List<IndentBlockOperation> list, SyntaxNode node, OptionSet optionSet, NextAction<IndentBlockOperation> nextOperation)
        {
            // for the common node itself, return absolute indentation
            if (this.commonNode == node)
            {
                // TODO: If the first line of the span includes a node, we want to align with the position of that node 
                // in the primary buffer.  That's what Dev12 does for C#, but it doesn't match Roslyn's current model
                // of each statement being formatted independently with respect to it's parent.
                list.Add(new IndentBlockOperation(token1, token2, span, baseIndentation, IndentBlockOption.AbsolutePosition));
            }
            else if (node.Span.Contains(this.span))
            {
                // any node bigger than our span is ignored.
                return;
            }

            // Add everything to the list.
            AddNextIndentBlockOperations(list, node, optionSet, nextOperation);

            // Filter out everything that encompasses our span.
            AdjustIndentBlockOperation(list);
        }

        private void AddNextIndentBlockOperations(List<IndentBlockOperation> list, SyntaxNode node, OptionSet optionSet, NextAction<IndentBlockOperation> nextOperation)
        {
            if (this.vbHelperFormattingRule == null)
            {
                base.AddIndentBlockOperations(list, node, optionSet, nextOperation);
                return;
            }

            vbHelperFormattingRule.AddIndentBlockOperations(list, node, optionSet, nextOperation);
        }

        private void AdjustIndentBlockOperation(List<IndentBlockOperation> list)
        {
            for (var i = 0; i < list.Count; i++)
            {
                var operation = list[i];

                // already filtered out operation
                if (operation == null)
                {
                    continue;
                }

                // if span is same as us, make sure we only include ourselves.
                if (this.span == operation.TextSpan && !Myself(operation))
                {
                    list[i] = null;
                    continue;
                }

                // inside of us, skip it.
                if (this.span.Contains(operation.TextSpan))
                {
                    continue;
                }

                // throw away operation that encloses overselves
                if (operation.TextSpan.Contains(this.span))
                {
                    list[i] = null;
                    continue;
                }

                // now we have an interesting case where indentation block intersects with us.
                // this can happen if code is splitted in two different script blocks or nuggets.
                // here, we will re-adjust block to be contained within our span.
                if (operation.TextSpan.IntersectsWith(this.span))
                {
                    list[i] = CloneAndAdjustFormattingOperation(operation);
                }
            }
        }

        private bool Myself(IndentBlockOperation operation)
        {
            return operation.TextSpan == this.span &&
                   operation.StartToken == this.token1 &&
                   operation.EndToken == this.token2 &&
                   operation.IndentationDeltaOrPosition == baseIndentation &&
                   operation.Option == IndentBlockOption.AbsolutePosition;
        }

        private IndentBlockOperation CloneAndAdjustFormattingOperation(IndentBlockOperation operation)
        {
            switch (operation.Option)
            {
                case IndentBlockOption.RelativeToFirstTokenOnBaseTokenLine:
                    return FormattingOperations.CreateRelativeIndentBlockOperation(operation.BaseToken, operation.StartToken, operation.EndToken, AdjustTextSpan(operation.TextSpan), operation.IndentationDeltaOrPosition, operation.Option);
                case IndentBlockOption.RelativePosition:
                case IndentBlockOption.AbsolutePosition:
                    return FormattingOperations.CreateIndentBlockOperation(operation.StartToken, operation.EndToken, AdjustTextSpan(operation.TextSpan), operation.IndentationDeltaOrPosition, operation.Option);
                default:
                    throw Contract.Unreachable;
            }
        }

        private TextSpan AdjustTextSpan(TextSpan textSpan)
        {
            return TextSpan.FromBounds(Math.Max(this.span.Start, textSpan.Start), Math.Min(this.span.End, textSpan.End));
        }

        private void SetInnermostNodeForSpan(SyntaxNode root, ref TextSpan span, out SyntaxToken token1, out SyntaxToken token2, out SyntaxNode commonNode)
        {
            commonNode = default(SyntaxNode);

            GetTokens(root, span, out token1, out token2);

            span = GetSpanFromTokens(span, token1, token2);

            if (token1.RawKind == 0 || token2.RawKind == 0)
            {
                return;
            }

            commonNode = token1.GetCommonRoot(token2);
        }

        private static void GetTokens(SyntaxNode root, TextSpan span, out SyntaxToken token1, out SyntaxToken token2)
        {
            // get tokens within given span
            token1 = root.FindToken(span.Start);
            token2 = root.FindTokenFromEnd(span.End);

            // it is possible the given span doesnt have any tokens in them. in that case, 
            // make tokens to be the adjacent ones to the given span
            if (span.End < token1.Span.Start)
            {
                token1 = token1.GetPreviousToken();
            }

            if (token2.Span.End < span.Start)
            {
                token2 = token2.GetNextToken();
            }
        }

        private static TextSpan GetSpanFromTokens(TextSpan span, SyntaxToken token1, SyntaxToken token2)
        {
            // adjust span to include all whitespace before and after the given span.
            var start = token1.Span.End;

            // current token is inside of the given span, get previous token's end position
            if (span.Start <= token1.Span.Start)
            {
                token1 = token1.GetPreviousToken();
                start = token1.Span.End;
                if (token1.RawKind == 0)
                {
                    start = token1.FullSpan.Start;
                }
            }

            var end = token2.Span.Start;

            // current token is inside of the given span, get next token's start position.
            if (token2.Span.End <= span.End)
            {
                token2 = token2.GetNextToken();
                end = token2.Span.Start;
                if (token2.RawKind == 0)
                {
                    end = token2.FullSpan.End;
                }
            }

            return TextSpan.FromBounds(start, end);
        }
    }
}
