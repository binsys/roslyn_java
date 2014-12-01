﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.Formatting.Rules;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp.Formatting
{
    internal abstract class BaseFormattingRule : AbstractFormattingRule
    {
        protected void AddUnindentBlockOperation(
            List<IndentBlockOperation> list,
            SyntaxToken startToken,
            SyntaxToken endToken,
            TextSpan textSpan,
            IndentBlockOption option = IndentBlockOption.RelativePosition)
        {
            if (startToken.CSharpKind() == SyntaxKind.None || endToken.CSharpKind() == SyntaxKind.None)
            {
                return;
            }

            list.Add(FormattingOperations.CreateIndentBlockOperation(startToken, endToken, textSpan, indentationDelta: -1, option: option));
        }

        protected void AddUnindentBlockOperation(
            List<IndentBlockOperation> list,
            SyntaxToken startToken,
            SyntaxToken endToken,
            IndentBlockOption option = IndentBlockOption.RelativePosition)
        {
            if (startToken.CSharpKind() == SyntaxKind.None || endToken.CSharpKind() == SyntaxKind.None)
            {
                return;
            }

            list.Add(FormattingOperations.CreateIndentBlockOperation(startToken, endToken, indentationDelta: -1, option: option));
        }

        protected void AddAbsoluteZeroIndentBlockOperation(
            List<IndentBlockOperation> list,
            SyntaxToken startToken,
            SyntaxToken endToken,
            IndentBlockOption option = IndentBlockOption.AbsolutePosition)
        {
            if (startToken.CSharpKind() == SyntaxKind.None || endToken.CSharpKind() == SyntaxKind.None)
            {
                return;
            }

            list.Add(FormattingOperations.CreateIndentBlockOperation(startToken, endToken, indentationDelta: 0, option: option));
        }

        protected void AddIndentBlockOperation(
            List<IndentBlockOperation> list,
            SyntaxToken startToken,
            SyntaxToken endToken,
            IndentBlockOption option = IndentBlockOption.RelativePosition)
        {
            if (startToken.CSharpKind() == SyntaxKind.None || endToken.CSharpKind() == SyntaxKind.None)
            {
                return;
            }

            list.Add(FormattingOperations.CreateIndentBlockOperation(startToken, endToken, indentationDelta: 1, option: option));
        }

        protected void AddIndentBlockOperation(
            List<IndentBlockOperation> list,
            SyntaxToken startToken,
            SyntaxToken endToken,
            TextSpan textSpan,
            IndentBlockOption option = IndentBlockOption.RelativePosition)
        {
            if (startToken.CSharpKind() == SyntaxKind.None || endToken.CSharpKind() == SyntaxKind.None)
            {
                return;
            }

            list.Add(FormattingOperations.CreateIndentBlockOperation(startToken, endToken, textSpan, indentationDelta: 1, option: option));
        }

        protected void AddIndentBlockOperation(
            List<IndentBlockOperation> list,
            SyntaxToken baseToken,
            SyntaxToken startToken,
            SyntaxToken endToken,
            IndentBlockOption option = IndentBlockOption.RelativePosition)
        {
            list.Add(FormattingOperations.CreateRelativeIndentBlockOperation(baseToken, startToken, endToken, indentationDelta: 1, option: option));
        }

        protected void SetAlignmentBlockOperation(
            List<IndentBlockOperation> list,
            SyntaxToken baseToken,
            SyntaxToken startToken,
            SyntaxToken endToken,
            IndentBlockOption option = IndentBlockOption.RelativePosition)
        {
            list.Add(FormattingOperations.CreateRelativeIndentBlockOperation(baseToken, startToken, endToken, indentationDelta: 0, option: option));
        }

        protected void AddSuppressWrappingIfOnSingleLineOperation(List<SuppressOperation> list, SyntaxToken startToken, SyntaxToken endToken, SuppressOption extraOption = SuppressOption.None)
        {
            AddSuppressOperation(list, startToken, endToken, SuppressOption.NoWrappingIfOnSingleLine | extraOption);
        }

        protected void AddSuppressAllOperationIfOnMultipleLine(List<SuppressOperation> list, SyntaxToken startToken, SyntaxToken endToken, SuppressOption extraOption = SuppressOption.None)
        {
            AddSuppressOperation(list, startToken, endToken, SuppressOption.NoSpacingIfOnMultipleLine | SuppressOption.NoWrapping | extraOption);
        }

        protected void AddSuppressOperation(List<SuppressOperation> list, SyntaxToken startToken, SyntaxToken endToken, SuppressOption option)
        {
            if (startToken.CSharpKind() == SyntaxKind.None || endToken.CSharpKind() == SyntaxKind.None)
            {
                return;
            }

            list.Add(FormattingOperations.CreateSuppressOperation(startToken, endToken, option));
        }

        protected void AddAnchorIndentationOperation(List<AnchorIndentationOperation> list, SyntaxToken anchorToken, SyntaxToken endToken)
        {
            if (anchorToken.CSharpKind() == SyntaxKind.None || endToken.CSharpKind() == SyntaxKind.None)
            {
                return;
            }

            list.Add(FormattingOperations.CreateAnchorIndentationOperation(anchorToken, endToken));
        }

        protected void AddAlignIndentationOfTokensToBaseTokenOperation(List<AlignTokensOperation> list, SyntaxNode containingNode, SyntaxToken baseNode, IEnumerable<SyntaxToken> tokens)
        {
            if (containingNode == null || tokens == null)
            {
                return;
            }

            list.Add(FormattingOperations.CreateAlignTokensOperation(baseNode, tokens, AlignTokensOption.AlignIndentationOfTokensToBaseToken));
        }

        protected void AddAlignIndentationOfTokensToFirstTokenOfBaseTokenLineOperation(List<AlignTokensOperation> list, SyntaxNode containingNode, SyntaxToken baseNode, IEnumerable<SyntaxToken> tokens)
        {
            if (containingNode == null || tokens == null)
            {
                return;
            }

            list.Add(FormattingOperations.CreateAlignTokensOperation(baseNode, tokens, AlignTokensOption.AlignToFirstTokenOnBaseTokenLine));
        }

        protected AdjustNewLinesOperation CreateAdjustNewLinesOperation(int line, AdjustNewLinesOption option)
        {
            return FormattingOperations.CreateAdjustNewLinesOperation(line, option);
        }

        protected AdjustSpacesOperation CreateAdjustSpacesOperation(int space, AdjustSpacesOption option)
        {
            return FormattingOperations.CreateAdjustSpacesOperation(space, option);
        }
    }
}
