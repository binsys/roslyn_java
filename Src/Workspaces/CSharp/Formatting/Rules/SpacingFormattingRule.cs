﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Formatting.Rules;
using Microsoft.CodeAnalysis.Options;

namespace Microsoft.CodeAnalysis.CSharp.Formatting
{
    internal class SpacingFormattingRule : BaseFormattingRule
    {
        public override AdjustSpacesOperation GetAdjustSpacesOperation(SyntaxToken previousToken, SyntaxToken currentToken, OptionSet optionSet, NextOperation<AdjustSpacesOperation> nextOperation)
        {
            if (optionSet == null)
            {
                return nextOperation.Invoke();
            }

            System.Diagnostics.Debug.Assert(previousToken.Parent != null && currentToken.Parent != null);

            var previousKind = previousToken.CSharpKind();
            var currentKind = currentToken.CSharpKind();
            var previousParentKind = previousToken.Parent.CSharpKind();
            var currentParentKind = currentToken.Parent.CSharpKind();

            // For Method Declaration
            if (currentToken.IsOpenParenInParameterList() && previousKind == SyntaxKind.IdentifierToken)
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.MethodDeclarationNameParenthesis);
            }

            if (previousToken.IsOpenParenInParameterList() && currentToken.IsCloseParenInParameterList())
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.MethodDeclarationEmptyArgument);
            }

            if (previousToken.IsOpenParenInParameterList())
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.MethodDeclarationParenthesisArgumentList);
            }

            if (currentToken.IsCloseParenInParameterList())
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.MethodDeclarationParenthesisArgumentList);
            }

            // For Method Call
            if (currentToken.IsOpenParenInArgumentList())
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.MethodCallNameParenthesis);
            }

            if (previousToken.IsOpenParenInArgumentList() && currentToken.IsCloseParenInArgumentList())
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.MethodCallEmptyArgument);
            }

            if (previousToken.IsOpenParenInArgumentList())
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.MethodCallArgumentList);
            }

            if (currentToken.IsCloseParenInArgumentList())
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.MethodCallArgumentList);
            }

            // For Spacing b/n control flow keyword and paren. Parent check not needed.
            if ((previousKind == SyntaxKind.IfKeyword || previousKind == SyntaxKind.WhileKeyword || previousKind == SyntaxKind.SwitchKeyword ||
                previousKind == SyntaxKind.ForKeyword || previousKind == SyntaxKind.ForEachKeyword)
                && (currentKind == SyntaxKind.OpenParenToken))
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.OtherAfterControlFlowKeyword);
            }

            // For spacing between parenthesis and expression
            if ((previousParentKind == SyntaxKind.ParenthesizedExpression && previousKind == SyntaxKind.OpenParenToken) ||
                (currentParentKind == SyntaxKind.ParenthesizedExpression && currentKind == SyntaxKind.CloseParenToken))
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.OtherBetweenParenthesisExpression);
            }

            // For spacing between the parenthesis and the cast expression
            if ((previousParentKind == SyntaxKind.CastExpression && previousKind == SyntaxKind.OpenParenToken) ||
                (currentParentKind == SyntaxKind.CastExpression && currentKind == SyntaxKind.CloseParenToken))
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.OtherParenthesisTypeCast);
            }

            // For spacing between the parenthesis and the expression inside the control flow expression
            if ((previousParentKind == SyntaxKind.IfStatement || previousParentKind == SyntaxKind.WhileStatement || previousParentKind == SyntaxKind.SwitchStatement ||
                previousParentKind == SyntaxKind.ForStatement || previousParentKind == SyntaxKind.ForEachStatement)
                && previousKind == SyntaxKind.OpenParenToken)
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.OtherParenControlFlow);
            }

            if ((currentParentKind == SyntaxKind.IfStatement || currentParentKind == SyntaxKind.WhileStatement || currentParentKind == SyntaxKind.SwitchStatement ||
                currentParentKind == SyntaxKind.ForStatement || currentParentKind == SyntaxKind.ForEachStatement)
                && currentKind == SyntaxKind.CloseParenToken)
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.OtherParenControlFlow);
            }

            // For spacing after the cast
            if (previousParentKind == SyntaxKind.CastExpression && previousKind == SyntaxKind.CloseParenToken)
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.OtherParenAfterCast);
            }

            // For spacing Before Square Braces
            if (currentKind == SyntaxKind.OpenBracketToken && HasFormattableBracketParent(currentToken))
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.SquareBracesBefore);
            }

            // For spacing empty square braces
            if (previousKind == SyntaxKind.OpenBracketToken && currentKind == SyntaxKind.OmittedArraySizeExpressionToken && HasFormattableBracketParent(previousToken))
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.SquareBracesEmpty);
            }

            // For spacing square brackets within
            if (previousKind == SyntaxKind.OpenBracketToken && HasFormattableBracketParent(previousToken))
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.SquareBracesAndValue);
            }
            else if (currentKind == SyntaxKind.CloseBracketToken && HasFormattableBracketParent(currentToken))
            {
                if (currentToken.Parent is ArrayRankSpecifierSyntax)
                {
                    var parent = currentToken.Parent as ArrayRankSpecifierSyntax;
                    if ((parent.Sizes.Any() && parent.Sizes.First().CSharpKind() != SyntaxKind.OmittedArraySizeExpression) || parent.Sizes.SeparatorCount > 0)
                    {
                        // int []: added spacing operation on open [
                        // int[1], int[,]: need spacing operation
                        return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.SquareBracesAndValue);
                    }
                }
                else
                {
                    return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.SquareBracesAndValue);
                }
            }

            // For spacing delimiters - after colon
            if (previousToken.IsColonInTypeBaseList())
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.DelimitersAfterColonInTypeDeclaration);
            }

            // For spacing delimiters - before colon
            if (currentToken.IsColonInTypeBaseList())
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.DelimitersBeforeColonInTypeDeclaration);
            }

            // For spacing delimiters - after comma
            if ((previousToken.IsCommaInArgumentOrParameterList() && currentKind != SyntaxKind.OmittedTypeArgumentToken) ||
                previousToken.IsCommaInInitializerExpression())
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.DelimitersAfterCommaInParameterArgument);
            }

            // For spacing delimiters - before comma
            if ((currentToken.IsCommaInArgumentOrParameterList() && previousKind != SyntaxKind.OmittedTypeArgumentToken) ||
                currentToken.IsCommaInInitializerExpression())
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.DelimitersBeforeCommaInParameterArgument);
            }

            // For Spacing delimiters - after Dot
            if (previousToken.IsDotInMemberAccessOrQualifiedName())
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.DelimitersAfterDotMemberAccessQualifiedName);
            }

            // For spacing delimiters - before Dot
            if (currentToken.IsDotInMemberAccessOrQualifiedName())
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.DelimitersBeforeDotMemberAccessQualifiedName);
            }

            // For spacing delimiters - after semicolon
            if (previousToken.IsSemicolonInForStatement() && currentKind != SyntaxKind.CloseParenToken)
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.DelimitersAfterSemiColonInForStatement);
            }

            // For spacing delimiters - before semicolon
            if (currentToken.IsSemicolonInForStatement())
            {
                return AdjustSpacesOperationZeroOrOne(optionSet, CSharpFormattingOptions.DelimitersBeforeSemiColonInForStatement);
            }

            // For spacing around the binary operators
            if (currentToken.Parent is BinaryExpressionSyntax || previousToken.Parent is BinaryExpressionSyntax)
            {
                switch (optionSet.GetOption(CSharpFormattingOptions.SpacingAroundBinaryOperator))
                {
                    case BinaryOperatorSpacingOptions.Single:
                        return CreateAdjustSpacesOperation(1, AdjustSpacesOption.ForceSpacesIfOnSingleLine);
                    case BinaryOperatorSpacingOptions.Remove:
                        return CreateAdjustSpacesOperation(0, AdjustSpacesOption.ForceSpacesIfOnSingleLine);
                    case BinaryOperatorSpacingOptions.Ignore:
                        return CreateAdjustSpacesOperation(0, AdjustSpacesOption.PreserveSpaces);
                    default:
                        System.Diagnostics.Debug.Assert(false, "Invalid BinaryOperatorSpacingOptions");
                        break;
                }
            }

            return nextOperation.Invoke();
        }

        public override void AddSuppressOperations(List<SuppressOperation> list, SyntaxNode node, OptionSet optionSet, NextAction<SuppressOperation> nextOperation)
        {
            nextOperation.Invoke(list);

            SuppressVariableDeclaration(list, node, optionSet);
        }

        private void SuppressVariableDeclaration(List<SuppressOperation> list, SyntaxNode node, OptionSet optionSet)
        {
            if (node.IsKind(SyntaxKind.FieldDeclaration) || node.IsKind(SyntaxKind.EventDeclaration) ||
                node.IsKind(SyntaxKind.EventFieldDeclaration) || node.IsKind(SyntaxKind.LocalDeclarationStatement))
            {
                if (optionSet.GetOption(CSharpFormattingOptions.OtherSpacesDeclarationIgnore))
                {
                    var firstToken = node.GetFirstToken(includeZeroWidth: true);
                    var lastToken = node.GetLastToken(includeZeroWidth: true);

                    list.Add(FormattingOperations.CreateSuppressOperation(firstToken, lastToken, SuppressOption.NoSpacing | SuppressOption.IgnoreElastic));
                }
            }
        }

        private AdjustSpacesOperation AdjustSpacesOperationZeroOrOne(OptionSet optionSet, Option<bool> option, AdjustSpacesOption explicitOption = AdjustSpacesOption.ForceSpacesIfOnSingleLine)
        {
            if (optionSet.GetOption(option))
            {
                return CreateAdjustSpacesOperation(1, explicitOption);
            }
            else
            {
                return CreateAdjustSpacesOperation(0, explicitOption);
            }
        }

        private bool HasFormattableBracketParent(SyntaxToken token)
        {
            return token.Parent.MatchesKind(SyntaxKind.ArrayRankSpecifier, SyntaxKind.BracketedArgumentList, SyntaxKind.BracketedParameterList, SyntaxKind.ImplicitArrayCreationExpression);
        }
    }
}
