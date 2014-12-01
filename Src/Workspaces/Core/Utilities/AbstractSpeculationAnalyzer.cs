﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis.LanguageServices;
using Microsoft.CodeAnalysis.Shared.Extensions;

namespace Microsoft.CodeAnalysis.Shared.Utilities
{
    /// <summary>
    /// Helper class to analyze the semantic effects of a speculated syntax node replacement on the parenting nodes.
    /// Given an expression node from a syntax tree and a new expression from a different syntax tree,
    /// it replaces the expression with the new expression to create a speculated syntax tree.
    /// It uses the original tree's semantic model to create a speculative semantic model and verifies that
    /// the syntax replacement doesn't break the semantics of any parenting nodes of the original expression.
    /// </summary>
    internal abstract class AbstractSpeculationAnalyzer<TSyntaxNode, TExpressionSyntax, TTypeSyntax, TAttributeSyntax,
        TArgumentSyntax, TForEachStatementSyntax, TThrowStatementSyntax, TSemanticModel>
        where TSyntaxNode : SyntaxNode
        where TExpressionSyntax : TSyntaxNode
        where TTypeSyntax : TExpressionSyntax
        where TAttributeSyntax : TSyntaxNode
        where TArgumentSyntax : TSyntaxNode
        where TForEachStatementSyntax : TSyntaxNode
        where TThrowStatementSyntax : TSyntaxNode
        where TSemanticModel : SemanticModel
    {
        private readonly TExpressionSyntax expression;
        private readonly TExpressionSyntax newExpressionForReplace;
        private readonly TSemanticModel semanticModel;
        private readonly CancellationToken cancellationToken;
        private readonly bool skipVerificationForReplacedNode;
        private readonly bool failOnOverloadResolutionFailuresInOriginalCode;
        private readonly bool isNewSemanticModelSpeculativeModel;

        private TSyntaxNode lazySemanticRootOfOriginalExpression;
        private TExpressionSyntax lazyReplacedExpression;
        private TSyntaxNode lazySemanticRootOfReplacedExpression;
        private TSemanticModel lazySpeculativeSemanticModel;

        /// <summary>
        /// Creates a semantic analyzer for speculative syntax replacement.
        /// </summary>
        /// <param name="expression">Original expression to be replaced.</param>
        /// <param name="newExpression">New expression to replace the original expression.</param>
        /// <param name="semanticModel">Semantic model of <paramref name="expression"/> node's syntax tree.</param>
        /// <param name="cancellationToken">Cancellation token.</param>
        /// <param name="skipVerificationForReplacedNode">
        /// True if semantic analysis should be skipped for the replaced node and performed starting from parent of the original and replaced nodes.
        /// This could be the case when custom verifications are required to be done by the caller or
        /// semantics of the replaced expression are different from the original expression.
        /// </param>
        /// <param name="failOnOverloadResolutionFailuresInOriginalCode">
        /// True if semantic analysis should fail when any of the invocation expression ancestors of <paramref name="expression"/> in original code has overload resolution failures.
        /// </param>
        public AbstractSpeculationAnalyzer(
            TExpressionSyntax expression,
            TExpressionSyntax newExpression,
            TSemanticModel semanticModel,
            CancellationToken cancellationToken,
            bool skipVerificationForReplacedNode = false,
            bool failOnOverloadResolutionFailuresInOriginalCode = false)
        {
            this.expression = expression;
            this.newExpressionForReplace = newExpression;
            this.semanticModel = semanticModel;
            this.cancellationToken = cancellationToken;
            this.skipVerificationForReplacedNode = skipVerificationForReplacedNode;
            this.failOnOverloadResolutionFailuresInOriginalCode = failOnOverloadResolutionFailuresInOriginalCode;
            this.isNewSemanticModelSpeculativeModel = true;
            this.lazyReplacedExpression = null;
            this.lazySemanticRootOfOriginalExpression = null;
            this.lazySemanticRootOfReplacedExpression = null;
            this.lazySpeculativeSemanticModel = null;
        }

        /// <summary>
        /// Original expression to be replaced.
        /// </summary>
        public TExpressionSyntax OriginalExpression
        {
            get { return this.expression; }
        }

        /// <summary>
        /// First ancestor of <see cref="P:OriginalExpression"/> which is either a statement, attribute, constructor initializer,
        /// field initializer, default parameter initializer or type syntax node.
        /// It serves as the root node for all semantic analysis for this syntax replacement.
        /// </summary>
        public TSyntaxNode SemanticRootOfOriginalExpression
        {
            get
            {
                if (this.lazySemanticRootOfOriginalExpression == null)
                {
                    this.lazySemanticRootOfOriginalExpression = GetSemanticRootForSpeculation(this.OriginalExpression);
                    Debug.Assert(this.lazySemanticRootOfOriginalExpression != null);
                }

                return this.lazySemanticRootOfOriginalExpression;
            }
        }

        /// <summary>
        /// Semantic model for the syntax tree corresponding to <see cref="OriginalExpression"/>
        /// </summary>
        public TSemanticModel OriginalSemanticModel
        {
            get { return this.semanticModel; }
        }

        /// <summary>
        /// Node which replaces the <see cref="OriginalExpression"/>.
        /// Note that this node is a cloned version of <see cref="newExpressionForReplace"/> node, which has been re-parented
        /// under the node to be speculated, i.e. <see cref="SemanticRootOfReplacedExpression"/>.
        /// </summary>
        public TExpressionSyntax ReplacedExpression
        {
            get
            {
                EnsureReplacedExpressionAndSemanticRoot();
                return this.lazyReplacedExpression;
            }
        }

        /// <summary>
        /// Node created by replacing <see cref="OriginalExpression"/> under <see cref="SemanticRootOfOriginalExpression"/> node.
        /// This node is used as the argument to the GetSpeculativeSemanticModel API and serves as the root node for all
        /// semantic analysis of the speculated tree.
        /// </summary>
        public TSyntaxNode SemanticRootOfReplacedExpression
        {
            get
            {
                EnsureReplacedExpressionAndSemanticRoot();
                return this.lazySemanticRootOfReplacedExpression;
            }
        }

        /// <summary>
        /// Speculative semantic model used for analyzing the semantics of the new tree.
        /// </summary>
        public TSemanticModel SpeculativeSemanticModel
        {
            get
            {
                EnsureSpeculativeSemanticModel();
                return this.lazySpeculativeSemanticModel;
            }
        }

        public CancellationToken CancellationToken
        {
            get
            {
                return this.cancellationToken;
            }
        }

        protected abstract TSyntaxNode GetSemanticRootForSpeculation(TExpressionSyntax expression);

        protected virtual TSyntaxNode GetSemanticRootOfReplacedExpression(TSyntaxNode semanticRootOfOriginalExpression, TExpressionSyntax annotatedReplacedExpression)
        {
            return semanticRootOfOriginalExpression.ReplaceNode(this.OriginalExpression, annotatedReplacedExpression);
        }

        private void EnsureReplacedExpressionAndSemanticRoot()
        {
            if (this.lazySemanticRootOfReplacedExpression == null)
            {
                // Because the new expression will change identity once we replace the old
                // expression in its parent, we annotate it here to allow us to get back to
                // it after replace.
                var annotation = new SyntaxAnnotation();
                var annotatedExpression = newExpressionForReplace.WithAdditionalAnnotations(annotation);
                this.lazySemanticRootOfReplacedExpression = GetSemanticRootOfReplacedExpression(this.SemanticRootOfOriginalExpression, annotatedExpression);
                this.lazyReplacedExpression = (TExpressionSyntax)this.lazySemanticRootOfReplacedExpression.GetAnnotatedNodesAndTokens(annotation).Single().AsNode();
            }
        }

        [Conditional("DEBUG")]
        protected abstract void ValidateSpeculativeSemanticModel(TSemanticModel speculativeSemanticModel, TSyntaxNode nodeToSpeculate);

        private void EnsureSpeculativeSemanticModel()
        {
            if (this.lazySpeculativeSemanticModel == null)
            {
                TSyntaxNode nodeToSpeculate = this.SemanticRootOfReplacedExpression;
                this.lazySpeculativeSemanticModel = CreateSpeculativeSemanticModel(this.SemanticRootOfOriginalExpression, nodeToSpeculate, this.semanticModel);
                ValidateSpeculativeSemanticModel(this.lazySpeculativeSemanticModel, nodeToSpeculate);
            }
        }

        protected abstract TSemanticModel CreateSpeculativeSemanticModel(TSyntaxNode originalNode, TSyntaxNode nodeToSpeculate, TSemanticModel semanticModel);

        #region Semantic comparison helpers

        protected bool TypesAreCompatible(TExpressionSyntax originalExpression, TExpressionSyntax newExpression)
        {
            Debug.Assert(originalExpression != null);
            Debug.Assert(this.SemanticRootOfOriginalExpression.DescendantNodesAndSelf().Contains(originalExpression));
            Debug.Assert(newExpression != null);
            Debug.Assert(this.SemanticRootOfReplacedExpression.DescendantNodesAndSelf().Contains(newExpression));

            var originalTypeInfo = this.OriginalSemanticModel.GetTypeInfo(originalExpression);
            var newTypeInfo = this.SpeculativeSemanticModel.GetTypeInfo(newExpression);
            return SymbolsAreCompatible(originalTypeInfo.Type, newTypeInfo.Type);
        }

        protected bool ConvertedTypesAreCompatible(TExpressionSyntax originalExpression, TExpressionSyntax newExpression)
        {
            Debug.Assert(originalExpression != null);
            Debug.Assert(this.SemanticRootOfOriginalExpression.DescendantNodesAndSelf().Contains(originalExpression));
            Debug.Assert(newExpression != null);
            Debug.Assert(this.SemanticRootOfReplacedExpression.DescendantNodesAndSelf().Contains(newExpression));

            var originalTypeInfo = this.OriginalSemanticModel.GetTypeInfo(originalExpression);
            var newTypeInfo = this.SpeculativeSemanticModel.GetTypeInfo(newExpression);
            return SymbolsAreCompatible(originalTypeInfo.ConvertedType, newTypeInfo.ConvertedType);
        }

        protected bool ImplicitConversionsAreCompatible(TExpressionSyntax originalExpression, TExpressionSyntax newExpression)
        {
            Debug.Assert(originalExpression != null);
            Debug.Assert(this.SemanticRootOfOriginalExpression.DescendantNodesAndSelf().Contains(originalExpression));
            Debug.Assert(newExpression != null);
            Debug.Assert(this.SemanticRootOfReplacedExpression.DescendantNodesAndSelf().Contains(newExpression));

            return ConversionsAreCompatible(this.OriginalSemanticModel, originalExpression, this.SpeculativeSemanticModel, newExpression);
        }

        protected abstract bool ConversionsAreCompatible(SemanticModel model1, TExpressionSyntax expression1, SemanticModel model2, TExpressionSyntax expression2);

        protected bool SymbolsAreCompatible(TSyntaxNode originalNode, TSyntaxNode newNode, bool requireNonNullSymbols = false)
        {
            Debug.Assert(originalNode != null);
            Debug.Assert(this.SemanticRootOfOriginalExpression.DescendantNodesAndSelf().Contains(originalNode));
            Debug.Assert(newNode != null);
            Debug.Assert(this.SemanticRootOfReplacedExpression.DescendantNodesAndSelf().Contains(newNode));

            var originalSymbolInfo = this.OriginalSemanticModel.GetSymbolInfo(originalNode);
            var newSymbolInfo = this.SpeculativeSemanticModel.GetSymbolInfo(newNode);
            return SymbolInfosAreCompatible(originalSymbolInfo, newSymbolInfo, requireNonNullSymbols);
        }

        public static bool SymbolInfosAreCompatible(SymbolInfo originalSymbolInfo, SymbolInfo newSymbolInfo, bool performEquivalenceCheck, bool requireNonNullSymbols = false)
        {
            return originalSymbolInfo.CandidateReason == newSymbolInfo.CandidateReason &&
                SymbolsAreCompatibleCore(originalSymbolInfo.Symbol, newSymbolInfo.Symbol, performEquivalenceCheck, requireNonNullSymbols);
        }

        protected bool SymbolInfosAreCompatible(SymbolInfo originalSymbolInfo, SymbolInfo newSymbolInfo, bool requireNonNullSymbols = false)
        {
            return SymbolInfosAreCompatible(originalSymbolInfo, newSymbolInfo, performEquivalenceCheck: !this.isNewSemanticModelSpeculativeModel, requireNonNullSymbols: requireNonNullSymbols);
        }

        protected bool SymbolsAreCompatible(ISymbol symbol, ISymbol newSymbol, bool requireNonNullSymbols = false)
        {
            return SymbolsAreCompatibleCore(symbol, newSymbol, performEquivalenceCheck: !isNewSemanticModelSpeculativeModel, requireNonNullSymbols: requireNonNullSymbols);
        }

        private static bool SymbolsAreCompatibleCore(ISymbol symbol, ISymbol newSymbol, bool performEquivalenceCheck, bool requireNonNullSymbols = false)
        {
            if (symbol == null && newSymbol == null)
            {
                return !requireNonNullSymbols;
            }

            if (symbol == null || newSymbol == null)
            {
                return false;
            }

            if (symbol.IsReducedExtension())
            {
                symbol = ((IMethodSymbol)symbol).GetConstructedReducedFrom();
            }

            if (newSymbol.IsReducedExtension())
            {
                newSymbol = ((IMethodSymbol)newSymbol).GetConstructedReducedFrom();
            }

            // TODO: Lambda function comparison performs syntax equality, hence is non-trivial to compare lambda methods across different compilations.
            // For now, just assume they are equal.
            if (symbol.IsAnonymousFunction())
            {
                return newSymbol.IsAnonymousFunction();
            }

            if (performEquivalenceCheck)
            {
                // We are comparing symbols across two semantic models (where neither is the speculative model of other one).
                // We will use the SymbolEquivalenceComparer to check if symbols are equivalent.

                switch (symbol.Kind)
                {
                    // SymbolEquivalenceComparer performs Location equality checks for Locals, Labels and RangeVariables.
                    // As we are comparing symbols from different semantic models, locations will differ.
                    // Hence perform minimal checks for these symbol kinds.
                    case SymbolKind.Local:
                        return newSymbol.Kind == SymbolKind.Local &&
                            newSymbol.IsImplicitlyDeclared == symbol.IsImplicitlyDeclared &&
                            string.Equals(symbol.Name, newSymbol.Name, StringComparison.Ordinal) &&
                            ((ILocalSymbol)newSymbol).Type.Equals(((ILocalSymbol)symbol).Type);

                    case SymbolKind.Label:
                    case SymbolKind.RangeVariable:
                        return newSymbol.Kind == symbol.Kind && string.Equals(newSymbol.Name, symbol.Name, StringComparison.Ordinal);

                    case SymbolKind.Parameter:
                        if (newSymbol.Kind == SymbolKind.Parameter && symbol.ContainingSymbol.IsAnonymousFunction())
                        {
                            var param = (IParameterSymbol)symbol;
                            var newParam = (IParameterSymbol)newSymbol;
                            return param.IsRefOrOut() == newParam.IsRefOrOut() &&
                                param.Name == newParam.Name &&
                                SymbolEquivalenceComparer.Instance.Equals(param.Type, newParam.Type) &&
                                newSymbol.ContainingSymbol.IsAnonymousFunction();
                        }

                        goto default;

                    default:
                        return SymbolEquivalenceComparer.Instance.Equals(symbol, newSymbol);
                }
            }

            if (symbol.Equals(newSymbol))
            {
                return true;
            }

            // Handle equivalence of special built-in comparison operators between enum types and it's underlying enum type.
            if (symbol.Kind == SymbolKind.Method && newSymbol.Kind == SymbolKind.Method)
            {
                var methodSymbol = (IMethodSymbol)symbol;
                var newMethodSymbol = (IMethodSymbol)newSymbol;

                PredefinedOperator originalOp, newOp;
                if (methodSymbol.TryGetPredefinedComparisonOperator(out originalOp) &&
                    newMethodSymbol.TryGetPredefinedComparisonOperator(out newOp) &&
                    originalOp == newOp)
                {
                    var type = methodSymbol.ContainingType;
                    var newType = newMethodSymbol.ContainingType;
                    if ((type.IsEnumType() && type.EnumUnderlyingType.SpecialType == newType.SpecialType) ||
                        (newType.IsEnumType() && newType.EnumUnderlyingType.SpecialType == type.SpecialType))
                    {
                        return true;
                    }
                }
            }

            return false;
        }

        #endregion

        /// <summary>
        /// Determines whether performing the given syntax replacement will change the semantics of any parenting expressions
        /// by performing a bottom up walk from the <see cref="OriginalExpression"/> upto <see cref="SemanticRootOfOriginalExpression"/>
        /// in the original tree and simultaneously walking bottom up from <see cref="ReplacedExpression"/> up to <see cref="SemanticRootOfReplacedExpression"/>
        /// in the speculated syntax tree and performing appropriate semantic comparisons.
        /// </summary>
        public bool ReplacementChangesSemantics()
        {
            if (this.SemanticRootOfOriginalExpression is TTypeSyntax)
            {
                var originalType = (TTypeSyntax)this.OriginalExpression;
                var newType = (TTypeSyntax)this.ReplacedExpression;
                return ReplacementBreaksTypeResolution(originalType, newType, useSpeculativeModel: false);
            }

            return ReplacementChangesSemantics(
                currentOriginalNode: this.OriginalExpression,
                currentReplacedNode: this.ReplacedExpression,
                originalRoot: this.SemanticRootOfOriginalExpression,
                skipVerificationForCurrentNode: this.skipVerificationForReplacedNode);
        }

        protected abstract bool IsParenthesizedExpression(TSyntaxNode node);

        protected bool ReplacementChangesSemantics(TSyntaxNode currentOriginalNode, TSyntaxNode currentReplacedNode, TSyntaxNode originalRoot, bool skipVerificationForCurrentNode)
        {
            if (this.SpeculativeSemanticModel == null)
            {
                // This is possible for some broken code scenarios with parse errors, bail out gracefully here.
                return true;
            }

            TSyntaxNode previousOriginalNode = null, previousReplacedNode = null;

            while (true)
            {
                if (!skipVerificationForCurrentNode && ReplacementChangesSemanticsForNode(currentOriginalNode,
                    currentReplacedNode, previousOriginalNode, previousReplacedNode))
                {
                    return true;
                }

                if (currentOriginalNode == originalRoot)
                {
                    break;
                }

                previousOriginalNode = currentOriginalNode;
                previousReplacedNode = currentReplacedNode;
                currentOriginalNode = (TSyntaxNode)currentOriginalNode.Parent;
                currentReplacedNode = (TSyntaxNode)currentReplacedNode.Parent;
                skipVerificationForCurrentNode = skipVerificationForCurrentNode && IsParenthesizedExpression(currentReplacedNode);
            }

            return false;
        }

        /// <summary>
        /// Checks whether the semantic symbols for the <see cref="OriginalExpression"/> and <see cref="ReplacedExpression"/> are non-null and compatible.
        /// </summary>
        /// <returns></returns>
        public bool SymbolsForOriginalAndReplacedNodesAreCompatible()
        {
            if (this.SpeculativeSemanticModel == null)
            {
                // This is possible for some broken code scenarios with parse errors, bail out gracefully here.
                return false;
            }

            return SymbolsAreCompatible(this.OriginalExpression, this.ReplacedExpression, requireNonNullSymbols: true);
        }

        protected abstract bool ReplacementChangesSemanticsForNodeLanguageSpecific(TSyntaxNode currentOriginalNode, TSyntaxNode currentReplacedNode, TSyntaxNode previousOriginalNode, TSyntaxNode previousReplacedNode);

        private bool ReplacementChangesSemanticsForNode(TSyntaxNode currentOriginalNode, TSyntaxNode currentReplacedNode, TSyntaxNode previousOriginalNode, TSyntaxNode previousReplacedNode)
        {
            Debug.Assert(previousOriginalNode == null || previousOriginalNode.Parent == currentOriginalNode);
            Debug.Assert(previousReplacedNode == null || previousReplacedNode.Parent == currentReplacedNode);

            if (IsInvocableExpression(currentOriginalNode))
            {
                // If replacing the node will result in a change in overload resolution, we won't remove it.
                var originalExpression = (TExpressionSyntax)currentOriginalNode;
                var newExpression = (TExpressionSyntax)currentReplacedNode;
                if (ReplacementBreaksInvocableExpression(originalExpression, newExpression))
                {
                    return true;
                }

                return !ImplicitConversionsAreCompatible(originalExpression, newExpression);
            }
            else if (currentOriginalNode is TForEachStatementSyntax)
            {
                var originalForEachStatement = (TForEachStatementSyntax)currentOriginalNode;
                var newForEachStatement = (TForEachStatementSyntax)currentReplacedNode;
                return ReplacementBreaksForEachStatement(originalForEachStatement, newForEachStatement);
            }
            else if (currentOriginalNode is TAttributeSyntax)
            {
                var originalAttribute = (TAttributeSyntax)currentOriginalNode;
                var newAttribute = (TAttributeSyntax)currentReplacedNode;
                return ReplacementBreaksAttribute(originalAttribute, newAttribute);
            }
            else if (currentOriginalNode is TThrowStatementSyntax)
            {
                var originalThrowStatement = (TThrowStatementSyntax)currentOriginalNode;
                var newThrowStatement = (TThrowStatementSyntax)currentReplacedNode;
                return ReplacementBreaksThrowStatement(originalThrowStatement, newThrowStatement);
            }
            else if (ReplacementChangesSemanticsForNodeLanguageSpecific(currentOriginalNode, currentReplacedNode, previousOriginalNode, previousReplacedNode))
            {
                return true;
            }

            if (currentOriginalNode is TTypeSyntax)
            {
                var originalType = (TTypeSyntax)currentOriginalNode;
                var newType = (TTypeSyntax)currentReplacedNode;
                return ReplacementBreaksTypeResolution(originalType, newType);
            }
            else if (currentOriginalNode is TExpressionSyntax)
            {
                var originalExpression = (TExpressionSyntax)currentOriginalNode;
                var newExpression = (TExpressionSyntax)currentReplacedNode;
                if (!ImplicitConversionsAreCompatible(originalExpression, newExpression))
                {
                    return true;
                }
            }

            return false;
        }

        private bool ReplacementBreaksAttribute(TAttributeSyntax attribute, TAttributeSyntax newAttribute)
        {
            var attributeSym = this.OriginalSemanticModel.GetSymbolInfo(attribute).Symbol;
            var newAttributeSym = this.SpeculativeSemanticModel.GetSymbolInfo(newAttribute).Symbol;
            return !SymbolsAreCompatible(attributeSym, newAttributeSym);
        }

        protected abstract TExpressionSyntax GetForEachStatementExpression(TForEachStatementSyntax forEachStatement);

        protected abstract bool IsForEachTypeInferred(TForEachStatementSyntax forEachStatement, TSemanticModel semanticModel);

        private bool ReplacementBreaksForEachStatement(TForEachStatementSyntax forEachStatement, TForEachStatementSyntax newForEachStatement)
        {
            var forEachExpression = GetForEachStatementExpression(forEachStatement);
            if (forEachExpression.IsMissing ||
                !forEachExpression.Span.Contains(expression.SpanStart))
            {
                return false;
            }

            // inferred variable type compatible
            if (IsForEachTypeInferred(forEachStatement, this.semanticModel))
            {
                var local = (ILocalSymbol)semanticModel.GetDeclaredSymbol(forEachStatement);
                var newLocal = (ILocalSymbol)this.SpeculativeSemanticModel.GetDeclaredSymbol(newForEachStatement);
                if (!SymbolsAreCompatible(local.Type, newLocal.Type))
                {
                    return true;
                }
            }

            IMethodSymbol originalGetEnumerator;
            ITypeSymbol originalElementType;
            GetForEachSymbols(this.OriginalSemanticModel, forEachStatement, out originalGetEnumerator, out originalElementType);

            IMethodSymbol newGetEnumerator;
            ITypeSymbol newElementType;
            GetForEachSymbols(this.SpeculativeSemanticModel, newForEachStatement, out newGetEnumerator, out newElementType);

            var newForEachExpression = GetForEachStatementExpression(newForEachStatement);

            if (ReplacementBreaksForEachGetEnumerator(originalGetEnumerator, newGetEnumerator, newForEachExpression) ||
                !ForEachConversionsAreCompatible(this.OriginalSemanticModel, forEachStatement, this.SpeculativeSemanticModel, newForEachStatement) ||
                !SymbolsAreCompatible(originalElementType, newElementType))
            {
                return true;
            }

            return false;
        }

        protected abstract bool ForEachConversionsAreCompatible(SemanticModel originalModel, TForEachStatementSyntax originalForEach, SemanticModel newModel, TForEachStatementSyntax newForEach);

        protected abstract void GetForEachSymbols(SemanticModel model, TForEachStatementSyntax forEach, out IMethodSymbol getEnumeratorMethod, out ITypeSymbol elementType);

        private bool ReplacementBreaksForEachGetEnumerator(IMethodSymbol getEnumerator, IMethodSymbol newGetEnumerator, TExpressionSyntax newForEachStatementExpression)
        {
            if (getEnumerator == null && newGetEnumerator == null)
            {
                return false;
            }

            if (getEnumerator == null || newGetEnumerator == null)
            {
                return true;
            }

            if (getEnumerator.ToSignatureDisplayString() != newGetEnumerator.ToSignatureDisplayString())
            {
                // Note this is likely an interface member from IEnumerable but the new member may be a
                // GetEnumerator method on a specific type.
                if (getEnumerator.IsImplementable())
                {
                    var expressionType = this.SpeculativeSemanticModel.GetTypeInfo(newForEachStatementExpression, cancellationToken).ConvertedType;
                    if (expressionType != null)
                    {
                        var implementationMember = expressionType.FindImplementationForInterfaceMember(getEnumerator);
                        if (implementationMember != null)
                        {
                            if (implementationMember.ToSignatureDisplayString() != newGetEnumerator.ToSignatureDisplayString())
                            {
                                return false;
                            }
                        }
                    }
                }

                return true;
            }

            return false;
        }

        protected abstract TExpressionSyntax GetThrowStatementExpression(TThrowStatementSyntax throwStatement);

        private bool ReplacementBreaksThrowStatement(TThrowStatementSyntax originalThrowStatement, TThrowStatementSyntax newThrowStatement)
        {
            var originalThrowExpression = GetThrowStatementExpression(originalThrowStatement);
            var originalThrowExpressionType = this.OriginalSemanticModel.GetTypeInfo(originalThrowExpression).Type;
            var newThrowExpression = GetThrowStatementExpression(newThrowStatement);
            var newThrowExpressionType = this.SpeculativeSemanticModel.GetTypeInfo(newThrowExpression).Type;

            // C# language specification requires that type of the expression passed to ThrowStatement is or derives from System.Exception.
            return originalThrowExpressionType.IsOrDerivesFromExceptionType(this.OriginalSemanticModel.Compilation) !=
                newThrowExpressionType.IsOrDerivesFromExceptionType(this.SpeculativeSemanticModel.Compilation);
        }

        protected abstract bool IsInNamespaceOrTypeContext(TExpressionSyntax node);

        private bool ReplacementBreaksTypeResolution(TTypeSyntax type, TTypeSyntax newType, bool useSpeculativeModel = true)
        {
            var symbol = this.OriginalSemanticModel.GetSymbolInfo(type).Symbol;

            ISymbol newSymbol;
            if (useSpeculativeModel)
            {
                newSymbol = this.SpeculativeSemanticModel.GetSymbolInfo(newType, cancellationToken).Symbol;
            }
            else
            {
                var bindingOption = IsInNamespaceOrTypeContext(type) ? SpeculativeBindingOption.BindAsTypeOrNamespace : SpeculativeBindingOption.BindAsExpression;
                newSymbol = this.OriginalSemanticModel.GetSpeculativeSymbolInfo(type.SpanStart, newType, bindingOption).Symbol;
            }

            return symbol != null && !SymbolsAreCompatible(symbol, newSymbol);
        }

        protected abstract bool IsInvocableExpression(TSyntaxNode node);

        private static bool IsDelegateInvoke(ISymbol symbol)
        {
            return symbol.Kind == SymbolKind.Method &&
                ((IMethodSymbol)symbol).MethodKind == MethodKind.DelegateInvoke;
        }

        private static bool IsAnonymousDelegateInvoke(ISymbol symbol)
        {
            return IsDelegateInvoke(symbol) &&
                symbol.ContainingType != null &&
                symbol.ContainingType.IsAnonymousType();
        }

        private bool ReplacementBreaksInvocableExpression(TExpressionSyntax expression, TExpressionSyntax newExpression)
        {
            var originalSymbolInfo = semanticModel.GetSymbolInfo(expression);
            if (this.failOnOverloadResolutionFailuresInOriginalCode && originalSymbolInfo.CandidateReason == CandidateReason.OverloadResolutionFailure)
            {
                return true;
            }

            var newSymbolInfo = this.SpeculativeSemanticModel.GetSymbolInfo(node: newExpression);
            var symbol = originalSymbolInfo.Symbol;
            var newSymbol = newSymbolInfo.Symbol;

            if (SymbolInfosAreCompatible(originalSymbolInfo, newSymbolInfo))
            {
                // Original and new symbols for the invocation expression are compatible.
                // However, if the symbols are interface members and if the receiver symbol for one of the expressions is a possible ValueType type parameter,
                // and the other one is not, then there might be a boxing conversion at runtime which causes different runtime behavior.
                if (symbol.IsImplementable())
                {
                    if (IsReceiverNonUniquePossibleValueTypeParam(expression, this.OriginalSemanticModel) !=
                        IsReceiverNonUniquePossibleValueTypeParam(newExpression, this.SpeculativeSemanticModel))
                    {
                        return true;
                    }
                }

                return false;
            }

            if (symbol == null || newSymbol == null || originalSymbolInfo.CandidateReason != newSymbolInfo.CandidateReason)
            {
                return true;
            }

            if (newSymbol.IsOverride)
            {
                var overriddenMember = newSymbol.OverriddenMember();

                while (overriddenMember != null)
                {
                    if (symbol.Equals(overriddenMember))
                    {
                        return !SymbolsHaveCompatibleParameterLists(symbol, newSymbol, expression);
                    }

                    overriddenMember = overriddenMember.OverriddenMember();
                }
            }

            if (symbol.IsImplementable() &&
                IsCompatibleInterfaceMemberImplementation(symbol, newSymbol, expression, newExpression, this.SpeculativeSemanticModel))
            {
                return false;
            }

            // Allow speculated invocation expression to bind to a different method symbol if the method's containing type is a delegate type
            // which has a delegate variance conversion to/from the original method's containing delegate type.
            if (newSymbol.ContainingType.IsDelegateType() &&
                symbol.ContainingType.IsDelegateType() &&
                IsReferenceConversion(this.OriginalSemanticModel.Compilation, newSymbol.ContainingType, symbol.ContainingType))
            {
                return false;
            }

            // Heuristic: If we now bind to an anonymous delegate's invoke method, assume that
            // this isn't a change in overload resolution.
            if (IsAnonymousDelegateInvoke(newSymbol))
            {
                return false;
            }

            return true;
        }

        protected abstract bool IsReferenceConversion(Compilation model, ITypeSymbol sourceType, ITypeSymbol targetType);

        private bool IsCompatibleInterfaceMemberImplementation(
            ISymbol symbol,
            ISymbol newSymbol,
            TExpressionSyntax originalInvocationExpression,
            TExpressionSyntax newInvocationExpression,
            TSemanticModel speculativeSemanticModel)
        {
            // If this is an interface member, we have to be careful. In general,
            // we have to treat an interface member as incompatible with the new member
            // due to virtual dispatch. I.e. A member in a sub class may be preferred
            // at runtime and we have no way of knowing that at compile-time.
            //
            // However, there are special circumstances where we can be sure
            // that the interface member won't result in virtual dispatch:
            //
            //     * The new member is on an effectively sealed class, i.e. at least one of the following is true for the containing type:
            //          (a) It is a sealed class.
            //          (b) It has no nested classes and no accessible instance constructors.
            //          (c) It is a private class with no nested or sibling classes.
            //     * The new member is on System.Array.
            //     * The new member is on System.Delegate.
            //     * The new member is on System.Enum.
            //     * The member is on a struct that we are sure we have a unique copy
            //       of (for example, if it's returned to us by an invocation,
            //       object-creation, element-access or member-access expression.

            INamedTypeSymbol newSymbolContainingType = newSymbol.ContainingType;
            if (newSymbolContainingType == null)
            {
                return false;
            }

            var newReceiver = GetReceiver(newInvocationExpression);
            ITypeSymbol newReceiverType = newReceiver != null ?
                speculativeSemanticModel.GetTypeInfo(newReceiver).Type :
                newSymbolContainingType;

            if (newReceiverType == null)
            {
                return false;
            }

            if (newReceiverType.IsReferenceType &&
                !IsEffectivelySealedClass(newReceiverType) &&
                newSymbolContainingType.SpecialType != SpecialType.System_Array &&
                newSymbolContainingType.SpecialType != SpecialType.System_Delegate &&
                newSymbolContainingType.SpecialType != SpecialType.System_Enum &&
                !IsReceiverUniqueInstance(newReceiver, speculativeSemanticModel))
            {
                return false;
            }

            if (newReceiverType.IsValueType && newReceiverType.SpecialType == SpecialType.None)
            {
                if (newReceiver == null ||
                    !IsReceiverUniqueInstance(newReceiver, speculativeSemanticModel))
                {
                    return false;
                }
            }

            var implementationMember = newSymbolContainingType.FindImplementationForInterfaceMember(symbol);
            if (implementationMember == null)
            {
                return false;
            }

            if (!newSymbol.Equals(implementationMember))
            {
                return false;
            }

            return SymbolsHaveCompatibleParameterLists(symbol, implementationMember, originalInvocationExpression);
        }

        private static bool IsEffectivelySealedClass(ITypeSymbol type)
        {
            var namedType = type as INamedTypeSymbol;
            if (namedType == null)
            {
                return false;
            }

            if (namedType.IsSealed)
            {
                return true;
            }

            if (!namedType.GetTypeMembers().Any(nestedType => nestedType.TypeKind == TypeKind.Class))
            {
                // No nested classes.

                // A class with no nested classes and no accessible instance constructors is effectively sealed.
                if (namedType.InstanceConstructors.All(ctor => ctor.DeclaredAccessibility == Accessibility.Private))
                {
                    return true;
                }

                // A private class with no nested or sibling classes is effectively sealed.
                if (namedType.DeclaredAccessibility == Accessibility.Private &&
                    namedType.ContainingType != null &&
                    !namedType.ContainingType.GetTypeMembers().Any(nestedType => nestedType.TypeKind == TypeKind.Class && namedType != nestedType))
                {
                    return true;
                }
            }

            return false;
        }

        private bool IsReceiverNonUniquePossibleValueTypeParam(TExpressionSyntax invocation, TSemanticModel semanticModel)
        {
            var receiver = GetReceiver(invocation);
            if (receiver != null)
            {
                var receiverType = semanticModel.GetTypeInfo(receiver).Type;
                if (receiverType.IsKind(SymbolKind.TypeParameter) && !receiverType.IsReferenceType)
                {
                    return !IsReceiverUniqueInstance(receiver, semanticModel);
                }
            }

            return false;
        }

        // Returns true if the given receiver expression for an invocation represents a unique copy of the underlying object
        // that is not referenced by any other variable.
        // For example, if the receiver expression is an invocation, object-creation, element-access or member-access expression.
        private static bool IsReceiverUniqueInstance(TExpressionSyntax receiver, TSemanticModel semanticModel)
        {
            var receiverSymbol = semanticModel.GetSymbolInfo(receiver).Symbol;

            if (receiverSymbol == null)
            {
                return false;
            }

            return receiverSymbol.IsKind(SymbolKind.Method) ||
                receiverSymbol.IsIndexer() ||
                (receiverSymbol.IsKind(SymbolKind.Field) && ((IFieldSymbol)receiverSymbol).IsReadOnly);
        }

        protected abstract ImmutableArray<TArgumentSyntax> GetArguments(TExpressionSyntax expression);
        protected abstract TExpressionSyntax GetReceiver(TExpressionSyntax expression);

        private bool SymbolsHaveCompatibleParameterLists(ISymbol originalSymbol, ISymbol newSymbol, TExpressionSyntax originalInvocation)
        {
            if (originalSymbol.IsKind(SymbolKind.Method) || originalSymbol.IsIndexer())
            {
                var symbolParameters = originalSymbol.GetParameters();
                var newSymbolParameters = newSymbol.GetParameters();
                var specifiedArguments = GetArguments(originalInvocation);
                return AreCompatibleParameterLists(specifiedArguments, symbolParameters, newSymbolParameters);
            }

            return true;
        }

        protected abstract bool IsNamedArgument(TArgumentSyntax argument);
        protected abstract string GetNamedArgumentIdentifierValueText(TArgumentSyntax argument);

        private bool AreCompatibleParameterLists(
            ImmutableArray<TArgumentSyntax> specifiedArguments,
            ImmutableArray<IParameterSymbol> signature1Parameters,
            ImmutableArray<IParameterSymbol> signature2Parameters)
        {
            Debug.Assert(signature1Parameters.Length == signature2Parameters.Length);
            Debug.Assert(specifiedArguments.Length <= signature1Parameters.Length ||
                        (signature1Parameters.Length > 0 && !signature1Parameters.Last().IsParams));

            if (signature1Parameters.Length != signature2Parameters.Length)
            {
                return false;
            }

            // If there aren't any parameters, we're OK.
            if (signature1Parameters.Length == 0)
            {
                return true;
            }

            // To ensure that the second parameter list is called in the same way as the
            // first, we need to use the specified arguments to bail out if...
            //
            //     * A named argument doesn't have a corresponding parameter in the
            //       in either parameter list, or...
            //
            //     * A named argument matches a parameter that is in a different position
            //       in the two parameter lists.
            //
            // After checking the specifed arguments, we walk the unspecified parameters
            // in both parameter lists to ensure that they have matching default values.

            var specifiedParameters1 = new List<IParameterSymbol>();
            var specifiedParameters2 = new List<IParameterSymbol>();

            for (int i = 0; i < specifiedArguments.Length; i++)
            {
                var argument = specifiedArguments[i];

                // Handle named argument
                if (IsNamedArgument(argument))
                {
                    var name = GetNamedArgumentIdentifierValueText(argument);

                    var parameter1 = signature1Parameters.FirstOrDefault(p => p.Name == name);
                    Debug.Assert(parameter1 != null);

                    var parameter2 = signature2Parameters.FirstOrDefault(p => p.Name == name);
                    if (parameter2 == null)
                    {
                        return false;
                    }

                    if (signature1Parameters.IndexOf(parameter1) != signature2Parameters.IndexOf(parameter2))
                    {
                        return false;
                    }

                    specifiedParameters1.Add(parameter1);
                    specifiedParameters2.Add(parameter2);
                }
                else
                {
                    // otherwise, treat the argument positionally, taking care to properly
                    // handle params parameters.
                    if (i < signature1Parameters.Length)
                    {
                        specifiedParameters1.Add(signature1Parameters[i]);
                        specifiedParameters2.Add(signature2Parameters[i]);
                    }
                }
            }

            // At this point, we can safely assume that specifiedParameters1 and signature2Parameters
            // contain parameters that appear at the same positions in their respective signatures
            // because we bailed out if named arguments referred to parameters at different positions.
            //
            // Now we walk the unspecified parameters to ensure that they have the same default
            // values.

            for (int i = 0; i < signature1Parameters.Length; i++)
            {
                var parameter1 = signature1Parameters[i];
                if (specifiedParameters1.Contains(parameter1))
                {
                    continue;
                }

                var parameter2 = signature2Parameters[i];

                Debug.Assert(parameter1.HasExplicitDefaultValue, "Expected all unspecified parameter to have default values");
                Debug.Assert(parameter1.HasExplicitDefaultValue == parameter2.HasExplicitDefaultValue);

                if (parameter1.HasExplicitDefaultValue && parameter2.HasExplicitDefaultValue)
                {
                    if (!object.Equals(parameter2.ExplicitDefaultValue, parameter1.ExplicitDefaultValue))
                    {
                        return false;
                    }

                    if (object.Equals(parameter1.ExplicitDefaultValue, 0.0))
                    {
                        bool isParam1DefaultValueNegativeZero = double.IsNegativeInfinity(1.0 / (double)parameter1.ExplicitDefaultValue);
                        bool isParam2DefaultValueNegativeZero = double.IsNegativeInfinity(1.0 / (double)parameter2.ExplicitDefaultValue);
                        if (isParam1DefaultValueNegativeZero != isParam2DefaultValueNegativeZero)
                        {
                            return false;
                        }
                    }
                }
            }

            return true;
        }
    }
}