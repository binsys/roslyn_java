﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.FindSymbols;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.LanguageServices;
using Microsoft.CodeAnalysis.Options;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Rename.ConflictEngine
{
    internal static partial class ConflictResolver
    {
        /// <summary>
        /// Helper class to track the state necessary for finding/resolving conflicts in a 
        /// rename session.
        /// </summary>
        private class Session
        {
            // Set of All Locations that will be renamed (does not include non-reference locations that need to be checked for conflicts)
            private readonly RenameLocationSet renameLocationSet;

            // Rename Symbol's Source Location
            private readonly Location renameSymbolDeclarationLocation;
            private readonly DocumentId documentIdOfRenameSymbolDeclaration;
            private readonly string originalText;
            private readonly string replacementText;
            private readonly OptionSet optionSet;
            private readonly CancellationToken cancellationToken;

            private readonly RenameAnnotation renamedSymbolDeclarationAnnotation;

            // Contains Strings like Bar -> BarAttribute ; Property Bar -> Bar , get_Bar, set_Bar
            private readonly List<string> possibleNameConflicts;
            private readonly HashSet<DocumentId> documentsIdsToBeCheckedForConflict;
            private readonly AnnotationTable<RenameAnnotation> renameAnnotations;

            private ISet<ConflictLocationInfo> conflictLocations;
            private bool replacementTextValid;
            private List<ProjectId> topologicallySortedProjects;
            private bool documentOfRenameSymbolHasBeenRenamed;

            public Session(RenameLocationSet renameLocationSet, Location renameSymbolDeclarationLocation, string originalText, string replacementText, OptionSet optionSet, CancellationToken cancellationToken)
            {
                this.renameLocationSet = renameLocationSet;
                this.renameSymbolDeclarationLocation = renameSymbolDeclarationLocation;
                this.originalText = originalText;
                this.replacementText = replacementText;
                this.optionSet = optionSet;
                this.cancellationToken = cancellationToken;

                this.renamedSymbolDeclarationAnnotation = new RenameAnnotation();

                this.conflictLocations = SpecializedCollections.EmptySet<ConflictLocationInfo>();
                this.replacementTextValid = true;
                this.possibleNameConflicts = new List<string>();

                // only process documents which possibly contain the identifiers.
                this.documentsIdsToBeCheckedForConflict = new HashSet<DocumentId>();
                this.documentIdOfRenameSymbolDeclaration = renameLocationSet.Solution.GetDocument(renameSymbolDeclarationLocation.SourceTree).Id;

                this.renameAnnotations = new AnnotationTable<RenameAnnotation>(RenameAnnotation.Kind);
            }

            private struct ConflictLocationInfo
            {
                // The span of the Node that needs to be complexified 
                public readonly TextSpan ComplexifiedSpan;
                public readonly DocumentId DocumentId;

                // The identifier span that needs to be checked for conflict
                public readonly TextSpan OriginalIdentifierSpan;

                public ConflictLocationInfo(RelatedLocation location)
                {
                    Debug.Assert(location.ComplexifiedTargetSpan.Contains(location.ConflictCheckSpan) || location.Type == RelatedLocationType.UnresolvableConflict);
                    this.ComplexifiedSpan = location.ComplexifiedTargetSpan;
                    this.DocumentId = location.DocumentId;
                    this.OriginalIdentifierSpan = location.ConflictCheckSpan;
                }
            }

            // The method which performs rename, resolves the conflict locations and returns the result of the rename operation
            public async Task<ConflictResolution> ResolveConflictsAsync()
            {
                await FindDocumentsAndPossibleNameConflicts().ConfigureAwait(false);
                var baseSolution = renameLocationSet.Solution;

                // Process rename one project at a time to improve caching and reduce syntax tree serialization.
                var documentsGroupedByTopologicallySortedProjectId = this.documentsIdsToBeCheckedForConflict
                    .GroupBy(d => d.ProjectId)
                    .OrderBy(g => this.topologicallySortedProjects.IndexOf(g.Key));

                this.replacementTextValid = IsIdentifierValid_Worker(baseSolution, replacementText, documentsGroupedByTopologicallySortedProjectId.Select(g => g.Key), cancellationToken);
                var renamedSpansTracker = new RenamedSpansTracker();
                var conflictResolution = new ConflictResolution(baseSolution, renamedSpansTracker, this.replacementText, this.replacementTextValid);

                foreach (var documentsByProject in documentsGroupedByTopologicallySortedProjectId)
                {
                    var documentIdsThatGetsAnnotatedAndRenamed = new HashSet<DocumentId>(documentsByProject);

                    // Rename is going to be in 4 phases.
                    // 1st phase - Does a simple token replacement
                    // If the 1st phase results in conflict then we perform then:
                    //      2nd phase is to expand and simplify only the reference locations with conflicts
                    //      3rd phase is to expand and simplify all the conflict locations (both reference and non-reference)
                    // If there are unresolved Conflicts after the 3rd phase then in 4th phase, 
                    //      We complexify and resolve locations that were resolvable and for the other locations we perform the normal token replacement like the first the phase.
                    for (int phase = 0; phase < 4; phase++)
                    {
                        // Step 1:
                        // The rename process and annotation for the bookkeeping is performed in one-step
                        // The Process in short is,
                        // 1. If renaming a token which is no conflict then replace the token and make a map of the oldspan to the newspan
                        // 2. If we encounter a node that has to be expanded( because there was a conflict in previous phase), we expand it.
                        //    If the node happens to contain a token that needs to be renamed then we annotate it and rename it after expansion else just expand and proceed
                        // 3. Through the whole process we maintain a map of the oldspan to newspan. In case of expansion & rename, we map the expanded node and the renamed token
                        conflictResolution.UpdateCurrentSolution(await AnnotateAndRename_WorkerAsync(
                            baseSolution,
                            conflictResolution.NewSolution,
                            documentIdsThatGetsAnnotatedAndRenamed,
                            renameLocationSet.Locations,
                            renamedSpansTracker,
                            replacementTextValid).ConfigureAwait(false));

                        // Step 2: Check for conflicts in the renamed solution
                        bool foundResolvableConflicts = await IdentifyConflictsAsync(
                                    documentIdsThatGetsAnnotatedAndRenamed,
                                    documentsByProject.Key,
                                    conflictResolution).ConfigureAwait(false);

                        if (!foundResolvableConflicts || phase == 3)
                        {
                            break;
                        }

                        if (phase == 0)
                        {
                            this.conflictLocations = conflictResolution.RelatedLocations
                                .Where(loc => (documentIdsThatGetsAnnotatedAndRenamed.Contains(loc.DocumentId) && loc.Type == RelatedLocationType.PossibilyResolvableConflict && loc.IsReference))
                                .Select(loc => new ConflictLocationInfo(loc))
                                .ToSet();

                            // If there were no conflicting locations in references, then the first conflict phase has to be skipped.
                            if (this.conflictLocations.Count == 0)
                            {
                                phase++;
                            }
                        }

                        if (phase == 1)
                        {
                            this.conflictLocations = this.conflictLocations.Concat(conflictResolution.RelatedLocations
                                .Where(loc => documentIdsThatGetsAnnotatedAndRenamed.Contains(loc.DocumentId) && loc.Type == RelatedLocationType.PossibilyResolvableConflict)
                                .Select(loc => new ConflictLocationInfo(loc)))
                                .ToSet();
                        }

                        // Set the documents with conflicts that need to be processed in the next phase.
                        // Note that we need to get the conflictLocations here since we're going to remove some locations below if phase == 2
                        documentIdsThatGetsAnnotatedAndRenamed = new HashSet<DocumentId>(this.conflictLocations.Select(l => l.DocumentId));

                        if (phase == 2)
                        {
                            // After phase 2, if there are still conflicts then remove the conflict locations from being expanded
                            var unresolvedLocations = conflictResolution.RelatedLocations
                                .Where(l => (l.Type & RelatedLocationType.UnresolvedConflict) != 0)
                                .Select(l => Tuple.Create(l.ComplexifiedTargetSpan, l.DocumentId)).Distinct();

                            this.conflictLocations = this.conflictLocations.Where(l => !unresolvedLocations.Any(c => c.Item2 == l.DocumentId && c.Item1.Contains(l.OriginalIdentifierSpan))).ToSet();
                        }

                        // Clean up side effects from rename before entering the next phase
                        conflictResolution.ClearDocuments(documentIdsThatGetsAnnotatedAndRenamed);
                    }

                    // Step 3: Simplify the project
                    conflictResolution.UpdateCurrentSolution(await renamedSpansTracker.SimplifyAsync(conflictResolution.NewSolution, documentsByProject, replacementTextValid, this.renameAnnotations, cancellationToken).ConfigureAwait(false));
                    conflictResolution.RemoveAllRenameAnnotations(documentsByProject, this.renameAnnotations, cancellationToken);
                }

                // This rename could break implicit references of this symbol (e.g. rename MoveNext on a collection like type in a 
                // foreach/for each statement
                ISymbol renamedSymbolInNewSolution = await GetRenamedSymbolInCurrentSolutionAsync(conflictResolution).ConfigureAwait(false);

                if (IsRenameValid(conflictResolution, renamedSymbolInNewSolution))
                {
                    AddImplicitConflicts(
                        renamedSymbolInNewSolution,
                        renameLocationSet.Symbol,
                        renameLocationSet.ImplicitLocations,
                        await conflictResolution.NewSolution.GetDocument(this.documentIdOfRenameSymbolDeclaration).GetSemanticModelAsync(cancellationToken).ConfigureAwait(false),
                        this.renameSymbolDeclarationLocation,
                        renamedSpansTracker.GetAdjustedPosition(this.renameSymbolDeclarationLocation.SourceSpan.Start, this.documentIdOfRenameSymbolDeclaration),
                        conflictResolution,
                        cancellationToken);
                }

                foreach (var relatedLocation in conflictResolution.RelatedLocations)
                {
                    if (relatedLocation.Type == RelatedLocationType.PossibilyResolvableConflict)
                    {
                        relatedLocation.Type = RelatedLocationType.UnresolvedConflict;
                    }
                }
#if DEBUG
                DebugVerifyNoErrors(conflictResolution, this.documentsIdsToBeCheckedForConflict);
#endif
                return conflictResolution;
            }

#if DEBUG
            private void DebugVerifyNoErrors(ConflictResolution conflictResolution, IEnumerable<DocumentId> documents)
            {
                var documentIdErrorStateLookup = new Dictionary<DocumentId, bool>();

                // we only check for the documentIds we add annotations to, which is a subset of the ones we're going 
                // to change the syntax in.
                foreach (var documentId in documents)
                {
                    // remember if there were issues in the document prior to renaming it.
                    var originalDoc = conflictResolution.OldSolution.GetDocument(documentId);
                    documentIdErrorStateLookup.Add(documentId, originalDoc.HasAnyErrors(cancellationToken).WaitAndGetResult(cancellationToken));
                }

                // We want to ignore few error message introduced by rename because the user is wantedly doing it.
                var ignoreErrorCodes = new List<string>();
                ignoreErrorCodes.Add("BC30420"); // BC30420 - Sub Main missing in VB Project
                ignoreErrorCodes.Add("CS5001"); // CS5001 - Missing Main in C# Project

                // only check if rename thinks it was successful
                if (conflictResolution.ReplacementTextValid && conflictResolution.RelatedLocations.All(loc => (loc.Type & RelatedLocationType.UnresolvableConflict) == 0))
                {
                    foreach (var documentId in documents)
                    {
                        // only check documents that had no errors before rename
                        // (we might have fixed them because of rename)
                        if (!documentIdErrorStateLookup[documentId])
                        {
                            conflictResolution.NewSolution.GetDocument(documentId).VerifyNoErrorsAsync("Rename introduced errors in error-free code", cancellationToken, ignoreErrorCodes).Wait(cancellationToken);
                        }
                    }
                }
            }
#endif

            /// <summary>
            /// Find conflicts in the new solution 
            /// </summary>
            private async Task<bool> IdentifyConflictsAsync(
                HashSet<DocumentId> documentIds,
                ProjectId projectId,
                ConflictResolution conflictResolution)
            {
                this.documentOfRenameSymbolHasBeenRenamed |= documentIds.Contains(documentIdOfRenameSymbolDeclaration);

                // Get the renamed symbol in complexified new solution
                ISymbol renamedSymbolInNewSolution = await GetRenamedSymbolInCurrentSolutionAsync(conflictResolution).ConfigureAwait(false);

                // if the text replacement is invalid, we just did a simple token replacement.
                // Therefore we don't need more mapping information and can skip the rest of 
                // the loop body.
                if (!IsRenameValid(conflictResolution, renamedSymbolInNewSolution))
                {
                    foreach (var documentId in documentIds)
                    {
                        var newDocument = conflictResolution.NewSolution.GetDocument(documentId);
                        var syntaxRoot = await newDocument.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

                        var nodesOrTokensWithConflictCheckAnnotations = GetNodesOrTokensToCheckForConflicts(documentId, syntaxRoot);
                        foreach (var nodeOrToken in nodesOrTokensWithConflictCheckAnnotations)
                        {
                            if (nodeOrToken.Item2.IsRenameLocation)
                            {
                                conflictResolution.AddRelatedLocation(new RelatedLocation(nodeOrToken.Item2.OriginalSpan, documentId, RelatedLocationType.UnresolvedConflict));
                            }
                        }
                    }

                    return false;
                }

                Dictionary<Location, Location> reverseMappedLocations = new Dictionary<Location, Location>();

                foreach (var documentId in documentIds)
                {
                    var newDocument = conflictResolution.NewSolution.GetDocument(documentId);
                    var syntaxRoot = await newDocument.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
                    var baseDocument = conflictResolution.OldSolution.GetDocument(documentId);
                    var baseSyntaxTree = await baseDocument.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
                    var baseRoot = await baseDocument.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
                    SemanticModel newDocumentSemanticModel = null;
                    var syntaxFactsService = LanguageService.GetService<ISyntaxFactsService>(newDocument);

                    // Get all tokens that need conflict check
                    var nodesOrTokensWithConflictCheckAnnotations = GetNodesOrTokensToCheckForConflicts(documentId, syntaxRoot);

                    var complexifiedLocationSpanForThisDocument =
                        this.conflictLocations
                        .Where(t => t.DocumentId == documentId)
                        .Select(t => t.OriginalIdentifierSpan).ToSet();

                    foreach (var nodeAndAnnotation in nodesOrTokensWithConflictCheckAnnotations)
                    {
                        var tokenOrNode = nodeAndAnnotation.Item1;
                        var conflictAnnotation = nodeAndAnnotation.Item2;
                        reverseMappedLocations[tokenOrNode.GetLocation()] = baseSyntaxTree.GetLocation(conflictAnnotation.OriginalSpan);
                        var originalLocation = conflictAnnotation.OriginalSpan;

                        var hasConflict = this.renameAnnotations.HasAnnotation(tokenOrNode, RenameInvalidIdentifierAnnotation.Instance);
                        if (!hasConflict)
                        {
                            newDocumentSemanticModel = newDocumentSemanticModel ?? await newDocument.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
                            var newReferencedSymbols = GetSymbolsInNewSolution(newDocument, newDocumentSemanticModel, conflictAnnotation, tokenOrNode);

                            // The semantic correctness, after rename, for each token of interest in the rename context is performed by getting the symbol pointed by
                            // each token and obtain the Symbol's First Ordered Location's  Span-Start and check to see if it is the same as before from the base solution.
                            // During rename, the spans would have been modified and so we need to adjust the old position to the new position for which we use the renameSpanTracker, which
                            // was tracking & mapping the old span -> new span during rename
                            hasConflict = await CheckForConflictAsync(conflictResolution, renamedSymbolInNewSolution, newDocument, conflictAnnotation, newReferencedSymbols).ConfigureAwait(false);
                        }

                        if (!hasConflict && !conflictAnnotation.IsInvocationExpression)
                        {
                            hasConflict = LocalVariableConflictPerLanguage((SyntaxToken)tokenOrNode, newDocument);
                        }

                        if (!hasConflict)
                        {
                            if (conflictAnnotation.IsRenameLocation)
                            {
                                conflictResolution.AddRelatedLocation(
                                    new RelatedLocation(originalLocation,
                                    documentId,
                                    complexifiedLocationSpanForThisDocument.Contains(originalLocation) ? RelatedLocationType.ResolvedReferenceConflict : RelatedLocationType.NoConflict,
                                    isReference: true));
                            }
                            else
                            {
                                // if a complexified location was not a reference location, then it was a resolved conflict of a non reference location
                                if (!conflictAnnotation.IsOriginalTextLocation && complexifiedLocationSpanForThisDocument.Contains(originalLocation))
                                {
                                    conflictResolution.AddRelatedLocation(
                                        new RelatedLocation(originalLocation,
                                        documentId,
                                        RelatedLocationType.ResolvedNonReferenceConflict,
                                        isReference: false));
                                }
                            }
                        }
                        else
                        {
                            var baseToken = baseRoot.FindToken(conflictAnnotation.OriginalSpan.Start, true);
                            var complexifiedTarget = GetExpansionTargetForLocationPerLanguage(baseToken, baseDocument);
                            conflictResolution.AddRelatedLocation(new RelatedLocation(
                                originalLocation,
                                documentId,
                                complexifiedTarget != null ? RelatedLocationType.PossibilyResolvableConflict : RelatedLocationType.UnresolvableConflict,
                                isReference: conflictAnnotation.IsRenameLocation,
                                complexifiedTargetSpan: complexifiedTarget != null ? complexifiedTarget.Span : default(TextSpan)));
                        }
                    }
                }

                // there are more conflicts that cannot be identified by checking if the tokens still reference the same
                // symbol. These conflicts are mostly language specific. A good example is a member with the same name
                // as the parent (yes I know, this is a simplification).
                if (this.documentIdOfRenameSymbolDeclaration.ProjectId == projectId)
                {
                    IEnumerable<ISymbol> referencedSymbols = renameLocationSet.ReferencedSymbols;
                    ISymbol renameSymbol = renameLocationSet.Symbol;
                    await AddDeclarationConflictsAsync(renamedSymbolInNewSolution, renameSymbol, referencedSymbols, conflictResolution, reverseMappedLocations, cancellationToken).ConfigureAwait(false);
                }

                return conflictResolution.RelatedLocations.Any(r => r.Type == RelatedLocationType.PossibilyResolvableConflict);
            }

            /// <summary>
            /// Gets the list of the nodes that were annotated for a conflict check 
            /// </summary>
            private IEnumerable<ValueTuple<SyntaxNodeOrToken, RenameActionAnnotation>> GetNodesOrTokensToCheckForConflicts(
                DocumentId documentId,
                SyntaxNode syntaxRoot)
            {
                return syntaxRoot.DescendantNodesAndTokens(descendIntoTrivia: true)
                    .Where(s => this.renameAnnotations.HasAnnotations<RenameActionAnnotation>(s))
                    .Select(s => ValueTuple.Create(s, this.renameAnnotations.GetAnnotations<RenameActionAnnotation>(s).Single()));
            }

            private async Task<bool> CheckForConflictAsync(
                ConflictResolution conflictResolution,
                ISymbol renamedSymbolInNewSolution,
                Document newDocument,
                RenameActionAnnotation conflictAnnotation,
                IEnumerable<ISymbol> newReferencedSymbols)
            {
                bool hasConflict;
                var solution = conflictResolution.NewSolution;

                if (conflictAnnotation.IsNamespaceDeclarationReference)
                {
                    hasConflict = false;
                }
                else if (!conflictAnnotation.IsRenameLocation && conflictAnnotation.IsOriginalTextLocation && conflictAnnotation.RenameDeclarationLocationReferences.Length > 1 && newReferencedSymbols.Count() == 1)
                {
                    // an ambiguous situation was resolved through rename in non reference locations
                    hasConflict = false;
                }
                else if (newReferencedSymbols.Count() != conflictAnnotation.RenameDeclarationLocationReferences.Length)
                {
                    // Don't show conflicts for errors in the old solution that now bind in the new solution.
                    if (newReferencedSymbols.Count() != 0 && conflictAnnotation.RenameDeclarationLocationReferences.Length == 0)
                    {
                        hasConflict = false;
                    }
                    else
                    {
                        hasConflict = true;
                    }
                }
                else
                {
                    hasConflict = false;
                    int symbolIndex = 0;
                    foreach (var symbol in newReferencedSymbols)
                    {
                        if (conflictAnnotation.RenameDeclarationLocationReferences[symbolIndex].SymbolLocationsCount != symbol.Locations.Count())
                        {
                            hasConflict = true;
                            break;
                        }

                        var newLocation = await GetSymbolLocationAsync(solution, symbol, cancellationToken).ConfigureAwait(false);

                        if (newLocation != null && conflictAnnotation.RenameDeclarationLocationReferences[symbolIndex].IsSourceLocation)
                        {
                            // location was in source before, but not after rename
                            if (!newLocation.IsInSource)
                            {
                                hasConflict = true;
                                break;
                            }

                            var renameDeclarationLocationReference = conflictAnnotation.RenameDeclarationLocationReferences[symbolIndex];
                            var newAdjustedStartPosition = conflictResolution.GetAdjustedTokenStartingPosition(renameDeclarationLocationReference.TextSpan.Start, renameDeclarationLocationReference.DocumentId);
                            if (newAdjustedStartPosition != newLocation.SourceSpan.Start)
                            {
                                hasConflict = true;
                                break;
                            }

                            if (conflictAnnotation.RenameDeclarationLocationReferences[symbolIndex].IsOverriddenFromMetadata)
                            {
                                var overridingSymbol = await SymbolFinder.FindSymbolAtPositionAsync(solution.GetDocument(newLocation.SourceTree), newLocation.SourceSpan.Start, cancellationToken: cancellationToken).ConfigureAwait(false);
                                if (overridingSymbol != null && renamedSymbolInNewSolution != overridingSymbol)
                                {
                                    if (!overridingSymbol.IsOverride)
                                    {
                                        hasConflict = true;
                                        break;
                                    }
                                    else
                                    {
                                        var overriddenSymbol = overridingSymbol.OverriddenMember();
                                        if (overriddenSymbol == null || !overriddenSymbol.Locations.All(loc => loc.IsInMetadata))
                                        {
                                            hasConflict = true;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                        else
                        {
                            var newMetadataName = symbol.ToDisplayString(metadataSymbolDisplayFormat);
                            var oldMetadataName = conflictAnnotation.RenameDeclarationLocationReferences[symbolIndex].Name;
                            if (newLocation.IsInSource ||
                                !HeuristicMetadataNameEquivalenceCheck(
                                    oldMetadataName,
                                    newMetadataName,
                                    originalText,
                                    replacementText))
                            {
                                hasConflict = true;
                                break;
                            }
                        }

                        symbolIndex++;
                    }
                }

                return hasConflict;
            }

            private IEnumerable<ISymbol> GetSymbolsInNewSolution(Document newDocument, SemanticModel newDocumentSemanticModel, RenameActionAnnotation conflictAnnotation, SyntaxNodeOrToken tokenOrNode)
            {
                var newReferencedSymbols = RenameUtilities.GetSymbolsTouchingPosition(tokenOrNode.Span.Start, newDocumentSemanticModel, newDocument.Project.Solution.Workspace, cancellationToken);

                if (conflictAnnotation.IsInvocationExpression)
                {
                    IEnumerable<ISymbol> invocationReferencedSymbols = null;
                    if (tokenOrNode.IsNode)
                    {
                        invocationReferencedSymbols = SymbolsForEnclosingInvocationExpressionWorker((SyntaxNode)tokenOrNode, newDocumentSemanticModel, cancellationToken);
                    }

                    if (invocationReferencedSymbols != null)
                    {
                        newReferencedSymbols = invocationReferencedSymbols;
                    }
                }

                // if there are more than one symbol, then remove the alias symbols.
                // When using (not declaring) an alias, the alias symbol and the target symbol are returned
                // by GetSymbolsTouchingPosition
                if (newReferencedSymbols.Skip(1).Any())
                {
                    newReferencedSymbols = newReferencedSymbols.Where(a => a.Kind != SymbolKind.Alias);
                }

                return newReferencedSymbols;
            }

            private async Task<ISymbol> GetRenamedSymbolInCurrentSolutionAsync(ConflictResolution conflictResolution)
            {
                // get the renamed symbol in complexified new solution
                int start = this.documentOfRenameSymbolHasBeenRenamed
                    ? conflictResolution.RenamedSpansTracker.GetAdjustedPosition(this.renameSymbolDeclarationLocation.SourceSpan.Start, this.documentIdOfRenameSymbolDeclaration)
                    : this.renameSymbolDeclarationLocation.SourceSpan.Start;

                var document = conflictResolution.NewSolution.GetDocument(documentIdOfRenameSymbolDeclaration);
                var newSymbol = await SymbolFinder.FindSymbolAtPositionAsync(document, start, cancellationToken: cancellationToken).ConfigureAwait(false);
                return newSymbol;
            }

            /// <summary>
            /// The method determines the set of document that has to be processed for Rename and also gives the possible set of Names
            /// that has to be checked for conflict
            /// </summary>
            private async Task FindDocumentsAndPossibleNameConflicts()
            {
                var allSourceTrees = renameLocationSet.Locations.Select(loc => loc.Location.SourceTree).Distinct();
                var symbol = renameLocationSet.Symbol;
                var newSolution = renameLocationSet.Solution;

                var dependencyGraph = await newSolution.GetProjectDependencyGraphAsync(cancellationToken).ConfigureAwait(false);
                this.topologicallySortedProjects = dependencyGraph.GetTopologicallySortedProjects(cancellationToken).ToList();

                if (symbol.Kind == SymbolKind.Local || symbol.Kind == SymbolKind.Label || symbol.Kind == SymbolKind.RangeVariable)
                {
                    // locals, labels and range variables cannot affect locations outside of the current method, therefore there's no need 
                    // to go through all other documents, because they can never reference this.
                    var sourceTree = allSourceTrees.Single();
                    var currentDocument = newSolution.GetDocument(sourceTree);
                    documentsIdsToBeCheckedForConflict.Add(currentDocument.Id);

                    var renameRewriterLanguageService = LanguageService.GetService<IRenameRewriterLanguageService>(currentDocument.Project);
                    renameRewriterLanguageService.TryAddPossibleNameConflicts(symbol, replacementText, possibleNameConflicts);
                }
                else if (symbol.Kind == SymbolKind.Parameter)
                {
                    var sourceTree = symbol.Locations.First().SourceTree;
                    var currentDocument = newSolution.GetDocument(sourceTree);
                    foreach (var location in renameLocationSet.Locations)
                    {
                        documentsIdsToBeCheckedForConflict.Add(location.DocumentId);
                    }

                    var renameRewriterLanguageService = LanguageService.GetService<IRenameRewriterLanguageService>(currentDocument.Project);
                    renameRewriterLanguageService.TryAddPossibleNameConflicts(symbol, replacementText, possibleNameConflicts);
                }
                else
                {
                    // all documents where actual renames are being performed must be part of the set of documentIds
                    foreach (var sourceTree in allSourceTrees)
                    {
                        documentsIdsToBeCheckedForConflict.Add(newSolution.GetDocument(sourceTree).Id);
                    }

                    // now add all documents that might contain conflicts
                    if ((symbol.Kind == SymbolKind.Field && ((IFieldSymbol)symbol).DeclaredAccessibility == Accessibility.Private) ||
                        (symbol.Kind == SymbolKind.Method && ((IMethodSymbol)symbol).DeclaredAccessibility == Accessibility.Private) ||
                        (symbol.Kind == SymbolKind.NamedType && ((INamedTypeSymbol)symbol).DeclaredAccessibility == Accessibility.Private))
                    {
                        // private members or classes cannot be used outside of the project, so we should only scan documents of this project.
                        var firstDocument = newSolution.GetDocument(allSourceTrees.First());
                        Debug.Assert(allSourceTrees.All(tree => newSolution.GetDocument(tree).Project == firstDocument.Project));
                        await DetermineDocumentsAndConflictingNamesAsync(firstDocument.Project).ConfigureAwait(false);
                    }
                    else
                    {
                        // We are trying to figure out the projects that directly depend on the project that contains the declaration for 
                        // the rename symbol.  Other projects should not be affected by the rename, or they miss a reference to that project.
                        var symbolProjectId = this.documentIdOfRenameSymbolDeclaration.ProjectId;
                        var relevantProjects = SpecializedCollections.SingletonEnumerable<ProjectId>(symbolProjectId).Concat(
                                dependencyGraph.GetProjectsThatDirectlyDependOnThisProject(symbolProjectId));

                        foreach (var pid in relevantProjects)
                        {
                            var project = newSolution.GetProject(pid);
                            await DetermineDocumentsAndConflictingNamesAsync(project).ConfigureAwait(false);
                        }
                    }
                }
            }

            private async Task DetermineDocumentsAndConflictingNamesAsync(Project project)
            {
                var symbol = renameLocationSet.Symbol;
                var renameRewriterLanguageService = LanguageService.GetService<IRenameRewriterLanguageService>(project);
                renameRewriterLanguageService.TryAddPossibleNameConflicts(symbol, replacementText, possibleNameConflicts);

                foreach (var document in project.Documents)
                {
                    if (this.documentsIdsToBeCheckedForConflict.Contains(document.Id))
                    {
                        continue;
                    }

                    var info = await SyntaxTreeInfo.GetIdentifierInfoAsync(document, CancellationToken.None).ConfigureAwait(false);
                    if (info.ProbablyContainsEscapedIdentifier(originalText))
                    {
                        this.documentsIdsToBeCheckedForConflict.Add(document.Id);
                        continue;
                    }

                    if (info.ProbablyContainsIdentifier(replacementText))
                    {
                        this.documentsIdsToBeCheckedForConflict.Add(document.Id);
                        continue;
                    }

                    foreach (var replacementName in possibleNameConflicts)
                    {
                        if (info.ProbablyContainsIdentifier(replacementName))
                        {
                            this.documentsIdsToBeCheckedForConflict.Add(document.Id);
                            break;
                        }
                    }
                }
            }

            // The rename process and annotation for the bookkeeping is performed in one-step
            private async Task<Solution> AnnotateAndRename_WorkerAsync(
                Solution originalSolution,
                Solution partiallyRenamedSolution,
                HashSet<DocumentId> documentIdsToRename,
                IEnumerable<RenameLocation> renameLocations,
                RenamedSpansTracker renameSpansTracker,
                bool replacementTextValid)
            {
                foreach (var documentId in documentIdsToRename.ToList())
                {
                    cancellationToken.ThrowIfCancellationRequested();

                    // We try to rewrite all locations that are not candidate locations. If there is only one location 
                    // it must be the correct one (the symbol is ambiguous to something else) and we always try to rewrite it.
                    var document = originalSolution.GetDocument(documentId);
                    var semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
                    var originalSyntaxRoot = await semanticModel.SyntaxTree.GetRootAsync(cancellationToken).ConfigureAwait(false);

                    // Get all rename locations for the current document.
                    var allTextSpansInSingleSourceTree = renameLocations
                        .Where(l => l.DocumentId == documentId && !l.IsRenameInStringOrComment && (renameLocations.Count() == 1 || !l.IsCandidateLocation))
                        .ToDictionary(l => l.Location.SourceSpan);

                    // All textspan in the document documentId, that requires rename in String or Comment
                    var stringAndCommentTextSpansInSingleSourceTree = renameLocations
                        .Where(l => l.DocumentId == documentId && l.IsRenameInStringOrComment)
                        .GroupBy(l => l.ContainingLocationForStringOrComment)
                        .Select(g => g.Key)
                        .ToSet();

                    var conflictLocationSpans = this.conflictLocations
                                                .Where(t => t.DocumentId == documentId)
                                                .Select(t => t.ComplexifiedSpan).ToSet();

                    // Annotate all nodes with a RenameLocation annotations to record old locations & old referenced symbols.
                    // Also annotate nodes that should get complexified (nodes for rename locations + conflict locations)
                    var parameters = new RenameRewriterParameters(
                        this.renamedSymbolDeclarationAnnotation,
                        document,
                        semanticModel,
                        originalSyntaxRoot,
                        replacementText,
                        originalText,
                        possibleNameConflicts,
                        allTextSpansInSingleSourceTree,
                        stringAndCommentTextSpansInSingleSourceTree,
                        conflictLocationSpans,
                        originalSolution,
                        renameLocationSet.Symbol,
                        replacementTextValid,
                        cancellationToken,
                        renameSpansTracker,
                        optionSet,
                        this.renameAnnotations);

                    var renameRewriterLanguageService = LanguageService.GetService<IRenameRewriterLanguageService>(document);
                    var newRoot = renameRewriterLanguageService.AnnotateAndRename(parameters);

                    if (newRoot == originalSyntaxRoot)
                    {
                        // Update the list for the current phase, some files with strings containing the original or replacement
                        // text may have been filtered out.
                        documentIdsToRename.Remove(documentId);
                    }
                    else
                    {
                        partiallyRenamedSolution = partiallyRenamedSolution.WithDocumentSyntaxRoot(documentId, newRoot, PreservationMode.PreserveIdentity);
                    }
                }

                return partiallyRenamedSolution;
            }
        }
    }
}
