﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.LanguageServices;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.WorkspaceServices;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.SemanticModelWorkspaceService
{
    [ExportWorkspaceServiceFactory(typeof(ISemanticModelService), WorkspaceKind.Any)]
    internal class SemanticModelWorkspaceServiceFactory : IWorkspaceServiceFactory
    {
        public IWorkspaceService CreateService(IWorkspaceServiceProvider workspaceServices)
        {
            return new SemanticModelService();
        }

        private class SemanticModelService : ISemanticModelService
        {
            private static readonly ConditionalWeakTable<Workspace, ConditionalWeakTable<BranchId, Dictionary<ProjectId, CompilationSet>>> map =
                new ConditionalWeakTable<Workspace, ConditionalWeakTable<BranchId, Dictionary<ProjectId, CompilationSet>>>();

            private static readonly ConditionalWeakTable<Compilation, ConditionalWeakTable<SyntaxNode, WeakReference<SemanticModel>>> semanticModelMap =
                new ConditionalWeakTable<Compilation, ConditionalWeakTable<SyntaxNode, WeakReference<SemanticModel>>>();

            private readonly ReaderWriterLockSlim gate = new ReaderWriterLockSlim(LockRecursionPolicy.NoRecursion);

            public async Task<SemanticModel> GetSemanticModelForNodeAsync(Document document, SyntaxNode node, CancellationToken cancellationToken = default(CancellationToken))
            {
                var syntaxFactsService = LanguageService.GetService<ISyntaxFactsService>(document);
                var semanticFactsService = LanguageService.GetService<ISemanticFactsService>(document);

                if (syntaxFactsService == null || semanticFactsService == null || node == null)
                {
                    // it only works if we can track member
                    return await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
                }

                if (IsPrimaryBranch(document) && !document.IsOpen())
                {
                    // for ones in primary branch, we only support opened documents (mostly to help typing scenario)
                    return await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
                }

                var versionMap = GetVersionMapFromBranchOrPrimary(document.Project.Solution.Workspace, document.Project.Solution.BranchId);

                var projectId = document.Project.Id;
                var version = await document.Project.GetDependentSemanticVersionAsync(cancellationToken).ConfigureAwait(false);

                CompilationSet compilationSet;
                using (gate.DisposableRead())
                {
                    versionMap.TryGetValue(projectId, out compilationSet);
                }

                // this is first time
                if (compilationSet == null)
                {
                    // update the cache
                    await AddVersionCacheAsync(document.Project, version, cancellationToken).ConfigureAwait(false);

                    // get the base one
                    return await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
                }

                // we have compilation set check whether it is something we can use
                if (version.Equals(compilationSet.Version))
                {
                    Compilation oldCompilation;
                    if (!compilationSet.Compilation.TryGetValue(out oldCompilation))
                    {
                        await AddVersionCacheAsync(document.Project, version, cancellationToken).ConfigureAwait(false);

                        // get the base one
                        return await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
                    }

                    // first check whether the set has this document
                    SyntaxTree oldTree;
                    if (!compilationSet.Trees.TryGetValue(document.Id, out oldTree))
                    {
                        // noop.
                        return await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
                    }

                    // Yes, we have compilation we might be able to re-use
                    var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
                    if (root.SyntaxTree == oldTree)
                    {
                        // the one we have and the one in the document is same one. but tree in other file might
                        // have changed (no top level change). in that case, just use one from the document.
                        return await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
                    }

                    // let's track member that we can re-use
                    var member = syntaxFactsService.GetContainingMemberDeclaration(root, node.SpanStart);
                    if (!syntaxFactsService.IsMethodLevelMember(member))
                    {
                        // oops, given node is not something we can support
                        return await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
                    }

                    // check whether we already have speculative semantic model for this
                    var cachedModel = GetCachedSemanticModel(oldCompilation, member);
                    if (cachedModel != null)
                    {
                        // Yes!
                        return cachedModel;
                    }

                    // alright, we have member id. find same member from old compilation
                    var memberId = syntaxFactsService.GetMethodLevelMemberId(root, member);
                    var oldRoot = await oldTree.GetRootAsync(cancellationToken).ConfigureAwait(false);

                    var oldMember = syntaxFactsService.GetMethodLevelMember(oldRoot, memberId);
                    var oldModel = oldCompilation.GetSemanticModel(oldTree);

                    SemanticModel model;
                    if (!semanticFactsService.TryGetSpeculativeSemanticModel(oldModel, oldMember, member, out model))
                    {
                        return await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
                    }

                    // cache the new speculative semantic model for the given node
                    Contract.ThrowIfNull(model);
                    return CacheSemanticModel(oldCompilation, member, model);
                }

                // oops, it looks like we can't use cached one. 
                // update the cache
                await UpdateVersionCacheAsync(document.Project, version, compilationSet, cancellationToken).ConfigureAwait(false);

                // get the base one
                return await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
            }

            private bool IsPrimaryBranch(Document document)
            {
                return document.Project.Solution.BranchId == document.Project.Solution.Workspace.PrimaryBranchId;
            }

            private Task AddVersionCacheAsync(Project project, VersionStamp version, CancellationToken cancellationToken)
            {
                return UpdateVersionCacheAsync(project, version, primarySet: null, cancellationToken: cancellationToken);
            }

            private async Task UpdateVersionCacheAsync(Project project, VersionStamp version, CompilationSet primarySet, CancellationToken cancellationToken)
            {
                var versionMap = GetVersionMapFromBranch(project.Solution.Workspace, project.Solution.BranchId);

                CompilationSet compilationSet;
                Compilation compilation;
                if (!AlreadyHasLatestCompilationSet(versionMap, project.Id, version, out compilationSet) ||
                    !compilationSet.Compilation.TryGetValue(out compilation))
                {
                    var newSet = await CompilationSet.CreateAsync(project, compilationSet ?? primarySet, cancellationToken).ConfigureAwait(false);

                    using (gate.DisposableWrite())
                    {
                        // we still don't have it or if someone has beaten us, check what we have is newer
                        if (!versionMap.TryGetValue(project.Id, out compilationSet) || version != compilationSet.Version)
                        {
                            versionMap[project.Id] = newSet;
                        }
                    }
                }
            }

            private bool AlreadyHasLatestCompilationSet(
                Dictionary<ProjectId, CompilationSet> versionMap, ProjectId projectId, VersionStamp version, out CompilationSet compilationSet)
            {
                using (gate.DisposableRead())
                {
                    // we still don't have it or if someone has beaten us, check what we have is newer
                    return versionMap.TryGetValue(projectId, out compilationSet) && version == compilationSet.Version;
                }
            }

            private static readonly ConditionalWeakTable<BranchId, Dictionary<ProjectId, CompilationSet>>.CreateValueCallback createVersionMap =
                _ => new Dictionary<ProjectId, CompilationSet>();

            private static readonly ConditionalWeakTable<Compilation, ConditionalWeakTable<SyntaxNode, WeakReference<SemanticModel>>>.CreateValueCallback createNodeMap =
                _ => new ConditionalWeakTable<SyntaxNode, WeakReference<SemanticModel>>();

            private static SemanticModel GetCachedSemanticModel(
                ConditionalWeakTable<SyntaxNode, WeakReference<SemanticModel>> nodeMap, SyntaxNode newMember)
            {
                SemanticModel model;
                WeakReference<SemanticModel> cached;
                if (!nodeMap.TryGetValue(newMember, out cached) || !cached.TryGetTarget(out model))
                {
                    return null;
                }

                return model;
            }

            private static SemanticModel GetCachedSemanticModel(Compilation oldCompilation, SyntaxNode newMember)
            {
                var nodeMap = semanticModelMap.GetValue(oldCompilation, createNodeMap);

                // see whether we have cached one
                return GetCachedSemanticModel(nodeMap, newMember);
            }

            private static SemanticModel CacheSemanticModel(Compilation oldCompilation, SyntaxNode newMember, SemanticModel speculativeSemanticModel)
            {
                var nodeMap = semanticModelMap.GetValue(oldCompilation, createNodeMap);

                // check whether somebody already have put one for me
                var model = GetCachedSemanticModel(nodeMap, newMember);
                if (model != null)
                {
                    return model;
                }

                // noop. put one
                var weakReference = new WeakReference<SemanticModel>(speculativeSemanticModel);
                var cached = nodeMap.GetValue(newMember, _ => weakReference);

                SemanticModel cachedModel;
                if (cached.TryGetTarget(out cachedModel))
                {
                    return cachedModel;
                }

                // oops. somebody has beaten me, but the model has gone.
                // set me as new target
                cached.SetTarget(speculativeSemanticModel);
                return speculativeSemanticModel;
            }

            private Dictionary<ProjectId, CompilationSet> GetVersionMapFromBranchOrPrimary(Workspace workspace, BranchId branchId)
            {
                var branchMap = GetBranchMap(workspace);

                // check whether we already have one
                Dictionary<ProjectId, CompilationSet> versionMap;
                if (branchMap.TryGetValue(branchId, out versionMap))
                {
                    return versionMap;
                }

                // check primary branch
                if (branchMap.TryGetValue(workspace.PrimaryBranchId, out versionMap))
                {
                    return versionMap;
                }

                // okay, create one
                return branchMap.GetValue(branchId, createVersionMap);
            }

            private Dictionary<ProjectId, CompilationSet> GetVersionMapFromBranch(Workspace workspace, BranchId branchId)
            {
                var branchMap = GetBranchMap(workspace);

                return branchMap.GetValue(branchId, createVersionMap);
            }

            private ConditionalWeakTable<BranchId, Dictionary<ProjectId, CompilationSet>> GetBranchMap(Workspace workspace)
            {
                ConditionalWeakTable<BranchId, Dictionary<ProjectId, CompilationSet>> branchMap;
                if (!map.TryGetValue(workspace, out branchMap))
                {
                    var newBranchMap = new ConditionalWeakTable<BranchId, Dictionary<ProjectId, CompilationSet>>();

                    branchMap = map.GetValue(workspace, _ => newBranchMap);
                    if (branchMap == newBranchMap)
                    {
                        // it is first time we see this workspace. subscribe to it
                        workspace.DocumentClosed += OnDocumentClosed;
                        workspace.WorkspaceChanged += OnWorkspaceChanged;
                    }
                }

                return branchMap;
            }

            private void OnDocumentClosed(object sender, DocumentEventArgs e)
            {
                ClearVersionMap(e.Document.Project.Solution.Workspace, e.Document.Id);
            }

            private void OnWorkspaceChanged(object sender, WorkspaceChangeEventArgs e)
            {
                switch (e.Kind)
                {
                    case WorkspaceChangeKind.SolutionAdded:
                    case WorkspaceChangeKind.SolutionChanged:
                    case WorkspaceChangeKind.SolutionRemoved:
                    case WorkspaceChangeKind.SolutionCleared:
                    case WorkspaceChangeKind.SolutionReloaded:
                        ClearVersionMap(e.NewSolution.Workspace, e.NewSolution.ProjectIds);
                        break;
                    case WorkspaceChangeKind.ProjectAdded:
                    case WorkspaceChangeKind.ProjectRemoved:
                    case WorkspaceChangeKind.ProjectChanged:
                    case WorkspaceChangeKind.ProjectReloaded:
                        ClearVersionMap(e.NewSolution.Workspace, e.ProjectId);
                        break;
                    case WorkspaceChangeKind.DocumentRemoved:
                        ClearVersionMap(e.NewSolution.Workspace, e.DocumentId);
                        break;
                    case WorkspaceChangeKind.DocumentAdded:
                    case WorkspaceChangeKind.DocumentReloaded:
                    case WorkspaceChangeKind.DocumentChanged:
                        break;
                    default:
                        Contract.Fail("Unknown event");
                        break;
                }
            }

            private void ClearVersionMap(Workspace workspace, DocumentId documentId)
            {
                if (workspace.GetOpenDocumentIds(documentId.ProjectId).Any())
                {
                    return;
                }

                var versionMap = GetVersionMapFromBranch(workspace, workspace.PrimaryBranchId);

                using (this.gate.DisposableWrite())
                {
                    versionMap.Remove(documentId.ProjectId);
                }
            }

            private void ClearVersionMap(Workspace workspace, ProjectId projectId)
            {
                var versionMap = GetVersionMapFromBranch(workspace, workspace.PrimaryBranchId);

                using (this.gate.DisposableWrite())
                {
                    versionMap.Remove(projectId);
                }
            }

            private void ClearVersionMap(Workspace workspace, IReadOnlyList<ProjectId> projectIds)
            {
                var versionMap = GetVersionMapFromBranch(workspace, workspace.PrimaryBranchId);

                using (this.gate.DisposableWrite())
                {
                    var set = SharedPools.Default<HashSet<ProjectId>>().AllocateAndClear();

                    set.UnionWith(versionMap.Keys);
                    set.ExceptWith(projectIds);

                    foreach (var projectId in set)
                    {
                        versionMap.Remove(projectId);
                    }

                    SharedPools.Default<HashSet<ProjectId>>().ClearAndFree(set);
                }
            }

            private class CompilationSet
            {
                private const int RebuildThreshold = 3;

                public readonly VersionStamp Version;
                public readonly ValueSource<Compilation> Compilation;
                public readonly ImmutableDictionary<DocumentId, SyntaxTree> Trees;

                public static async Task<CompilationSet> CreateAsync(Project project, CompilationSet oldCompilationSet, CancellationToken cancellationToken)
                {
                    var compilation = await project.GetCompilationAsync(cancellationToken).ConfigureAwait(false);
                    var version = await project.GetDependentSemanticVersionAsync(cancellationToken).ConfigureAwait(false);
                    var map = GetTreeMap(project, compilation, oldCompilationSet, cancellationToken);

                    ValidateTreeMap(map, project, compilation);
                    return new CompilationSet(version, GetCompilation(project, compilation), map);
                }

                private CompilationSet(VersionStamp version, ValueSource<Compilation> compilation, ImmutableDictionary<DocumentId, SyntaxTree> map)
                {
                    this.Version = version;
                    this.Compilation = compilation;
                    this.Trees = map;
                }

                private static ImmutableDictionary<DocumentId, SyntaxTree> GetTreeMap(Project project, Compilation compilation, CompilationSet oldCompilationSet, CancellationToken cancellationToken)
                {
                    // enumerable count should take a quick path since ImmutableArray implements ICollection
                    var newTreeCount = compilation.SyntaxTrees.Count();

                    // TODO: all this could go away if this is maintained by project itself and one can just get the map from it.
                    if (oldCompilationSet == null || Math.Abs(oldCompilationSet.Trees.Count - newTreeCount) > RebuildThreshold)
                    {
                        return ImmutableDictionary.CreateRange(GetNewTreeMap(project, compilation));
                    }

                    var map = AddOrUpdateNewTreeToOldMap(project, compilation, oldCompilationSet, cancellationToken);

                    // check simple case. most of typing case should hit this.
                    // number of items in the map is same as number of new trees and old compilation doesnt have
                    // more trees than current one
                    if (map.Count == newTreeCount && oldCompilationSet.Trees.Count <= newTreeCount)
                    {
                        return map;
                    }

                    // a bit more expensive case where there is a document in oldCompilationSet that doesnt exist in new compilation
                    return RemoveOldTreeFromMap(compilation, oldCompilationSet.Trees, map, cancellationToken);
                }

                private static ImmutableDictionary<DocumentId, SyntaxTree> RemoveOldTreeFromMap(
                    Compilation newCompilation,
                    ImmutableDictionary<DocumentId, SyntaxTree> oldMap, ImmutableDictionary<DocumentId, SyntaxTree> map,
                    CancellationToken cancellationToken)
                {
                    foreach (var oldIdAndTree in oldMap)
                    {
                        cancellationToken.ThrowIfCancellationRequested();

                        // check whether new compilation still has the tree
                        if (newCompilation.ContainsSyntaxTree(oldIdAndTree.Value))
                        {
                            continue;
                        }

                        var documentId = oldIdAndTree.Key;

                        // check whether the tree has been updated
                        var currentTree = map[documentId];
                        if (currentTree != oldIdAndTree.Value)
                        {
                            continue;
                        }

                        // this has been removed
                        map = map.Remove(oldIdAndTree.Key);
                    }

                    return map;
                }

                private static ImmutableDictionary<DocumentId, SyntaxTree> AddOrUpdateNewTreeToOldMap(
                    Project newProject, Compilation newCompilation, CompilationSet oldSet, CancellationToken cancellationToken)
                {
                    Compilation oldCompilation;
                    if (!oldSet.Compilation.TryGetValue(out oldCompilation))
                    {
                        return ImmutableDictionary.CreateRange(GetNewTreeMap(newProject, newCompilation));
                    }

                    var map = oldSet.Trees;
                    foreach (var newTree in newCompilation.SyntaxTrees)
                    {
                        cancellationToken.ThrowIfCancellationRequested();

                        if (oldCompilation.ContainsSyntaxTree(newTree))
                        {
                            continue;
                        }

                        var documentId = newProject.GetDocumentId(newTree);
                        Contract.Requires(documentId != null);

                        map = map.SetItem(documentId, newTree);
                    }

                    return map;
                }

                private static IEnumerable<KeyValuePair<DocumentId, SyntaxTree>> GetNewTreeMap(Project project, Compilation compilation)
                {
                    foreach (var tree in compilation.SyntaxTrees)
                    {
                        var documentId = project.GetDocumentId(tree);
                        if (documentId != null)
                        {
                            yield return KeyValuePair.Create(documentId, tree);
                        }
                    }
                }

                private static ValueSource<Compilation> GetCompilation(Project project, Compilation compilation)
                {
                    var workspace = project.Solution.Workspace;
                    var compilationCache = WorkspaceService.GetService<ICompilationCacheService>(workspace);
                    if (compilationCache == null)
                    {
                        return new ConstantValueSource<Compilation>(compilation);
                    }

                    if (project.Solution.BranchId == workspace.PrimaryBranchId && compilationCache.Primary != null)
                    {
                        return new CachedObjectSource<Compilation>(compilation, compilationCache.Primary);
                    }

                    return new ConstantValueSource<Compilation>(compilation);
                }

                [Conditional("DEBUG")]
                private static void ValidateTreeMap(ImmutableDictionary<DocumentId, SyntaxTree> actual, Project project, Compilation compilation)
                {
                    var expected = ImmutableDictionary.CreateRange(GetNewTreeMap(project, compilation));
                    Contract.Requires(actual.SetEquals(expected));
                }
            }
        }
    }
}
