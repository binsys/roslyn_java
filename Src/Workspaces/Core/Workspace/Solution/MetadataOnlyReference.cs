﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading;
using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.WorkspaceServices;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{
    internal class MetadataOnlyReference
    {
        // version based cache
        private static readonly ConditionalWeakTable<BranchId, ConditionalWeakTable<ProjectId, MetadataOnlyReferenceSet>> cache
            = new ConditionalWeakTable<BranchId, ConditionalWeakTable<ProjectId, MetadataOnlyReferenceSet>>();

        // snapshot based cache
        private static readonly ConditionalWeakTable<Compilation, MetadataOnlyReferenceSet> snapshotCache
            = new ConditionalWeakTable<Compilation, MetadataOnlyReferenceSet>();

        private static readonly ConditionalWeakTable<BranchId, ConditionalWeakTable<ProjectId, MetadataOnlyReferenceSet>>.CreateValueCallback createReferenceSetMap =
            _ => new ConditionalWeakTable<ProjectId, MetadataOnlyReferenceSet>();

        internal static MetadataReference GetOrBuildReference(
            Solution solution,
            ProjectReference projectReference,
            Compilation finalCompilation,
            VersionStamp version,
            CancellationToken cancellationToken)
        {
            MetadataReference reference;
            if (TryGetReference(solution, projectReference, finalCompilation, version, out reference))
            {
                return reference;
            }

            // okay, we don't have one. so create one now.

            // first, prepare image
            // * NOTE * image is cancellable, do not create it inside of canditional weak table.
            var service = WorkspaceService.GetService<ITemporaryStorageService>(solution.Workspace);
            var image = MetadataOnlyImage.Create(service, finalCompilation, cancellationToken);
            if (image.IsEmpty)
            {
                // unfortunately, we couldn't create one. do best effort
                if (TryGetReference(solution, projectReference, finalCompilation, VersionStamp.Default, out reference))
                {
                    // we have one from previous compilation!!, it might be out-of-date big time, but better than nothing.
                    // re-use it
                    return reference;
                }
            }

            // okay, proceed with whatever image we have

            // now, remove existing set
            var mapFromBranch = cache.GetValue(solution.BranchId, createReferenceSetMap);
            mapFromBranch.Remove(projectReference.ProjectId);

            // create new one
            var newReferenceSet = new MetadataOnlyReferenceSet(version, image);
            var referenceSet = snapshotCache.GetValue(finalCompilation, _ => newReferenceSet);
            if (newReferenceSet != referenceSet)
            {
                // someone else has beaten us. 
                // let image go eagarly. otherwise, finalizer in temporary storage will take care of it
                image.Cleanup();

                // return new reference
                return referenceSet.GetMetadataReference(finalCompilation, projectReference.Alias, projectReference.EmbedInteropTypes);
            }

            // record it to version based cache as well. snapshot cache always has a higher priority. we don't need to check returned set here
            // since snapshot based cache will take care of same compilation for us.
            mapFromBranch.GetValue(projectReference.ProjectId, _ => referenceSet);

            // return new reference
            return referenceSet.GetMetadataReference(finalCompilation, projectReference.Alias, projectReference.EmbedInteropTypes);
        }

        internal static bool TryGetReference(
            Solution solution, ProjectReference projectReference, Compilation finalOrDeclarationCompilation, VersionStamp version, out MetadataReference reference)
        {
            // if we have one from snapshot cache, use it. it will make sure same compilation will get same metadata reference always.
            MetadataOnlyReferenceSet referenceSet;
            if (snapshotCache.TryGetValue(finalOrDeclarationCompilation, out referenceSet))
            {
                reference = referenceSet.GetMetadataReference(finalOrDeclarationCompilation, projectReference.Alias, projectReference.EmbedInteropTypes);
                return true;
            }

            // okay, now use version based cache that can live multiple compilation as long as there is no semantic changes.

            // get one for the branch
            if (TryGetReferenceFromBranch(solution.BranchId, projectReference, finalOrDeclarationCompilation, version, out reference))
            {
                return true;
            }

            // see whether we can use primary branch one
            var primaryBranchId = solution.Workspace.PrimaryBranchId;
            if (solution.BranchId != primaryBranchId &&
                TryGetReferenceFromBranch(primaryBranchId, projectReference, finalOrDeclarationCompilation, version, out reference))
            {
                return true;
            }

            // noop, we don't have any
            reference = null;
            return false;
        }

        private static bool TryGetReferenceFromBranch(
            BranchId branchId, ProjectReference projectReference, Compilation finalOrDeclarationCompilation, VersionStamp version, out MetadataReference reference)
        {
            // get map for the branch
            var mapFromBranch = cache.GetValue(branchId, createReferenceSetMap);

            // if we have one, return it
            MetadataOnlyReferenceSet referenceSet;
            if (mapFromBranch.TryGetValue(projectReference.ProjectId, out referenceSet) &&
               (version == VersionStamp.Default || referenceSet.Version == version))
            {
                // record it to snapshot based cache.
                var newReferenceSet = snapshotCache.GetValue(finalOrDeclarationCompilation, _ => referenceSet);

                reference = newReferenceSet.GetMetadataReference(finalOrDeclarationCompilation, projectReference.Alias, projectReference.EmbedInteropTypes);
                return true;
            }

            reference = null;
            return false;
        }

        private class MetadataOnlyReferenceSet
        {
            // use WeakReference so we don't keep MetadataReference's alive if they are not being consumed
            private readonly NonReentrantLock gate = new NonReentrantLock(useThisInstanceForSynchronization: true);

            // here, there is a very small chance of leaking Tuple and WeakReference, but it is so small chance,
            // I don't believe it will actually happen in real life situation. basically, for leak to happen, 
            // every image creation except the first one has to fail so that we end up re-use old reference set.
            // and the user creates many different metadata references with multiple combination of the key (tuple).
            private readonly Dictionary<Tuple<string, bool>, WeakReference<MetadataReference>> metadataReferences
                = new Dictionary<Tuple<string, bool>, WeakReference<MetadataReference>>();

            private readonly VersionStamp version;
            private readonly MetadataOnlyImage image;

            public MetadataOnlyReferenceSet(VersionStamp version, MetadataOnlyImage image)
            {
                this.version = version;
                this.image = image;
            }

            public VersionStamp Version
            {
                get
                {
                    return version;
                }
            }

            public MetadataReference GetMetadataReference(Compilation compilation, string alias, bool embedInteropTypes)
            {
                var key = Tuple.Create(alias, embedInteropTypes);

                using (gate.DisposableWait())
                {
                    WeakReference<MetadataReference> weakMetadata;
                    MetadataReference metadataReference;
                    if (!metadataReferences.TryGetValue(key, out weakMetadata) || !weakMetadata.TryGetTarget(out metadataReference))
                    {
                        // here we give out strong reference to compilation. so there is possibility that we end up making 2 compilations for same project alive.
                        // one for final compilation and one for declaration only compilation. but the final compilation will be eventually kicked out from compilation cache
                        // if there is no activity on the project. or the declaration compilation will go away if the project that depends on the reference doesn't have any
                        // activity when it is kicked out from compilation cache. if there is an activity, then both will updated as activity happens.
                        // so simply put, things will go away when compilations are kicked out from the cache or due to user activity.
                        //
                        // there is one case where we could have 2 compilations for same project alive. if a user opens a file that requires a skeleton assembly when the skeleton
                        // assembly project didn't reach the final stage yet and then the user opens another document that is part of the skeleton assembly project 
                        // and then never change it. declaration compilation will be alive by skeleton assembly and final compilation will be alive by background compiler.
                        metadataReference = this.image.CreateReference(alias, embedInteropTypes, new DeferredDocumentationProvider(compilation));
                        weakMetadata = new WeakReference<MetadataReference>(metadataReference);
                        this.metadataReferences[key] = weakMetadata;
                    }

                    return metadataReference;
                }
            }
        }
    }
}