﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using Microsoft.CodeAnalysis.Collections;
using System.IO;

namespace Microsoft.CodeAnalysis
{
    using MetadataOrDiagnostic = System.Object;

    /// <summary>
    /// The base class for language specific assembly managers.
    /// </summary>
    /// <typeparam name="TCompilation">Language specific representation for a compilation</typeparam>
    /// <typeparam name="TAssemblySymbol">Language specific representation for an assembly symbol.</typeparam>
    internal abstract partial class CommonReferenceManager<TCompilation, TAssemblySymbol>
        where TCompilation : Compilation
        where TAssemblySymbol : class, IAssemblySymbol
    {
        protected abstract CommonMessageProvider MessageProvider { get; }

        protected abstract AssemblyData CreateAssemblyDataForFile(
            PEAssembly assembly,
            WeakList<IAssemblySymbol> cachedSymbols,
            DocumentationProvider documentationProvider,
            string sourceAssemblySimpleName,
            MetadataImportOptions importOptions,
            bool embedInteropTypes);

        protected abstract AssemblyData CreateAssemblyDataForCompilation(
            CompilationReference compilationReference);

        /// <summary>
        /// Checks if the properties of <paramref name="duplicateReference"/> are compatible with properties of <paramref name="primaryReference"/>.
        /// Reports inconsistencies to the given diagnostic bag.
        /// </summary>
        /// <returns>True if the properties are compatible and hence merged, false if the duplicate reference should not merge it's properties with primary reference.</returns>
        protected abstract bool CheckPropertiesConsistency(MetadataReference primaryReference, MetadataReference duplicateReference, DiagnosticBag diagnostics);

        /// <summary>
        /// Called to compare two weakly named identities with the same name.
        /// </summary>
        protected abstract bool WeakIdentityPropertiesEquivalent(AssemblyIdentity identity1, AssemblyIdentity identity2);

        [DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
        protected struct ResolvedReference
        {
            private readonly MetadataImageKind kind;
            private readonly int index;
            private readonly ImmutableArray<string> aliases;

            public static readonly ResolvedReference Skipped = default(ResolvedReference);

            public ResolvedReference(int index, MetadataImageKind kind, ImmutableArray<string> aliases)
            {
                Debug.Assert(index >= 0);

                this.index = index + 1;
                this.kind = kind;
                this.aliases = aliases;
            }

            public ImmutableArray<string> Aliases
            {
                get
                {
                    Debug.Assert(!aliases.IsDefault);
                    return aliases;
                }
            }

            public bool IsSkipped
            {
                get
                {
                    return index == 0;
                }
            }

            public MetadataImageKind Kind
            {
                get
                {
                    Debug.Assert(!IsSkipped);
                    return kind;
                }
            }

            public int Index
            {
                get
                {
                    Debug.Assert(!IsSkipped);
                    return index - 1;
                }
            }

            private string GetDebuggerDisplay()
            {
                return IsSkipped ? "<skipped>" : (kind == MetadataImageKind.Assembly ? "A[" : "M[") + Index + "]: aliases=" + aliases.ToString();
            }
        }

        private struct ReferencedAssemblyIdentity
        {
            public readonly AssemblyIdentity Identity;
            public readonly MetadataReference MetadataReference;

            public ReferencedAssemblyIdentity(AssemblyIdentity identity, MetadataReference metadataReference)
            {
                Identity = identity;
                MetadataReference = metadataReference;
            }
        }

        /// <summary>
        /// Resolves given metadata references to assemblies and modules.
        /// </summary>
        /// <param name="compilation">The compilation whose references are being resolved.</param>
        /// <param name="references">List where to store resolved references. References from #r directives will follow references passed to the compilation constructor.</param>
        /// <param name="boundReferenceDirectiveMap">Maps #r values to successuflly resolved metadata references. Does not contain values that failed to resolve.</param>
        /// <param name="boundReferenceDirectives">Unique metadata references resolved from #r directives.</param>
        /// <param name="assemblies">List where to store information about resolved assemblies to.</param>
        /// <param name="modules">List where to store information about resolved modules to.</param>
        /// <param name="diagnostics">Diagnostic bag where to report resolution errors.</param>
        /// <returns>
        /// Maps index to <paramref name="references"/> to an index of a resolved assembly or module in <paramref name="assemblies"/> or <paramref name="modules"/>, respectively.
        ///</returns>
        protected ImmutableArray<ResolvedReference> ResolveMetadataReferences(
            TCompilation compilation,
            out ImmutableArray<MetadataReference> references,
            out IDictionary<string, MetadataReference> boundReferenceDirectiveMap,
            out ImmutableArray<MetadataReference> boundReferenceDirectives,
            out ImmutableArray<AssemblyData> assemblies,
            out ImmutableArray<PEModule> modules,
            DiagnosticBag diagnostics)
        {
            // Locations of all #r directives in the order they are listed in the references list.
            ImmutableArray<Location> referenceDirectiveLocations;
            GetCompilationReferences(compilation, diagnostics, out references, out boundReferenceDirectiveMap, out referenceDirectiveLocations);

            // References originating from #r directives precede references supplied as arguments of the compilation.
            int referenceCount = references.Length;
            int referenceDirectiveCount = (referenceDirectiveLocations != null ? referenceDirectiveLocations.Length : 0);
            int externalReferenceCount = referenceCount - referenceDirectiveCount;

            var referenceMap = new ResolvedReference[referenceCount];

            // Maps references that were added to the reference set (i.e. not filtered out as duplicates) to a set of names that 
            // can be used to alias these references. Duplicate assemblies contribute their aliases into this set.
            Dictionary<MetadataReference, ArrayBuilder<string>> aliasMap = null;

            // Used to filter out duplicate references that reference the same file (resolve to the same full normalized path).
            var boundReferences = new Dictionary<MetadataReference, MetadataReference>(MetadataReferenceEqualityComparer.Instance);

            // Used to filter out assemblies that have the same strong or weak identity.
            // Maps simple name to a list of full names.
            Dictionary<string, List<ReferencedAssemblyIdentity>> assemblyReferencesBySimpleName = null;

            ArrayBuilder<MetadataReference> uniqueDirectiveReferences = (referenceDirectiveLocations != null) ? ArrayBuilder<MetadataReference>.GetInstance() : null;
            ArrayBuilder<AssemblyData> assembliesBuilder = null;
            ArrayBuilder<PEModule> modulesBuilder = null;

            // When duplicate references with conflicting EmbedInteropTypes flag are encountered,
            // VB uses the flag from the last one, C# reports an error. We need to enumerate in reverse order
            // so that we find the one that matters first.
            for (int referenceIndex = referenceCount - 1; referenceIndex >= 0; referenceIndex--)
            {
                var boundReference = references[referenceIndex];
                if (boundReference == null)
                {
                    continue;
                }

                // add bound reference if it doesn't exist yet, merging aliases:
                MetadataReference existingReference;
                if (boundReferences.TryGetValue(boundReference, out existingReference))
                {
                    if (CheckPropertiesConsistency(boundReference, existingReference, diagnostics))
                    {
                        AddAlias(existingReference, boundReference.Properties.Alias, ref aliasMap);
                    }

                    continue;
                }

                boundReferences.Add(boundReference, boundReference);

                Location location;
                if (referenceIndex < referenceDirectiveCount)
                {
                    location = referenceDirectiveLocations[referenceIndex];
                    uniqueDirectiveReferences.Add(boundReference);
                }
                else
                {
                    location = Location.None;
                }

                // compilation reference

                var compilationReference = boundReference as CompilationReference;
                if (compilationReference != null)
                {
                    switch (compilationReference.Properties.Kind)
                    {
                        case MetadataImageKind.Assembly:
                            existingReference = TryAddAssembly(
                                compilationReference.Compilation.Assembly.Identity,
                                boundReference,
                                diagnostics,
                                location,
                                ref assemblyReferencesBySimpleName);

                            if (existingReference != null)
                            {
                                AddAlias(existingReference, boundReference.Properties.Alias, ref aliasMap);
                                continue;
                            }

                            // Note, if SourceAssemblySymbol hasn't been created for 
                            // compilationAssembly.Compilation yet, we want this to happen 
                            // right now. Conveniently, this constructor will trigger creation of the 
                            // SourceAssemblySymbol.
                            var asmData = CreateAssemblyDataForCompilation(compilationReference);
                            AddAssembly(asmData, referenceIndex, referenceMap, ref assembliesBuilder);
                            break;

                        default:
                            throw ExceptionUtilities.UnexpectedValue(compilationReference.Properties.Kind);
                    }

                    continue;
                }

                // PE reference

                var peReference = (PortableExecutableReference)boundReference;
                Metadata metadata = GetMetadata(peReference, MessageProvider, location, diagnostics);
                Debug.Assert(metadata != null || diagnostics.HasAnyErrors());

                if (metadata != null)
                {
                    Debug.Assert(metadata != null);

                    switch (peReference.Properties.Kind)
                    {
                        case MetadataImageKind.Assembly:
                            var assemblyMetadata = (AssemblyMetadata)metadata;
                            WeakList<IAssemblySymbol> cachedSymbols = assemblyMetadata.CachedSymbols;

                            if (assemblyMetadata.IsValidAssembly())
                            {
                                PEAssembly assembly = assemblyMetadata.Assembly;
                                existingReference = TryAddAssembly(
                                    assembly.Identity,
                                    peReference,
                                    diagnostics,
                                    location,
                                    ref assemblyReferencesBySimpleName);

                                if (existingReference != null)
                                {
                                    AddAlias(existingReference, boundReference.Properties.Alias, ref aliasMap);
                                    continue;
                                }

                                var asmData = CreateAssemblyDataForFile(
                                    assembly,
                                    cachedSymbols,
                                    peReference.DocumentationProvider,
                                    SimpleAssemblyName,
                                    compilation.Options.MetadataImportOptions,
                                    peReference.Properties.EmbedInteropTypes);

                                AddAssembly(asmData, referenceIndex, referenceMap, ref assembliesBuilder);
                            }
                            else
                            {
                                diagnostics.Add(MessageProvider.CreateDiagnostic(MessageProvider.ERR_MetadataFileNotAssembly, location, peReference.Display));
                            }

                            // asmData keeps strong ref after this point
                            GC.KeepAlive(assemblyMetadata);
                            break;

                        case MetadataImageKind.Module:
                            var moduleMetadata = (ModuleMetadata)metadata;
                            if (moduleMetadata.Module.IsLinkedModule)
                            {
                                // We don't support netmodules since some checks in the compiler need information from the full PE image
                                // (Machine, Bit32Required, PE image hash).
                                if (!moduleMetadata.Module.IsEntireImageAvailable)
                                {
                                    diagnostics.Add(MessageProvider.CreateDiagnostic(MessageProvider.ERR_LinkedNetmoduleMetadataMustProvideFullPEImage, location, peReference.Display));
                                }

                                AddModule(moduleMetadata.Module, referenceIndex, referenceMap, ref modulesBuilder);
                            }
                            else
                            {
                                diagnostics.Add(MessageProvider.CreateDiagnostic(MessageProvider.ERR_MetadataFileNotModule, location, peReference.Display));
                            }
                            break;

                        default:
                            throw ExceptionUtilities.UnexpectedValue(peReference.Properties.Kind);
                    }
                }
            }

            if (uniqueDirectiveReferences != null)
            {
                uniqueDirectiveReferences.ReverseContents();
                boundReferenceDirectives = uniqueDirectiveReferences.ToImmutableAndFree();
            }
            else
            {
                boundReferenceDirectives = ImmutableArray<MetadataReference>.Empty;
            }

            for (int i = 0; i < referenceMap.Length; i++)
            {
                if (!referenceMap[i].IsSkipped)
                {
                    int count = referenceMap[i].Kind == MetadataImageKind.Assembly
                        ? ((object)assembliesBuilder == null ? 0 : assembliesBuilder.Count)
                        : ((object)modulesBuilder == null ? 0 : modulesBuilder.Count);
                    int reversedIndex = count - 1 - referenceMap[i].Index;
                    referenceMap[i] = new ResolvedReference(reversedIndex, referenceMap[i].Kind, GetAliases(references[i], aliasMap));
                }
            }

            if (assembliesBuilder == null)
            {
                assemblies = ImmutableArray<AssemblyData>.Empty;
            }
            else
            {
                assembliesBuilder.ReverseContents();
                assemblies = assembliesBuilder.ToImmutableAndFree();
            }

            if (modulesBuilder == null)
            {
                modules = ImmutableArray<PEModule>.Empty;
            }
            else
            {
                modulesBuilder.ReverseContents();
                modules = modulesBuilder.ToImmutableAndFree();
            }

            return ImmutableArray.CreateRange(referenceMap);
        }

        private static ImmutableArray<string> GetAliases(MetadataReference reference, Dictionary<MetadataReference, ArrayBuilder<string>> aliasMap)
        {
            ArrayBuilder<string> aliases;
            if (aliasMap != null && aliasMap.TryGetValue(reference, out aliases))
            {
                return aliases.ToImmutableAndFree();
            }

            if (reference.Properties.Alias != null)
            {
                return ImmutableArray.Create(reference.Properties.Alias);
            }

            return ImmutableArray<string>.Empty;
        }

        /// <summary>
        /// Creates or gets metadata for PE reference.
        /// </summary>
        /// <remarks>
        /// If any of the following exceptions: <see cref="BadImageFormatException"/>, <see cref="FileNotFoundException"/>, <see cref="IOException"/>,
        /// are thrown while reading the metadata file, the exception is caught and an appropriate diagnostic stored in <paramref name="diagnostics"/>.
        /// </remarks>
        private Metadata GetMetadata(PortableExecutableReference peReference, CommonMessageProvider messageProvider, Location location, DiagnosticBag diagnostics)
        {
            Metadata existingMetadata;

            lock (ObservedMetadata)
            {
                if (TryGetObservedMetadata(peReference, diagnostics, out existingMetadata))
                {
                    return existingMetadata;
                }
            }

            Metadata newMetadata;
            Diagnostic newDiagnostic;
            PortableExecutableReference.TryGetMetadata(
                 peReference.Properties.Kind == MetadataImageKind.Assembly,
                 messageProvider,
                 location,
                 peReference.Display,
                 peReference.GetMetadata,
                 out newMetadata,
                 out newDiagnostic);

            lock (ObservedMetadata)
            {
                if (TryGetObservedMetadata(peReference, diagnostics, out existingMetadata))
                {
                    return existingMetadata;
                }

                if (newDiagnostic != null)
                {
                    diagnostics.Add(newDiagnostic);
                }

                ObservedMetadata.Add(peReference, (MetadataOrDiagnostic)newMetadata ?? newDiagnostic);
                return newMetadata;
            }
        }

        private bool TryGetObservedMetadata(PortableExecutableReference peReference, DiagnosticBag diagnostics, out Metadata metadata)
        {
            MetadataOrDiagnostic existing;
            if (ObservedMetadata.TryGetValue(peReference, out existing))
            {
                Debug.Assert(existing is Metadata || existing is Diagnostic);

                metadata = existing as Metadata;
                if (metadata == null)
                {
                    diagnostics.Add((Diagnostic)existing);
                }

                return true;
            }

            metadata = null;
            return false;
        }

        /// <summary>
        /// Decides whether 2 references are interchangeable when used in the same compilation.
        /// PE references are interchangeable if they have the same non-null full path, compilation references if they refer to the same compilation.
        /// </summary>
        internal sealed class MetadataReferenceEqualityComparer : IEqualityComparer<MetadataReference>
        {
            internal static readonly MetadataReferenceEqualityComparer Instance = new MetadataReferenceEqualityComparer();

            public bool Equals(MetadataReference x, MetadataReference y)
            {
                if (ReferenceEquals(x, y))
                {
                    return true;
                }

                var px = x as PortableExecutableReference;
                if (px != null)
                {
                    var py = y as PortableExecutableReference;
                    if (py == null)
                    {
                        return false;
                    }

                    return px.FullPath != null && py.FullPath != null && string.Equals(px.FullPath, py.FullPath, StringComparison.OrdinalIgnoreCase);
                }

                var cy = y as CompilationReference;
                return cy != null && ((CompilationReference)x).Compilation == cy.Compilation;
            }

            public int GetHashCode(MetadataReference reference)
            {
                var compilationReference = reference as CompilationReference;
                if (compilationReference != null)
                {
                    return compilationReference.Compilation.GetHashCode();
                }

                var peReference = (PortableExecutableReference)reference;
                if (peReference.FullPath != null)
                {
                    return StringComparer.OrdinalIgnoreCase.GetHashCode(peReference.FullPath);
                }

                return RuntimeHelpers.GetHashCode(reference);
            }
        }

        private static void AddAlias(MetadataReference primaryReference, string newAlias, ref Dictionary<MetadataReference, ArrayBuilder<string>> aliasMap)
        {
            if (aliasMap == null)
            {
                aliasMap = new Dictionary<MetadataReference, ArrayBuilder<string>>();
            }

            ArrayBuilder<string> aliases;
            if (!aliasMap.TryGetValue(primaryReference, out aliases))
            {
                aliases = ArrayBuilder<string>.GetInstance();
                aliasMap.Add(primaryReference, aliases);

                // even if the alias is null we want to add it since C# then allows to use symbols from the assembly unqualified:
                aliases.Add(primaryReference.Properties.Alias);
            }

            // we could avoid duplicates but there is no need to do so:
            aliases.Add(newAlias);
        }

        /// <remarks>
        /// Caller is responsible for freeing any allocated ArrayBuilders.
        /// </remarks>
        private static void AddAssembly(AssemblyData data, int referenceIndex, ResolvedReference[] referenceMap, ref ArrayBuilder<AssemblyData> assemblies)
        {
            if (assemblies == null)
            {
                assemblies = ArrayBuilder<AssemblyData>.GetInstance();
            }

            referenceMap[referenceIndex] = new ResolvedReference(assemblies.Count, MetadataImageKind.Assembly, default(ImmutableArray<string>));
            assemblies.Add(data);
        }

        /// <remarks>
        /// Caller is responsible for freeing any allocated ArrayBuilders.
        /// </remarks>
        private static void AddModule(PEModule module, int referenceIndex, ResolvedReference[] referenceMap, ref ArrayBuilder<PEModule> modules)
        {
            if (modules == null)
            {
                modules = ArrayBuilder<PEModule>.GetInstance();
            }

            referenceMap[referenceIndex] = new ResolvedReference(modules.Count, MetadataImageKind.Module, default(ImmutableArray<string>));
            modules.Add(module);
        }

        // Returns null if an assembly of an equivalent identity has not been added previously, otherwise returns the reference that added it.
        // - Both assembly names are strong (have keys) and are either equal or FX unified 
        // - Both assembly names are weak (no keys) and have the same simple name.
        private MetadataReference TryAddAssembly(AssemblyIdentity identity, MetadataReference boundReference, DiagnosticBag diagnostics, Location location,
            ref Dictionary<string, List<ReferencedAssemblyIdentity>> referencesBySimpleName)
        {
            if (referencesBySimpleName == null)
            {
                referencesBySimpleName = new Dictionary<string, List<ReferencedAssemblyIdentity>>(StringComparer.OrdinalIgnoreCase);
            }

            List<ReferencedAssemblyIdentity> sameSimpleNameIdentities;
            string simpleName = identity.Name;
            if (!referencesBySimpleName.TryGetValue(simpleName, out sameSimpleNameIdentities))
            {
                referencesBySimpleName.Add(simpleName, new List<ReferencedAssemblyIdentity> { new ReferencedAssemblyIdentity(identity, boundReference) });
                return null;
            }

            ReferencedAssemblyIdentity equivalent = default(ReferencedAssemblyIdentity);
            if (identity.IsStrongName)
            {
                foreach (var other in sameSimpleNameIdentities)
                {
                    // only compare strong with strong (weak is never equivalent to strong and vice versa)
                    if (other.Identity.IsStrongName && IdentityComparer.ReferenceMatchesDefinition(identity, other.Identity))
                    {
                        equivalent = other;
                        break;
                    }
                }
            }
            else
            {
                foreach (var other in sameSimpleNameIdentities)
                {
                    // only compare weak with weak
                    if (!other.Identity.IsStrongName && WeakIdentityPropertiesEquivalent(identity, other.Identity))
                    {
                        equivalent = other;
                        break;
                    }
                }
            }

            if (equivalent.Identity == null)
            {
                sameSimpleNameIdentities.Add(new ReferencedAssemblyIdentity(identity, boundReference));
                return null;
            }

            // equivalent found - ignore and/or report an error:

            if (identity.IsStrongName)
            {
                Debug.Assert(equivalent.Identity.IsStrongName);

                // versions migth have been unified for a Framework assembly:
                if (identity != equivalent.Identity)
                {
                    // Dev11 C# reports an error
                    // Dev11 VB keeps both references in the compilation and reports an ambiguity error when a symbol is used.
                    // BREAKING CHANGE in VB: we report an error for both languages

                    // Multiple assemblies with equivalent identity have been imported: '{0}' and '{1}'. Remove one of the duplicate references.
                    MessageProvider.ReportDuplicateMetadataReferenceStrong(diagnostics, location, boundReference, identity, equivalent.MetadataReference, equivalent.Identity);
                }

                // If the versions match exactly we ignore duplicates w/o reporting errors while 
                // Dev11 C# reports:
                //   error CS1703: An assembly with the same identity '{0}' has already been imported. Try removing one of the duplicate references.
                // Dev11 VB reports:
                //   Fatal error BC2000 : compiler initialization failed unexpectedly: Project already has a reference to assembly System. 
                //   A second reference to 'D:\Temp\System.dll' cannot be added.
            }
            else
            {
                MessageProvider.ReportDuplicateMetadataReferenceWeak(diagnostics, location, boundReference, identity, equivalent.MetadataReference, equivalent.Identity);
            }

            Debug.Assert(equivalent.MetadataReference != null);
            return equivalent.MetadataReference;
        }

        protected void GetCompilationReferences(
            TCompilation compilation,
            DiagnosticBag diagnostics,
            out ImmutableArray<MetadataReference> references,
            out IDictionary<string, MetadataReference> boundReferenceDirectives,
            out ImmutableArray<Location> referenceDirectiveLocations)
        {
            boundReferenceDirectives = null;

            ArrayBuilder<MetadataReference> referencesBuilder = ArrayBuilder<MetadataReference>.GetInstance();
            ArrayBuilder<Location> referenceDirectiveLocationsBuilder = null;

            try
            {
                foreach (var referenceDirective in compilation.ReferenceDirectives)
                {
                    // we already successfully bound #r with the same value:
                    if (boundReferenceDirectives != null && boundReferenceDirectives.ContainsKey(referenceDirective.File))
                    {
                        continue;
                    }

                    MetadataReference boundReference = ResolveReferenceDirective(referenceDirective.File, referenceDirective.Location, compilation);
                    if (boundReference == null)
                    {
                        diagnostics.Add(MessageProvider.CreateDiagnostic(MessageProvider.ERR_MetadataFileNotFound, referenceDirective.Location, referenceDirective.File));
                        continue;
                    }

                    if (boundReferenceDirectives == null)
                    {
                        boundReferenceDirectives = new Dictionary<string, MetadataReference>();
                        referenceDirectiveLocationsBuilder = ArrayBuilder<Location>.GetInstance();
                    }

                    referencesBuilder.Add(boundReference);
                    referenceDirectiveLocationsBuilder.Add(referenceDirective.Location);
                    boundReferenceDirectives.Add(referenceDirective.File, boundReference);
                }

                // Workaround for bug #531342: include facades if we reference a Portable Library:
                referencesBuilder.AddRange(ResolveFacadeReferences(compilation));

                // add external reference at the end, so that they are processed first:
                referencesBuilder.AddRange(compilation.ExternalReferences);

                if (boundReferenceDirectives == null)
                {
                    // no directive references resolved successfully:
                    boundReferenceDirectives = SpecializedCollections.EmptyDictionary<string, MetadataReference>();
                }

                references = referencesBuilder.ToImmutable();
                referenceDirectiveLocations = referenceDirectiveLocationsBuilder == null ? ImmutableArray<Location>.Empty : referenceDirectiveLocationsBuilder.ToImmutableAndFree();
            }
            finally
            {
                // Put this in a finally because we have tests that (intentionally) cause ResolveReferenceDirective to throw and 
                // we don't want to clutter the test output with leak reports.
                referencesBuilder.Free();
            }
        }

        // TODO: remove
        #region Workaround for bug #531342

        private IEnumerable<MetadataReference> ResolveFacadeReferences(TCompilation compilation)
        {
            bool referencesPortableLibrary = false;
            string mscorlibPath = null;

            foreach (var reference in compilation.ExternalReferences)
            {
                if (reference.Properties.Kind != MetadataImageKind.Assembly)
                {
                    continue;
                }

                if (DetectPortableDependency(reference as CompilationReference, ref referencesPortableLibrary) ||
                    DetectPortableDependency(reference as PortableExecutableReference, ref mscorlibPath, ref referencesPortableLibrary))
                {
                    return SpecializedCollections.EmptyEnumerable<MetadataReference>();
                }
            }

            if (!referencesPortableLibrary || mscorlibPath == null)
            {
                return SpecializedCollections.EmptyEnumerable<MetadataReference>();
            }

            return GetFacadeReferences(mscorlibPath);
        }

        private bool DetectPortableDependency(CompilationReference compilationReference, ref bool referencesPortableLibrary)
        {
            if (compilationReference == null)
            {
                return false;
            }

            if (referencesPortableLibrary)
            {
                return false;
            }

            var data = CreateAssemblyDataForCompilation(compilationReference);
            referencesPortableLibrary |= HasSystemRuntime(data.AssemblyReferences);
            return false;
        }

        private bool DetectPortableDependency(PortableExecutableReference peReference, ref string mscorlibPath, ref bool referencesPortableLibrary)
        {
            if (peReference == null)
            {
                return false;
            }

            DiagnosticBag diagnostics = DiagnosticBag.GetInstance();
            Metadata metadata = GetMetadata(peReference, MessageProvider, NoLocation.Singleton, diagnostics);

            // ignore diagnostics, we will report diagnostics again when reading the metadata later:
            diagnostics.Free();

            if (metadata == null)
            {
                return false;
            }

            var assemblyMetadata = (AssemblyMetadata)metadata;
            if (!assemblyMetadata.IsValidAssembly())
            {
                return false;
            }

            PEAssembly assembly = assemblyMetadata.Assembly;
            if (IsSystemRuntime(assembly.Identity))
            {
                return true;
            }

            if (IsMscorlib(assembly.Identity))
            {
                mscorlibPath = peReference.FullPath;
                return false;
            }

            if (referencesPortableLibrary)
            {
                return false;
            }

            referencesPortableLibrary |= HasSystemRuntime(assembly.AssemblyReferences);
            return false;
        }

        private static bool HasSystemRuntime(ImmutableArray<AssemblyIdentity> identities)
        {
            foreach (AssemblyIdentity identity in identities)
            {
                if (IsSystemRuntime(identity))
                {
                    return true;
                }
            }

            return false;
        }

        private static IEnumerable<MetadataReference> GetFacadeReferences(string mscorlibPath)
        {
            string facadesDirectory = PathUtilities.CombineAbsoluteAndRelativePaths(PathUtilities.GetDirectoryName(mscorlibPath), @"Facades");
            string[] files;

            if (!Directory.Exists(facadesDirectory))
            {
                yield break;
            }

            try
            {
                files = Directory.GetFiles(facadesDirectory, "*.dll", SearchOption.TopDirectoryOnly);
            }
            catch
            {
                yield break;
            }

            foreach (string file in files)
            {
                yield return new MetadataFileReference(file, MetadataReferenceProperties.Assembly);
            }
        }

        private static bool IsMscorlib(AssemblyIdentity identity)
        {
            return string.Equals(identity.Name, "mscorlib", StringComparison.Ordinal) && ByteSequenceComparer.Instance.Equals(identity.PublicKeyToken, MscorlibPublicKeyToken);
        }

        private static bool IsSystemRuntime(AssemblyIdentity identity)
        {
            return string.Equals(identity.Name, "System.Runtime", StringComparison.Ordinal) && ByteSequenceComparer.Instance.Equals(identity.PublicKeyToken, SystemRuntimePublicKeyToken);
        }

        private static readonly ImmutableArray<byte> SystemRuntimePublicKeyToken =
            ImmutableArray.Create(new byte[] { 0xb0, 0x3f, 0x5f, 0x7f, 0x11, 0xd5, 0x0a, 0x3a });

        private static readonly ImmutableArray<byte> MscorlibPublicKeyToken =
            ImmutableArray.Create(new byte[] { 0xb7, 0x7a, 0x5c, 0x56, 0x19, 0x34, 0xe0, 0x89 });

        #endregion

        /// <summary>
        /// For each given directive return a bound PE reference, or null if the binding fails.
        /// </summary>
        private PortableExecutableReference ResolveReferenceDirective(string reference, Location location, TCompilation compilation)
        {
            var tree = location.SourceTree;
            string basePath = (tree != null && tree.FilePath.Length > 0) ? tree.FilePath : null;

            string fullPath = compilation.Options.FileResolver.ResolveMetadataReference(reference, basePath);
            if (fullPath == null)
            {
                return null;
            }

            return compilation.Options.MetadataReferenceProvider.GetReference(fullPath);
        }

        internal static AssemblyReferenceBinding[] ResolveReferencedAssemblies(
            ImmutableArray<AssemblyIdentity> references,
            ImmutableArray<AssemblyData> definitions,
            AssemblyIdentityComparer assemblyIdentityComparer,
            bool okToResolveAgainstCompilationBeingCreated)
        {
            var boundReferences = new AssemblyReferenceBinding[references.Length];
            for (int j = 0; j < references.Length; j++)
            {
                boundReferences[j] = ResolveReferencedAssembly(references[j], definitions, assemblyIdentityComparer, okToResolveAgainstCompilationBeingCreated);
            }

            return boundReferences;
        }

        /// <summary>
        /// Used to match AssemblyRef with AssemblyDef.
        /// </summary>
        /// <param name="definitions">Array of definition identities to match against.</param>
        /// <param name="reference">Reference identity to resolve.</param>
        /// <param name="assemblyIdentityComparer">Assembly identity comparer.</param>
        /// <param name="okToResolveAgainstCompilationBeingCreated"> Is it Ok to resolve reference against the compilation we are creating?</param>
        /// <returns>
        /// Returns an index the reference is bound.
        /// </returns>
        internal static AssemblyReferenceBinding ResolveReferencedAssembly(
            AssemblyIdentity reference,
            ImmutableArray<AssemblyData> definitions,
            AssemblyIdentityComparer assemblyIdentityComparer,
            bool okToResolveAgainstCompilationBeingCreated)
        {
            // Dev11 C# compiler allows the versions to not match exactly, assuming that a newer library may be used instead of an older version.
            // For a given reference it finds a definition with the lowest version that is higher then or equal to the reference version.
            // If match.Version != reference.Version a warning is reported.

            // definition with the lowest version higher than reference version, unless exact version found
            int minHigherVersionDefinition = -1;
            int maxLowerVersionDefinition = -1;

            // NB: Start at 1, since we checked 0 above.
            for (int i = 1; i < definitions.Length; i++)
            {
                AssemblyIdentity definition = definitions[i].Identity;

                switch (assemblyIdentityComparer.Compare(reference, definition))
                {
                    case AssemblyIdentityComparer.ComparisonResult.NotEquivalent:
                        continue;

                    case AssemblyIdentityComparer.ComparisonResult.Equivalent:
                        return new AssemblyReferenceBinding(reference, i);

                    case AssemblyIdentityComparer.ComparisonResult.EquivalentIgnoringVersion:
                        if (reference.Version < definition.Version)
                        {
                            // Refers to an older assembly than we have
                            if (minHigherVersionDefinition == -1 || definition.Version < definitions[minHigherVersionDefinition].Identity.Version)
                            {
                                minHigherVersionDefinition = i;
                            }
                        }
                        else
                        {
                            Debug.Assert(reference.Version > definition.Version);

                            // Refers to a newer assembly than we have
                            if (maxLowerVersionDefinition == -1 || definition.Version > definitions[maxLowerVersionDefinition].Identity.Version)
                            {
                                maxLowerVersionDefinition = i;
                            }
                        }

                        continue;
                }
            }

            // we haven't found definition that matches the reference

            if (minHigherVersionDefinition != -1)
            {
                return new AssemblyReferenceBinding(reference, minHigherVersionDefinition, versionDifference: +1);
            }

            if (maxLowerVersionDefinition != -1)
            {
                return new AssemblyReferenceBinding(reference, maxLowerVersionDefinition, versionDifference: -1);
            }

            // As in the native compiler (see IMPORTER::MapAssemblyRefToAid), we compare against the
            // compilation (i.e. source) assembly as a last resort.  We follow the native approach of
            // skipping the public key comparison since we have yet to compute it.
            if (okToResolveAgainstCompilationBeingCreated &&
                AssemblyIdentityComparer.SimpleNameComparer.Equals(reference.Name, definitions[0].Identity.Name))
            {
                Debug.Assert(definitions[0].Identity.PublicKeyToken.IsEmpty);
                return new AssemblyReferenceBinding(reference, 0);
            }

            return new AssemblyReferenceBinding(reference);
        }
    }
}
