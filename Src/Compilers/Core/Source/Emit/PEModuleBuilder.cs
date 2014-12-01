// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Immutable;
using Microsoft.CodeAnalysis.CodeGen;
using Roslyn.Utilities;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading;
using System.IO;
using System.Security;

namespace Microsoft.CodeAnalysis
{
    internal abstract class CommonPEModuleBuilder
    {
        internal abstract Cci.IAssemblyReference Translate(IAssemblySymbol symbol, DiagnosticBag diagnostics);
        internal abstract Cci.ITypeReference Translate(ITypeSymbol symbol, SyntaxNode syntaxOpt, DiagnosticBag diagnostics);
        internal abstract Cci.IMethodReference EntryPoint { get; }
        internal virtual Microsoft.CodeAnalysis.Emit.EmitBaseline PreviousGeneration { get { return null; } }
        internal virtual Microsoft.CodeAnalysis.Emit.DefinitionMap PreviousDefinitions { get { return null; } }
        internal virtual Microsoft.CodeAnalysis.Emit.SymbolChanges Changes { get { return null; } }
        internal abstract bool SupportsPrivateImplClass { get; }
        internal virtual void OnCreatedIndices(DiagnosticBag diagnostics) { }
        internal abstract IEnumerable<Cci.INamespaceTypeDefinition> GetAnonymousTypes();
        internal abstract IReadOnlyDictionary<Microsoft.CodeAnalysis.Emit.AnonymousTypeKey, Microsoft.CodeAnalysis.Emit.AnonymousTypeValue> GetAnonymousTypeMap();
        internal abstract Compilation CommonCompilation { get; }
    }

    /// <summary>
    /// Common base class for C# and VB PE module builder.
    /// </summary>
    internal abstract class PEModuleBuilder<TCompilation, TSymbol, TSourceModuleSymbol, TModuleSymbol, TAssemblySymbol, TNamespaceSymbol, TTypeSymbol, TNamedTypeSymbol, TMethodSymbol, TSyntaxNode, TEmbeddedTypesManager> : CommonPEModuleBuilder, Cci.IModule, ITokenDeferral
        where TCompilation : Compilation
        where TSymbol : class
        where TSourceModuleSymbol : class, TModuleSymbol
        where TModuleSymbol : class, TSymbol
        where TAssemblySymbol : class, TSymbol
        where TNamespaceSymbol : class, TSymbol
        where TTypeSymbol : class, TSymbol
        where TNamedTypeSymbol : class, TTypeSymbol, Cci.INamespaceTypeDefinition
        where TMethodSymbol : class, TSymbol, Cci.IMethodDefinition
        where TSyntaxNode : SyntaxNode
        where TEmbeddedTypesManager : Emit.NoPia.CommonEmbeddedTypesManager
    {
        private readonly Microsoft.Cci.RootModuleType rootModuleType = new Microsoft.Cci.RootModuleType();

        private readonly TSourceModuleSymbol sourceModule;
        private readonly TCompilation compilation;
        private readonly OutputKind outputKind;
        private readonly ModulePropertiesForSerialization serializationProperties;
        private readonly ConcurrentCache<ValueTuple<string, string>, string> normalizedPathsCache = new ConcurrentCache<ValueTuple<string, string>, string>(16);

        /// <summary>
        /// Used to translate assembly symbols to assembly references in scenarios when the physical assemblies 
        /// being emitted don't correspond to the assembly symbols 1:1. This happens, for example, in interactive sessions where
        /// multiple code submissions might be compiled into a single dynamic assembly or into multiple assemblies 
        /// depending on properties of the code being emitted. If null we map assembly symbol exactly to its assembly name.
        /// </summary>
        protected readonly Func<TAssemblySymbol, AssemblyIdentity> assemblySymbolMapper;

        private readonly TokenMap<Cci.IReference> referencesInILMap = new TokenMap<Cci.IReference>();
        private readonly StringTokenMap stringsInILMap = new StringTokenMap();
        private readonly ConcurrentDictionary<TMethodSymbol, Cci.IMethodBody> methodBodyMap =
            new ConcurrentDictionary<TMethodSymbol, Cci.IMethodBody>(ReferenceEqualityComparer.Instance);

        private TMethodSymbol entryPoint;
        private PrivateImplementationDetails compilerGeneratedPrivateImpl;
        private ArrayMethods lazyArrayMethods;
        private HashSet<string> namesOfTopLevelTypes;

        internal IEnumerable<Cci.IWin32Resource> Win32Resources { set; private get; }
        internal Cci.ResourceSection Win32ResourceSection { set; private get; }
        protected readonly IEnumerable<ResourceDescription> ManifestResources;

        // this is a map from the document "name" to the document.
        // document "name" is typically a file path like "C:\Abc\Def.cs" however that is not guaranteed.
        // For compatibility reasons the names treated as case-sensitive in C# and case-insensitive in VB 
        // Both languages do not trim the names, so they are both sensitive to the leading and trailing whitespaces.
        // NOTE: we are not considering how filesystem or debugger do the comparisons, but how native implementations did
        //       deviating from that may result in unexpected warings or different behavior (possibly without warnings).
        private readonly ConcurrentDictionary<string, Cci.DebugSourceDocument> debugDocuments;

        public abstract TEmbeddedTypesManager EmbeddedTypesManagerOpt { get; }

        protected PEModuleBuilder(
            TCompilation compilation,
            TSourceModuleSymbol sourceModule,
            ModulePropertiesForSerialization serializationProperties,
            IEnumerable<ResourceDescription> manifestResources,
            OutputKind outputKind,
            Func<TAssemblySymbol, AssemblyIdentity> assemblySymbolMapper)
        {
            Debug.Assert(sourceModule != null);
            Debug.Assert(serializationProperties != null);

            this.compilation = compilation;
            this.sourceModule = sourceModule;
            this.serializationProperties = serializationProperties;
            this.ManifestResources = manifestResources;
            this.outputKind = outputKind;
            this.assemblySymbolMapper = assemblySymbolMapper;

            if (compilation.IsCaseSensitive)
            {
                this.debugDocuments = new ConcurrentDictionary<string, Cci.DebugSourceDocument>(StringComparer.Ordinal);
            }
            else
            {
                this.debugDocuments = new ConcurrentDictionary<string, Cci.DebugSourceDocument>(StringComparer.OrdinalIgnoreCase);
            }
        }

        internal abstract string ModuleName { get; }
        internal abstract string Name { get; }
        internal abstract TAssemblySymbol CorLibrary { get; }

        internal abstract byte LinkerMajorVersion { get; }
        internal abstract byte LinkerMinorVersion { get; }

        internal abstract IEnumerable<Cci.ICustomAttribute> GetSourceAssemblyAttributes();
        internal abstract IEnumerable<Cci.SecurityAttribute> GetSourceAssemblySecurityAttributes();
        internal abstract IEnumerable<Cci.ICustomAttribute> GetSourceModuleAttributes();
        internal abstract Cci.ICustomAttribute SynthesizeAttribute(WellKnownMember attributeConstructor);

        internal abstract Cci.INamedTypeReference GetSystemType(TSyntaxNode syntaxOpt, DiagnosticBag diagnostics);
        internal abstract Cci.INamedTypeReference GetSpecialType(SpecialType specialType, TSyntaxNode syntaxNodeOpt, DiagnosticBag diagnostics);

        protected bool HaveDeterminedTopLevelTypes
        {
            get { return this.namesOfTopLevelTypes != null; }
        }

        protected bool ContainsTopLevelType(string fullEmittedName)
        {
            return this.namesOfTopLevelTypes.Contains(fullEmittedName);
        }

        internal abstract IEnumerable<Cci.INamespaceTypeDefinition> GetTopLevelTypesCore(Microsoft.CodeAnalysis.Emit.Context context);

        private IEnumerable<Cci.INamespaceTypeDefinition> GetTopLevelTypes(Microsoft.CodeAnalysis.Emit.Context context)
        {
            Microsoft.Cci.NoPiaReferenceIndexer noPiaIndexer = null;
            HashSet<string> names;

            // First time through, we need to collect emitted names of all top level types.
            if (this.namesOfTopLevelTypes == null)
            {
                names = new HashSet<string>();
            }
            else
            {
                names = null;
            }

            // First time through, we need to push things through NoPiaReferenceIndexer
            // to make sure we collect all to be embedded NoPia types and members.
            if (EmbeddedTypesManagerOpt != null && !EmbeddedTypesManagerOpt.IsFrozen)
            {
                noPiaIndexer = new Microsoft.Cci.NoPiaReferenceIndexer(context);
                Debug.Assert(names != null);
                this.Dispatch(noPiaIndexer);
            }

            AddTopLevelType(names, rootModuleType);
            VisitTopLevelType(noPiaIndexer, rootModuleType);
            yield return rootModuleType;

            foreach (var type in GetAnonymousTypes())
            {
                AddTopLevelType(names, type);
                VisitTopLevelType(noPiaIndexer, type);
                yield return type;
            }

            foreach (var type in GetTopLevelTypesCore(context))
            {
                AddTopLevelType(names, type);
                VisitTopLevelType(noPiaIndexer, type);
                yield return type;
            }

            var privateImpl = this.PrivateImplClass;
            if (privateImpl != null)
            {
                AddTopLevelType(names, privateImpl);
                VisitTopLevelType(noPiaIndexer, privateImpl);
                yield return privateImpl;
            }

            if (EmbeddedTypesManagerOpt != null)
            {
                foreach (var embedded in EmbeddedTypesManagerOpt.GetTypes(context.Diagnostics, names))
                {
                    AddTopLevelType(names, embedded);
                    yield return embedded;
                }
            }

            if (names != null)
            {
                Debug.Assert(this.namesOfTopLevelTypes == null);
                this.namesOfTopLevelTypes = names;
            }
        }

        internal abstract Cci.IAssemblyReference Translate(TAssemblySymbol symbol, DiagnosticBag diagnostics);
        internal abstract Cci.ITypeReference Translate(TTypeSymbol symbol, TSyntaxNode syntaxNodeOpt, DiagnosticBag diagnostics);

        internal sealed override Cci.IAssemblyReference Translate(IAssemblySymbol symbol, DiagnosticBag diagnostics)
        {
            return Translate((TAssemblySymbol)symbol, diagnostics);
        }

        internal sealed override Cci.ITypeReference Translate(ITypeSymbol symbol, SyntaxNode syntaxNodeOpt, DiagnosticBag diagnostics)
        {
            return Translate((TTypeSymbol)symbol, (TSyntaxNode)syntaxNodeOpt, diagnostics);
        }

        internal OutputKind OutputKind
        {
            get
            {
                return this.outputKind;
            }
        }

        internal TSourceModuleSymbol SourceModule
        {
            get
            {
                return this.sourceModule;
            }
        }

        internal TCompilation Compilation
        {
            get
            {
                return this.compilation;
            }
        }

        internal override Compilation CommonCompilation
        {
            get
            {
                return this.compilation;
            }
        }

        // General entry point method. May be a PE entry point or a submission entry point.
        internal sealed override Cci.IMethodReference EntryPoint
        {
            get
            {
                return entryPoint;
            }
        }

        internal void SetEntryPoint(TMethodSymbol value)
        {
            Debug.Assert(value == null ||
                ((object)((IMethodSymbol)value).ContainingModule == (object)sourceModule && ReferenceEquals(value, ((IMethodSymbol)value).OriginalDefinition)));
            entryPoint = value;
        }

        internal MetadataConstant CreateConstant(
            TTypeSymbol type,
            object value,
            TSyntaxNode syntaxNodeOpt,
            DiagnosticBag diagnostics)
        {
            return new MetadataConstant(Translate(type, syntaxNodeOpt, diagnostics), value);
        }

        private static void AddTopLevelType(HashSet<string> names, Cci.INamespaceTypeDefinition type)
        {
            if (names != null)
            {
                names.Add(MetadataHelpers.BuildQualifiedName(type.NamespaceName, Cci.PeWriter.GetMangledName(type)));
            }
        }

        private static void VisitTopLevelType(Cci.NoPiaReferenceIndexer noPiaIndexer, Cci.INamespaceTypeDefinition type)
        {
            if (noPiaIndexer != null)
            {
                noPiaIndexer.Visit((Microsoft.Cci.ITypeDefinition)type);
            }
        }

        #region Compiler Generated Members

        /// <summary>
        /// Captures the set of compiler generated definitions that should be added to a type
        /// during emit process.
        /// </summary>
        private sealed class CompilerGeneratedDefinitions
        {
            public ConcurrentQueue<Cci.INestedTypeDefinition> NestedTypes;
            public ConcurrentQueue<Cci.IMethodDefinition> Methods;
            public ConcurrentQueue<Cci.IPropertyDefinition> Properties;
            public ConcurrentQueue<Cci.IFieldDefinition> Fields;
        }

        private readonly ConcurrentDictionary<TNamedTypeSymbol, CompilerGeneratedDefinitions> compilerGeneratedDefs =
            new ConcurrentDictionary<TNamedTypeSymbol, CompilerGeneratedDefinitions>();

        public void AddCompilerGeneratedDefinition(TNamedTypeSymbol container, Cci.INestedTypeDefinition nestedType)
        {
            Debug.Assert(nestedType != null);

            CompilerGeneratedDefinitions defs = GetCacheOfCompilerGeneratedDefinitions(container);
            if (defs.NestedTypes == null)
            {
                Interlocked.CompareExchange(ref defs.NestedTypes, new ConcurrentQueue<Cci.INestedTypeDefinition>(), null);
            }

            defs.NestedTypes.Enqueue(nestedType);
        }

        internal abstract IEnumerable<Cci.INestedTypeDefinition> GetSynthesizedNestedTypes(TNamedTypeSymbol container);

        /// <summary>
        /// Returns null if there are no compiler generated types.
        /// </summary>
        public IEnumerable<Cci.INestedTypeDefinition> GetCompilerGeneratedTypes(TNamedTypeSymbol container)
        {
            IEnumerable<Cci.INestedTypeDefinition> declareTypes = GetSynthesizedNestedTypes(container);
            IEnumerable<Cci.INestedTypeDefinition> compileEmitTypes = null;

            CompilerGeneratedDefinitions defs = GetCacheOfCompilerGeneratedDefinitions(container, addIfNotFound: false);
            if (defs != null)
            {
                compileEmitTypes = defs.NestedTypes;
            }

            if (declareTypes != null)
            {
                if (compileEmitTypes != null)
                {
                    return System.Linq.Enumerable.Concat(declareTypes, compileEmitTypes);
                }

                return declareTypes;
            }

            return compileEmitTypes;
        }

        private CompilerGeneratedDefinitions GetCacheOfCompilerGeneratedDefinitions(TNamedTypeSymbol container, bool addIfNotFound = true)
        {
            Debug.Assert(((INamedTypeSymbol)container).IsDefinition);
            if (addIfNotFound)
            {
                return compilerGeneratedDefs.GetOrAdd(container, _ => new CompilerGeneratedDefinitions());
            }
            else
            {
                CompilerGeneratedDefinitions defs;
                if (!compilerGeneratedDefs.TryGetValue(container, out defs))
                {
                    defs = null;
                }

                return defs;
            }
        }

        public void AddCompilerGeneratedDefinition(TNamedTypeSymbol container, Cci.IMethodDefinition method)
        {
            Debug.Assert(method != null);

            CompilerGeneratedDefinitions defs = GetCacheOfCompilerGeneratedDefinitions(container);
            if (defs.Methods == null)
            {
                Interlocked.CompareExchange(ref defs.Methods, new ConcurrentQueue<Cci.IMethodDefinition>(), null);
            }

            defs.Methods.Enqueue(method);
        }

        /// <summary>
        /// Returns null if there are no compiler generated methods.
        /// </summary>
        /// <param name="container"></param>
        /// <returns></returns>
        public IEnumerable<Cci.IMethodDefinition> GetCompilerGeneratedMethods(TNamedTypeSymbol container)
        {
            CompilerGeneratedDefinitions defs = GetCacheOfCompilerGeneratedDefinitions(container, addIfNotFound: false);

            if (defs != null)
            {
                return defs.Methods;
            }

            return null;
        }

        public void AddCompilerGeneratedDefinition(TNamedTypeSymbol container, Cci.IPropertyDefinition property)
        {
            Debug.Assert(property != null);

            CompilerGeneratedDefinitions defs = GetCacheOfCompilerGeneratedDefinitions(container);
            if (defs.Properties == null)
            {
                Interlocked.CompareExchange(ref defs.Properties, new ConcurrentQueue<Cci.IPropertyDefinition>(), null);
            }

            defs.Properties.Enqueue(property);
        }

        /// <summary>
        /// Returns null if there are no compiler generated properties.
        /// </summary>
        /// <param name="container"></param>
        /// <returns></returns>
        public IEnumerable<Cci.IPropertyDefinition> GetCompilerGeneratedProperties(TNamedTypeSymbol container)
        {
            CompilerGeneratedDefinitions defs = GetCacheOfCompilerGeneratedDefinitions(container, addIfNotFound: false);

            if (defs != null)
            {
                return defs.Properties;
            }

            return null;
        }

        public void AddCompilerGeneratedDefinition(TNamedTypeSymbol container, Cci.IFieldDefinition field)
        {
            Debug.Assert(field != null);

            CompilerGeneratedDefinitions defs = GetCacheOfCompilerGeneratedDefinitions(container);
            if (defs.Fields == null)
            {
                Interlocked.CompareExchange(ref defs.Fields, new ConcurrentQueue<Cci.IFieldDefinition>(), null);
            }

            defs.Fields.Enqueue(field);
        }

        /// <summary>
        /// Returns null if there are no compiler generated fields.
        /// </summary>
        public IEnumerable<Cci.IFieldDefinition> GetCompilerGeneratedFields(TNamedTypeSymbol container)
        {
            CompilerGeneratedDefinitions defs = GetCacheOfCompilerGeneratedDefinitions(container, addIfNotFound: false);

            if (defs != null)
            {
                return defs.Fields;
            }

            return null;
        }

        #endregion

        #region Token Mapping

        Cci.IFieldReference ITokenDeferral.GetFieldForData(byte[] data, SyntaxNode syntaxNode, DiagnosticBag diagnostics)
        {
            Debug.Assert(this.SupportsPrivateImplClass);

            var privateImpl = this.GetPrivateImplClass((TSyntaxNode)syntaxNode, diagnostics);

            // map a field to the block (that makes it addressable via a token)
            return privateImpl.CreateDataField(data);
        }

        public abstract Microsoft.Cci.IMethodReference GetInitArrayHelper();

        public ArrayMethods ArrayMethods
        {
            get
            {
                ArrayMethods result = this.lazyArrayMethods;

                if (result == null)
                {
                    result = new ArrayMethods();

                    if (Interlocked.CompareExchange(ref this.lazyArrayMethods, result, null) != null)
                    {
                        result = this.lazyArrayMethods;
                    }
                }

                return result;
            }
        }

        public uint GetFakeSymbolTokenForIL(Cci.IReference symbol, SyntaxNode syntaxNode, DiagnosticBag diagnostics)
        {
            bool added;
            uint token = referencesInILMap.GetOrAddTokenFor(symbol, out added);
            if (added)
            {
                ReferenceDependencyWalker.VisitReference(symbol, new Microsoft.CodeAnalysis.Emit.Context(this, syntaxNode, diagnostics));
            }
            return token;
        }

        public Cci.IReference GetReferenceFromToken(uint token)
        {
            return referencesInILMap.GetItem(token);
        }

        public uint GetFakeStringTokenForIL(string str)
        {
            return stringsInILMap.GetOrAddTokenFor(str);
        }

        public string GetStringFromToken(uint token)
        {
            return stringsInILMap.GetItem(token);
        }

        IEnumerable<Cci.IReference> Cci.IModule.ReferencesInIL(out int count)
        {
            return referencesInILMap.GetAllItemsAndCount(out count);
        }

        #endregion

        #region Private Implementation Details Type

        internal PrivateImplementationDetails GetPrivateImplClass(TSyntaxNode syntaxNodeOpt, DiagnosticBag diagnostics)
        {
            var result = this.compilerGeneratedPrivateImpl;

            if ((result == null) && this.SupportsPrivateImplClass)
            {
                result = new PrivateImplementationDetails(
                        this,
                        this.GetSpecialType(SpecialType.System_Object, syntaxNodeOpt, diagnostics),
                        this.GetSpecialType(SpecialType.System_ValueType, syntaxNodeOpt, diagnostics),
                        this.GetSpecialType(SpecialType.System_Byte, syntaxNodeOpt, diagnostics),
                        this.GetSpecialType(SpecialType.System_Int16, syntaxNodeOpt, diagnostics),
                        this.GetSpecialType(SpecialType.System_Int32, syntaxNodeOpt, diagnostics),
                        this.GetSpecialType(SpecialType.System_Int64, syntaxNodeOpt, diagnostics),
                        SynthesizeAttribute(WellKnownMember.System_Runtime_CompilerServices_CompilerGeneratedAttribute__ctor));

                if (Interlocked.CompareExchange(ref this.compilerGeneratedPrivateImpl, result, null) != null)
                {
                    result = this.compilerGeneratedPrivateImpl;
                }
            }

            return result;
        }

        internal PrivateImplementationDetails PrivateImplClass
        {
            get { return compilerGeneratedPrivateImpl; }
        }

        internal override bool SupportsPrivateImplClass
        {
            get { return true; }
        }

        #endregion

        #region Method Body Map

        internal Cci.IMethodBody GetMethodBody(TMethodSymbol methodSymbol)
        {
            Debug.Assert(((IMethodSymbol)methodSymbol).ContainingModule == this.SourceModule &&
                ((IMethodSymbol)methodSymbol).IsDefinition);

            Cci.IMethodBody body;

            if (methodBodyMap.TryGetValue(methodSymbol, out body))
            {
                return body;
            }

            return null;
        }

        public void SetMethodBody(TMethodSymbol methodSymbol, Cci.IMethodBody body)
        {
            Debug.Assert(((IMethodSymbol)methodSymbol).ContainingModule == this.SourceModule &&
                ((IMethodSymbol)methodSymbol).IsDefinition);

            methodBodyMap.Add(methodSymbol, body);
        }

        #endregion

        #region IModule

        public virtual void Dispatch(Cci.MetadataVisitor visitor)
        {
            visitor.Visit((Cci.IModule)this);
        }

        ushort Cci.IModule.MajorSubsystemVersion
        {
            get { return (ushort)this.serializationProperties.SubsystemVersion.Major; }
        }

        ushort Cci.IModule.MinorSubsystemVersion
        {
            get { return (ushort)this.serializationProperties.SubsystemVersion.Minor; }
        }

        byte Cci.IModule.LinkerMajorVersion
        {
            get
            {
                return LinkerMajorVersion;
            }
        }

        byte Cci.IModule.LinkerMinorVersion
        {
            get
            {
                return LinkerMinorVersion;
            }
        }

        IEnumerable<Cci.INamespaceTypeDefinition> Cci.IModule.GetTopLevelTypes(Microsoft.CodeAnalysis.Emit.Context context)
        {
            return GetTopLevelTypes(context);
        }

        public abstract IEnumerable<Microsoft.Cci.IAliasForType> GetExportedTypes(Microsoft.CodeAnalysis.Emit.Context context);

        Cci.ITypeReference Cci.IModule.GetPlatformType(Cci.PlatformType platformType, Microsoft.CodeAnalysis.Emit.Context context)
        {
            Debug.Assert((object)this == context.Module);

            switch (platformType)
            {
                case Cci.PlatformType.SystemType:
                    return GetSystemType((TSyntaxNode)context.SyntaxNodeOpt, context.Diagnostics);

                default:
                    return GetSpecialType((SpecialType)platformType, (TSyntaxNode)context.SyntaxNodeOpt, context.Diagnostics);
            }
        }

        protected abstract bool IsPlatformType(Cci.ITypeReference typeRef, Cci.PlatformType platformType);

        bool Cci.IModule.IsPlatformType(Cci.ITypeReference typeRef, Cci.PlatformType platformType)
        {
            return IsPlatformType(typeRef, platformType);
        }

        IEnumerable<Cci.ICustomAttribute> Cci.IModule.AssemblyAttributes
        {
            get
            {
                return GetSourceAssemblyAttributes();
            }
        }

        IEnumerable<Cci.SecurityAttribute> Cci.IModule.AssemblySecurityAttributes
        {
            get
            {
                return GetSourceAssemblySecurityAttributes();
            }
        }

        IEnumerable<Cci.ICustomAttribute> Cci.IModule.ModuleAttributes
        {
            get { return GetSourceModuleAttributes(); }
        }

        IEnumerable<Cci.IExternNamespace> Cci.IModule.ExternNamespaces
        {
            get
            {
                Compilation compilation = this.compilation;

                // Enumerate external references (#r's don't define aliases) to preserve the order.
                foreach (MetadataReference reference in compilation.ExternalReferences)
                {
                    if (reference.Properties.Alias != null)
                    {
                        IAssemblySymbol referencedAssembly = this.Compilation.GetAssemblyOrModuleSymbol(reference) as IAssemblySymbol;

                        // duplicate references might have been skipped by the assembly binder:
                        if (referencedAssembly != null)
                        {
                            yield return new ExternNamespace(reference.Properties.Alias, referencedAssembly.Identity.GetDisplayName());
                        }
                    }
                }
            }
        }

        // PE entry point, only available for console and windows apps:
        Cci.IMethodReference Cci.IModule.EntryPoint
        {
            get
            {
                return outputKind.IsApplication() ? entryPoint : null;
            }
        }

        protected abstract Cci.IAssemblyReference GetCorLibraryReferenceToEmit(Microsoft.CodeAnalysis.Emit.Context context);

        /// <summary>
        /// Builds symbol definition to location map used for emitting token -> location info
        /// into PDB to be consumed by WinMdExp.exe tool (only applicable for /t:winmdobj)
        /// </summary>
        protected abstract MultiDictionary<Cci.DebugSourceDocument, Cci.DefinitionWithLocation> GetSymbolToLocationMap();

        MultiDictionary<Cci.DebugSourceDocument, Cci.DefinitionWithLocation> Cci.IModule.GetSymbolToLocationMap()
        {
            return GetSymbolToLocationMap();
        }

        IEnumerable<Cci.IAssemblyReference> Cci.IModule.GetAssemblyReferences(Microsoft.CodeAnalysis.Emit.Context context)
        {
            Cci.IAssemblyReference corLibrary = GetCorLibraryReferenceToEmit(context);

            // Only add Cor Library reference explicitly, PeWriter will add
            // other references implicitly on as needed basis.
            if (corLibrary != null)
            {
                yield return corLibrary;
            }

            if (OutputKind != CodeAnalysis.OutputKind.NetModule)
            {
                // Explicitly add references from added modules
                foreach (var aRef in GetAssemblyReferencesFromAddedModules(context.Diagnostics))
                {
                    yield return aRef;
                }
            }
        }

        protected abstract IEnumerable<Cci.IAssemblyReference> GetAssemblyReferencesFromAddedModules(DiagnosticBag diagnostics);

        private IEnumerable<ManagedResource> lazyManagedResources;

        IEnumerable<Cci.IResourceReference> Cci.IModule.GetResources(Microsoft.CodeAnalysis.Emit.Context context)
        {
            if (lazyManagedResources == null)
            {
                var builder = ArrayBuilder<ManagedResource>.GetInstance();

                foreach (ResourceDescription r in ManifestResources)
                {
                    builder.Add(r.ToManagedResource(this));
                }

                if (OutputKind != OutputKind.NetModule)
                {
                    // Explicitly add resources from added modules
                    AddEmbeddedResourcesFromAddedModules(builder, context.Diagnostics);
                }

                lazyManagedResources = builder.ToImmutableAndFree();
            }

            return lazyManagedResources;
        }

        protected abstract void AddEmbeddedResourcesFromAddedModules(ArrayBuilder<ManagedResource> builder, DiagnosticBag diagnostics);

        Cci.IAssembly Cci.IModule.AsAssembly
        {
            get { return this as Cci.IAssembly; }
        }

        Cci.IAssemblyReference Cci.IModule.GetCorLibrary(Microsoft.CodeAnalysis.Emit.Context context)
        {
            return Translate(CorLibrary, context.Diagnostics);
        }

        ulong Cci.IModule.BaseAddress
        {
            get { return serializationProperties.BaseAddress; }
        }

        Cci.IAssembly Cci.IModule.GetContainingAssembly(Microsoft.CodeAnalysis.Emit.Context context)
        {
            return this.OutputKind.IsNetModule() ? null : (Cci.IAssembly)this;
        }

        Cci.IAssemblyReference Cci.IModuleReference.GetContainingAssembly(Microsoft.CodeAnalysis.Emit.Context context)
        {
            return this.OutputKind.IsNetModule() ? null : (Cci.IAssemblyReference)this;
        }

        ushort Cci.IModule.DllCharacteristics
        {
            get { return serializationProperties.DllCharacteristics; }
        }

        uint Cci.IModule.FileAlignment
        {
            get { return serializationProperties.FileAlignment; }
        }

        IEnumerable<string> Cci.IModule.GetStrings()
        {
            return this.stringsInILMap.GetAllItems();
        }

        bool Cci.IModule.ILOnly
        {
            get { return serializationProperties.ILOnly; }
        }

        Cci.ModuleKind Cci.IModule.Kind
        {
            get
            {
                switch (this.outputKind)
                {
                    case OutputKind.ConsoleApplication:
                        return Cci.ModuleKind.ConsoleApplication;

                    case OutputKind.WindowsRuntimeApplication: // TODO: separate ModuleKind?
                    case OutputKind.WindowsApplication:
                        return Cci.ModuleKind.WindowsApplication;

                    case OutputKind.WindowsRuntimeMetadata:
                        return Cci.ModuleKind.WindowsRuntimeMetadata;

                    case OutputKind.DynamicallyLinkedLibrary:
                    case OutputKind.NetModule:
                        return Cci.ModuleKind.DynamicallyLinkedLibrary;

                    default:
                        throw ExceptionUtilities.UnexpectedValue(this.outputKind);
                }
            }
        }

        byte Cci.IModule.MetadataFormatMajorVersion
        {
            get { return serializationProperties.MetadataFormatMajorVersion; }
        }

        byte Cci.IModule.MetadataFormatMinorVersion
        {
            get { return serializationProperties.MetadataFormatMinorVersion; }
        }

        string Cci.IModule.ModuleName
        {
            get { return ModuleName; }
        }

        IEnumerable<Cci.IModuleReference> Cci.IModule.ModuleReferences
        {
            get
            {
                // Let's not add any module references explicitly,
                // PeWriter will implicitly add those needed.
                return SpecializedCollections.EmptyEnumerable<Cci.IModuleReference>();
            }
        }

        Guid Cci.IModule.PersistentIdentifier
        {
            get { return serializationProperties.PersistentIdentifier; }
        }

        bool Cci.IModule.StrongNameSigned
        {
            get { return serializationProperties.StrongNameSigned; }
        }

        Cci.Machine Cci.IModule.Machine
        {
            get { return serializationProperties.Machine; }
        }

        bool Cci.IModule.RequiresStartupStub
        {
            get { return serializationProperties.RequiresStartupStub; }
        }

        bool Cci.IModule.Prefers32bits
        {
            get { return serializationProperties.Platform == Platform.AnyCpu32BitPreferred; }
        }

        bool Cci.IModule.RequiresAmdInstructionSet
        {
            get { return serializationProperties.Platform.RequiresAmdInstructionSet(); }
        }

        bool Cci.IModule.Requires32bits
        {
            get { return serializationProperties.Platform.Requires32Bit(); }
        }

        bool Cci.IModule.Requires64bits
        {
            get { return serializationProperties.Platform.Requires64Bit(); }
        }

        ulong Cci.IModule.SizeOfHeapCommit
        {
            get { return serializationProperties.SizeOfHeapCommit; }
        }

        ulong Cci.IModule.SizeOfHeapReserve
        {
            get { return serializationProperties.SizeOfHeapReserve; }
        }

        ulong Cci.IModule.SizeOfStackCommit
        {
            get { return serializationProperties.SizeOfStackCommit; }
        }

        ulong Cci.IModule.SizeOfStackReserve
        {
            get { return serializationProperties.SizeOfStackReserve; }
        }

        string Cci.IModule.TargetRuntimeVersion
        {
            get { return serializationProperties.TargetRuntimeVersion; }
        }

        bool Cci.IModule.TrackDebugData
        {
            get { return serializationProperties.TrackDebugData; }
        }

        Cci.ResourceSection Cci.IModule.Win32ResourceSection
        {
            get
            {
                return this.Win32ResourceSection;
            }
        }

        IEnumerable<Cci.IWin32Resource> Cci.IModule.Win32Resources
        {
            get
            {
                return this.Win32Resources;
            }
        }

        int Cci.IModule.HintNumberOfMethodDefinitions
        {
            get
            {
                return this.methodBodyMap.Count;
            }
        }

        #endregion

        #region INamedEntity

        string Cci.INamedEntity.Name
        {
            get
            {
                return Name;
            }
        }

        #endregion

        #region IReference

        IEnumerable<Cci.ICustomAttribute> Cci.IReference.GetAttributes(Microsoft.CodeAnalysis.Emit.Context context)
        {
            return SpecializedCollections.EmptyEnumerable<Cci.ICustomAttribute>();
        }

        Cci.IDefinition Cci.IReference.AsDefinition(Microsoft.CodeAnalysis.Emit.Context context)
        {
            Debug.Assert(ReferenceEquals(context.Module, this));
            return this;
        }

        #endregion

        #region Debug Documents

        internal void AddDebugDocument(Cci.DebugSourceDocument document)
        {
            debugDocuments.Add(document.Location, document);
        }

        internal Cci.DebugSourceDocument TryGetDebugDocument(string path, string basePath)
        {
            return TryGetDebugDocumentForNormalizedPath(NormalizeDebugDocumentPath(path, basePath));
        }

        internal Cci.DebugSourceDocument TryGetDebugDocumentForNormalizedPath(string normalizedPath)
        {
            Cci.DebugSourceDocument document;
            debugDocuments.TryGetValue(normalizedPath, out document);
            return document;
        }

        internal Cci.DebugSourceDocument GetOrAddDebugDocument(string path, string basePath, Func<string, Cci.DebugSourceDocument> factory)
        {
            return debugDocuments.GetOrAdd(NormalizeDebugDocumentPath(path, basePath), factory);
        }

        internal string NormalizeDebugDocumentPath(string path, string basePath)
        {
            var key = ValueTuple.Create(path, basePath);
            string normalizedPath;
            if (!normalizedPathsCache.TryGetValue(key, out normalizedPath))
            {
                normalizedPath = compilation.Options.FileResolver.NormalizePath(path, basePath);
                normalizedPathsCache.TryAdd(key, normalizedPath);
            }

            return normalizedPath;
        }

        #endregion
    }
}
