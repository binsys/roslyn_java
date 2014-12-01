﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Internal.Log;
using Microsoft.CodeAnalysis.LanguageServices;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.WorkspaceServices;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{
    internal partial class DocumentState
    {
        private readonly ILanguageServiceProvider languageServices;
        private readonly SolutionServices solutionServices;
        private readonly DocumentInfo info;
        private readonly ParseOptions options;

        private readonly ValueSource<TextAndVersion> textSource;
        private readonly ValueSource<TreeAndVersion> treeSource;

        private DocumentState(
            ILanguageServiceProvider languageServices,
            SolutionServices solutionServices,
            DocumentInfo info,
            ParseOptions options,
            ValueSource<TextAndVersion> textSource,
            ValueSource<TreeAndVersion> treeSource)
        {
            this.languageServices = languageServices;
            this.solutionServices = solutionServices;
            this.info = info;
            this.options = options;
            this.textSource = textSource;
            this.treeSource = treeSource;
        }

        public bool IsGenerated
        {
            get { return info.IsGenerated; }
        }

        public static DocumentState Create(
            DocumentInfo info,
            ParseOptions options,
            ILanguageServiceProvider language,
            SolutionServices services)
        {
            var textSource = info.TextLoader != null
                ? CreateRecoverableText(info.TextLoader, info.Id, services)
                : CreateStrongText(TextAndVersion.Create(SourceText.From(string.Empty), VersionStamp.Default, info.FilePath));

            var treeSource = CreateLazyFullyParsedTree(
                textSource,
                GetSyntaxTreeFilePath(info),
                options,
                workspaceServices: services.WorkspaceServices,
                languageServices: language);

            // remove any initial loader so we don't keep source alive
            info = info.WithTextLoader(null);

            return new DocumentState(
                languageServices: language,
                solutionServices: services,
                info: info,
                options: options,
                textSource: textSource,
                treeSource: treeSource);
        }

        // This is the string used to represent the FilePath property on a SyntaxTree object.
        // if the document does not yet have a file path, use the document's name instead.
        private static string GetSyntaxTreeFilePath(DocumentInfo info)
        {
            return info.FilePath ?? info.Name;
        }

        private static ValueSource<TextAndVersion> CreateStrongText(TextAndVersion text)
        {
            return new ConstantValueSource<TextAndVersion>(text);
        }

        private static ValueSource<TextAndVersion> CreateStrongText(TextLoader loader, DocumentId documentId, SolutionServices services)
        {
            return new AsyncLazy<TextAndVersion>(c => LoadTextAsync(loader, documentId, services, c), cacheResult: true);
        }

        private static ValueSource<TextAndVersion> CreateRecoverableText(TextAndVersion text, SolutionServices services)
        {
            return new RecoverableTextAndVersion(CreateStrongText(text), services.TemporaryStorage, services.TextCache);
        }

        private static ValueSource<TextAndVersion> CreateRecoverableText(TextLoader loader, DocumentId documentId, SolutionServices services)
        {
            return new RecoverableTextAndVersion(
                new AsyncLazy<TextAndVersion>(c => LoadTextAsync(loader, documentId, services, c), cacheResult: false),
                services.TemporaryStorage,
                services.TextCache);
        }

        private static async Task<TextAndVersion> LoadTextAsync(TextLoader loader, DocumentId documentId, SolutionServices services, CancellationToken cancellationToken)
        {
            try
            {
                using (ExceptionHelpers.SuppressFailFast())
                {
                    var result = await loader.LoadTextAndVersionAsync(services.Workspace, documentId, cancellationToken).ConfigureAwait(continueOnCapturedContext: false);
                    return result;
                }
            }
            catch (OperationCanceledException)
            {
                // if load text is failed due to a cancellation, make sure we propagate it out to the caller
                throw;
            }
            catch (Exception e)
            {
                services.Workspace.OnWorkspaceFailed(new DocumentDiagnostic(WorkspaceDiagnosticKind.FileAccessFailure, e.Message, documentId));
                return TextAndVersion.Create(SourceText.From(string.Empty), VersionStamp.Default, documentId.DebuggerText);
            }
        }

        private static ValueSource<TreeAndVersion> CreateLazyFullyParsedTree(
            ValueSource<TextAndVersion> newTextSource,
            string filePath,
            ParseOptions options,
            IWorkspaceServiceProvider workspaceServices,
            ILanguageServiceProvider languageServices,
            PreservationMode mode = PreservationMode.PreserveValue)
        {
            return new AsyncLazy<TreeAndVersion>(
                c => FullyParseTreeAsync(newTextSource, filePath, options, workspaceServices, languageServices, mode, c),
                cacheResult: true);
        }

        private static async Task<TreeAndVersion> FullyParseTreeAsync(
            ValueSource<TextAndVersion> newTextSource,
            string filePath,
            ParseOptions options,
            IWorkspaceServiceProvider workspaceServices,
            ILanguageServiceProvider languageServices,
            PreservationMode mode,
            CancellationToken cancellationToken)
        {
            using (Logger.LogBlock(FeatureId.DocumentState, FunctionId.DocumentState_FullyParseSyntaxTree, cancellationToken))
            {
                var textAndVersion = await newTextSource.GetValueAsync(cancellationToken).ConfigureAwait(false);
                var text = textAndVersion.Text;

                var treeFactory = languageServices.GetService<ISyntaxTreeFactoryService>();

                var tree = treeFactory.ParseSyntaxTree(filePath, options, text, cancellationToken);

                if (mode == PreservationMode.PreserveValue)
                {
                    var root = await tree.GetRootAsync(cancellationToken).ConfigureAwait(false);

                    // get a recoverable tree that reparses from the source text if it gets used after being kicked out of memory
                    tree = treeFactory.CreateRecoverableTree(tree.FilePath, tree.Options, newTextSource, root, reparse: true);
                }

                Contract.ThrowIfNull(tree);

                // text version for this document should be unique. use it as a starting point.
                return TreeAndVersion.Create(tree, textAndVersion.Version);
            }
        }

        private static ValueSource<TreeAndVersion> CreateLazyIncrementallyParsedTree(
            ValueSource<TreeAndVersion> oldTreeSource,
            ValueSource<TextAndVersion> newTextSource)
        {
            return new AsyncLazy<TreeAndVersion>(
                c => IncrementallyParseTreeAsync(oldTreeSource, newTextSource, c),
                cacheResult: true);
        }

        private static async Task<TreeAndVersion> IncrementallyParseTreeAsync(
            ValueSource<TreeAndVersion> oldTreeSource,
            ValueSource<TextAndVersion> newTextSource,
            CancellationToken cancellationToken)
        {
            using (Logger.LogBlock(FeatureId.DocumentState, FunctionId.DocumentState_IncrementallyParseSyntaxTree, cancellationToken))
            {
                var newTextAndVersion = await newTextSource.GetValueAsync(cancellationToken).ConfigureAwait(false);
                var newText = newTextAndVersion.Text;

                var oldTreeAndVersion = await oldTreeSource.GetValueAsync(cancellationToken).ConfigureAwait(false);
                var oldTree = oldTreeAndVersion.Tree;
                var oldText = await oldTree.GetTextAsync(cancellationToken).ConfigureAwait(false);

                var newTree = oldTree.WithChangedText(newText);
                Contract.ThrowIfNull(newTree);

                return MakeNewTreeAndVersion(oldTree, oldText, oldTreeAndVersion.Version, newTree, newText, newTextAndVersion.Version);
            }
        }

        private static TreeAndVersion MakeNewTreeAndVersion(SyntaxTree oldTree, SourceText oldText, VersionStamp oldVersion, SyntaxTree newTree, SourceText newText, VersionStamp newVersion)
        {
            var topLevelChanged = TopLevelChanged(oldTree, oldText, newTree, newText);
            var version = topLevelChanged ? newVersion : oldVersion;
            return TreeAndVersion.Create(newTree, version);
        }

        private const int MaxTextChangeRangeLength = 1024 * 4;

        private static bool TopLevelChanged(SyntaxTree oldTree, SourceText oldText, SyntaxTree newTree, SourceText newText)
        {
            // ** currently, it doesnt do any text based quick check. we can add them later if current logic is not performant enough for typing case.
            var change = newText.GetEncompassingTextChangeRange(oldText);
            if (change == default(TextChangeRange))
            {
                // nothing has changed
                return false;
            }

            // if texts are small enough, just use the equivalent to find out whether there was top level edits
            if (oldText.Length < MaxTextChangeRangeLength && newText.Length < MaxTextChangeRangeLength)
            {
                var topLevel = !newTree.IsEquivalentTo(oldTree, topLevel: true);
                return topLevel;
            }

            // okay, text is not small and whole text is changed, then we always treat it as top level edit
            if (change.NewLength == newText.Length)
            {
                return true;
            }

            // if changes are small enough, we use IsEquivalentTo to find out whether there was a top level edit
            if (change.Span.Length < MaxTextChangeRangeLength && change.NewLength < MaxTextChangeRangeLength)
            {
                var topLevel = !newTree.IsEquivalentTo(oldTree, topLevel: true);
                return topLevel;
            }

            // otherwise, we always consider top level change
            return true;
        }

        public DocumentState UpdateParseOptions(ParseOptions options)
        {
            var originalSourceKind = this.SourceCodeKind;

            var newState = this.SetParseOptions(options);
            if (newState.SourceCodeKind != originalSourceKind)
            {
                newState = newState.UpdateSourceCodeKind(originalSourceKind);
            }

            return newState;
        }

        private DocumentState SetParseOptions(ParseOptions options)
        {
            if (options == null)
            {
                throw new ArgumentNullException("OptionSet");
            }

            var newTreeSource = CreateLazyFullyParsedTree(
                this.textSource,
                GetSyntaxTreeFilePath(this.info),
                options,
                workspaceServices: this.solutionServices.WorkspaceServices,
                languageServices: this.languageServices);

            return new DocumentState(
                this.LanguageServices,
                this.solutionServices,
                this.info,
                options,
                this.textSource,
                newTreeSource);
        }

        public DocumentState UpdateSourceCodeKind(SourceCodeKind kind)
        {
            if (this.ParseOptions == null || kind == this.SourceCodeKind)
            {
                return this;
            }

            return this.SetParseOptions(this.ParseOptions.WithKind(kind));
        }

        public DocumentState UpdateFolders(IList<string> folders)
        {
            return new DocumentState(
                this.languageServices,
                this.solutionServices,
                this.info.WithFolders(folders.ToImmutableListOrEmpty()),
                this.options,
                this.textSource,
                this.treeSource);
        }

        public DocumentState UpdateText(SourceText newText, PreservationMode mode)
        {
            if (newText == null)
            {
                throw new ArgumentNullException("newText");
            }

            // check to see if this docstate has already been branched before with the same text.
            // this helps reduce duplicate parsing when typing.
            if (mode == PreservationMode.PreserveIdentity)
            {
                var br = this.firstBranch;
                if (br != null && br.Text == newText)
                {
                    return br.State;
                }
            }

            var newVersion = this.GetNewerVersion();
            var newTextAndVersion = TextAndVersion.Create(newText, newVersion, this.FilePath);

            var newState = this.UpdateText(newTextAndVersion, mode);

            if (mode == PreservationMode.PreserveIdentity && this.firstBranch == null)
            {
                Interlocked.CompareExchange(ref this.firstBranch, new DocumentBranch(newText, newState), null);
            }

            return newState;
        }

        private DocumentBranch firstBranch;

        private class DocumentBranch
        {
            internal readonly SourceText Text;
            internal readonly DocumentState State;

            internal DocumentBranch(SourceText text, DocumentState state)
            {
                this.Text = text;
                this.State = state;
            }
        }

        public DocumentState UpdateText(TextAndVersion newTextAndVersion, PreservationMode mode)
        {
            if (newTextAndVersion == null)
            {
                throw new ArgumentNullException("newTextAndVesion");
            }

            var newTextSource = mode == PreservationMode.PreserveIdentity
                ? CreateStrongText(newTextAndVersion)
                : CreateRecoverableText(newTextAndVersion, this.solutionServices);

            // always chain incremental parsing request, it will internally put 
            // appropriate request such as full parsing request if there are too many pending 
            // incremental parsing requests hanging around.
            var newTreeSource = CreateLazyIncrementallyParsedTree(this.treeSource, newTextSource);

            return new DocumentState(
                this.LanguageServices,
                this.solutionServices,
                this.info,
                this.options,
                newTextSource,
                newTreeSource);
        }

        public DocumentState UpdateText(TextLoader loader, PreservationMode mode)
        {
            if (loader == null)
            {
                throw new ArgumentNullException("loader");
            }

            var newTextSource = (mode == PreservationMode.PreserveIdentity)
                ? CreateStrongText(loader, this.Id, this.solutionServices)
                : CreateRecoverableText(loader, this.Id, this.solutionServices);

            var newTreeSource = CreateLazyFullyParsedTree(
                newTextSource,
                GetSyntaxTreeFilePath(this.info),
                this.options,
                this.solutionServices.WorkspaceServices,
                this.languageServices,
                mode);

            return new DocumentState(
                this.LanguageServices,
                this.solutionServices,
                this.info,
                this.options,
                textSource: newTextSource,
                treeSource: newTreeSource);
        }

        public DocumentState UpdateTree(SyntaxNode newRoot, PreservationMode mode)
        {
            if (newRoot == null)
            {
                throw new ArgumentNullException("newRoot");
            }

            var newTextVersion = this.GetNewerVersion();
            var newTreeVersion = GetNewTreeVersionForUpdatedTree(newRoot, newTextVersion, mode);

            var syntaxTreeFactory = this.languageServices.GetService<ISyntaxTreeFactoryService>();

            var result = CreateRecoverableTextAndTree(newRoot, newTextVersion, newTreeVersion, this.info, this.options, syntaxTreeFactory, mode);

            return new DocumentState(
                this.LanguageServices,
                this.solutionServices,
                this.info,
                this.options,
                textSource: result.Item1,
                treeSource: result.Item2);
        }

        private VersionStamp GetNewTreeVersionForUpdatedTree(SyntaxNode newRoot, VersionStamp newTextVersion, PreservationMode mode)
        {
            if (mode != PreservationMode.PreserveIdentity)
            {
                return newTextVersion;
            }

            TreeAndVersion oldTreeAndVersion;
            SyntaxNode oldRoot;
            if (!this.treeSource.TryGetValue(out oldTreeAndVersion) || !oldTreeAndVersion.Tree.TryGetRoot(out oldRoot))
            {
                return newTextVersion;
            }

            return oldRoot.IsEquivalentTo(newRoot, topLevel: true) ? oldTreeAndVersion.Version : newTextVersion;
        }

        // use static method so we don't capture references to this
        private static Tuple<AsyncLazy<TextAndVersion>, AsyncLazy<TreeAndVersion>> CreateRecoverableTextAndTree(
            SyntaxNode newRoot, VersionStamp textVersion, VersionStamp treeVersion,
            DocumentInfo info, ParseOptions options, ISyntaxTreeFactoryService factory, PreservationMode mode)
        {
            string filePath = info.FilePath;
            AsyncLazy<TreeAndVersion> lazyTree = null;

            // this captures the lazyTree local
            var lazyText = new AsyncLazy<TextAndVersion>(
                c => GetTextAndVersionAsync(lazyTree, textVersion, filePath, c),
                c => GetTextAndVersion(lazyTree, textVersion, filePath, c),
                cacheResult: false);

            // this should be only called when we do forking, since there is no cheap way to figure out what has been changed,
            // we will always consider top level being changed by giving new version here.
            if (mode == PreservationMode.PreserveIdentity)
            {
                lazyTree = new AsyncLazy<TreeAndVersion>(
                    TreeAndVersion.Create(factory.CreateSyntaxTree(GetSyntaxTreeFilePath(info), options, newRoot), treeVersion));
            }
            else
            {
                lazyTree = new AsyncLazy<TreeAndVersion>(
                    TreeAndVersion.Create(factory.CreateRecoverableTree(GetSyntaxTreeFilePath(info), options, lazyText, newRoot, reparse: false), treeVersion));
            }

            return Tuple.Create(lazyText, lazyTree);
        }

        private static TextAndVersion GetTextAndVersion(
            ValueSource<TreeAndVersion> newTreeSource, VersionStamp version, string filePath, CancellationToken cancellationToken)
        {
            var treeAndVersion = newTreeSource.GetValue(cancellationToken);
            var text = treeAndVersion.Tree.GetRoot(cancellationToken).GetText();
            return TextAndVersion.Create(text, version, filePath);
        }

        private static async Task<TextAndVersion> GetTextAndVersionAsync(
            ValueSource<TreeAndVersion> newTreeSource, VersionStamp version, string filePath, CancellationToken cancellationToken)
        {
            var treeAndVersion = await newTreeSource.GetValueAsync(cancellationToken).ConfigureAwait(false);
            var root = await treeAndVersion.Tree.GetRootAsync(cancellationToken).ConfigureAwait(false);

            return TextAndVersion.Create(root.GetText(), version, filePath);
        }

        public bool TryGetText(out SourceText text)
        {
            TextAndVersion textAndVersion;
            if (this.textSource.TryGetValue(out textAndVersion))
            {
                text = textAndVersion.Text;
                return true;
            }
            else
            {
                text = null;
                return false;
            }
        }

        public bool TryGetTextVersion(out VersionStamp version)
        {
            // try fast path first
            if (TryGetTextVersionFromRecoverableTextAndVersion(out version))
            {
                return true;
            }

            TextAndVersion textAndVersion;
            if (this.textSource.TryGetValue(out textAndVersion))
            {
                version = textAndVersion.Version;
                return true;
            }
            else
            {
                version = default(VersionStamp);
                return false;
            }
        }

        public async Task<SourceText> GetTextAsync(CancellationToken cancellationToken)
        {
            var textAndVersion = await this.textSource.GetValueAsync(cancellationToken).ConfigureAwait(false);
            return textAndVersion.Text;
        }

        public async Task<VersionStamp> GetTextVersionAsync(CancellationToken cancellationToken)
        {
            // try fast path first
            VersionStamp version;
            if (TryGetTextVersionFromRecoverableTextAndVersion(out version))
            {
                return version;
            }

            TextAndVersion textAndVersion;
            if (this.textSource.TryGetValue(out textAndVersion))
            {
                return textAndVersion.Version;
            }
            else
            {
                textAndVersion = await this.textSource.GetValueAsync(cancellationToken).ConfigureAwait(false);
                return textAndVersion.Version;
            }
        }

        private bool TryGetTextVersionFromRecoverableTextAndVersion(out VersionStamp version)
        {
            version = default(VersionStamp);

            var recoverable = this.textSource as RecoverableTextAndVersion;
            return recoverable != null && recoverable.TryGetTextVersion(out version);
        }

        private VersionStamp GetNewerVersion()
        {
            TextAndVersion textAndVersion;
            if (this.textSource.TryGetValue(out textAndVersion))
            {
                return textAndVersion.Version.GetNewerVersion();
            }

            TreeAndVersion treeAndVersion;
            if (this.treeSource.TryGetValue(out treeAndVersion))
            {
                return treeAndVersion.Version.GetNewerVersion();
            }

            return VersionStamp.Create();
        }

        public bool TryGetSyntaxTree(out SyntaxTree syntaxTree)
        {
            syntaxTree = default(SyntaxTree);

            TreeAndVersion treeAndVersion;
            if (this.treeSource.TryGetValue(out treeAndVersion))
            {
                syntaxTree = treeAndVersion.Tree;
                BindSyntaxTreeToId(syntaxTree, this.Id);
                return true;
            }

            return false;
        }

        public async Task<SyntaxTree> GetSyntaxTreeAsync(CancellationToken cancellationToken)
        {
            var treeAndVersion = await this.treeSource.GetValueAsync(cancellationToken).ConfigureAwait(false);

            // make sure there is an association between this tree and this doc id before handing it out
            BindSyntaxTreeToId(treeAndVersion.Tree, this.Id);
            return treeAndVersion.Tree;
        }

        public bool TryGetTopLevelChangeTextVersion(out VersionStamp version)
        {
            TreeAndVersion treeAndVersion;
            if (this.treeSource.TryGetValue(out treeAndVersion))
            {
                version = treeAndVersion.Version;
                return true;
            }
            else
            {
                version = default(VersionStamp);
                return false;
            }
        }

        public async Task<VersionStamp> GetTopLevelChangeTextVersionAsync(CancellationToken cancellationToken)
        {
            TreeAndVersion treeAndVersion;
            if (this.treeSource.TryGetValue(out treeAndVersion))
            {
                return treeAndVersion.Version;
            }
            else
            {
                treeAndVersion = await this.treeSource.GetValueAsync(cancellationToken).ConfigureAwait(false);
                return treeAndVersion.Version;
            }
        }

        private static readonly ReaderWriterLockSlim syntaxTreeToIdMapLock = new ReaderWriterLockSlim(LockRecursionPolicy.NoRecursion);
        private static readonly ConditionalWeakTable<SyntaxTree, DocumentId> syntaxTreeToIdMap =
            new ConditionalWeakTable<SyntaxTree, DocumentId>();

        private static void BindSyntaxTreeToId(SyntaxTree tree, DocumentId id)
        {
            using (syntaxTreeToIdMapLock.DisposableWrite())
            {
                DocumentId existingId;
                if (syntaxTreeToIdMap.TryGetValue(tree, out existingId))
                {
                    Contract.ThrowIfFalse(existingId == id);
                }
                else
                {
                    syntaxTreeToIdMap.Add(tree, id);
                }
            }
        }

        public static DocumentId GetDocumentIdForTree(SyntaxTree tree)
        {
            using (syntaxTreeToIdMapLock.DisposableRead())
            {
                DocumentId id;
                syntaxTreeToIdMap.TryGetValue(tree, out id);
                return id;
            }
        }
    }
}