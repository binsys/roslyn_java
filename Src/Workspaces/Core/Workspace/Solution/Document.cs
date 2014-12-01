﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Internal.Log;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{
    /// <summary>
    /// Represents a source code document that is part of a project.
    /// It provides access to the source text, parsed syntax tree and the corresponding semantic model.
    /// </summary>
    [DebuggerDisplay("{DebuggerDisplay,nq}")]
    public partial class Document
    {
        private readonly DocumentState state;

        private WeakReference<SemanticModel> model;
        private Task<SyntaxTree> syntaxTreeResultTask;

        /// <summary>
        /// The project this document belongs to.
        /// </summary>
        public Project Project { get; private set; }

        internal Document(Project project, DocumentState state)
        {
            Contract.ThrowIfNull(project);
            Contract.ThrowIfNull(state);

            this.Project = project;
            this.state = state;
        }

        internal DocumentState State
        {
            get
            {
                return this.state;
            }
        }

        /// <summary>
        /// The document's identifier. Many document instances may share the same ID, but only one
        /// document in a solution may have that ID.
        /// </summary>
        public DocumentId Id
        {
            get { return this.state.Id; }
        }

        /// <summary>
        /// The path to the document file or null if there is no document file.
        /// </summary>
        public string FilePath
        {
            get { return this.state.FilePath; }
        }

        /// <summary>
        /// The name of the document.
        /// </summary>
        public string Name
        {
            get
            {
                return this.state.Name;
            }
        }

        /// <summary>
        /// The sequence of logical folders the document is contained in.
        /// </summary>
        public IReadOnlyList<string> Folders
        {
            get
            {
                return this.state.Folders;
            }
        }

        /// <summary>
        /// The kind of source code this document contains.
        /// </summary>
        public SourceCodeKind SourceCodeKind
        {
            get
            {
                return this.state.SourceCodeKind;
            }
        }

        /// <summary>
        /// Get the current text for the document if it is already loaded and available.
        /// </summary>
        public bool TryGetText(out SourceText text)
        {
            return this.state.TryGetText(out text);
        }

        /// <summary>
        /// Gets the version of the document's text if it is already loaded and available.
        /// </summary>
        public bool TryGetTextVersion(out VersionStamp version)
        {
            return this.state.TryGetTextVersion(out version);
        }

        /// <summary>
        /// Get the current syntax tree for the document if the text is already loaded and the tree is already parsed.
        /// Returns true if the syntax tree is already available, or false if getting the syntax tree would have incurred additional work.
        /// </summary>
        public bool TryGetSyntaxTree(out SyntaxTree syntaxTree)
        {
            // if we already have cache, use it
            if (this.syntaxTreeResultTask != null)
            {
                syntaxTree = this.syntaxTreeResultTask.Result;
            }

            if (!this.state.TryGetSyntaxTree(out syntaxTree))
            {
                return false;
            }

            // cache the result if it is not already cached
            if (this.syntaxTreeResultTask == null)
            {
                var result = Task.FromResult(syntaxTree);
                Interlocked.CompareExchange(ref syntaxTreeResultTask, result, null);
            }

            return true;
        }

        /// <summary>
        /// Get the current syntax tree version for the document if the text is already loaded and the tree is already parsed.
        /// Returns true if the syntax tree is already available, or false if getting the syntax tree would have incurred additional work.
        /// </summary>
        public bool TryGetSyntaxVersion(out VersionStamp version)
        {
            version = default(VersionStamp);

            VersionStamp textVersion;
            if (!this.TryGetTextVersion(out textVersion))
            {
                return false;
            }

            var projectVersion = this.Project.Version;
            version = textVersion.GetNewerVersion(projectVersion);
            return true;
        }

        /// <summary>
        /// Gets the version of the document's top level signature if it is already loaded and available.
        /// </summary>
        internal bool TryGetTopLevelChangeTextVersion(out VersionStamp version)
        {
            return this.state.TryGetTopLevelChangeTextVersion(out version);
        }

        /// <summary>
        /// Gets the current text for the document asynchronously.
        /// </summary>
        public Task<SourceText> GetTextAsync(CancellationToken cancellationToken = default(CancellationToken))
        {
            return this.state.GetTextAsync(cancellationToken);
        }

        /// <summary>
        /// Gets the version of the document's text.
        /// </summary>
        public Task<VersionStamp> GetTextVersionAsync(CancellationToken cancellationToken = default(CancellationToken))
        {
            return this.state.GetTextVersionAsync(cancellationToken);
        }

        /// <summary>
        /// Gets the version of the syntax tree. This is generally the newer of the text version and the project's version.
        /// </summary>
        public async Task<VersionStamp> GetSyntaxVersionAsync(CancellationToken cancellationToken = default(CancellationToken))
        {
            var textVersion = await this.GetTextVersionAsync(cancellationToken).ConfigureAwait(false);
            var projectVersion = this.Project.Version;
            return textVersion.GetNewerVersion(projectVersion);
        }

        /// <summary>
        /// Gets the version of the document's top level signature.
        /// </summary>
        internal Task<VersionStamp> GetTopLevelChangeTextVersionAsync(CancellationToken cancellationToken = default(CancellationToken))
        {
            return this.state.GetTopLevelChangeTextVersionAsync(cancellationToken);
        }

        /// <summary>
        /// Gets the SyntaxTree for this document asynchronously.
        /// </summary>
        public Task<SyntaxTree> GetSyntaxTreeAsync(CancellationToken cancellationToken = default(CancellationToken))
        {
            if (syntaxTreeResultTask != null)
            {
                return syntaxTreeResultTask;
            }

            // First seee if we already have a semantic model computed.  If so, we can just return
            // that syntax tree.
            SemanticModel semanticModel;
            if (TryGetSemanticModel(out semanticModel))
            {
                // PERF: This is a hot code path, so cache the result to reduce allocations
                var result = Task.FromResult(semanticModel.SyntaxTree);
                Interlocked.CompareExchange(ref syntaxTreeResultTask, result, null);
                return syntaxTreeResultTask;
            }

            // second, see whether we already computed the tree, if we already did, return the cache
            SyntaxTree tree;
            if (TryGetSyntaxTree(out tree))
            {
                if (this.syntaxTreeResultTask == null)
                {
                    var result = Task.FromResult(tree);
                    Interlocked.CompareExchange(ref syntaxTreeResultTask, result, null);
                }

                return syntaxTreeResultTask;
            }

            // we can't cache this result, since internally it uses AsyncLazy which
            // care about cancellation token
            return this.state.GetSyntaxTreeAsync(cancellationToken);
        }

        /// <summary>
        /// Gets the root node of the current syntax tree if it is available.
        /// </summary>
        public bool TryGetSyntaxRoot(out SyntaxNode root)
        {
            root = null;
            SyntaxTree tree;
            return this.TryGetSyntaxTree(out tree) && tree.TryGetRoot(out root);
        }

        /// <summary>
        /// Gets the root node of the syntax tree asynchronously.
        /// </summary>
        public async Task<SyntaxNode> GetSyntaxRootAsync(CancellationToken cancellationToken = default(CancellationToken))
        {
            var tree = await this.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
            return await tree.GetRootAsync(cancellationToken).ConfigureAwait(false);
        }

        /// <summary>
        /// Gets the current semantic model for this document if the model is already computed.
        /// </summary>
        public bool TryGetSemanticModel(out SemanticModel semanticModel)
        {
            semanticModel = null;
            return this.model != null && this.model.TryGetTarget(out semanticModel);
        }

        /// <summary>
        /// Gets the semantic model for this document asynchronously.
        /// </summary>
        public async Task<SemanticModel> GetSemanticModelAsync(CancellationToken cancellationToken = default(CancellationToken))
        {
            SemanticModel semanticModel;
            if (this.TryGetSemanticModel(out semanticModel))
            {
                return semanticModel;
            }

            var syntaxTree = await this.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
            var compilation = await this.Project.GetCompilationAsync(cancellationToken).ConfigureAwait(false);

            var result = compilation.GetSemanticModel(syntaxTree);

            // first try set the cache if it has not been set
            var original = Interlocked.CompareExchange(ref this.model, new WeakReference<SemanticModel>(result), null);

            // okay, it is first time.
            if (original == null)
            {
                return result;
            }

            // it looks like someone has set it. try to reuse same semantic model
            if (original.TryGetTarget(out semanticModel))
            {
                return semanticModel;
            }

            // it looks like cache is gone. reset the cache.
            original.SetTarget(result);
            return result;
        }

        /// <summary>
        /// Creates a new instance of this document updated to have the source code kind specified.
        /// </summary>
        public Document WithSourceCodeKind(SourceCodeKind kind)
        {
            return this.Project.Solution.WithDocumentSourceCodeKind(this.Id, kind).GetDocument(this.Id);
        }

        /// <summary>
        /// Creates a new instance of this document updated to have the text specified.
        /// </summary>
        public Document WithText(SourceText text)
        {
            return this.Project.Solution.WithDocumentText(this.Id, text, PreservationMode.PreserveIdentity).GetDocument(this.Id);
        }

        /// <summary>
        /// Creates a new instance of this document updated to have a syntax tree rooted by the specified syntax node.
        /// </summary>
        public Document WithSyntaxRoot(SyntaxNode root)
        {
            return this.Project.Solution.WithDocumentSyntaxRoot(this.Id, root, PreservationMode.PreserveIdentity).GetDocument(this.Id);
        }

        /// <summary>
        /// Get the text changes between this document and a prior version of the same document.
        /// The changes, when applied to the text of the old document, will produce the text of the current document.
        /// </summary>
        public async Task<IEnumerable<TextChange>> GetTextChangesAsync(Document oldDocument, CancellationToken cancellationToken = default(CancellationToken))
        {
            try
            {
                using (Logger.LogBlock(FeatureId.Document, FunctionId.Document_GetTextChanges, this.Name, cancellationToken))
                {
                    if (oldDocument == this)
                    {
                        // no changes
                        return SpecializedCollections.EmptyEnumerable<TextChange>();
                    }

                    if (this.Id != oldDocument.Id)
                    {
                        throw new ArgumentException(WorkspacesResources.DocumentVersionIsDifferent);
                    }

                    // first try to see if text already knows its changes
                    IList<TextChange> textChanges = null;

                    SourceText text;
                    SourceText oldText;
                    if (this.TryGetText(out text) && oldDocument.TryGetText(out oldText))
                    {
                        if (text == oldText)
                        {
                            return SpecializedCollections.EmptyEnumerable<TextChange>();
                        }

                        var container = text.Container;
                        if (container != null)
                        {
                            textChanges = text.GetTextChanges(oldText).ToList();

                            // if changes are significant (not the whole document being replaced) then use these changes
                            if (textChanges.Count > 1 || (textChanges.Count == 1 && textChanges[0].Span != new TextSpan(0, oldText.Length)))
                            {
                                return textChanges;
                            }
                        }
                    }

                    // get changes by diffing the trees
                    SyntaxTree tree = await this.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
                    SyntaxTree oldTree = await oldDocument.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);

                    return tree.GetChanges(oldTree);
                }
            }
            catch (Exception e) if (ExceptionHelpers.CrashUnlessCanceled(e))
            {
                throw ExceptionUtilities.Unreachable;
            }
        }

        /// <summary>
        /// Creates a branched version of this document that has its semantic model frozen in whatever state it is available at the time,
        /// assuming a background process is constructing the semantics asynchronously. Repeated calls to this method may return
        /// documents with increasingly more complete semantics.
        /// 
        /// Use this method to gain access to potentially incomplete semantics quickly.
        /// </summary>
        internal async Task<Document> WithFrozenPartialSemanticsAsync(CancellationToken cancellationToken)
        {
            var solution = this.Project.Solution;
            var workspace = solution.Workspace;

            // only produce doc with frozen semantics if this document is part of the workspace's primary branch and there is actual background compilation going on,
            // since w/o background compilation the semantics won't be moving toward completeness.
            if (solution.BranchId == workspace.PrimaryBranchId && workspace.SupportsPartialSemantics)
            {
                var newSolution = await this.Project.Solution.WithFrozenPartialCompilationIncludingSpecificDocumentAsync(this.Id, cancellationToken).ConfigureAwait(false);
                return newSolution.GetDocument(this.Id);
            }
            else
            {
                return this;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        private string DebuggerDisplay
        {
            get
            {
                return this.Name;
            }
        }
    }
}