﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.LanguageServices;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Collections.Immutable;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{
    /// <summary>
    /// Represents a project that is part of a solution.
    /// </summary>
    [DebuggerDisplay("{DebuggerDisplay,nq}")]
    public partial class Project
    {
        private readonly Solution solution;
        private readonly ProjectState projectState;
        private ImmutableHashMap<DocumentId, Document> idToDocumentMap = ImmutableHashMap<DocumentId, Document>.Empty;

        internal Project(Solution solution, ProjectState projectState)
        {
            Contract.ThrowIfNull(solution);
            Contract.ThrowIfNull(projectState);

            this.solution = solution;
            this.projectState = projectState;
        }

        internal ProjectState State
        {
            get { return this.projectState; }
        }

        /// <summary>
        /// The solution this project is part of.
        /// </summary>
        public Solution Solution
        {
            get
            {
                return this.solution;
            }
        }

        /// <summary>
        /// The ID of the project. Multiple IProject instances may share the same ID. However, only
        /// one project may have this ID in any given solution.
        /// </summary>
        public ProjectId Id
        {
            get
            {
                return projectState.Id;
            }
        }

        /// <summary>
        /// The path to the project file or null if there is no project file.
        /// </summary>
        public string FilePath
        {
            get
            {
                return projectState.FilePath;
            }
        }

        /// <summary>
        /// The path to the output file, or null if it is not known.
        /// </summary>
        public string OutputFilePath
        {
            get
            {
                return projectState.OutputFilePath;
            }
        }

        internal ILanguageServiceProvider GetLanguageServiceProviderInternal()
        {
            return projectState.LanguageServices;
        }

        /// <summary>
        /// The language associated with the project.
        /// </summary>
        public string Language
        {
            get
            {
                return projectState.LanguageServices.Language;
            }
        }

        /// <summary>
        /// The name of the assembly this project represents.
        /// </summary>
        public string AssemblyName
        {
            get
            {
                return projectState.AssemblyName;
            }
        }

        /// <summary>
        /// The name of the project. This may be different than the assembly name.
        /// </summary>
        public string Name
        {
            get
            {
                return projectState.Name;
            }
        }

        /// <summary>
        /// The list of all other metadata sources (assemblies) that this project references.
        /// </summary>
        public IReadOnlyList<MetadataReference> MetadataReferences
        {
            get
            {
                return this.projectState.MetadataReferences;
            }
        }

        /// <summary>
        /// The list of all other projects within the same solution that this project references.
        /// </summary>
        public IEnumerable<ProjectReference> ProjectReferences
        {
            get
            {
                return this.projectState.ProjectReferences.Where(pr => this.Solution.ContainsProject(pr.ProjectId));
            }
        }

        /// <summary>
        /// The list of all other projects that this project references, including projects that 
        /// are not part of the solution.
        /// </summary>
        public IReadOnlyList<ProjectReference> AllProjectReferences
        {
            get
            {
                return this.projectState.ProjectReferences;
            }
        }

        /// <summary>
        /// The options used when building the compilation for this project.
        /// </summary>
        public CompilationOptions CompilationOptions
        {
            get
            {
                return this.projectState.CompilationOptions;
            }
        }

        /// <summary>
        /// The options used when parsing documents for this project.
        /// </summary>
        public ParseOptions ParseOptions
        {
            get
            {
                return this.projectState.ParseOptions;
            }
        }

        /// <summary>
        /// Returns true if this is a submission project.
        /// </summary>
        public bool IsSubmission
        {
            get
            {
                return this.projectState.IsSubmission;
            }
        }

        /// <summary>
        /// True if the project has any documents.
        /// </summary>
        public bool HasDocuments
        {
            get
            {
                return this.projectState.HasDocuments;
            }
        }

        /// <summary>
        /// All the document IDs associated with this project.
        /// </summary>
        public IReadOnlyList<DocumentId> DocumentIds
        {
            get
            {
                return projectState.DocumentIds;
            }
        }

        /// <summary>
        /// All the documents associated with this project.
        /// </summary>
        public IEnumerable<Document> Documents
        {
            get
            {
                return projectState.DocumentIds.Select(GetDocument);
            }
        }

        /// <summary>
        /// True if the project contains a document with the specified ID.
        /// </summary>
        public bool ContainsDocument(DocumentId documentId)
        {
            return this.projectState.ContainsDocument(documentId);
        }

        /// <summary>
        /// Get the documentId in this project with the specified syntax tree.
        /// </summary>
        public DocumentId GetDocumentId(SyntaxTree syntaxTree)
        {
            return this.solution.GetDocumentId(syntaxTree, this.Id);
        }

        /// <summary>
        /// Get the document in this project with the specified syntax tree.
        /// </summary>
        public Document GetDocument(SyntaxTree syntaxTree)
        {
            return this.solution.GetDocument(syntaxTree, this.Id);
        }

        /// <summary>
        /// Get the document in this project with the specified document Id.
        /// </summary>
        public Document GetDocument(DocumentId documentId)
        {
            if (!ContainsDocument(documentId))
            {
                return null;
            }

            return ImmutableHashMapExtensions.GetOrAdd(ref this.idToDocumentMap, documentId, createDocumentFunction, this);
        }

        internal DocumentState GetDocumentState(DocumentId documentId)
        {
            return this.projectState.GetDocumentState(documentId);
        }

        private static readonly Func<DocumentId, Project, Document> createDocumentFunction = CreateDocument;
        private static Document CreateDocument(DocumentId documentId, Project project)
        {
            return new Document(project, project.projectState.GetDocumentState(documentId));
        }

        /// <summary>
        /// Get the compilation for this project if it is available.
        /// </summary>
        public bool TryGetCompilation(out Compilation compilation)
        {
            return this.solution.TryGetCompilation(this.Id, out compilation);
        }

        /// <summary>
        /// Get the compilation for this project asynchronously.
        /// </summary>
        public Task<Compilation> GetCompilationAsync(CancellationToken cancellationToken = default(CancellationToken))
        {
            return this.solution.GetCompilationAsync(this.Id, cancellationToken);
        }

        /// <summary>
        /// Gets an object that lists the added, changed and removed documents between this project and the specified project.
        /// </summary>
        public ProjectChanges GetChanges(Project oldProject)
        {
            if (oldProject == null)
            {
                throw new ArgumentNullException("oldProject");
            }

            var op = oldProject as Project;
            if (op == null)
            {
                throw new ArgumentException("oldProject");
            }

            return new ProjectChanges(this, op);
        }

        private void CheckContainsDocument(DocumentId documentId)
        {
            if (!this.ContainsDocument(documentId))
            {
                throw new InvalidOperationException(WorkspacesResources.DocumentNotInProject);
            }
        }

        /// <summary>
        /// The project version. This equates to the version of the project file.
        /// </summary>
        public VersionStamp Version
        {
            get
            {
                return this.projectState.Version;
            }
        }

        /// <summary>
        /// The version of the most recently modified document.
        /// </summary>
        public Task<VersionStamp> GetLatestDocumentVersionAsync(CancellationToken cancellationToken = default(CancellationToken))
        {
            return this.projectState.GetLatestDocumentVersionAsync(cancellationToken);
        }

        /// <summary>
        /// The most recent version of the project, its documents and all dependent projects and documents.
        /// </summary>
        public Task<VersionStamp> GetDependentVersionAsync(CancellationToken cancellationToken = default(CancellationToken))
        {
            return this.solution.GetDependentVersionAsync(this.Id, cancellationToken);
        }

        /// <summary>
        /// The semantic version of this project including the semantics of referenced projects.
        /// This version changes whenever the consumable declarations of this project and/or projects it depends on change.
        /// </summary>
        public Task<VersionStamp> GetDependentSemanticVersionAsync(CancellationToken cancellationToken = default(CancellationToken))
        {
            return this.solution.GetDependentSemanticVersionAsync(this.Id, cancellationToken);
        }

        /// <summary>
        /// The semantic version of this project not including the semantics of referenced projects.
        /// This version changes only when the consumable declarations of this project change.
        /// </summary>
        public async Task<VersionStamp> GetSemanticVersionAsync(CancellationToken cancellationToken = default(CancellationToken))
        {
            var projVersion = this.Version;
            var docVersion = await this.projectState.GetLatestDocumentTopLevelChangeVersionAsync(cancellationToken).ConfigureAwait(false);
            return docVersion.GetNewerVersion(projVersion);
        }

        /// <summary>
        /// Creates a new instance of this project updated to have the new assembly name.
        /// </summary>
        public Project WithAssemblyName(string assemblyName)
        {
            return this.Solution.WithProjectAssemblyName(this.Id, assemblyName).GetProject(this.Id);
        }

        /// <summary>
        /// Creates a new instance of this project updated to have the specified compilation options.
        /// </summary>
        public Project WithCompilationOptions(CompilationOptions options)
        {
            return this.Solution.WithProjectCompilationOptions(this.Id, options).GetProject(this.Id);
        }

        /// <summary>
        /// Creates a new instance of this project updated to have the specified parse options.
        /// </summary>
        public Project WithParseOptions(ParseOptions options)
        {
            return this.Solution.WithProjectParseOptions(this.Id, options).GetProject(this.Id);
        }

        /// <summary>
        /// Creates a new instance of this project updated to include the specified project reference.
        /// </summary>
        public Project AddProjectReference(ProjectReference projectReference)
        {
            return this.Solution.AddProjectReference(this.Id, projectReference).GetProject(this.Id);
        }

        /// <summary>
        /// Creates a new instance of this project updated to include the specified project references.
        /// </summary>
        public Project AddProjectReferences(IEnumerable<ProjectReference> projectReferences)
        {
            return this.Solution.AddProjectReferences(this.Id, projectReferences).GetProject(this.Id);
        }

        /// <summary>
        /// Creates a new instance of this project updated to no longer include the specified project reference.
        /// </summary>
        public Project RemoveProjectReference(ProjectReference projectReference)
        {
            return this.Solution.RemoveProjectReference(this.Id, projectReference).GetProject(this.Id);
        }

        /// <summary>
        /// Creates a new instance of this project updated to contain no project references.
        /// </summary>
        public Project WithProjectReferences(IEnumerable<ProjectReference> projectReferences)
        {
            return this.Solution.WithProjectReferences(this.Id, projectReferences).GetProject(this.Id);
        }

        /// <summary>
        /// Creates a new instance of this project updated to include the specified metadata reference.
        /// </summary>
        public Project AddMetadataReference(MetadataReference metadataReference)
        {
            return this.Solution.AddMetadataReference(this.Id, metadataReference).GetProject(this.Id);
        }

        /// <summary>
        /// Creates a new instance of this project updated to include the specified metadata references.
        /// </summary>
        public Project AddMetadataReferences(IEnumerable<MetadataReference> metadataReferences)
        {
            return this.Solution.AddMetadataReferences(this.Id, metadataReferences).GetProject(this.Id);
        }

        /// <summary>
        /// Creates a new instance of this project updated to no longer include the specified metadata reference.
        /// </summary>
        public Project RemoveMetadataReference(MetadataReference metadataReference)
        {
            return this.Solution.RemoveMetadataReference(this.Id, metadataReference).GetProject(this.Id);
        }

        /// <summary>
        /// Creates a new instance of this project updated to include the specified metadata references.
        /// </summary>
        public Project WithMetadataReferences(IEnumerable<MetadataReference> metadataReferences)
        {
            return this.Solution.WithProjectMetadataReferences(this.Id, metadataReferences).GetProject(this.Id);
        }

        /// <summary>
        /// Creates a new document in a new instance of this project.
        /// </summary>
        public Document AddDocument(string name, SourceText text, IEnumerable<string> folders = null)
        {
            var id = DocumentId.CreateNewId(this.Id);
            return this.Solution.AddDocument(id, name, text, folders).GetDocument(id);
        }

        /// <summary>
        /// Creates a new document in a new instance of this project.
        /// </summary>
        public Document AddDocument(string name, string text, IEnumerable<string> folders = null)
        {
            var id = DocumentId.CreateNewId(this.Id, debugName: name);
            return this.Solution.AddDocument(id, name, text, folders).GetDocument(id);
        }

        /// <summary>
        /// Creates a new instance of this project updated to no longer include the specified document.
        /// </summary>
        public Project RemoveDocument(DocumentId documentId)
        {
            return this.Solution.RemoveDocument(documentId).GetProject(this.Id);
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