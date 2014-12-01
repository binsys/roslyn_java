﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{
    /// <summary>
    /// A class that represents all the arguments necessary to create a new project instance.
    /// </summary>
    public sealed class ProjectInfo
    {
        /// <summary>
        /// The unique Id of the project.
        /// </summary>
        public ProjectId Id { get; private set; }

        /// <summary>
        /// The version of the project.
        /// </summary>
        public VersionStamp Version { get; private set; }

        /// <summary>
        /// The name of the project. This may differ from the project's filename.
        /// </summary>
        public string Name { get; private set; }

        /// <summary>
        /// The name of the assembly that this project will create, without file extension.
        /// </summary>,
        public string AssemblyName { get; private set; }

        /// <summary>
        /// The language of the project.
        /// </summary>
        public string Language { get; private set; }

        /// <summary>
        /// The path to the project file or null if there is no project file.
        /// </summary>
        public string FilePath { get; private set; }

        /// <summary>
        /// The path to the output file (module or assembly).
        /// </summary>
        public string OutputFilePath { get; private set; }

        /// <summary>
        /// The initial compilation options for the project.
        /// </summary>
        public CompilationOptions CompilationOptions { get; private set; }

        /// <summary>
        /// The initial parse options for the source code documents in this project.
        /// </summary>
        public ParseOptions ParseOptions { get; private set; }

        /// <summary>
        /// The list of source documents initially associated with the project.
        /// </summary>
        public IReadOnlyList<DocumentInfo> Documents { get; private set; }

        /// <summary>
        /// The project references initially defined for the project.
        /// </summary>
        public IReadOnlyList<ProjectReference> ProjectReferences { get; private set; }

        /// <summary>
        /// The metadata references initially defined for the project.
        /// </summary>
        public IReadOnlyList<MetadataReference> MetadataReferences { get; private set; }
        
        /// <summary>
        /// True if this is a submission project for interactive sessions.
        /// </summary>
        public bool IsSubmission { get; private set; }

        /// <summary>
        /// Type of the host object.
        /// </summary>
        public Type HostObjectType { get; private set; }

        private ProjectInfo(
            ProjectId id,
            VersionStamp version,
            string name,
            string assemblyName,
            string language,
            string filePath,
            string outputFilePath,
            CompilationOptions compilationOptions,
            ParseOptions parseOptions,
            IEnumerable<DocumentInfo> documents,
            IEnumerable<ProjectReference> projectReferences,
            IEnumerable<MetadataReference> metadataReferences,
            bool isSubmission,
            Type hostObjectType)
        {
            if (id == null)
            {
                throw new ArgumentNullException("id");
            }

            if (name == null)
            {
                throw new ArgumentNullException("displayName");
            }

            if (assemblyName == null)
            {
                throw new ArgumentNullException("assemblyName");
            }

            if (language == null)
            {
                throw new ArgumentNullException("language");
            }

            this.Id = id;
            this.Version = version;
            this.Name = name;
            this.AssemblyName = assemblyName;
            this.Language = language;
            this.FilePath = filePath;
            this.OutputFilePath = outputFilePath;
            this.CompilationOptions = compilationOptions;
            this.ParseOptions = parseOptions;
            this.Documents = documents.ToImmutableListOrEmpty();
            this.ProjectReferences = projectReferences.ToImmutableListOrEmpty();
            this.MetadataReferences = metadataReferences.ToImmutableListOrEmpty();
            this.IsSubmission = isSubmission;
            this.HostObjectType = hostObjectType;
        }

        /// <summary>
        /// Create a new instance of a ProjectInfo.
        /// </summary>
        public static ProjectInfo Create(
            ProjectId id,
            VersionStamp version,
            string name,
            string assemblyName,
            string language,
            string filePath = null,
            string outputFilePath = null,
            CompilationOptions compilationOptions = null,
            ParseOptions parseOptions = null,
            IEnumerable<DocumentInfo> documents = null,
            IEnumerable<ProjectReference> projectReferences = null,
            IEnumerable<MetadataReference> metadataReferences = null,
            bool isSubmission = false,
            Type hostObjectType = null)
        {
            return new ProjectInfo(
                id,
                version,
                name,
                assemblyName,
                language,
                filePath,
                outputFilePath,
                compilationOptions,
                parseOptions,
                documents,
                projectReferences,
                metadataReferences,
                isSubmission,
                hostObjectType);
        }

        private ProjectInfo With(
            ProjectId id = null,
            VersionStamp? version = default(VersionStamp?),
            string name = null,
            string assemblyName = null,
            string language = null,
            Optional<string> filePath = default(Optional<string>),
            Optional<string> outputPath = default(Optional<string>),
            CompilationOptions compilationOptions = null,
            ParseOptions parseOptions = null,
            IEnumerable<DocumentInfo> documents = null,
            IEnumerable<ProjectReference> projectReferences = null,
            IEnumerable<MetadataReference> metadataReferences = null,
            Optional<bool> isSubmission = default(Optional<bool>),
            Optional<Type> hostObjectType = default(Optional<Type>))
        {
            var newId = id ?? this.Id;
            var newVersion = version.HasValue ? version.Value : this.Version;
            var newName = name ?? this.Name;
            var newAssemblyName = assemblyName ?? this.AssemblyName;
            var newLanguage = language ?? this.Language;
            var newFilepath = filePath.HasValue ? filePath.Value : this.FilePath;
            var newOutputPath = outputPath.HasValue ? outputPath.Value : this.OutputFilePath;
            var newCompilationOptions = compilationOptions ?? this.CompilationOptions;
            var newParseOptions = parseOptions ?? this.ParseOptions;
            var newDocuments = documents ?? this.Documents;
            var newProjectReferences = projectReferences ?? this.ProjectReferences;
            var newMetadataReferences = metadataReferences ?? this.MetadataReferences;
            var newIsSubmission = isSubmission.HasValue ? isSubmission.Value : this.IsSubmission;
            var newHostObjectType = hostObjectType.HasValue ? hostObjectType.Value : this.HostObjectType;

            if (newId == this.Id &&
                newVersion == this.Version &&
                newName == this.Name &&
                newAssemblyName == this.AssemblyName &&
                newLanguage == this.Language &&
                newFilepath == this.FilePath &&
                newOutputPath == this.OutputFilePath &&
                newCompilationOptions == this.CompilationOptions &&
                newParseOptions == this.ParseOptions &&
                newDocuments == this.Documents &&
                newProjectReferences == this.ProjectReferences &&
                newMetadataReferences == this.MetadataReferences &&
                newIsSubmission == this.IsSubmission &&
                newHostObjectType == this.HostObjectType)
            {
                return this;
            }

            return new ProjectInfo(
                    newId,
                    newVersion,
                    newName,
                    newAssemblyName,
                    newLanguage,
                    newFilepath,
                    newOutputPath,
                    newCompilationOptions,
                    newParseOptions,
                    newDocuments,
                    newProjectReferences,
                    newMetadataReferences,
                    newIsSubmission,
                    newHostObjectType);
        }

        public ProjectInfo WithDocuments(IEnumerable<DocumentInfo> documents)
        {
            return this.With(documents: documents);
        }

        public ProjectInfo WithVersion(VersionStamp version)
        {
            return this.With(version: version);
        }

        public ProjectInfo WithAssemblyName(string assemblyName)
        {
            return this.With(assemblyName: assemblyName);
        }

        public ProjectInfo WithOutputFilePath(string outputFilePath)
        {
            return this.With(outputPath: outputFilePath);
        }

        public ProjectInfo WithCompilationOptions(CompilationOptions compilationOptions)
        {
            return this.With(compilationOptions: compilationOptions);
        }

        public ProjectInfo WithParseOptions(ParseOptions parseOptions)
        {
            return this.With(parseOptions: parseOptions);
        }

        public ProjectInfo WithProjectReferences(IEnumerable<ProjectReference> projectReferences)
        {
            return this.With(projectReferences: projectReferences);
        }

        public ProjectInfo WithMetadataReferences(IEnumerable<MetadataReference> metadataReferences)
        {
            return this.With(metadataReferences: metadataReferences);
        }
    }
}