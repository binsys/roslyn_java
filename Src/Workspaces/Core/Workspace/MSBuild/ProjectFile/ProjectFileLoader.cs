// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using MSB = Microsoft.Build;

namespace Microsoft.CodeAnalysis.MSBuild
{
    internal abstract class ProjectFileLoader : IProjectFileLoader
    {
        public ProjectFileLoader()
        {
        }

        public abstract string Language { get; }
        public abstract bool IsProjectTypeGuid(Guid guid);
        public abstract bool IsProjectFileExtension(string fileExtension);

        protected abstract ProjectFile CreateProjectFile(MSB.Evaluation.Project loadedProject);

        public async Task<IProjectFile> LoadProjectFileAsync(string path, IDictionary<string, string> globalProperties, CancellationToken cancellationToken)
        {
            if (path == null)
            {
                throw new ArgumentNullException("path");
            }

            // load project file async
            var loadedProject = await LoadProjectAsync(path, globalProperties, cancellationToken).ConfigureAwait(false);

            return this.CreateProjectFile(loadedProject);
        }

        private static async Task<MSB.Evaluation.Project> LoadProjectAsync(string path, IDictionary<string, string> globalProperties, CancellationToken cancellationToken)
        {
            var properties = new Dictionary<string, string>(globalProperties ?? ImmutableDictionary<string, string>.Empty);
            properties["DesignTimeBuild"] = "true"; // this will tell msbuild to not build the dependent projects
            properties["BuildingInsideVisualStudio"] = "true"; // this will force CoreCompile task to execute even if all inputs and outputs are up to date

            var xmlReader = System.Xml.XmlReader.Create(await ReadFileAsync(path, cancellationToken).ConfigureAwait(false));
            var collection = new MSB.Evaluation.ProjectCollection();
            var xml = MSB.Construction.ProjectRootElement.Create(xmlReader, collection);

            // When constructing a project from an XmlReader, MSBuild cannot determine the project file path.  Setting the
            // path explicitly is necessary so that the reserved properties like $(MSBuildProjectDirectory) will work.
            xml.FullPath = path;

            return new MSB.Evaluation.Project(
                xml,
                globalProperties,
                toolsVersion: null,
                projectCollection: collection);
        }

        public static async Task<string> GetOutputFilePathAsync(string path, IDictionary<string, string> globalProperties, CancellationToken cancellationToken)
        {
            var project = await LoadProjectAsync(path, globalProperties, cancellationToken);
            return project.GetPropertyValue("TargetPath");
        }

        private static async Task<MemoryStream> ReadFileAsync(string path, CancellationToken cancellationToken)
        {
            MemoryStream memoryStream = new MemoryStream();
            var buffer = new byte[1024];
            using (var stream = File.OpenRead(path))
            {
                int bytesRead = 0;
                do
                {
                    bytesRead = await stream.ReadAsync(buffer, 0, buffer.Length, cancellationToken).ConfigureAwait(continueOnCapturedContext: false);
                    memoryStream.Write(buffer, 0, bytesRead);
                }
                while (bytesRead > 0);
            }

            memoryStream.Position = 0;
            return memoryStream;
        }
    }
}