// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Build.Execution;
using Microsoft.CodeAnalysis;
using Roslyn.Utilities;
using MSB = Microsoft.Build;

namespace Microsoft.CodeAnalysis.MSBuild
{
    internal abstract class ProjectFile : IProjectFile
    {
        private readonly ProjectFileLoader loader;
        private readonly MSB.Evaluation.Project loadedProject;

        public ProjectFile(ProjectFileLoader loader, MSB.Evaluation.Project loadedProject)
        {
            this.loader = loader;
            this.loadedProject = loadedProject;
        }

        ~ProjectFile()
        {
            try
            {
                // unload project so collection will release global strings
                loadedProject.ProjectCollection.UnloadAllProjects();
            }
            catch
            {
            }
        }

        public virtual string FilePath
        {
            get { return this.loadedProject.FullPath; }
        }

        public Guid Guid
        {
            get
            {
                var propertyValue = this.loadedProject.GetPropertyValue("ProjectGuid");
                Guid result;
                Guid.TryParse(propertyValue, out result);
                return result;
            }
        }

        public string GetPropertyValue(string name)
        {
            return this.loadedProject.GetPropertyValue(name);
        }

        public abstract SourceCodeKind GetSourceCodeKind(string documentFileName);
        public abstract string GetDocumentExtension(SourceCodeKind kind);
        public abstract Task<ProjectFileInfo> GetProjectFileInfoAsync(CancellationToken cancellationToken);

        protected class BuildResult
        {
            public readonly MSB.Execution.BuildResult Result;
            public readonly MSB.Execution.ProjectInstance Instance;

            internal BuildResult(MSB.Execution.BuildResult result, MSB.Execution.ProjectInstance instance)
            {
                this.Result = result;
                this.Instance = instance;
            }
        }

        protected async Task<BuildResult> BuildAsync(string taskName, MSB.Framework.ITaskHost taskHost, CancellationToken cancellationToken)
        {
            // prepare for building
            var buildTargets = new BuildTargets(loadedProject, "Compile");

            // Don't execute this one. It will build referenced projects.
            // Even when DesignTimeBuild is defined above, it will still add the referenced project's output to the references list
            // which we don't want.
            buildTargets.Remove("ResolveProjectReferences");

            // don't execute anything after CoreCompile target, since we've
            // already done everything we need to compute compiler inputs by then.
            buildTargets.RemoveAfter("CoreCompile", includeTargetInRemoval: false);

            // create a project instance to be executed by build engine.
            // The executed project will hold the final model of the project after execution via msbuild.
            var executedProject = loadedProject.CreateProjectInstance();

            var hostServices = new Microsoft.Build.Execution.HostServices();

            // connect the host "callback" object with the host services, so we get called back with the exact inputs to the compiler task.
            hostServices.RegisterHostObject(this.loadedProject.FullPath, "CoreCompile", taskName, taskHost);

            var buildParameters = new MSB.Execution.BuildParameters(loadedProject.ProjectCollection);

            var buildRequestData = new MSB.Execution.BuildRequestData(executedProject, buildTargets.Targets, hostServices);

            var result = await this.BuildAsync(buildParameters, buildRequestData, cancellationToken).ConfigureAwait(continueOnCapturedContext: false);

            return new BuildResult(result, executedProject);
        }

        // this lock is static because we are  using the default build manager, and there is only one per process
        private static readonly AsyncSemaphore buildManagerLock = new AsyncSemaphore(1);

        private MSB.Execution.BuildResult Build(MSB.Execution.BuildParameters parameters, MSB.Execution.BuildRequestData requestData, CancellationToken cancellationToken)
        {
            using (buildManagerLock.DisposableWait())
            {
                var buildManager = MSB.Execution.BuildManager.DefaultBuildManager;

                buildManager.BeginBuild(parameters);

                // enable cancellation of build
                CancellationTokenRegistration registration = default(CancellationTokenRegistration);
                if (cancellationToken.CanBeCanceled)
                {
                    registration = cancellationToken.Register(() =>
                    {
                        buildManager.CancelAllSubmissions();
                    });
                }

                // execute build sync
                try
                {
                    return buildManager.BuildRequest(requestData);
                }
                finally
                {
                    if (registration != default(CancellationTokenRegistration))
                    {
                        registration.Dispose();
                    }

                    buildManager.EndBuild();
                }
            }
        }

        private async Task<MSB.Execution.BuildResult> BuildAsync(MSB.Execution.BuildParameters parameters, MSB.Execution.BuildRequestData requestData, CancellationToken cancellationToken)
        {
            // only allow one build to use the default build manager at a time
            using (await buildManagerLock.DisposableWaitAsync(cancellationToken).ConfigureAwait(continueOnCapturedContext: false))
            {
                return await BuildAsync(MSB.Execution.BuildManager.DefaultBuildManager, parameters, requestData, cancellationToken).ConfigureAwait(continueOnCapturedContext: false);
            }
        }

        private static Task<MSB.Execution.BuildResult> BuildAsync(MSB.Execution.BuildManager buildManager, MSB.Execution.BuildParameters parameters, MSB.Execution.BuildRequestData requestData, CancellationToken cancellationToken)
        {
            var taskSource = new TaskCompletionSource<MSB.Execution.BuildResult>();

            buildManager.BeginBuild(parameters);

            // enable cancellation of build
            CancellationTokenRegistration registration = default(CancellationTokenRegistration);
            if (cancellationToken.CanBeCanceled)
            {
                registration = cancellationToken.Register(() =>
                {
                    try
                    {
                        buildManager.CancelAllSubmissions();
                        buildManager.EndBuild();
                        registration.Dispose();
                    }
                    finally
                    {
                        taskSource.TrySetCanceled();
                    }
                });
            }

            // execute build async
            try
            {
                buildManager.PendBuildRequest(requestData).ExecuteAsync(sub =>
                {
                    // when finished
                    try
                    {
                        var result = sub.BuildResult;
                        buildManager.EndBuild();
                        registration.Dispose();
                        taskSource.TrySetResult(result);
                    }
                    catch (Exception e)
                    {
                        taskSource.TrySetException(e);
                    }
                }, null);
            }
            catch (Exception e)
            {
                taskSource.SetException(e);
            }

            return taskSource.Task;
        }

        protected virtual string GetTargetPath()
        {
            return this.GetAbsolutePath(this.loadedProject.GetPropertyValue("TargetPath"));
        }

        protected virtual string GetAssemblyName()
        {
            return PathUtilities.GetFileName(this.loadedProject.GetPropertyValue("AssemblyName"));
        }

        protected virtual IEnumerable<ProjectFileReference> GetProjectReferences(MSB.Execution.ProjectInstance executedProject)
        {
            return executedProject.GetItems("ProjectReference")
                        .Select(reference => new ProjectFileReference(Guid.Parse(reference.GetMetadataValue("Project")), reference.EvaluatedInclude))
                        .ToImmutableList();
        }

        protected virtual IEnumerable<MSB.Framework.ITaskItem> GetDocumentsFromModel(MSB.Execution.ProjectInstance executedProject)
        {
            return executedProject.GetItems("Compile");
        }

        protected virtual IEnumerable<MSB.Framework.ITaskItem> GetMetadataReferencesFromModel(MSB.Execution.ProjectInstance executedProject)
        {
            return executedProject.GetItems("ReferencePath");
        }

        public MSB.Evaluation.ProjectProperty GetProperty(string name)
        {
            return this.loadedProject.GetProperty(name);
        }

        protected IEnumerable<MSB.Framework.ITaskItem> GetTaskItems(MSB.Execution.ProjectInstance executedProject, string itemType)
        {
            return executedProject.GetItems(itemType);
        }

        protected string GetItemString(MSB.Execution.ProjectInstance executedProject, string itemType)
        {
            string text = "";
            foreach (var item in executedProject.GetItems(itemType))
            {
                if (text.Length > 0)
                {
                    text = text + " ";
                }

                text = text + item.EvaluatedInclude;
            }

            return text;
        }

        protected string ReadPropertyString(MSB.Execution.ProjectInstance executedProject, string propertyName)
        {
            return this.ReadPropertyString(executedProject, propertyName, propertyName);
        }

        protected string ReadPropertyString(MSB.Execution.ProjectInstance executedProject, string executedPropertyName, string evaluatedPropertyName)
        {
            var executedProperty = executedProject.GetProperty(executedPropertyName);
            if (executedProperty != null)
            {
                return executedProperty.EvaluatedValue;
            }

            var evaluatedProperty = this.loadedProject.GetProperty(evaluatedPropertyName);
            if (evaluatedProperty != null)
            {
                return evaluatedProperty.EvaluatedValue;
            }

            return null;
        }

        protected bool ReadPropertyBool(MSB.Execution.ProjectInstance executedProject, string propertyName)
        {
            return ConvertToBool(ReadPropertyString(executedProject, propertyName));
        }

        protected bool ReadPropertyBool(MSB.Execution.ProjectInstance executedProject, string executedPropertyName, string evaluatedPropertyName)
        {
            return ConvertToBool(ReadPropertyString(executedProject, executedPropertyName, evaluatedPropertyName));
        }

        private static bool ConvertToBool(string value)
        {
            return value != null && (string.Equals("true", value, StringComparison.OrdinalIgnoreCase) ||
                string.Equals("On", value, StringComparison.OrdinalIgnoreCase));
        }

        protected int ReadPropertyInt(MSB.Execution.ProjectInstance executedProject, string propertyName)
        {
            return ConvertToInt(ReadPropertyString(executedProject, propertyName));
        }

        protected int ReadPropertyInt(MSB.Execution.ProjectInstance executedProject, string executedPropertyName, string evaluatedPropertyName)
        {
            return ConvertToInt(ReadPropertyString(executedProject, executedPropertyName, evaluatedPropertyName));
        }

        private static int ConvertToInt(string value)
        {
            if (value == null)
            {
                return 0;
            }
            else
            {
                int result;
                int.TryParse(value, out result);
                return result;
            }
        }

        protected ulong ReadPropertyULong(MSB.Execution.ProjectInstance executedProject, string propertyName)
        {
            return ConvertToULong(ReadPropertyString(executedProject, propertyName));
        }

        protected ulong ReadPropertyULong(MSB.Execution.ProjectInstance executedProject, string executedPropertyName, string evaluatedPropertyName)
        {
            return ConvertToULong(this.ReadPropertyString(executedProject, executedPropertyName, evaluatedPropertyName));
        }

        private static ulong ConvertToULong(string value)
        {
            if (value == null)
            {
                return 0;
            }
            else
            {
                ulong result;
                ulong.TryParse(value, out result);
                return result;
            }
        }

        protected TEnum? ReadPropertyEnum<TEnum>(MSB.Execution.ProjectInstance executedProject, string propertyName)
            where TEnum : struct
        {
            return ConvertToEnum<TEnum>(ReadPropertyString(executedProject, propertyName));
        }

        protected TEnum? ReadPropertyEnum<TEnum>(MSB.Execution.ProjectInstance executedProject, string executedPropertyName, string evaluatedPropertyName)
            where TEnum : struct
        {
            return ConvertToEnum<TEnum>(ReadPropertyString(executedProject, executedPropertyName, evaluatedPropertyName));
        }

        private static TEnum? ConvertToEnum<TEnum>(string value)
            where TEnum : struct
        {
            if (value == null)
            {
                return null;
            }
            else
            {
                TEnum result;
                if (Enum.TryParse<TEnum>(value, out result))
                {
                    return result;
                }
                else
                {
                    return null;
                }
            }
        }

        /// <summary>
        /// Resolves the given path that is possibly relative to the project directory.
        /// </summary>
        /// <remarks>
        /// The resulting path is absolute but might not be normalized.
        /// </remarks>
        protected string GetAbsolutePath(string path)
        {
            // TODO (tomat): should we report an error when drive-relative path (e.g. "C:foo.cs") is encountered?
            return Path.GetFullPath(FileUtilities.ResolveRelativePath(path, this.loadedProject.DirectoryPath) ?? path);
        }

        protected string GetDocumentFilePath(MSB.Framework.ITaskItem documentItem)
        {
            return GetAbsolutePath(documentItem.ItemSpec);
        }

        protected static bool IsDocumentLinked(MSB.Framework.ITaskItem documentItem)
        {
            return !string.IsNullOrEmpty(documentItem.GetMetadata("Link"));
        }

        private IDictionary<string, MSB.Evaluation.ProjectItem> documents = null;

        protected bool IsDocumentGenerated(MSB.Framework.ITaskItem documentItem)
        {
            if (this.documents == null)
            {
                this.documents = new Dictionary<string, MSB.Evaluation.ProjectItem>();
                foreach (var item in this.loadedProject.GetItems("compile"))
                {
                    this.documents[GetAbsolutePath(item.EvaluatedInclude)] = item;
                }
            }

            return !this.documents.ContainsKey(GetAbsolutePath(documentItem.ItemSpec));
        }

        protected static string GetDocumentLogicalPath(MSB.Framework.ITaskItem documentItem, string projectDirectory)
        {
            var link = documentItem.GetMetadata("Link");
            if (!string.IsNullOrEmpty(link))
            {
                // if a specific link is specified in the project file then use it to form the logical path.
                return link;
            }
            else
            {
                var result = documentItem.ItemSpec;
                if (Path.IsPathRooted(result))
                {
                    // If we have an absolute path, there are two possibilities:
                    result = Path.GetFullPath(result);

                    // If the document is within the current project directory (or subdirectory), then the logical path is the relative path 
                    // from the project's directory.
                    if (result.StartsWith(projectDirectory, StringComparison.OrdinalIgnoreCase))
                    {
                        result = result.Substring(projectDirectory.Length);
                    }
                    else
                    {
                        // if the document lies outside the project's directory (or subdirectory) then place it logically at the root of the project.
                        // if more than one document ends up with the same logical name then so be it (the workspace will survive.)
                        return Path.GetFileName(result);
                    }
                }

                return result;
            }
        }

        protected string GetReferenceFilePath(ProjectItemInstance projectItem)
        {
            return GetAbsolutePath(projectItem.EvaluatedInclude);
        }

        public void AddDocument(string filePath, string logicalPath = null)
        {
            var relativePath = FilePathUtilities.GetRelativePath(this.loadedProject.DirectoryPath, filePath);

            Dictionary<string, string> metadata = null;
            if (logicalPath != null && relativePath != logicalPath)
            {
                metadata = new Dictionary<string, string>();
                metadata.Add("link", logicalPath);
                relativePath = filePath; // link to full path
            }

            this.loadedProject.AddItem("Compile", relativePath, metadata);
        }

        public void RemoveDocument(string filePath)
        {
            var relativePath = FilePathUtilities.GetRelativePath(this.loadedProject.DirectoryPath, filePath);
            foreach (var item in this.loadedProject.GetItems("Compile").Where(i => i.EvaluatedInclude == relativePath).ToList())
            {
                this.loadedProject.RemoveItem(item);
            }
        }

        public void Save()
        {
            this.loadedProject.Save();
        }

        internal static bool TryGetOutputKind(string outputKind, out OutputKind kind)
        {
            if (string.Equals(outputKind, "Library", StringComparison.OrdinalIgnoreCase))
            {
                kind = OutputKind.DynamicallyLinkedLibrary;
                return true;
            }
            else if (string.Equals(outputKind, "Exe", StringComparison.OrdinalIgnoreCase))
            {
                kind = OutputKind.ConsoleApplication;
                return true;
            }
            else if (string.Equals(outputKind, "WinExe", StringComparison.OrdinalIgnoreCase))
            {
                kind = OutputKind.WindowsApplication;
                return true;
            }
            else if (string.Equals(outputKind, "Module", StringComparison.OrdinalIgnoreCase))
            {
                kind = OutputKind.NetModule;
                return true;
            }
            else if (string.Equals(outputKind, "WinMDObj", StringComparison.OrdinalIgnoreCase))
            {
                kind = OutputKind.WindowsRuntimeMetadata;
                return true;
            }
            else
            {
                kind = OutputKind.DynamicallyLinkedLibrary;
                return false;
            }
        }
    }
}
