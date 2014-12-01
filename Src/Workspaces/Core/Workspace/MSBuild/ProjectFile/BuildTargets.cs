// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using MSB = Microsoft.Build;

namespace Microsoft.CodeAnalysis.MSBuild
{
    /// <summary>
    /// Defines a list of build targets and operations to modify that list.
    /// </summary>
    internal class BuildTargets
    {
        private MSB.Evaluation.Project project;
        private List<string> buildTargets;

        public BuildTargets(MSB.Evaluation.Project project, params string[] targets)
        {
            this.project = project;
            this.buildTargets = new List<string>();
            this.buildTargets.AddRange(targets);
        }

        public string[] Targets
        {
            get { return this.buildTargets.ToArray(); }
        }

        /// <summary>
        /// Remove the specified target from the build targets. 
        /// 
        /// If the target is nested (a dependent target) of one of the build targets, 
        /// promote the siblings of the target to the formal list (in execution order.)
        /// </summary>
        public void Remove(string target)
        {
            // keep a set of targets already known to be in the build target list so we can keep
            // from redundantly adding multiple of the same targets when we replace a target with
            // its children
            var knownTargets = new HashSet<string>();

            for (int i = 0; i < this.buildTargets.Count;)
            {
                var buildTarget = this.buildTargets[i];
                if (buildTarget == target)
                {
                    // we found it!
                    this.buildTargets.RemoveAt(i);

                    // it might exist multiple times
                    continue;
                }
                else if (DependsOn(buildTarget, target))
                {
                    // replace this build target with its children and check again
                    this.buildTargets.RemoveAt(i);

                    int loc = i;
                    foreach (var dependsOnTarget in GetTargetDependents(this.project, buildTarget))
                    {
                        if (!knownTargets.Contains(dependsOnTarget))
                        {
                            this.buildTargets.Insert(loc, dependsOnTarget);
                            loc++;
                        }
                    }

                    continue;
                }
                else
                {
                    knownTargets.Add(buildTarget);
                    i++;
                }
            }
        }

        /// <summary>
        /// Remove all targets after the specified target (and possibly also removing the specified target.)
        /// 
        /// If the a removed target is nested (a dependent target) of one of the build targets, 
        /// promote the siblings of the removed target to the formal list (in execution order.)
        /// </summary>
        public void RemoveAfter(string target, bool includeTargetInRemoval)
        {
            // keep a set of targets already known to be in the build target list
            // so we can keep from redundantly adding mulitple of the same targets
            // when we replace a target with its children
            var knownTargets = new HashSet<string>();

            bool found = false;

            for (int i = 0; i < this.buildTargets.Count;)
            {
                var buildTarget = this.buildTargets[i];
                if (found)
                {
                    this.buildTargets.RemoveAt(i);
                    continue;
                }
                else if (buildTarget == target)
                {
                    // we found it!
                    found = true;
                    if (!includeTargetInRemoval)
                    {
                        i++;
                    }
                }
                else if (DependsOn(buildTarget, target))
                {
                    // replace this build target with its children and check again
                    this.buildTargets.RemoveAt(i);

                    int loc = i;
                    foreach (var dependsOnTarget in GetTargetDependents(this.project, buildTarget))
                    {
                        if (!knownTargets.Contains(dependsOnTarget))
                        {
                            this.buildTargets.Insert(loc, dependsOnTarget);
                            loc++;
                        }
                    }

                    continue;
                }
                else
                {
                    knownTargets.Add(buildTarget);
                    i++;
                }
            }
        }

        private bool DependsOn(string target, string dependentTarget)
        {
            foreach (var dependsOnTarget in GetTargetDependents(this.project, target))
            {
                if (dependsOnTarget == dependentTarget || DependsOn(dependsOnTarget, dependentTarget))
                {
                    return true;
                }
            }

            return false;
        }

        private static readonly char[] targetsSplitChars = new char[] { ';', '\r', '\n', '\t', ' ' };

        private static IEnumerable<string> SplitTargets(string targets)
        {
            return targets.Split(targetsSplitChars, StringSplitOptions.RemoveEmptyEntries);
        }

        private static IEnumerable<string> GetTargetDependents(MSB.Evaluation.Project project, string targetName)
        {
            MSB.Execution.ProjectTargetInstance targetInstance;
            if (project.Targets.TryGetValue(targetName, out targetInstance))
            {
                return SplitTargets(project.ExpandString(targetInstance.DependsOnTargets));
            }
            else
            {
                return SpecializedCollections.EmptyEnumerable<string>();
            }
        }

        internal static IEnumerable<string> GetTopLevelTargets(MSB.Evaluation.Project project)
        {
            // start with set of all targets
            HashSet<string> targets = new HashSet<string>(project.Targets.Keys);

            // remove any target that another target depends on
            foreach (var target in project.Targets.Keys)
            {
                var dependents = GetTargetDependents(project, target).ToList();
                foreach (var depTarget in dependents)
                {
                    targets.Remove(depTarget);
                }
            }

            return targets;
        }
    }
}