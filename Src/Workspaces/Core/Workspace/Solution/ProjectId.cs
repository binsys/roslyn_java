﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{
    /// <summary>
    /// An identifier that can be used to refer to the same Project across versions.
    /// </summary>
    [DebuggerDisplay("{DebuggerText}")]
    [Serializable]
    public class ProjectId : IEquatable<ProjectId>
    {
        /// <summary>
        /// The SolutionId this project id is relative to.
        /// </summary>
        //// private SolutionId SolutionId { get; private set; }

        private string debugName;

        /// <summary>
        /// The system generated unique id.
        /// </summary>
        public Guid Id { get; private set; }

        private ProjectId(string debugName)
        {
            this.Id = Guid.NewGuid();
            this.debugName = debugName;
        }

        /// <summary>
        /// Create a new ProjectId instance.
        /// </summary>
        /// <param name="debugName">An optional name to make this id easier to recognize while debugging.</param>
        public static ProjectId CreateNewId(string debugName = null)
        {
            return new ProjectId(debugName);
        }

        private string DebuggerText
        {
            get { return string.Format("({0}, #{1} - {2})", this.GetType().Name, this.Id, this.debugName); }
        }

        public override string ToString()
        {
            return DebuggerText;
        }

        public override bool Equals(object obj)
        {
            return this.Equals(obj as ProjectId);
        }

        public bool Equals(ProjectId other)
        {
            return
                !ReferenceEquals(other, null) &&
                this.Id == other.Id;
        }

        public static bool operator ==(ProjectId left, ProjectId right)
        {
            return EqualityComparer<ProjectId>.Default.Equals(left, right);
        }

        public static bool operator !=(ProjectId left, ProjectId right)
        {
            return !(left == right);
        }

        public override int GetHashCode()
        {
            return this.Id.GetHashCode();
        }
    }
}