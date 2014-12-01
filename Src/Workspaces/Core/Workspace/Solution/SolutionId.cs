﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis
{
    /// <summary>
    /// An identifier that can be used to refer to the same Solution across versions. 
    /// </summary>
    [DebuggerDisplay("{DebuggerText}")]
    [Serializable]
    public class SolutionId : IEquatable<SolutionId>
    {
        /// <summary>
        /// The unique id of the solution.
        /// </summary>
        public Guid Id { get; private set; }

        private string debugName;

        private SolutionId(string debugName)
        {
            this.Id = Guid.NewGuid();
            this.debugName = debugName;
        }

        /// <summary>
        /// Create a new Solution Id
        /// </summary>
        /// <param name="debugName">An optional name to make this id easier to recognize while debugging.</param>
        public static SolutionId CreateNewId(string debugName = null)
        {
            debugName = debugName ?? "unsaved";

            return new SolutionId(debugName);
        }

        private string DebuggerText
        {
            get { return string.Format("({0}, #{1} - {2})", GetType().Name, this.Id, this.debugName); }
        }

        public override bool Equals(object obj)
        {
            return this.Equals(obj as SolutionId);
        }

        public bool Equals(SolutionId other)
        {
            return
                !ReferenceEquals(other, null) &&
                this.Id == other.Id;
        }

        public static bool operator ==(SolutionId left, SolutionId right)
        {
            return EqualityComparer<SolutionId>.Default.Equals(left, right);
        }

        public static bool operator !=(SolutionId left, SolutionId right)
        {
            return !(left == right);
        }

        public override int GetHashCode()
        {
            return this.Id.GetHashCode();
        }
    }
}