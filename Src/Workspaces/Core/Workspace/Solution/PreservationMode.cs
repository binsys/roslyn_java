﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using Microsoft.CodeAnalysis.Text;
namespace Microsoft.CodeAnalysis
{
    /// <summary>
    /// The mode in which value is preserved.
    /// </summary>
    public enum PreservationMode
    {
        /// <summary>
        /// The value is guaranteed to have the same contents across multiple accesses.
        /// </summary>
        PreserveValue,

        /// <summary>
        /// The value is guaranteed to the same instance across multiple accesses.
        /// </summary>
        PreserveIdentity
    }
}