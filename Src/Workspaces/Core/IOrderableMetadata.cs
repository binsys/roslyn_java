﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using System.ComponentModel;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{
    /// <summary>
    /// This interface exists purely to enable some shared code that operates over orderable metadata.
    /// This interface should not be used directly with MEF, used OrderableMetadata instead.
    /// </summary>
    internal interface IOrderableMetadata
    {
        IEnumerable<string> After { get; }
        IEnumerable<string> Before { get; }
        string Name { get; }
    }
}