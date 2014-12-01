﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.LanguageServices
{
    /// <summary>
    /// This interface is provided purely to enable some shared logic that handles multiple kinds of 
    /// metadata that share the Language property. It should not be used directly with MEF,
    /// use LanguageMetadata instead.
    /// </summary>
    internal interface ILanguageMetadata
    {
        string Language { get; }
    }
}