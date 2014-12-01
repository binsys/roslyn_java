﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Microsoft.CodeAnalysis.Rename
{
    public enum RenameEntityKind
    {
        /// <summary>
        /// mentions that the result is for the base symbol of the rename
        /// </summary>
        BaseSymbol,

        /// <summary>
        /// mentions that the result is for the overloaded symbols of the rename
        /// </summary>
        OverloadedSymbols
    }
}
