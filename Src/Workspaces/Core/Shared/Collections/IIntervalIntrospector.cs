﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using Microsoft.CodeAnalysis.Text;
namespace Microsoft.CodeAnalysis.Shared.Collections
{
    internal interface IIntervalIntrospector<T>
    {
        int GetStart(T value);
        int GetLength(T value);
    }
}
