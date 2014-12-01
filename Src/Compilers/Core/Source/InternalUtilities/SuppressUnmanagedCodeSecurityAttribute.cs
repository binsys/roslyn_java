﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace System.Security
{
    // The atttribute is not portable, but is needed to improve perf of interop calls on desktop.
    internal class SuppressUnmanagedCodeSecurityAttribute : Attribute
    {
    }
}
