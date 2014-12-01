﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.Host
{
    internal interface IFileTracker : IDisposable
    {
        bool IsTracking(string filePath);
        void Track(string filePath, Action onChanged);
        void StopTracking(string filePath);
    }
}