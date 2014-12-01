﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.ComponentModel.Composition;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.Options.Providers
{
    [AttributeUsage(AttributeTargets.Class)]
    internal sealed class ExportOptionProviderAttribute : ExportAttribute
    {
        public ExportOptionProviderAttribute() : base(typeof(IOptionProvider))
        {
        }
    }
}