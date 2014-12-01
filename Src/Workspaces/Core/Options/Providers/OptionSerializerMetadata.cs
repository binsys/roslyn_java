﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using Microsoft.CodeAnalysis.LanguageServices;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Options.Providers
{
    internal class OptionSerializerMetadata : LanguageMetadata
    {
        public IEnumerable<string> Features { get; private set; }

        public OptionSerializerMetadata(IDictionary<string, object> data) : base(data)
        {
            this.Features = (IEnumerable<string>)data.GetValueOrDefault("Features");
        }
    }
}
