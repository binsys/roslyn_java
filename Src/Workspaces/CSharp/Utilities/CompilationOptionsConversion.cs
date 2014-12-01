﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Microsoft.CodeAnalysis.CSharp.Utilities
{
    internal static class CompilationOptionsConversion
    {
        internal static LanguageVersion? GetLanguageVersion(string projectLanguageVersion)
        {
            switch (projectLanguageVersion)
            {
                case "ISO-1":
                    return LanguageVersion.CSharp1;
                case "ISO-2":
                    return LanguageVersion.CSharp2;
                default:
                    if (!string.IsNullOrEmpty(projectLanguageVersion))
                    {
                        int version;
                        if (int.TryParse(projectLanguageVersion, out version))
                        {
                            return (LanguageVersion)version;
                        }
                    }

                    // use default;
                    return null;
            }
        }
    }
}
