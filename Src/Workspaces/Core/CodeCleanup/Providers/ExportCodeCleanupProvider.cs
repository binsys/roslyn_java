﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CodeCleanup.Providers
{
    /// <summary>
    /// Specifies the exact type of the code cleanup exported
    /// </summary>
    [MetadataAttribute]
    [AttributeUsage(AttributeTargets.Class)]
    internal class ExportCodeCleanupProvider : ExportAttribute
    {
        public string Name { get; private set; }
        public IEnumerable<string> Languages { get; private set; }

        public ExportCodeCleanupProvider(string name, params string[] languages)
            : base(typeof(ICodeCleanupProvider))
        {
            if (languages == null)
            {
                throw new ArgumentNullException("languages");
            }

            if (languages.Length == 0)
            {
                throw new ArgumentException("languages");
            }

            this.Name = name;
            this.Languages = languages;
        }
    }
}
