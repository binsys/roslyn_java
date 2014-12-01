﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Threading;
using Microsoft.CodeAnalysis.CaseCorrection;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp.CaseCorrection
{
#if MEF
    using Microsoft.CodeAnalysis.LanguageServices;

    [ExportLanguageService(typeof(ICaseCorrectionService), LanguageNames.CSharp)]
#endif
    internal class CSharpCaseCorrectionService : AbstractCaseCorrectionService
    {
        [ExcludeFromCodeCoverage]
        protected override void AddReplacements(
            SemanticModel semanticModel,
            SyntaxNode root,
            IEnumerable<TextSpan> spans,
            Workspace workspace,
            ConcurrentDictionary<SyntaxToken, SyntaxToken> replacements,
            CancellationToken cancellationToken)
        {
            // C# doesn't support case correction since we are a case sensitive language.
            return;
        }
    }
}
