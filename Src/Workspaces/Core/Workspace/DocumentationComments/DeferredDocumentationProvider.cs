﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Globalization;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;

namespace Microsoft.CodeAnalysis
{
    internal class DeferredDocumentationProvider : DocumentationProvider
    {
        private readonly Compilation compilation;

        public DeferredDocumentationProvider(Compilation compilation)
        {
            this.compilation = compilation;
        }

        protected override string GetDocumentationForSymbol(string documentationMemberID, CultureInfo preferredCulture, CancellationToken cancellationToken = default(CancellationToken))
        {
            var symbol = DocumentationCommentId.GetFirstSymbolForDeclarationId(documentationMemberID, this.compilation);

            if (symbol != null)
            {
                return symbol.GetDocumentationCommentXml(preferredCulture, cancellationToken: cancellationToken);
            }

            return string.Empty;
        }

        public override bool Equals(object obj)
        {
            return object.ReferenceEquals(this, obj);
        }

        public override int GetHashCode()
        {
            return this.compilation.GetHashCode();
        }
    }
}