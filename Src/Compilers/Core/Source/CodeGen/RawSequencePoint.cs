﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CodeGen
{
    /// <summary>
    /// Represents a sequence point before translation by #line/ExternalSource directives.
    /// </summary>
    internal struct RawSequencePoint
    {
        internal readonly SyntaxTree SyntaxTree;
        internal readonly int ILMarker;
        internal readonly TextSpan Span;

        // Special text span indicating a hidden sequence point.
        internal static readonly TextSpan HiddenSequencePointSpan = new TextSpan(0x7FFFFFFF, 0);

        internal RawSequencePoint(SyntaxTree syntaxTree, int ilMarker, TextSpan span)
        {
            this.SyntaxTree = syntaxTree;
            this.ILMarker = ilMarker;
            this.Span = span;
        }
    }
}
