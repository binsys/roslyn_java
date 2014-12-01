﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using Microsoft.CodeAnalysis.Text;
namespace Microsoft.CodeAnalysis.Formatting.Rules
{
    /// <summary>
    /// option to control AlignTokensOperation behavior
    /// </summary>
    internal enum AlignTokensOption
    {
        AlignIndentationOfTokensToBaseToken,
        AlignPositionOfTokensToIndentation,

        /// <summary>
        /// Alignment will made to the first token in which the base token is present
        /// </summary>
        AlignToFirstTokenOnBaseTokenLine
    }
}
