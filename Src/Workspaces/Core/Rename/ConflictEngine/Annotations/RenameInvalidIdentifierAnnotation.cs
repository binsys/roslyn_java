﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;

namespace Microsoft.CodeAnalysis.Rename.ConflictEngine
{
    internal class RenameInvalidIdentifierAnnotation : RenameAnnotation
    {
        public static RenameInvalidIdentifierAnnotation Instance = new RenameInvalidIdentifierAnnotation();

        private RenameInvalidIdentifierAnnotation()
        {
        }
    }
}
