﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Threading;

namespace Microsoft.CodeAnalysis
{
    internal abstract partial class SymbolKey
    {
        [Serializable]
        private class NullSymbolKey : SymbolKey, IObjectReference
        {
            public override SymbolKeyResolution Resolve(Compilation compilation, bool ignoreAssemblyKey, CancellationToken cancellationToken)
            {
                return default(SymbolKeyResolution);
            }

            internal override bool Equals(SymbolKey other, ComparisonOptions options)
            {
                return ReferenceEquals(this, other);
            }

            internal override int GetHashCode(ComparisonOptions options)
            {
                return RuntimeHelpers.GetHashCode(this);
            }

            object IObjectReference.GetRealObject(StreamingContext context)
            {
                return Null;
            }
        }
    }
}