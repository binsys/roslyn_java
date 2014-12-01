﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;

namespace Microsoft.CodeAnalysis
{
    internal abstract partial class SymbolKey
    {
        internal struct ComparisonOptions
        {
            [Flags]
            private enum Option : byte
            {
                None = 0x0,
                IgnoreCase = 0x1,
                IgnoreAssemblyKeys = 0x2,
                CompareMethodTypeParametersByName = 0x4
            }

            private readonly Option flags;

            public ComparisonOptions(bool ignoreCase, bool ignoreAssemblyKeys, bool compareMethodTypeParametersByName)
            {
                this.flags =
                    BoolToOption(ignoreCase, Option.IgnoreCase) |
                    BoolToOption(ignoreAssemblyKeys, Option.IgnoreAssemblyKeys) |
                    BoolToOption(compareMethodTypeParametersByName, Option.CompareMethodTypeParametersByName);
            }

            public bool IgnoreCase
            {
                get { return (this.flags & Option.IgnoreCase) == Option.IgnoreCase; }
            }

            public bool IgnoreAssemblyKey
            {
                get { return (this.flags & Option.IgnoreAssemblyKeys) == Option.IgnoreAssemblyKeys; }
            }

            public bool CompareMethodTypeParametersByName
            {
                get { return (this.flags & Option.CompareMethodTypeParametersByName) == Option.CompareMethodTypeParametersByName; }
            }

            public byte FlagsValue
            {
                get { return (byte)flags; }
            }

            private static Option BoolToOption(bool value, Option option)
            {
                return value ? option : Option.None;
            }
        }
    }
}
