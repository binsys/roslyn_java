﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.Shared.Utilities
{
    internal partial class Matcher<T>
    {
        private class SequenceMatcher : Matcher<T>
        {
            private readonly Matcher<T>[] matchers;

            public SequenceMatcher(params Matcher<T>[] matchers)
            {
                this.matchers = matchers;
            }

            public override bool TryMatch(IList<T> sequence, ref int index)
            {
                var currentIndex = index;
                foreach (var matcher in matchers)
                {
                    if (!matcher.TryMatch(sequence, ref currentIndex))
                    {
                        return false;
                    }
                }

                index = currentIndex;
                return true;
            }

            public override string ToString()
            {
                return string.Format("({0})", string.Join(",", (object[])matchers));
            }
        }
    }
}