﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.Internal.Log
{
    /// <summary>
    /// An interaction class defines how much time is expected to reach a time point, the response 
    /// time point being the most commonly used. The interaction classes correspond to human perception,
    /// so, for example, all interactions in the Fast class are perceived as fast and roughly feel like 
    /// they have the same performance. By defining these interaction classes, we can describe 
    /// performance using adjectives that have a precise, consistent meaning.
    /// </summary>
    internal enum InteractionClass
    {
        Undefined, // use when we don't have a specific goal for this operation

        // Name             Target (ms)     Upper Bound (ms)        UX / Feedback
        Instant,         // <=50            100                     No noticeable delay
        Fast,            // 50-100          200                     Minimally noticeable delay
        Typical,         // 100-300         500                     Slower, but still no feedback necessary
        Responsive,      // 300-500         1,000                   Slower yet, potentially show Wait cursor
        Captive,         // >500            10,000                  Long, show Progress Dialog w/Cancel
        Extended,        // >500            >10,000                 Long enough for the user to switch to something else
    }

    [AttributeUsage(AttributeTargets.Field, AllowMultiple = false, Inherited = false)]
    internal class PerfGoalAttribute : Attribute
    {
        private readonly InteractionClass interactionClass;

        public PerfGoalAttribute(InteractionClass interactionClass)
        {
            this.interactionClass = interactionClass;
        }

        public InteractionClass InteractionClass
        {
            get { return this.interactionClass; }
        }
    }
}
