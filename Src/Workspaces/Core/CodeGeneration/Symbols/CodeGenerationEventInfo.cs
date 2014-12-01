﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Runtime.CompilerServices;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CodeGeneration
{
    internal class CodeGenerationEventInfo
    {
        private static readonly ConditionalWeakTable<IEventSymbol, CodeGenerationEventInfo> eventToInfoMap =
            new ConditionalWeakTable<IEventSymbol, CodeGenerationEventInfo>();

        private readonly bool isUnsafe;
        private CodeGenerationEventInfo(bool isUnsafe)
        {
            this.isUnsafe = isUnsafe;
        }

        public static void Attach(IEventSymbol @event, bool isUnsafe)
        {
            var info = new CodeGenerationEventInfo(isUnsafe);
            eventToInfoMap.Add(@event, info);
        }

        private static CodeGenerationEventInfo GetInfo(IEventSymbol @event)
        {
            CodeGenerationEventInfo info;
            eventToInfoMap.TryGetValue(@event, out info);
            return info;
        }

        public static bool GetIsUnsafe(IEventSymbol @event)
        {
            return GetIsUnsafe(GetInfo(@event));
        }

        private static bool GetIsUnsafe(CodeGenerationEventInfo info)
        {
            return info != null && info.isUnsafe;
        }
    }
}