﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;

namespace Microsoft.CodeAnalysis.Notification
{
    internal class GlobalOperationEventArgs : EventArgs
    {
        public IReadOnlyList<string> Operations { get; private set; }
        public bool Cancelled { get; private set; }

        public GlobalOperationEventArgs(IReadOnlyList<string> operations, bool cancelled)
        {
            this.Operations = operations;
            this.Cancelled = cancelled;
        }
    }
}
