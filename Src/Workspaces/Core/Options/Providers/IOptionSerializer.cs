﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.CodeAnalysis.Options.Providers
{
    /// <summary>
    /// Exportable by a host to specify the save and restore behavior for a particular set of
    /// values.
    /// </summary>
    internal interface IOptionSerializer
    {
        bool TryFetch(OptionKey optionKey, out object value);
        bool TryPersist(OptionKey optionKey, object value);
    }
}
