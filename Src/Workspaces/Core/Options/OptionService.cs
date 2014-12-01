﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
#if MEF
using System.ComponentModel.Composition;
#endif
using Microsoft.CodeAnalysis.Composition;
using Microsoft.CodeAnalysis.Options.Providers;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Options
{
#if MEF
    [Export(typeof(IOptionService))]
#endif
    internal class OptionService : IOptionService
    {
        private readonly Lazy<HashSet<IOption>> options;
        private readonly ImmutableDictionary<string, ImmutableList<Lazy<IOptionSerializer, OptionSerializerMetadata>>> featureNameToOptionSerializers =
            ImmutableDictionary.Create<string, ImmutableList<Lazy<IOptionSerializer, OptionSerializerMetadata>>>();

        private readonly object gate = new object();
        private ImmutableDictionary<OptionKey, object> currentValues;

#if MEF
        [ImportingConstructor]
        public OptionService(
            [ImportMany] IEnumerable<Lazy<IOptionProvider>> optionProviders,
            [ImportMany] IEnumerable<Lazy<IOptionSerializer, OptionSerializerMetadata>> optionSerializers)
#else
        public OptionService(
            IEnumerable<Lazy<IOptionProvider>> optionProviders,
            IEnumerable<Lazy<IOptionSerializer, OptionSerializerMetadata>> optionSerializers)
#endif
        {
            this.options = new Lazy<HashSet<IOption>>(() =>
            {
                var options = new HashSet<IOption>();

                foreach (var provider in optionProviders)
                {
                    options.AddRange(provider.Value.GetOptions());
                }

                return options;
            });

            foreach (var optionSerializerAndMetadata in optionSerializers)
            {
                foreach (var featureName in optionSerializerAndMetadata.Metadata.Features)
                {
                    ImmutableList<Lazy<IOptionSerializer, OptionSerializerMetadata>> existingSerializers;
                    if (!featureNameToOptionSerializers.TryGetValue(featureName, out existingSerializers))
                    {
                        existingSerializers = ImmutableList.Create<Lazy<IOptionSerializer, OptionSerializerMetadata>>();
                    }

                    this.featureNameToOptionSerializers = this.featureNameToOptionSerializers.SetItem(featureName, existingSerializers.Add(optionSerializerAndMetadata));
                }
            }

            this.currentValues = ImmutableDictionary.Create<OptionKey, object>();
        }

        public OptionService(ExportSource exports)
            : this(
                exports.GetExports<IOptionProvider>(),
                exports.GetExports<IOptionSerializer, OptionSerializerMetadata>())
        {
        }

        private object LoadOptionFromSerializerOrGetDefault(OptionKey optionKey)
        {
            lock (gate)
            {
                ImmutableList<Lazy<IOptionSerializer, OptionSerializerMetadata>> optionSerializers;
                if (featureNameToOptionSerializers.TryGetValue(optionKey.Option.Feature, out optionSerializers))
                {
                    foreach (var serializer in optionSerializers)
                    {
                        // there can be options (ex, formatting) that only exist in only one specific language. in those cases,
                        // feature's serialzier should exist in only that language.
                        if (!SupportedSerializer(optionKey, serializer.Metadata))
                        {
                            continue;
                        }

                        // We have one a deserializer, so deserialize and use that value
                        object deserializedValue;
                        if (serializer.Value.TryFetch(optionKey, out deserializedValue))
                        {
                            return deserializedValue;
                        }
                    }
                }

                // Just use the default. We will still cache this so we aren't trying to deserialize
                // over and over.
                return optionKey.Option.DefaultValue;
            }
        }

        public IEnumerable<IOption> GetRegisteredOptions()
        {
            return this.options.Value;
        }

        public OptionSet GetOptions()
        {
            return new OptionSet(this);
        }

        public T GetOption<T>(Option<T> option)
        {
            return (T)GetOption(new OptionKey(option, language: null));
        }

        public T GetOption<T>(PerLanguageOption<T> option, string language)
        {
            return (T)GetOption(new OptionKey(option, language));
        }

        public object GetOption(OptionKey optionKey)
        {
            lock (gate)
            {
                object value;

                if (currentValues.TryGetValue(optionKey, out value))
                {
                    return value;
                }

                value = LoadOptionFromSerializerOrGetDefault(optionKey);

                currentValues = currentValues.Add(optionKey, value);

                return value;
            }
        }

        public void SetOptions(OptionSet optionSet)
        {
            if (optionSet == null)
            {
                throw new ArgumentNullException("OptionSet");
            }

            var changedOptions = new List<OptionChangedEventArgs>();

            lock (gate)
            {
                foreach (var optionKey in optionSet.GetAccessedOptions())
                {
                    var setValue = optionSet.GetOption(optionKey);
                    object currentValue = this.GetOption(optionKey);

                    if (object.Equals(currentValue, setValue))
                    {
                        // Identical, so nothing changing
                        continue;
                    }

                    // The value is actually changing, so update
                    changedOptions.Add(new OptionChangedEventArgs(optionKey, setValue));

                    currentValues = currentValues.SetItem(optionKey, setValue);

                    ImmutableList<Lazy<IOptionSerializer, OptionSerializerMetadata>> optionSerializers;
                    if (featureNameToOptionSerializers.TryGetValue(optionKey.Option.Feature, out optionSerializers))
                    {
                        foreach (var serializer in optionSerializers)
                        {
                            // there can be options (ex, formatting) that only exist in only one specific language. in those cases,
                            // feature's serialzier should exist in only that language.
                            if (!SupportedSerializer(optionKey, serializer.Metadata))
                            {
                                continue;
                            }

                            if (serializer.Value.TryPersist(optionKey, setValue))
                            {
                                break;
                            }
                        }
                    }
                }
            }

            // Outside of the lock, raise events
            var optionChanged = OptionChanged;
            if (optionChanged != null)
            {
                foreach (var changedOption in changedOptions)
                {
                    optionChanged(this, changedOption);
                }
            }
        }

        private static bool SupportedSerializer(OptionKey optionKey, OptionSerializerMetadata metadata)
        {
            return optionKey.Language == null || optionKey.Language == metadata.Language;
        }

        public event EventHandler<OptionChangedEventArgs> OptionChanged;
    }
}
