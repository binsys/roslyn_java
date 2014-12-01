﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.IO;
using System.Runtime.CompilerServices;
using System.Threading;
using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.Internal.Log;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{
    internal class MetadataOnlyImage
    {
        public static readonly MetadataOnlyImage Empty = new MetadataOnlyImage(storage: null, assemblyName: string.Empty);

        private readonly ITemporaryStorage storage;
        private readonly string assemblyName;

        private MetadataOnlyImage(ITemporaryStorage storage, string assemblyName)
        {
            this.storage = storage;
            this.assemblyName = assemblyName;
        }

        public bool IsEmpty
        {
            get { return this.storage == null; }
        }

        public static MetadataOnlyImage Create(ITemporaryStorageService service, Compilation compilation, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            using (Logger.LogBlock(FeatureId.SkeletonAssembly, FunctionId.SkeletonAssembly_EmitMetadataOnlyImage, cancellationToken))
            {
                // TODO: make it to use SerializableBytes.WritableStream rather than MemoryStream so that
                //       we don't allocate anything for skeleton assembly.
                using (var stream = SerializableBytes.CreateWritableStream())
                {
                    // note: cloning compilation so we don't retain all the generated symbols after its emitted.
                    // * REVIEW * is cloning clone p2p reference compilation as well?
                    var emitResult = compilation.Clone().EmitMetadataOnly(stream, cancellationToken: cancellationToken);

                    if (emitResult.Success)
                    {
                        var storage = service.CreateTemporaryStorage(cancellationToken);

                        stream.Position = 0;
                        storage.WriteStream(stream, cancellationToken);

                        return new MetadataOnlyImage(storage, compilation.AssemblyName);
                    }
                }
            }

            return Empty;
        }

        private static readonly ConditionalWeakTable<MetadataReference, Stream> lifetime = new ConditionalWeakTable<MetadataReference, Stream>();

        public MetadataReference CreateReference(string alias, bool embedInteropTypes, DocumentationProvider documentationProvider)
        {
            if (this.IsEmpty)
            {
                return null;
            }

            // first see whether we can use native memory directly.
            var stream = this.storage.ReadStream();
            var supportNativeMemory = stream as ISupportDirectMemoryAccess;
            if (supportNativeMemory != null)
            {
                // this is unfortunate that if we give stream, compiler will just re-copy whole content to 
                // native memory again. this is a way to get around the issue by we getting native memory ourselves and then
                // give them pointer to the native memory. also we need to handle lifetime ourselves.
                var referenceWithNativeMemory = new MetadataImageReference(
                    AssemblyMetadata.Create(ModuleMetadata.CreateFromImage(supportNativeMemory.GetPointer(), (int)stream.Length)),
                    documentation: documentationProvider,
                    alias: alias,
                    embedInteropTypes: embedInteropTypes,
                    display: this.assemblyName);

                // tie lifetime of stream to metadata reference we created. native memory's lifetime is tied to
                // stream internally and stream is shared between same temporary storage. so here, we should be 
                // sharing same native memory for all skeleton assemblies from same project snapshot.
                lifetime.GetValue(referenceWithNativeMemory, _ => stream);

                return referenceWithNativeMemory;
            }

            // otherwise, we just let it use stream. unfortunately, if we give stream, compiler will
            // internally copy it to native memory again. since compiler owns lifetime of stream,
            // it would be great if compiler can be little bit smarter on how it deals with stream.
            return new MetadataImageReference(
                stream,
                documentation: documentationProvider,
                alias: alias,
                embedInteropTypes: embedInteropTypes,
                display: this.assemblyName);
        }

        public void Cleanup()
        {
            if (this.storage != null)
            {
                this.storage.Dispose();
            }
        }
    }
}