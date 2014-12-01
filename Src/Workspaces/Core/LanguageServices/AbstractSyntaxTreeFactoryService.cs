﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.LanguageServices
{
    internal abstract partial class AbstractSyntaxTreeFactoryService : ISyntaxTreeFactoryService
    {
        private readonly ILanguageServiceProvider languageServices;

        public AbstractSyntaxTreeFactoryService(ILanguageServiceProvider languageServices)
        {
            this.languageServices = languageServices;
        }

        public abstract ParseOptions GetDefaultParseOptions();
        public abstract SyntaxTree CreateSyntaxTree(string filePath, ParseOptions options, SyntaxNode node);
        public abstract SyntaxTree ParseSyntaxTree(string filePath, ParseOptions options, SourceText text, CancellationToken cancellationToken);
        public abstract SyntaxTree CreateRecoverableTree(string filePath, ParseOptions options, ValueSource<TextAndVersion> text, SyntaxNode root, bool reparse);
        public abstract SyntaxNode DeserializeNodeFrom(Stream stream, CancellationToken cancellationToken);

        protected static SyntaxNode RecoverNode(SyntaxTree tree, TextSpan textSpan, int kind)
        {
            var token = tree.GetRoot().FindToken(textSpan.Start, findInsideTrivia: true);
            var node = token.Parent;

            while (node != null)
            {
                if (node.Span == textSpan && node.RawKind == kind)
                {
                    return node;
                }

                var structuredTrivia = node as IStructuredTriviaSyntax;
                if (structuredTrivia != null)
                {
                    node = structuredTrivia.ParentTrivia.Token.Parent;
                }
                else
                {
                    node = node.Parent;
                }
            }

            throw Contract.Unreachable;
        }

        private static Task latestTask = SpecializedTasks.EmptyTask;
        private static readonly NonReentrantLock taskGuard = new NonReentrantLock();

        protected static Task SaveTreeAsync(SyntaxNode root, ITemporaryStorage storage)
        {
            using (taskGuard.DisposableWait())
            {
                // force all save tasks to be in sequence
                latestTask = latestTask.SafeContinueWith(t => SaveTreeWorkerAsync(root, storage, CancellationToken.None), CancellationToken.None, TaskScheduler.Default).Unwrap();
                return latestTask;
            }
        }

        private static async Task SaveTreeWorkerAsync(SyntaxNode node, ITemporaryStorage storage, CancellationToken cancellationToken)
        {
            using (var stream = SerializableBytes.CreateWritableStream())
            {
                node.SerializeTo(stream);
                stream.Position = 0;
                await storage.WriteStreamAsync(stream, cancellationToken).ConfigureAwait(false);
            }
        }
    }
}