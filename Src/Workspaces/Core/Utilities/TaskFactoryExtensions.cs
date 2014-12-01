﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Threading;
using System.Threading.Tasks;

namespace Roslyn.Utilities
{
#if !COMPILERCORE
    [System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage]
#endif
    internal static partial class TaskFactoryExtensions
    {
        public static Task SafeStartNew(this TaskFactory factory, Action action, TaskScheduler scheduler)
        {
            return factory.SafeStartNew(action, CancellationToken.None, scheduler);
        }

        public static Task SafeStartNew(this TaskFactory factory, Action action, CancellationToken cancellationToken, TaskScheduler scheduler)
        {
            return factory.SafeStartNew(action, cancellationToken, TaskCreationOptions.None, scheduler);
        }

        public static Task SafeStartNew(
            this TaskFactory factory,
            Action action,
            CancellationToken cancellationToken,
            TaskCreationOptions creationOptions,
            TaskScheduler scheduler)
        {
            Action wrapped = () =>
            {
                try
                {
                    action();
                }
                catch (Exception e) if (ExceptionHelpers.CrashUnlessCanceled(e))
                {
                    throw ExceptionUtilities.Unreachable;
                }
            };

            // The one and only place we can call StartNew().
            return factory.StartNew(wrapped, cancellationToken, creationOptions, scheduler);
        }

        public static Task<TResult> SafeStartNew<TResult>(this TaskFactory factory, Func<TResult> func, TaskScheduler scheduler)
        {
            return factory.SafeStartNew(func, CancellationToken.None, scheduler);
        }

        public static Task<TResult> SafeStartNew<TResult>(this TaskFactory factory, Func<TResult> func, CancellationToken cancellationToken, TaskScheduler scheduler)
        {
            return factory.SafeStartNew(func, cancellationToken, TaskCreationOptions.None, scheduler);
        }

        public static Task<TResult> SafeStartNew<TResult>(
            this TaskFactory factory,
            Func<TResult> func,
            CancellationToken cancellationToken,
            TaskCreationOptions creationOptions,
            TaskScheduler scheduler)
        {
            Func<TResult> wrapped = () =>
            {
                try
                {
                    return func();
                }
                catch (Exception e) if (ExceptionHelpers.CrashUnlessCanceled(e))
                {
                    throw ExceptionUtilities.Unreachable;
                }
            };

            // The one and only place we can call StartNew<>().
            return factory.StartNew(wrapped, cancellationToken, creationOptions, scheduler);
        }
    }
}
