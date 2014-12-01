// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;

namespace Roslyn.Utilities
{
    internal static class FileUtilities
    {
        /// <summary>
        /// Resolves relative path and returns absolute path.
        /// The method depends only on values of its parameters and their implementation (for fileExists).
        /// It doesn't itself depend on the state of the current process (namely on the current drive directories) or 
        /// the state of file system.
        /// </summary>
        /// <param name="path">
        /// Path to resolve.
        /// </param>
        /// <param name="basePath">
        /// Base file path to resolve CWD-relative paths against. Null if not available.
        /// </param>
        /// <param name="baseDirectory">
        /// Base directory to resolve CWD-relative paths against if <paramref name="basePath"/> isn't specified. 
        /// Must be absolute path.
        /// Null if not available.
        /// </param>
        /// <param name="searchPaths">
        /// Sequence of paths used to search for unqualified relative paths.
        /// </param>
        /// <param name="fileExists">
        /// Method that tests existence of a file.
        /// </param>
        /// <returns>
        /// The resolved path or null if the path can't be resolved.
        /// </returns>
        internal static string ResolveRelativePath(
            string path,
            string basePath,
            string baseDirectory,
            IEnumerable<string> searchPaths,
            Func<string, bool> fileExists)
        {
            Debug.Assert(baseDirectory == null || PathUtilities.IsAbsolute(baseDirectory));
            Debug.Assert(searchPaths != null);
            Debug.Assert(fileExists != null);

            var kind = PathUtilities.GetPathKind(path);
            if (kind == PathKind.Relative)
            {
                // first, look in the base directory:
                baseDirectory = GetBaseDirectory(basePath, baseDirectory);
                if (baseDirectory != null)
                {
                    string combinedPath = PathUtilities.CombinePathsUnchecked(baseDirectory, path);
                    Debug.Assert(PathUtilities.IsAbsolute(combinedPath));

                    if (fileExists == null || fileExists(combinedPath))
                    {
                        return combinedPath;
                    }
                }

                // try search paths:
                foreach (var searchPath in searchPaths)
                {
                    string combinedPath = PathUtilities.CombinePathsUnchecked(searchPath, path);

                    Debug.Assert(PathUtilities.IsAbsolute(combinedPath));
                    if (fileExists == null || fileExists(combinedPath))
                    {
                        return combinedPath;
                    }
                }

                return null;
            }

            return ResolveRelativePath(kind, path, basePath, baseDirectory);
        }

        internal static string ResolveRelativePath(string path, string baseDirectory)
        {
            return ResolveRelativePath(path, null, baseDirectory);
        }

        internal static string ResolveRelativePath(string path, string basePath, string baseDirectory)
        {
            Debug.Assert(baseDirectory == null || PathUtilities.IsAbsolute(baseDirectory));
            return ResolveRelativePath(PathUtilities.GetPathKind(path), path, basePath, baseDirectory);
        }

        private static string ResolveRelativePath(PathKind kind, string path, string basePath, string baseDirectory)
        {
            switch (kind)
            {
                case PathKind.Empty:
                    return null;

                case PathKind.Relative:
                    baseDirectory = GetBaseDirectory(basePath, baseDirectory);
                    if (baseDirectory == null)
                    {
                        return null;
                    }

                    // with no search paths relative paths are relative to the base directory:
                    return PathUtilities.CombinePathsUnchecked(baseDirectory, path);

                case PathKind.RelativeToCurrentDirectory:
                    baseDirectory = GetBaseDirectory(basePath, baseDirectory);
                    if (baseDirectory == null)
                    {
                        return null;
                    }

                    if (path.Length == 1)
                    {
                        // "."
                        return baseDirectory;
                    }
                    else
                    {
                        // ".\path"
                        return PathUtilities.CombinePathsUnchecked(baseDirectory, path);
                    }

                case PathKind.RelativeToCurrentParent:
                    baseDirectory = GetBaseDirectory(basePath, baseDirectory);
                    if (baseDirectory == null)
                    {
                        return null;
                    }

                    // ".."
                    return PathUtilities.CombinePathsUnchecked(baseDirectory, path);

                case PathKind.RelativeToCurrentRoot:
                    string baseRoot;
                    if (basePath != null)
                    {
                        baseRoot = PathUtilities.GetPathRoot(basePath);
                    }
                    else if (baseDirectory != null)
                    {
                        baseRoot = PathUtilities.GetPathRoot(baseDirectory);
                    }
                    else
                    {
                        return null;
                    }

                    if (baseRoot == null)
                    {
                        return null;
                    }

                    Debug.Assert(PathUtilities.IsDirectorySeparator(path[0]));
                    Debug.Assert(path.Length == 1 || !PathUtilities.IsDirectorySeparator(path[1]));
                    Debug.Assert(baseRoot.Length >= 3);
                    return PathUtilities.CombinePathsUnchecked(baseRoot, path.Substring(1));

                case PathKind.RelativeToDriveDirectory:
                    // drive relative paths not supported, can't resolve:
                    return null;

                case PathKind.Absolute:
                    return path;

                default:
                    // EDMAURER this is not using ExceptionUtilities.UnexpectedValue() because this file
                    // is shared via linking with other code that doesn't have the ExceptionUtilities.
                    throw new InvalidOperationException(string.Format("Unexpected PathKind {0}.", kind));
            }
        }

        private static string GetBaseDirectory(string basePath, string baseDirectory)
        {
            // relative base paths are relative to the base directory:
            string resolvedBasePath = ResolveRelativePath(basePath, baseDirectory);
            if (resolvedBasePath == null)
            {
                return baseDirectory;
            }

            // Note: Path.GetDirectoryName doesn't normalize the path and so it doesn't depend on the process state.
            Debug.Assert(PathUtilities.IsAbsolute(resolvedBasePath));
            try
            {
                return Path.GetDirectoryName(resolvedBasePath);
            }
            catch (ArgumentException)
            {
                // invalid characters in path
                // TODO (tomat): parse the directory name ourselves and handle invalid characters
                return null;
            }
            catch (PathTooLongException)
            {
                // TODO (tomat): parse the directory name ourselves and handle arbitrarily long path
                return null;
            }
        }

        private static readonly char[] InvalidPathChars = Path.GetInvalidPathChars();

        internal static string NormalizeRelativePath(string path, string basePath, string baseDirectory)
        {
            // Does this look like a URI at all or does it have any invalid path characters? If so, just use it as is.
            if (path.IndexOf("://") >= 0 || path.IndexOfAny(InvalidPathChars) >= 0)
            {
                return null;
            }

            string resolvedPath = ResolveRelativePath(path, basePath, baseDirectory);
            if (resolvedPath == null)
            {
                return null;
            }

            string normalizedPath = TryNormalizeAbsolutePath(resolvedPath);
            if (normalizedPath == null)
            {
                return null;
            }

            return normalizedPath;
        }

        /// <summary>
        /// Normalizes an absolute path.
        /// </summary>
        /// <param name="path">Path to normalize.</param>
        /// <exception cref="ArgumentException"/>
        /// <exception cref="System.Security.SecurityException"/>
        /// <exception cref="ArgumentNullException"/>
        /// <exception cref="NotSupportedException"/>
        /// <exception cref="PathTooLongException"/>
        /// <returns>Normalized path.</returns>
        internal static string NormalizeAbsolutePath(string path)
        {
            // we can only call GetFullPath on an absolute path to avoid dependency on process state (current directory):
            Debug.Assert(PathUtilities.IsAbsolute(path));
            return Path.GetFullPath(path);
        }

        internal static string TryNormalizeAbsolutePath(string path)
        {
            Debug.Assert(PathUtilities.IsAbsolute(path));
            try
            {
                return Path.GetFullPath(path);
            }
            catch
            {
                return null;
            }
        }

        [DllImport("kernel32.dll", PreserveSig = false)]
        private static extern void SetFileInformationByHandle(SafeFileHandle handle, int fileInformationClass, ref uint fileDispositionInfoDeleteFile, int bufferSize);

        private const int FileDispositionInfo = 4;

        internal static void PrepareDeleteOnCloseStreamForDisposal(FileStream stream)
        {
            // tomat: Set disposition to "delete" on the stream, so to avoid ForeFront EndPoint
            // Protection driver scanning the file. Note that after calling this on a file that's open with DeleteOnClose, 
            // the file can't be opened again, not even by the same process.
            uint trueValue = 1;
            SetFileInformationByHandle(stream.SafeFileHandle, FileDispositionInfo, ref trueValue, sizeof(uint));
        }

        /// <summary>
        /// Marks given file for automatic deletion when all its handles are closed.
        /// Note that after doing this the file can't be opened again, not even by the same process.
        /// </summary>
        internal static void DeleteFileOnClose(string fullPath)
        {
            using (var stream = new FileStream(fullPath, FileMode.Open, FileAccess.ReadWrite, FileShare.Delete | FileShare.ReadWrite, 8, FileOptions.DeleteOnClose))
            {
                PrepareDeleteOnCloseStreamForDisposal(stream);
            }
        }

        /// <exception cref="IOException"/>
        internal static DateTime GetFileTimeStamp(string fullPath)
        {
            Debug.Assert(PathUtilities.IsAbsolute(fullPath));
            try
            {
                return File.GetLastWriteTimeUtc(fullPath);
            }
            catch (Exception e)
            {
                throw new IOException(e.Message);
            }
        }
    }
}
