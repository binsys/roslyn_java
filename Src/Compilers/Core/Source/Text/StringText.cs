﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Text
{
    /// <summary>
    /// Implementation of SourceText based on a <see cref="T:System.String"/> input
    /// </summary>
    internal partial class StringText : SourceText
    {
        private readonly string source;
        private ImmutableArray<byte> sha1Checksum;

        internal StringText(string source, ImmutableArray<byte> sha1Checksum)
        {
            Debug.Assert(source != null);
            Debug.Assert(sha1Checksum.Length == 0 || sha1Checksum.Length == Hash.Sha1HashSize);

            this.source = source;
            this.sha1Checksum = sha1Checksum;
        }

        /// <summary>
        /// Underlying string which is the source of this SourceText instance
        /// </summary>
        public string Source
        {
            get
            {
                return source;
            }
        }

        /// <summary>
        /// The length of the text represented by <see cref="T:StringText"/>.
        /// </summary>
        public override int Length
        {
            get { return this.Source.Length; }
        }

        /// <summary>
        /// Returns a character at given position.
        /// </summary>
        /// <param name="position">The position to get the character from.</param>
        /// <returns>The character.</returns>
        /// <exception cref="T:ArgumentOutOfRangeException">When position is negative or 
        /// greater than <see cref="T:"/> length.</exception>
        public override char this[int position]
        {
            get
            {
                // NOTE: we are not validating position here as that would not 
                //       add any value to the range check that string accessor performs anyways.

                return this.source[position];
            }
        }

        /// <summary>
        /// Provides a string representation of the StringText located within given span.
        /// </summary>
        /// <exception cref="T:ArgumentOutOfRangeException">When given span is outside of the text range.</exception>
        public override string ToString(TextSpan span)
        {
            if (span.End > this.Source.Length)
            {
                throw new ArgumentOutOfRangeException("span");
            }

            if (span.Start == 0 && span.Length == this.Length)
            {
                return this.Source;
            }
            else
            {
                return this.Source.Substring(span.Start, span.Length);
            }
        }

        public override void CopyTo(int sourceIndex, char[] destination, int destinationIndex, int count)
        {
            this.Source.CopyTo(sourceIndex, destination, destinationIndex, count);
        }

        public override void Write(TextWriter textWriter, TextSpan span)
        {
            if (span.Start == 0 && span.End == this.Length)
            {
                textWriter.Write(this.Source);
            }
            else
            {
                base.Write(textWriter, span);
            }
        }

        protected override ImmutableArray<byte> GetSha1ChecksumImpl()
        {
            return this.sha1Checksum;
        }
    }
}
