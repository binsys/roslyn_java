﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Threading;
using System.Xml;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{
    /// <summary>
    /// VersionStamp should be only used to compare versions returned by same API.
    /// </summary>
    [Serializable]
    public struct VersionStamp : IEquatable<VersionStamp>, IObjectWritable
    {
        private const int GlobalVersionMarker = -1;
        private const int InitialGlobalVersion = 10000;

        /// <summary>
        /// global counter to avoid collision within same session. 
        /// it starts with a big initial number just for a clarity in debugging
        /// </summary>
        private static int globalVersion = InitialGlobalVersion;

        /// <summary>
        /// time stamp
        /// </summary>
        private readonly DateTime utcLastModified;

        /// <summary>
        /// indicate whether there was a collision on same item
        /// </summary>
        private readonly int localIncrement;

        /// <summary>
        /// unique version in same session
        /// </summary>
        private readonly int globalIncrement;

        private VersionStamp(DateTime utcLastModified)
            : this(utcLastModified, 0)
        {
        }

        private VersionStamp(DateTime utcLastModified, int localIncrement)
        {
            this.utcLastModified = utcLastModified;
            this.localIncrement = localIncrement;
            this.globalIncrement = GetNextGlobalVersion();
        }

        private VersionStamp(DateTime utcLastModified, int localIncrement, int globalIncrement)
        {
            Contract.ThrowIfFalse(utcLastModified == default(DateTime) || utcLastModified.Kind == DateTimeKind.Utc);

            this.utcLastModified = utcLastModified;
            this.localIncrement = localIncrement;
            this.globalIncrement = globalIncrement;
        }

        /// <summary>
        /// Creates a new instance of a VersionStamp.
        /// </summary>
        public static VersionStamp Create()
        {
            return new VersionStamp(DateTime.UtcNow);
        }

        /// <summary>
        /// Creates a new instance of a version stamp based on the specified DateTime.
        /// </summary>
        public static VersionStamp Create(DateTime utcIimeLastModified)
        {
            return new VersionStamp(utcIimeLastModified);
        }

        /// <summary>
        /// compare two different versions and return either one of the versions if there is no collision, otherwise, create a new version
        /// that can be used later to compare versions between different items
        /// </summary>
        public VersionStamp GetNewerVersion(VersionStamp version)
        {
            // * NOTE *
            // in current design/implementation, there are 4 possible ways for a version to be created.
            //
            // 1. created from a file stamp (most likely by starting a new session). "increment" will have 0 as value
            // 2. created by modifing existing item (text changes, project changes etc).
            //    "increment" will have either 0 or previous increment + 1 if there was a collision.
            // 3. created from deserialization (probably by using persistent service).
            // 4. created by accumulating versions of mutliple items.
            //
            // and this method is the one that is responsible for #4 case.

            if (this.utcLastModified > version.utcLastModified)
            {
                return this;
            }

            if (this.utcLastModified == version.utcLastModified)
            {
                var thisGlobalVersion = GetGlobalVersion(this);
                var thatGlobalVersion = GetGlobalVersion(version);

                if (thisGlobalVersion == thatGlobalVersion)
                {
                    // given versions are same one
                    return this;
                }

                // mark it as global version
                // global version can't be moved to newer version.
                return new VersionStamp(this.utcLastModified, (thisGlobalVersion > thatGlobalVersion) ? thisGlobalVersion : thatGlobalVersion, GlobalVersionMarker);
            }

            return version;
        }

        /// <summary>
        /// Gets a new VersionStamp that is guaranteed to be newer than its base one
        /// this should only be used for same item to move it to newer version
        /// </summary>
        public VersionStamp GetNewerVersion()
        {
            // global version can't be moved to newer version
            Contract.Requires(this.globalIncrement != GlobalVersionMarker);

            var now = DateTime.UtcNow;
            var incr = (now == this.utcLastModified) ? this.localIncrement + 1 : 0;

            return new VersionStamp(now, incr);
        }

        /// <summary>
        /// Returns the serialized text form of the VersionStamp.
        /// </summary>
        public override string ToString()
        {
            return XmlConvert.ToString(this.utcLastModified, XmlDateTimeSerializationMode.Utc);
        }

        public override int GetHashCode()
        {
            return Hash.Combine(this.utcLastModified.GetHashCode(), this.localIncrement);
        }

        public override bool Equals(object obj)
        {
            if (obj is VersionStamp)
            {
                return this.Equals((VersionStamp)obj);
            }

            return false;
        }

        public bool Equals(VersionStamp version)
        {
            if (this.utcLastModified == version.utcLastModified)
            {
                return GetGlobalVersion(this) == GetGlobalVersion(version);
            }

            return false;
        }

        public static bool operator ==(VersionStamp left, VersionStamp right)
        {
            return left.Equals(right);
        }

        public static bool operator !=(VersionStamp left, VersionStamp right)
        {
            return !left.Equals(right);
        }

        /// <summary>
        /// check whether given persisted version is re-usable
        /// </summary>
        internal static bool CanReusePersistedVersion(VersionStamp baseVersion, VersionStamp persistedVersion)
        {
            if (baseVersion == persistedVersion)
            {
                return true;
            }

            // there was a collision, we can't use these
            if (baseVersion.localIncrement != 0 || persistedVersion.localIncrement != 0)
            {
                return false;
            }

            return baseVersion.utcLastModified == persistedVersion.utcLastModified;
        }

        void IObjectWritable.WriteTo(ObjectWriter writer)
        {
            WriteTo(writer);
        }

        internal void WriteTo(ObjectWriter writer)
        {
            writer.WriteInt64(this.utcLastModified.ToBinary());
            writer.WriteInt32(this.localIncrement);
            writer.WriteInt32(this.globalIncrement);
        }

        internal static VersionStamp ReadFrom(ObjectReader reader)
        {
            var raw = reader.ReadInt64();
            var localIncrement = reader.ReadInt32();
            var globalIncrement = reader.ReadInt32();

            return new VersionStamp(DateTime.FromBinary(raw), localIncrement, globalIncrement);
        }

        private static int GetGlobalVersion(VersionStamp version)
        {
            // global increment < 0 means it is a global version which has its global increment in local increment
            return version.globalIncrement >= 0 ? version.globalIncrement : version.localIncrement;
        }

        private static int GetNextGlobalVersion()
        {
            // REVIEW: not sure what is best way to wrap it when it overflows. should I just throw or don't care.
            // with 50ms (typing) as an interval for a new version, it gives more than 1 year before int32 to overflow.
            // with 5ms as an interval, it gives more than 120 days before it overflows.
            // since global version is only for per VS session, I think we don't need to worry about overflow.
            // or we could use Int64 which will give more than a milliion years turn around even on 1ms internval.

            // this will let versions to be compared safely between multiple items
            // without worring about collision within same session
            var globalVersion = Interlocked.Increment(ref VersionStamp.globalVersion);

            return globalVersion;
        }

        public static readonly VersionStamp Default = new VersionStamp(default(DateTime));

        /// <summary>
        /// True if this VersionStamp is newer than the specified one.
        /// </summary>
        internal bool TestOnly_IsNewerThan(VersionStamp version)
        {
            if (this.utcLastModified > version.utcLastModified)
            {
                return true;
            }

            if (this.utcLastModified == version.utcLastModified)
            {
                return GetGlobalVersion(this) > GetGlobalVersion(version);
            }

            return false;
        }
    }
}