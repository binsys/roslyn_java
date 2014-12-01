// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Runtime.Serialization;

namespace Microsoft.CodeAnalysis.CSharp
{
	/// <summary>
	/// A diagnostic, along with the location where it occurred.
	/// </summary>
	[Serializable]
	internal sealed class CSDiagnostic : DiagnosticWithInfo
	{
		internal CSDiagnostic(DiagnosticInfo info, Location location)
			: base(info, location)
		{
		}

		private CSDiagnostic(SerializationInfo info, StreamingContext context)
			: base(info, context)
		{
		}

		protected override void GetObjectData(SerializationInfo info, StreamingContext context)
		{
			base.GetObjectData(info, context);
		}

		public override string ToString()
		{
			return CSharpDiagnosticFormatter.Instance.Format(this);
		}

		internal override Diagnostic WithLocation(Location location)
		{
			if (location == null)
			{
				throw new ArgumentNullException("location");
			}

			if (location != this.Location)
			{
				return new CSDiagnostic(this.Info, location);
			}

			return this;
		}

		internal override Diagnostic WithWarningAsError(bool isWarningAsError)
		{
			if (this.IsWarningAsError != isWarningAsError)
			{
				return new CSDiagnostic(this.Info.GetInstanceWithReportWarning(isWarningAsError), this.Location);
			}

			return this;
		}
	}
}