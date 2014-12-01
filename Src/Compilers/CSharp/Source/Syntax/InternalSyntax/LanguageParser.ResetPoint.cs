// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading;

using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
	internal partial class LanguageParser : SyntaxParser
	{

		private new struct ResetPoint
		{
			internal SyntaxParser.ResetPoint BaseResetPoint;
			internal readonly TerminatorState TerminatorState;
			internal readonly bool IsInTry;

			internal ResetPoint(
				SyntaxParser.ResetPoint resetPoint,
				TerminatorState terminatorState,
				bool isInTry)
			{
				this.BaseResetPoint = resetPoint;
				this.TerminatorState = terminatorState;
				this.IsInTry = isInTry;
			}
		}
	}
}