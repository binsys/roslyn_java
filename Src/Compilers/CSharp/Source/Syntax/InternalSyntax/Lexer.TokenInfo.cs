// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Text;

using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{



	internal partial class Lexer : LexerBase
	{

		internal struct TokenInfo
		{
			// scanned values
			internal SyntaxKind Kind;
			internal SyntaxKind ContextualKind;
			internal string Text;
			internal SpecialType ValueKind;
			internal bool RequiresTextForXmlEntity;
			internal bool HasIdentifierEscapeSequence;
			internal string StringValue;
			internal char CharValue;
			internal int IntValue;
			internal uint UintValue;
			internal long LongValue;
			internal ulong UlongValue;
			internal float FloatValue;
			internal double DoubleValue;
			internal decimal DecimalValue;
			internal bool IsVerbatim;
		}
	}
}