// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Threading;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp
{
	public partial class CSharpSyntaxTree
	{
		internal sealed class DummySyntaxTree : CSharpSyntaxTree
		{
			private readonly CompilationUnitSyntax node;

			public DummySyntaxTree()
			{
				node = this.CloneNodeAsRoot(SyntaxFactory.ParseCompilationUnit(string.Empty));
			}

			public override SourceText GetText(CancellationToken cancellationToken)
			{
				return SourceText.From(String.Empty);
			}

			public override bool TryGetText(out SourceText text)
			{
				text = SourceText.From(string.Empty);
				return true;
			}

			public override int Length
			{
				get { return 0; }
			}

			public override CSharpParseOptions Options
			{
				get { return CSharpParseOptions.Default; }
			}

			public override string FilePath
			{
				get { return String.Empty; }
			}

			public override SyntaxReference GetReference(SyntaxNode node)
			{
				return new SimpleSyntaxReference(node);
			}

			public override CSharpSyntaxNode GetRoot(CancellationToken cancellationToken)
			{
				return node;
			}

			public override bool TryGetRoot(out CSharpSyntaxNode root)
			{
				root = node;
				return true;
			}

			public override bool HasCompilationUnitRoot
			{
				get { return true; }
			}

			public override FileLinePositionSpan GetLineSpan(TextSpan span, CancellationToken cancellationToken = default(CancellationToken))
			{
				return default(FileLinePositionSpan);
			}
		}
	}
}
