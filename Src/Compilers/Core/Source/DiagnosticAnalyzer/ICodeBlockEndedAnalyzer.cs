﻿using System;
using System.Collections.Generic;
using System.Threading;

namespace Microsoft.CodeAnalysis.Diagnostics
{
    /// <summary>
    /// An analyzer that is invoked after running all other analyzers on a method body or field initializer.
    /// </summary>
    public interface ICodeBlockEndedAnalyzer : IDiagnosticAnalyzer
    {
        /// <summary>
        /// Invoked after running all other analyzers on a method body or field initializer.
        /// </summary>
        /// <param name="codeBlock">The code block of a method or a field initializer</param>
        /// <param name="ownerSymbol">The method or field</param>
        /// <param name="semanticModel">A SemanticModel for the compilation unit</param>
        /// <param name="addDiagnostic">A delegate to be used to emit diagnostics</param>
        /// <param name="cancellationToken">A token for cancelling the computation</param>
        void OnCodeBlockEnded(SyntaxNode codeBlock, ISymbol ownerSymbol, SemanticModel semanticModel, Action<Diagnostic> addDiagnostic, CancellationToken cancellationToken);
    }
}
