﻿using System;
using System.Collections.Generic;
using System.Threading;

namespace Microsoft.CodeAnalysis.Diagnostics
{
    /// <summary>
    /// An analyzer that is invoked when the compiler's semantic analysis has completed.
    /// </summary>
    public interface ICompilationEndedAnalyzer : IDiagnosticAnalyzer
    {
        /// <summary>
        /// Called when the compiler's semantic analysis has completed.
        /// </summary>
        /// <param name="compilation">The compilation</param>
        /// <param name="addDiagnostic">A delegate to be used to emit diagnostics</param>
        /// <param name="cancellationToken">A token for cancelling the computation</param>
        void OnCompilationEnded(Compilation compilation, Action<Diagnostic> addDiagnostic, CancellationToken cancellationToken);
    }
}
