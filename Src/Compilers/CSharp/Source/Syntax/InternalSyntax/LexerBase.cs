using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
	/// <summary>
	/// separate out text windowing implementation (keeps scanning &amp; lexing functions from abusing details)
	/// </summary>
	internal class LexerBase : IDisposable
	{
		internal readonly SlidingTextWindow TextWindow;

		private List<SyntaxDiagnosticInfo> _errors;

		protected bool HasErrors
		{
			get { return this._errors != null; }
		}

		protected SyntaxDiagnosticInfo[] Errors
		{
			get { return this._errors == null ? null : this._errors.ToArray(); }
		}

		protected LexerBase(SourceText text)
		{
			this.TextWindow = new SlidingTextWindow(text);
		}

		public virtual void Dispose()
		{
			this.TextWindow.Dispose();
		}

		protected void Start()
		{
			TextWindow.Start();
			this._errors = null;
		}

		protected void AddError(int position, int width, ErrorCode code, params object[] args)
		{
			this.AddError(this.MakeError(position, width, code, args));
		}

		protected void AddError(int position, int width, XmlParseErrorCode code, params object[] args)
		{
			this.AddError(this.MakeError(position, width, code, args));
		}

		protected void AddError(ErrorCode code, params object[] args)
		{
			this.AddError(MakeError(code, args));
		}

		protected void AddError(XmlParseErrorCode code, params object[] args)
		{
			this.AddError(MakeError(code, args));
		}

		protected void AddError(SyntaxDiagnosticInfo error)
		{
			if (error != null)
			{
				if (this._errors == null)
				{
					this._errors = new List<SyntaxDiagnosticInfo>(8);
				}

				this._errors.Add(error);
			}
		}

		protected SyntaxDiagnosticInfo MakeError(int position, int width, ErrorCode code, params object[] args)
		{
			int offset = position >= TextWindow.LexemeStartPosition ? position - TextWindow.LexemeStartPosition : position;
			return new SyntaxDiagnosticInfo(offset, width, code, args);
		}

		protected XmlSyntaxDiagnosticInfo MakeError(int position, int width, XmlParseErrorCode code, params object[] args)
		{
			int offset = position >= TextWindow.LexemeStartPosition ? position - TextWindow.LexemeStartPosition : position;
			return new XmlSyntaxDiagnosticInfo(offset, width, code, args);
		}

		protected static SyntaxDiagnosticInfo MakeError(ErrorCode code, params object[] args)
		{
			return new SyntaxDiagnosticInfo(code, args);
		}

		protected static XmlSyntaxDiagnosticInfo MakeError(XmlParseErrorCode code, params object[] args)
		{
			return new XmlSyntaxDiagnosticInfo(0, 0, code, args);
		}
	}
}
