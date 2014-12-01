// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
	internal abstract partial class SyntaxParser : IDisposable
	{
		private readonly Lexer _lexer;
		private readonly bool _isIncremental;
		private readonly bool _allowModeReset;
		

		private LexerMode _mode;
		private Blender _firstBlender;
		private BlendedNode _currentNode;
		private SyntaxToken _currentToken;
		private ArrayElement<SyntaxToken>[] _lexedTokens;
		private CSharpSyntaxNode _prevTokenTrailingTrivia;
		private int _firstToken;
		private int _tokenOffset;
		private int _tokenCount;
		private int _resetCount;
		private int _resetStart;

		private static readonly ObjectPool<BlendedNode[]> BlendedNodesPool = new ObjectPool<BlendedNode[]>(() => new BlendedNode[32], 2);

		private BlendedNode[] _blendedTokens;

		protected readonly CancellationToken CancellationToken;

		protected SyntaxParser(
			Lexer lexer,
			LexerMode mode,
			CSharp.CSharpSyntaxNode oldTree,
			IEnumerable<TextChangeRange> changes,
			bool allowModeReset,
			bool preLexIfNotIncremental = false,
			CancellationToken cancellationToken = default(CancellationToken))
		{
			this._lexer = lexer;
			this._mode = mode;
			this._allowModeReset = allowModeReset;
			this.CancellationToken = cancellationToken;
			this._currentNode = default(BlendedNode);
			this._isIncremental = oldTree != null;

			if (this.IsIncremental || allowModeReset)
			{
				this._firstBlender = new Blender(lexer, oldTree, changes);
				this._blendedTokens = BlendedNodesPool.Allocate();
			}
			else
			{
				this._firstBlender = default(Blender);
				this._lexedTokens = new ArrayElement<SyntaxToken>[32];
			}

			// PreLex is not cancellable. 
			//      If we may cancell why would we aggressively lex ahead?
			//      Cancellations in a constructor make disposing complicated
			//
			// So, if we have a real cancellation token, do not do prelexing.
			if (preLexIfNotIncremental && !this.IsIncremental && !cancellationToken.CanBeCanceled)
			{
				this.PreLex();
			}
		}

		public void Dispose()
		{
			var blendedTokens = this._blendedTokens;
			if (blendedTokens != null)
			{
				Array.Clear(this._blendedTokens, 0, this._blendedTokens.Length);
				BlendedNodesPool.Free(this._blendedTokens);
				this._blendedTokens = null;
			}
		}

		protected void ReInitialize()
		{
			this._firstToken = 0;
			this._tokenOffset = 0;
			this._tokenCount = 0;
			this._resetCount = 0;
			this._resetStart = 0;
			this._currentToken = null;
			this._prevTokenTrailingTrivia = null;
			if (this.IsIncremental || _allowModeReset)
			{
				this._firstBlender = new Blender(this._lexer, null, null);
			}
		}

		protected bool IsIncremental
		{
			get
			{
				return this._isIncremental;
			}
		}

		private void PreLex()
		{
			// NOTE: Do not cancel in this method. It is called from the constructor.
			var size = Math.Min(4096, Math.Max(32, this._lexer.TextWindow.Text.Length / 2));
			this._lexedTokens = new ArrayElement<SyntaxToken>[size];
			var lexer = this._lexer;
			var mode = this._mode;

			for (int i = 0; i < size; i++)
			{
				var token = lexer.Lex(mode);
				this.AddLexedToken(token);
				if (token.Kind == SyntaxKind.EndOfFileToken)
				{
					break;
				}
			}
		}

		protected ResetPoint GetResetPoint()
		{
			var pos = this._firstToken + this._tokenOffset;
			if (this._resetCount == 0)
			{
				this._resetStart = pos; // low water mark
			}

			this._resetCount++;
			return new ResetPoint(this._resetCount, this._mode, pos, _prevTokenTrailingTrivia);
		}

		protected void Reset(ref ResetPoint point)
		{
			this._mode = point.Mode;
			var offset = point.Position - this._firstToken;
			Debug.Assert(offset >= 0 && offset < this._tokenCount);
			this._tokenOffset = offset;
			this._currentToken = default(SyntaxToken);
			this._currentNode = default(BlendedNode);
			this._prevTokenTrailingTrivia = point.PrevTokenTrailingTrivia;
			if (this._blendedTokens != null)
			{
				// look forward for slots not holding a token
				for (int i = this._tokenOffset; i < this._tokenCount; i++)
				{
					if (this._blendedTokens[i].Token == null)
					{
						// forget anything after and including any slot not holding a token
						this._tokenCount = i;
						break;
					}
				}
			}
		}

		protected void Release(ref ResetPoint point)
		{
			Debug.Assert(this._resetCount == point.ResetCount);
			this._resetCount--;
			if (this._resetCount == 0)
			{
				this._resetStart = -1;
			}
		}

		public CSharpParseOptions Options
		{
			get { return this._lexer.Options; }
		}


		protected LexerMode Mode
		{
			get
			{
				return this._mode;
			}

			set
			{
				if (this._mode != value)
				{
					Debug.Assert(this._allowModeReset);

					this._mode = value;
					this._currentToken = default(SyntaxToken);
					this._currentNode = default(BlendedNode);
					this._tokenCount = this._tokenOffset;
				}
			}
		}

		protected CSharp.CSharpSyntaxNode CurrentNode
		{
			get
			{
				// we will fail anyways. Assert is just to catch that earlier.
				Debug.Assert(this._blendedTokens != null);

				//PERF: currentNode is a BlendedNode, which is a fairly large struct.
				// the following code tries not to pull the whole struct into a local
				// we only need .Node
				var node = this._currentNode.Node;
				if (node != null)
				{
					return node;
				}

				this.ReadCurrentNode();
				return this._currentNode.Node;
			}
		}

		protected SyntaxKind CurrentNodeKind
		{
			get
			{
				var cn = this.CurrentNode;
				return cn != null ? cn.Kind : SyntaxKind.None;
			}
		}

		private void ReadCurrentNode()
		{
			if (this._tokenOffset == 0)
			{
				this._currentNode = this._firstBlender.ReadNode(this._mode);
			}
			else
			{
				this._currentNode = this._blendedTokens[this._tokenOffset - 1].Blender.ReadNode(this._mode);
			}
		}

		protected GreenNode EatNode()
		{
			// we will fail anyways. Assert is just to catch that earlier.
			Debug.Assert(this._blendedTokens != null);

			// remember result
			var result = CurrentNode.Green;

			// store possible non-token in token sequence 
			if (this._tokenOffset >= this._blendedTokens.Length)
			{
				this.AddTokenSlot();
			}

			this._blendedTokens[this._tokenOffset++] = this._currentNode;
			this._tokenCount = this._tokenOffset; // forget anything after this slot

			// erase current state
			this._currentNode = default(BlendedNode);
			this._currentToken = default(SyntaxToken);

			return result;
		}

		protected SyntaxToken CurrentToken
		{
			get
			{
				return this._currentToken ?? (this._currentToken = this.FetchCurrentToken());
			}
		}

		private SyntaxToken FetchCurrentToken()
		{
			if (this._tokenOffset >= this._tokenCount)
			{
				this.AddNewToken();
			}

			if (this._blendedTokens != null)
			{
				return this._blendedTokens[this._tokenOffset].Token;
			}
			else
			{
				return this._lexedTokens[this._tokenOffset];
			}
		}

		private void AddNewToken()
		{
			if (this._blendedTokens != null)
			{
				if (this._tokenCount > 0)
				{
					this.AddToken(this._blendedTokens[this._tokenCount - 1].Blender.ReadToken(this._mode));
				}
				else
				{
					if (this._currentNode.Token != null)
					{
						this.AddToken(this._currentNode);
					}
					else
					{
						this.AddToken(this._firstBlender.ReadToken(this._mode));
					}
				}
			}
			else
			{
				this.AddLexedToken(this._lexer.Lex(this._mode));
			}
		}

		// adds token to end of current token array
		private void AddToken(BlendedNode tokenResult)
		{
			Debug.Assert(tokenResult.Token != null);
			if (this._tokenCount >= this._blendedTokens.Length)
			{
				this.AddTokenSlot();
			}

			this._blendedTokens[this._tokenCount] = tokenResult;
			this._tokenCount++;
		}

		private void AddLexedToken(SyntaxToken token)
		{
			Debug.Assert(token != null);
			if (this._tokenCount >= this._lexedTokens.Length)
			{
				this.AddLexedTokenSlot();
			}

			this._lexedTokens[this._tokenCount].Value = token;
			this._tokenCount++;
		}

		private void AddTokenSlot()
		{
			// shift tokens to left if we are far to the right
			// don't shift if reset points have fixed locked tge starting point at the token in the window
			if (this._tokenOffset > (this._blendedTokens.Length >> 1)
				&& (this._resetStart == -1 || this._resetStart > this._firstToken))
			{
				int shiftOffset = (this._resetStart == -1) ? this._tokenOffset : this._resetStart - this._firstToken;
				int shiftCount = this._tokenCount - shiftOffset;
				Debug.Assert(shiftOffset > 0);
				this._firstBlender = this._blendedTokens[shiftOffset - 1].Blender;
				if (shiftCount > 0)
				{
					Array.Copy(this._blendedTokens, shiftOffset, this._blendedTokens, 0, shiftCount);
				}

				this._firstToken += shiftOffset;
				this._tokenCount -= shiftOffset;
				this._tokenOffset -= shiftOffset;
			}
			else
			{
				var old = this._blendedTokens;
				Array.Resize(ref this._blendedTokens, this._blendedTokens.Length * 2);
				BlendedNodesPool.ForgetTrackedObject(old, replacement: this._blendedTokens);
			}
		}

		private void AddLexedTokenSlot()
		{
			// shift tokens to left if we are far to the right
			// don't shift if reset points have fixed locked tge starting point at the token in the window
			if (this._tokenOffset > (this._lexedTokens.Length >> 1)
				&& (this._resetStart == -1 || this._resetStart > this._firstToken))
			{
				int shiftOffset = (this._resetStart == -1) ? this._tokenOffset : this._resetStart - this._firstToken;
				int shiftCount = this._tokenCount - shiftOffset;
				Debug.Assert(shiftOffset > 0);
				if (shiftCount > 0)
				{
					Array.Copy(this._lexedTokens, shiftOffset, this._lexedTokens, 0, shiftCount);
				}

				this._firstToken += shiftOffset;
				this._tokenCount -= shiftOffset;
				this._tokenOffset -= shiftOffset;
			}
			else
			{
				var tmp = new ArrayElement<SyntaxToken>[this._lexedTokens.Length * 2];
				Array.Copy(this._lexedTokens, tmp, this._lexedTokens.Length);
				this._lexedTokens = tmp;
			}
		}

		protected SyntaxToken PeekToken(int n)
		{
			Debug.Assert(n >= 0);
			while (this._tokenOffset + n >= this._tokenCount)
			{
				this.AddNewToken();
			}

			if (this._blendedTokens != null)
			{
				return this._blendedTokens[this._tokenOffset + n].Token;
			}
			else
			{
				return this._lexedTokens[this._tokenOffset + n];
			}
		}

		//this method is called very frequently
		//we should keep it simple so that it can be inlined.
		protected SyntaxToken EatToken()
		{
			var ct = this.CurrentToken;
			MoveToNextToken();
			return ct;
		}

		private void MoveToNextToken()
		{
			this._prevTokenTrailingTrivia = this._currentToken.GetTrailingTrivia();

			this._currentToken = default(SyntaxToken);

			if (this._blendedTokens != null)
			{
				this._currentNode = default(BlendedNode);
			}

			this._tokenOffset++;
		}

		//this method is called very frequently
		//we should keep it simple so that it can be inlined.
		protected SyntaxToken EatToken(SyntaxKind kind)
		{
			Debug.Assert(SyntaxKindFacts.IsAnyToken(kind));

			var ct = this.CurrentToken;
			if (ct.Kind == kind)
			{
				MoveToNextToken();
				return ct;
			}

			//slow part of EatToken(SyntaxKind kind)
			return CreateMissingToken(kind, this.CurrentToken.Kind, reportError: true);
		}

		private SyntaxToken CreateMissingToken(SyntaxKind expected, SyntaxKind actual, bool reportError)
		{
			// should we eat the current ParseToken's leading trivia?
			var token = SyntaxFactory.MissingToken(expected);
			if (reportError)
			{
				token = WithAdditionalDiagnostics(token, this.GetExpectedTokenError(expected, actual));
			}

			return token;
		}

		private SyntaxToken CreateMissingToken(SyntaxKind expected, ErrorCode code, bool reportError)
		{
			// should we eat the current ParseToken's leading trivia?
			var token = SyntaxFactory.MissingToken(expected);
			if (reportError)
			{
				token = AddError(token, code);
			}

			return token;
		}

		protected SyntaxToken EatToken(SyntaxKind kind, bool reportError)
		{
			if (reportError)
			{
				return EatToken(kind);
			}

			Debug.Assert(SyntaxKindFacts.IsAnyToken(kind));
			if (this.CurrentToken.Kind != kind)
			{
				// should we eat the current ParseToken's leading trivia?
				return SyntaxFactory.MissingToken(kind);
			}
			else
			{
				return this.EatToken();
			}
		}

		protected SyntaxToken EatToken(SyntaxKind kind, ErrorCode code, bool reportError = true)
		{
			Debug.Assert(SyntaxKindFacts.IsAnyToken(kind));
			if (this.CurrentToken.Kind != kind)
			{
				return CreateMissingToken(kind, code, reportError);
			}
			else
			{
				return this.EatToken();
			}
		}

		protected SyntaxToken EatTokenWithPrejudice(SyntaxKind kind)
		{
			var token = this.CurrentToken;
			Debug.Assert(SyntaxKindFacts.IsAnyToken(kind));
			if (token.Kind != kind)
			{
				token = WithAdditionalDiagnostics(token, this.GetExpectedTokenError(kind, token.Kind));
			}

			this.MoveToNextToken();
			return token;
		}

		protected SyntaxToken EatTokenWithPrejudice(ErrorCode errorCode, params object[] args)
		{
			var token = this.EatToken();
			token = WithAdditionalDiagnostics(token, MakeError(token.GetLeadingTriviaWidth(), token.Width, errorCode, args));
			return token;
		}

		protected SyntaxToken EatContextualToken(SyntaxKind kind, ErrorCode code, bool reportError = true)
		{
			Debug.Assert(SyntaxKindFacts.IsAnyToken(kind));

			if (this.CurrentToken.ContextualKind != kind)
			{
				return CreateMissingToken(kind, code, reportError);
			}
			else
			{
				return ConvertToKeyword(this.EatToken());
			}
		}

		protected SyntaxToken EatContextualToken(SyntaxKind kind, bool reportError = true)
		{
			Debug.Assert(SyntaxKindFacts.IsAnyToken(kind));

			var contextualKind = this.CurrentToken.ContextualKind;
			if (contextualKind != kind)
			{
				return CreateMissingToken(kind, contextualKind, reportError);
			}
			else
			{
				return ConvertToKeyword(this.EatToken());
			}
		}

		protected virtual SyntaxDiagnosticInfo GetExpectedTokenError(SyntaxKind expected, SyntaxKind actual, int offset, int width)
		{
			var code = GetExpectedTokenErrorCode(expected, actual);
			if (code == ErrorCode.ERR_SyntaxError || code == ErrorCode.ERR_IdentifierExpectedKW)
			{
				return new SyntaxDiagnosticInfo(offset, width, code, SyntaxKindFacts.GetText(expected), SyntaxKindFacts.GetText(actual));
			}
			else
			{
				return new SyntaxDiagnosticInfo(offset, width, code);
			}
		}

		protected virtual SyntaxDiagnosticInfo GetExpectedTokenError(SyntaxKind expected, SyntaxKind actual)
		{
			int offset, width;
			this.GetDiagnosticSpanForMissingToken(out offset, out width);

			return this.GetExpectedTokenError(expected, actual, offset, width);
		}

		private static ErrorCode GetExpectedTokenErrorCode(SyntaxKind expected, SyntaxKind actual)
		{
			switch (expected)
			{
				case SyntaxKind.IdentifierToken:
					if (SyntaxKindFacts.IsReservedKeyword(actual))
					{
						return ErrorCode.ERR_IdentifierExpectedKW;   // A keyword -- use special message.
					}
					else
					{
						return ErrorCode.ERR_IdentifierExpected;
					}

				case SyntaxKind.SemicolonToken:
					return ErrorCode.ERR_SemicolonExpected;

				// case TokenKind::Colon:         iError = ERR_ColonExpected;          break;
				// case TokenKind::OpenParen:     iError = ERR_LparenExpected;         break;
				case SyntaxKind.CloseParenToken:
					return ErrorCode.ERR_CloseParenExpected;
				case SyntaxKind.OpenBraceToken:
					return ErrorCode.ERR_LbraceExpected;
				case SyntaxKind.CloseBraceToken:
					return ErrorCode.ERR_RbraceExpected;

				// case TokenKind::CloseSquare:   iError = ERR_CloseSquareExpected;    break;
				default:
					return ErrorCode.ERR_SyntaxError;
			}
		}

		protected void GetDiagnosticSpanForMissingToken(out int offset, out int width)
		{
			// If the previous token has a trailing EndOfLineTrivia,
			// the missing token diagnostic position is moved to the
			// end of line containing the previous token and
			// its width is set to zero.
			// Otherwise the diagnostic offset and width is set
			// to the corresponding values of the current token

			var trivia = this._prevTokenTrailingTrivia;
			if (trivia != null)
			{
				SyntaxList<CSharpSyntaxNode> triviaList = new SyntaxList<CSharpSyntaxNode>(trivia);
				bool prevTokenHasEndOfLineTrivia = triviaList.Any(SyntaxKind.EndOfLineTrivia);
				if (prevTokenHasEndOfLineTrivia)
				{
					offset = -trivia.FullWidth;
					width = 0;
					return;
				}
			}

			SyntaxToken ct = this.CurrentToken;
			offset = ct.GetLeadingTriviaWidth();
			width = ct.Width;
		}

		protected virtual TNode WithAdditionalDiagnostics<TNode>(TNode node, params DiagnosticInfo[] diagnostics) where TNode : CSharpSyntaxNode
		{
			DiagnosticInfo[] existingDiags = node.GetDiagnostics();

			//EDMAURER avoid the creation of a bunch of temporary objects that aggravate the memory situation
			//when the parser gets profoundly confused and produces many missing token diagnostics.

			int existingLength = existingDiags.Length;
			if (existingLength == 0)
				return node.WithDiagnosticsGreen(diagnostics);
			else
			{
				DiagnosticInfo[] result = new DiagnosticInfo[existingDiags.Length + diagnostics.Length];
				existingDiags.CopyTo(result, 0);
				diagnostics.CopyTo(result, existingLength);
				return node.WithDiagnosticsGreen(result);
			}
		}

		protected TNode AddError<TNode>(TNode node, ErrorCode code, params object[] args) where TNode : CSharpSyntaxNode
		{
			if (!node.IsMissing)
			{
				return WithAdditionalDiagnostics(node, MakeError(node, code, args));
			}

			int offset, width;

			SyntaxToken token = node as SyntaxToken;
			if (token != null && token.ContainsSkippedText)
			{
				// This code exists to clean up an anti-pattern:
				//   1) an undesirable token is parsed,
				//   2) a desirable missing token is created and the parsed token is appended as skipped text,
				//   3) an error is attached to the missing token describing the problem.
				// If this occurs, then this.previousTokenTrailingTrivia is still populated with the trivia 
				// of the undesirable token (now skipped text).  Since the trivia no longer precedes the
				// node to which the error is to be attached, the computed offset will be incorrect.

				offset = token.GetLeadingTriviaWidth(); // Should always be zero, but at least we'll do something sensible if it's not.
				Debug.Assert(offset == 0, "Why are we producing a missing token that has both skipped text and leading trivia?");

				width = 0;
				bool seenSkipped = false;
				foreach (var trivia in token.TrailingTrivia)
				{
					if (trivia.Kind == SyntaxKind.SkippedTokensTrivia)
					{
						seenSkipped = true;
						width += trivia.Width;
					}
					else if (seenSkipped)
					{
						break;
					}
					else
					{
						offset += trivia.Width;
					}
				}
			}
			else
			{
				this.GetDiagnosticSpanForMissingToken(out offset, out width);
			}

			return WithAdditionalDiagnostics(node, MakeError(offset, width, code, args));
		}

		protected TNode AddError<TNode>(TNode node, int offset, int length, ErrorCode code, params object[] args) where TNode : CSharpSyntaxNode
		{
			return WithAdditionalDiagnostics(node, MakeError(offset, length, code, args));
		}

		protected TNode AddError<TNode>(TNode node, CSharpSyntaxNode location, ErrorCode code, params object[] args) where TNode : CSharpSyntaxNode
		{
			// assumes non-terminals will at most appear once in sub-tree
			int offset;
			FindOffset(node, location, out offset);
			return WithAdditionalDiagnostics(node, MakeError(offset, location.Width, code, args));
		}

		protected TNode AddErrorToFirstToken<TNode>(TNode node, ErrorCode code, params object[] args) where TNode : CSharpSyntaxNode
		{
			var firstToken = node.GetFirstToken();
			return WithAdditionalDiagnostics(node, MakeError(firstToken.GetLeadingTriviaWidth(), firstToken.Width, code, args));
		}

		protected TNode AddErrorToLastToken<TNode>(TNode node, ErrorCode code, params object[] args) where TNode : CSharpSyntaxNode
		{
			var lastToken = node.GetLastNonmissingToken();
			var offset = node.FullWidth; //advance to end of entire node
			var width = 0;
			if (lastToken != null) //will be null if all tokens are missing
			{
				offset -= lastToken.FullWidth; //rewind past last token
				offset += lastToken.GetLeadingTriviaWidth(); //advance past last token leading trivia - now at start of last token
				width += lastToken.Width;
			}
			return WithAdditionalDiagnostics(node, MakeError(offset, width, code, args));
		}

		protected static SyntaxDiagnosticInfo MakeError(int offset, int width, ErrorCode code, params object[] args)
		{
			return new SyntaxDiagnosticInfo(offset, width, code, args);
		}

		protected static SyntaxDiagnosticInfo MakeError(CSharpSyntaxNode node, ErrorCode code, params object[] args)
		{
			return new SyntaxDiagnosticInfo(node.GetLeadingTriviaWidth(), node.Width, code, args);
		}

		protected static SyntaxDiagnosticInfo MakeError(ErrorCode code, params object[] args)
		{
			return new SyntaxDiagnosticInfo(code, args);
		}

		protected TNode AddLeadingSkippedSyntax<TNode>(TNode node, CSharpSyntaxNode skippedSyntax) where TNode : CSharpSyntaxNode
		{
			var oldToken = node as SyntaxToken ?? node.GetFirstToken();
			var newToken = AddSkippedSyntax(oldToken, skippedSyntax, trailing: false);
			return SyntaxFirstTokenReplacer.Replace(node, oldToken, newToken, skippedSyntax.FullWidth);
		}

		protected TNode AddTrailingSkippedSyntax<TNode>(TNode node, CSharpSyntaxNode skippedSyntax) where TNode : CSharpSyntaxNode
		{
			var token = node as SyntaxToken;
			if (token != null)
			{
				return (TNode)(object)AddSkippedSyntax(token, skippedSyntax, trailing: true);
			}
			else
			{
				var lastToken = node.GetLastToken();
				var newToken = AddSkippedSyntax(lastToken, skippedSyntax, trailing: true);
				return SyntaxLastTokenReplacer.Replace(node, newToken);
			}
		}

		/// <summary>
		/// Converts skippedSyntax node into tokens and adds these as trivia on the target token.
		/// Also adds the first error (in depth-first preorder) found in the skipped syntax tree to the target token.
		/// </summary>
		internal SyntaxToken AddSkippedSyntax(SyntaxToken target, CSharpSyntaxNode skippedSyntax, bool trailing)
		{
			var builder = new SyntaxListBuilder(4);

			// the error in we'll attach to the node
			SyntaxDiagnosticInfo diagnostic = null;

			// the position of the error within the skipedSyntax node full tree
			int diagnosticOffset = 0;

			int currentOffset = 0;
			foreach (var node in skippedSyntax.EnumerateNodes())
			{
				SyntaxToken token = node as SyntaxToken;
				if (token != null)
				{
					builder.Add(token.GetLeadingTrivia());

					if (token.Width > 0)
					{
						// separate trivia from the tokens
						SyntaxToken tk = token.WithLeadingTrivia(null).WithTrailingTrivia(null);

						// adjust relative offsets of diagnostics attached to the token:
						int leadingWidth = token.GetLeadingTriviaWidth();
						if (leadingWidth > 0)
						{
							var tokenDiagnostics = tk.GetDiagnostics();
							for (int i = 0; i < tokenDiagnostics.Length; i++)
							{
								var d = (SyntaxDiagnosticInfo)tokenDiagnostics[i];
								tokenDiagnostics[i] = new SyntaxDiagnosticInfo(d.Offset - leadingWidth, d.Width, (ErrorCode)d.Code, d.Arguments);
							}
						}

						builder.Add(SyntaxFactory.SkippedTokensTrivia(tk));
					}
					else
					{
						// do not create zero-width structured trivia, GetStructure doesn't work well for them
						var existing = (SyntaxDiagnosticInfo)token.GetDiagnostics().FirstOrDefault();
						if (existing != null)
						{
							diagnostic = existing;
							diagnosticOffset = currentOffset;
						}
					}
					builder.Add(token.GetTrailingTrivia());

					currentOffset += token.FullWidth;
				}
				else if (node.ContainsDiagnostics && diagnostic == null)
				{
					// only propagate the first error to reduce noise:
					var existing = (SyntaxDiagnosticInfo)node.GetDiagnostics().FirstOrDefault();
					if (existing != null)
					{
						diagnostic = existing;
						diagnosticOffset = currentOffset;
					}
				}
			}

			int triviaWidth = currentOffset;
			var trivia = builder.ToListNode();

			// total width of everything preceding the added trivia
			int triviaOffset;
			if (trailing)
			{
				var trailingTrivia = target.GetTrailingTrivia();
				triviaOffset = target.FullWidth; //added trivia is full width (before addition)
				target = target.WithTrailingTrivia(SyntaxList.Concat(trailingTrivia, trivia));
			}
			else
			{
				// Since we're adding triviaWidth before the token, we have to add that much to
				// the offset of each of its diagnostics.
				if (triviaWidth > 0)
				{
					var targetDiagnostics = target.GetDiagnostics();
					for (int i = 0; i < targetDiagnostics.Length; i++)
					{
						var d = (SyntaxDiagnosticInfo)targetDiagnostics[i];
						targetDiagnostics[i] = new SyntaxDiagnosticInfo(d.Offset + triviaWidth, d.Width, (ErrorCode)d.Code, d.Arguments);
					}
				}

				var leadingTrivia = target.GetLeadingTrivia();
				target = target.WithLeadingTrivia(SyntaxList.Concat(trivia, leadingTrivia));
				triviaOffset = 0; //added trivia is first, so offset is zero
			}

			if (diagnostic != null)
			{
				int newOffset = triviaOffset + diagnosticOffset + diagnostic.Offset;

				target = WithAdditionalDiagnostics(target,
					new SyntaxDiagnosticInfo(newOffset, diagnostic.Width, (ErrorCode)diagnostic.Code, diagnostic.Arguments)
				);
			}

			return target;
		}

		/// <summary>
		/// This function searches for the given location node within the subtree rooted at root node. 
		/// If it finds it, the function computes the offset span of that child node within the root and returns true, 
		/// otherwise it returns false.
		/// </summary>
		/// <param name="root">Root node</param>
		/// <param name="location">Node to search in the subtree rooted at root node</param>
		/// <param name="offset">Offset of the location node within the subtree rooted at child</param>
		/// <returns></returns>
		private bool FindOffset(GreenNode root, CSharpSyntaxNode location, out int offset)
		{
			int currentOffset = 0;
			offset = 0;
			if (root != null)
			{
				for (int i = 0, n = root.SlotCount; i < n; i++)
				{
					var child = root.GetSlot(i);
					if (child == null)
					{
						// ignore null slots
						continue;
					}

					// check if the child node is the location node
					if (child == location)
					{
						// Found the location node in the subtree
						// Initialize offset with the offset of the location node within its parent
						// and walk up the stack of recursive calls adding the offset of each node
						// within its parent
						offset = currentOffset;
						return true;
					}

					// search for the location node in the subtree rooted at child node
					if (this.FindOffset(child, location, out offset))
					{
						// Found the location node in child's subtree
						// Add the offset of child node within its parent to offset
						// and continue walking up the stack
						offset += child.GetLeadingTriviaWidth() + currentOffset;
						return true;
					}

					// We didn't find the location node in the subtree rooted at child
					// Move on to the next child
					currentOffset += child.FullWidth;
				}
			}

			// We didn't find the location node within the subtree rooted at root node
			return false;
		}

		protected static SyntaxToken ConvertToKeyword(SyntaxToken token)
		{
			if (token.Kind != token.ContextualKind)
			{
				var kw = token.IsMissing
						? SyntaxFactory.MissingToken(token.LeadingTrivia.Node, token.ContextualKind, token.TrailingTrivia.Node)
						: SyntaxFactory.Token(token.LeadingTrivia.Node, token.ContextualKind, token.TrailingTrivia.Node);
				var d = token.GetDiagnostics();
				if (d != null && d.Length > 0)
				{
					kw = kw.WithDiagnosticsGreen(d);
				}

				return kw;
			}

			return token;
		}


		/// <remarks>
		/// NOTE: we are specifically diverging from dev11 to improve the user experience.
		/// Since treating the "async" keyword as an identifier in older language
		/// versions can never result in a correct program, we instead accept it as the a
		/// keyword regardless of the language version and produce an error if the version
		/// is insufficient.
		/// </remarks>
		protected TNode CheckFeatureAvailability<TNode>(TNode node, MessageID feature, bool forceWarning = false)
			where TNode : CSharpSyntaxNode
		{
			LanguageVersion availableVersion = this.Options.LanguageVersion;

			if (feature == MessageID.IDS_FeatureModuleAttrLoc)
			{
				// There's a special error code for this feature, so handle it separately.
				return availableVersion >= LanguageVersion.CSharp2
					? node
					: this.AddError(node, ErrorCode.WRN_NonECMAFeature, feature.Localize());
			}

			LanguageVersion requiredVersion = feature.RequiredVersion();

			if (availableVersion >= requiredVersion)
			{
				return node;
			}

			if (!forceWarning)
			{
				return this.AddError(node, availableVersion.GetErrorCode(), feature.Localize(), (int)requiredVersion);
			}

			SyntaxDiagnosticInfo rawInfo = new SyntaxDiagnosticInfo(availableVersion.GetErrorCode(), feature.Localize(), (int)requiredVersion);
			return this.AddError(node, ErrorCode.WRN_ErrorOverride, rawInfo, rawInfo.Code);
		}
	}
}
