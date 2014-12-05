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
	[Flags]
	internal enum LexerMode
	{
		Syntax = 0x0001,
		DebuggerSyntax = 0x0002,
		Directive = 0x0004,

		XmlDocComment = 0x0008,
		XmlElementTag = 0x0010,
		XmlAttributeTextQuote = 0x0020,
		XmlAttributeTextDoubleQuote = 0x0040,
		XmlCrefQuote = 0x0080,
		XmlCrefDoubleQuote = 0x0100,
		XmlNameQuote = 0x0200,
		XmlNameDoubleQuote = 0x0400,
		XmlCDataSectionText = 0x0800,
		XmlCommentText = 0x1000,
		XmlProcessingInstructionText = 0x2000,
		XmlCharacter = 0x4000,
		MaskLexMode = 0xFFFF,

		// The following are lexer driven, which is to say the lexer can push a change back to the
		// blender. There is in general no need to use a whole bit per enum value, but the debugging
		// experience is bad if you don't do that.

		XmlDocCommentLocationStart = 0x00000,
		XmlDocCommentLocationInterior = 0x10000,
		XmlDocCommentLocationExterior = 0x20000,
		XmlDocCommentLocationEnd = 0x40000,
		MaskXmlDocCommentLocation = 0xF0000,

		XmlDocCommentStyleSingleLine = 0x000000,
		XmlDocCommentStyleDelimited = 0x100000,
		MaskXmlDocCommentStyle = 0x300000,

		None = 0
	}



	internal partial class Lexer : LexerBase
	{
		private const int TriviaListInitialCapacity = 8;

		private readonly CSharpParseOptions _options;
		private readonly StringBuilder _builder;
		private readonly LexerCache _cache;

		private LexerMode _mode;
		private char[] _identBuffer;
		private int _identLen;
		
		private SyntaxListBuilder _leadingTriviaCache = new SyntaxListBuilder(10);
		private SyntaxListBuilder _trailingTriviaCache = new SyntaxListBuilder(10);
		private Func<SyntaxTrivia> _createWhitespaceTriviaFunction;

#if DEBUG
		internal static int TokensLexed;
#endif

		public CSharpParseOptions Options
		{
			get { return this._options; }
		}

		public Lexer(SourceText text, CSharpParseOptions options)
			: base(text)
		{
			Debug.Assert(options != null);

			this._options = options;
			this._builder = new StringBuilder();
			this._identBuffer = new char[32];
			this._cache = new LexerCache();
			this.createQuickTokenFunction = this.CreateQuickToken;
		}

		public override void Dispose()
		{
			_cache.Free();

			if (_xmlParser != null)
			{
				_xmlParser.Dispose();
			}

			base.Dispose();
		}


		public void Reset(int position)
		{
			this.TextWindow.Reset(position);
		}

		

		private bool ModeIs(LexerMode mode)
		{
			return ModeOf(this._mode) == mode;
		}



		private static LexerMode ModeOf(LexerMode mode)
		{
			return mode & LexerMode.MaskLexMode;
		}

		public SyntaxToken Lex(ref LexerMode mode)
		{
			var result = Lex(mode);
			mode = this._mode;
			return result;
		}

		public SyntaxToken Lex(LexerMode mode)
		{
#if DEBUG
			TokensLexed++;
#endif
			this._mode = mode;
			switch (this._mode)
			{
				case LexerMode.Syntax:
				case LexerMode.DebuggerSyntax:
#if true
					var result = this.QuickScanSyntaxToken();
					if (result == null)
					{
						result = this.LexSyntaxToken();
					}

					return result;
#else
					return this.LexSyntaxToken();
#endif
				case LexerMode.Directive:
					return this.LexDirectiveToken();
			}

			switch (ModeOf(this._mode))
			{
				case LexerMode.XmlDocComment:
					return this.LexXmlToken();
				case LexerMode.XmlElementTag:
					return this.LexXmlElementTagToken();
				case LexerMode.XmlAttributeTextQuote:
				case LexerMode.XmlAttributeTextDoubleQuote:
					return this.LexXmlAttributeTextToken();
				case LexerMode.XmlCDataSectionText:
					return this.LexXmlCDataSectionTextToken();
				case LexerMode.XmlCommentText:
					return this.LexXmlCommentTextToken();
				case LexerMode.XmlProcessingInstructionText:
					return this.LexXmlProcessingInstructionTextToken();
				case LexerMode.XmlCrefQuote:
				case LexerMode.XmlCrefDoubleQuote:
					return this.LexXmlCrefOrNameToken();
				case LexerMode.XmlNameQuote:
				case LexerMode.XmlNameDoubleQuote:
					// Same lexing as a cref _annotation, just treat the identifiers a little differently.
					return this.LexXmlCrefOrNameToken();
				case LexerMode.XmlCharacter:
					return this.LexXmlCharacter();
			}

			Debug.Assert(false, "Unknown LexMode passed to Lexer.Lex");
			return this.LexSyntaxToken();
		}



		private SyntaxToken LexSyntaxToken()
		{
			_leadingTriviaCache.Clear();
			this.LexSyntaxTrivia(afterFirstToken: TextWindow.Position > 0, isTrailing: false, triviaList: ref _leadingTriviaCache);
			var leading = _leadingTriviaCache;

			var tokenInfo = default(TokenInfo);

			this.Start();
			this.ScanSyntaxToken(ref tokenInfo);
			var errors = this.Errors;

			_trailingTriviaCache.Clear();
			this.LexSyntaxTrivia(afterFirstToken: true, isTrailing: true, triviaList: ref _trailingTriviaCache);
			var trailing = _trailingTriviaCache;

			return Create(ref tokenInfo, leading, trailing, errors);
		}

		internal SyntaxTriviaList LexSyntaxLeadingTrivia()
		{
			_leadingTriviaCache.Clear();
			this.LexSyntaxTrivia(afterFirstToken: TextWindow.Position > 0, isTrailing: false, triviaList: ref _leadingTriviaCache);
			return new SyntaxTriviaList(default(Microsoft.CodeAnalysis.SyntaxToken), SyntaxList.List(_leadingTriviaCache), 0, 0);
		}

		internal SyntaxTriviaList LexSyntaxTrailingTrivia()
		{
			_trailingTriviaCache.Clear();
			this.LexSyntaxTrivia(afterFirstToken: true, isTrailing: true, triviaList: ref _trailingTriviaCache);
			return new SyntaxTriviaList(default(Microsoft.CodeAnalysis.SyntaxToken), SyntaxList.List(_trailingTriviaCache), 0, 0);
		}

		private SyntaxToken Create(ref TokenInfo info, SyntaxListBuilder leading, SyntaxListBuilder trailing, SyntaxDiagnosticInfo[] errors)
		{
			Debug.Assert(info.Kind != SyntaxKind.IdentifierToken || info.StringValue != null);

			var leadingNode = SyntaxList.List(leading);
			var trailingNode = SyntaxList.List(trailing);

			SyntaxToken token;
			if (info.RequiresTextForXmlEntity)
			{
				token = SyntaxFactory.Token(leadingNode, info.Kind, info.Text, info.StringValue, trailingNode);
			}
			else
			{
				switch (info.Kind)
				{
					case SyntaxKind.IdentifierToken:
						token = SyntaxFactory.Identifier(info.ContextualKind, leadingNode, info.Text, info.StringValue, trailingNode);
						break;
					case SyntaxKind.NumericLiteralToken:
						switch (info.ValueKind)
						{
							case SpecialType.System_Int32:
								token = SyntaxFactory.Literal(leadingNode, info.Text, info.IntValue, trailingNode);
								break;
							case SpecialType.System_UInt32:
								token = SyntaxFactory.Literal(leadingNode, info.Text, info.UintValue, trailingNode);
								break;
							case SpecialType.System_Int64:
								token = SyntaxFactory.Literal(leadingNode, info.Text, info.LongValue, trailingNode);
								break;
							case SpecialType.System_UInt64:
								token = SyntaxFactory.Literal(leadingNode, info.Text, info.UlongValue, trailingNode);
								break;
							case SpecialType.System_Single:
								token = SyntaxFactory.Literal(leadingNode, info.Text, info.FloatValue, trailingNode);
								break;
							case SpecialType.System_Double:
								token = SyntaxFactory.Literal(leadingNode, info.Text, info.DoubleValue, trailingNode);
								break;
							case SpecialType.System_Decimal:
								token = SyntaxFactory.Literal(leadingNode, info.Text, info.DecimalValue, trailingNode);
								break;
							default:
								throw ExceptionUtilities.UnexpectedValue(info.ValueKind);
						}

						break;
					case SyntaxKind.StringLiteralToken:
						token = SyntaxFactory.Literal(leadingNode, info.Text, info.StringValue, trailingNode);
						break;
					case SyntaxKind.CharacterLiteralToken:
						token = SyntaxFactory.Literal(leadingNode, info.Text, info.CharValue, trailingNode);
						break;
					case SyntaxKind.XmlTextLiteralNewLineToken:
						token = SyntaxFactory.XmlTextNewLine(leadingNode, info.Text, info.StringValue, trailingNode);
						break;
					case SyntaxKind.XmlTextLiteralToken:
						token = SyntaxFactory.XmlTextLiteral(leadingNode, info.Text, info.StringValue, trailingNode);
						break;
					case SyntaxKind.XmlEntityLiteralToken:
						token = SyntaxFactory.XmlEntity(leadingNode, info.Text, info.StringValue, trailingNode);
						break;
					case SyntaxKind.EndOfDocumentationCommentToken:
					case SyntaxKind.EndOfFileToken:
						token = SyntaxFactory.Token(leadingNode, info.Kind, trailingNode);
						break;
					case SyntaxKind.None:
						token = SyntaxFactory.BadToken(leadingNode, info.Text, trailingNode);
						break;

					default:
						Debug.Assert(SyntaxKindFacts.IsPunctuationOrKeyword(info.Kind));
						token = SyntaxFactory.Token(leadingNode, info.Kind, trailingNode);
						break;
				}
			}

			if (errors != null && (_options.DocumentationMode >= DocumentationMode.Diagnose || !InDocumentationComment))
			{
				token = token.WithDiagnosticsGreen(errors);
			}

			return token;
		}

		private void ScanSyntaxToken(ref TokenInfo info)
		{
			// Initialize for new token scan
			info.Kind = SyntaxKind.None;
			info.ContextualKind = SyntaxKind.None;
			info.Text = null;
			char surrogateCharacter = SlidingTextWindow.InvalidCharacter;
			bool isEscaped = false;

			// Start scanning the token
			char character = TextWindow.PeekChar();
			switch (character)
			{
				case '\"':
				case '\'':
					this.ScanStringLiteral(ref info);
					break;

				case '/':
					TextWindow.AdvanceChar();
					if (TextWindow.PeekChar() == '=')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.SlashEqualsToken;
					}
					else
					{
						info.Kind = SyntaxKind.SlashToken;
					}

					break;

				case '.':
					if (!this.ScanNumericLiteral(ref info))
					{
						if (TextWindow.PeekChar(1) == '.' && TextWindow.PeekChar(2) == '.')
						{
							TextWindow.AdvanceChar();
							TextWindow.AdvanceChar();
							TextWindow.AdvanceChar();
							info.Kind = SyntaxKind.DotDotDotToken;
						}
						else
						{
							TextWindow.AdvanceChar();
							info.Kind = SyntaxKind.DotToken;
						}
					}

					break;

				case ',':
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.CommaToken;
					break;

				case ':':
					TextWindow.AdvanceChar();
					if (TextWindow.PeekChar() == ':')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.ColonColonToken;
					}
					else
					{
						info.Kind = SyntaxKind.ColonToken;
					}

					break;

				case ';':
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.SemicolonToken;
					break;

				case '~':
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.TildeToken;
					break;

				case '!':
					TextWindow.AdvanceChar();
					if (TextWindow.PeekChar() == '=')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.ExclamationEqualsToken;
					}
					else
					{
						info.Kind = SyntaxKind.ExclamationToken;
					}

					break;

				case '=':
					TextWindow.AdvanceChar();
					if ((character = TextWindow.PeekChar()) == '=')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.EqualsEqualsToken;
					}
					//else if (character == '>')
					//{
					//	TextWindow.AdvanceChar();
					//	info.Kind = SyntaxKind.EqualsGreaterThanToken;
					//}
					else
					{
						info.Kind = SyntaxKind.EqualsToken;
					}

					break;

				case '*':
					TextWindow.AdvanceChar();
					if (TextWindow.PeekChar() == '=')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.AsteriskEqualsToken;
					}
					else
					{
						info.Kind = SyntaxKind.AsteriskToken;
					}

					break;

				case '(':
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.OpenParenToken;
					break;

				case ')':
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.CloseParenToken;
					break;

				case '{':
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.OpenBraceToken;
					break;

				case '}':
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.CloseBraceToken;
					break;

				case '[':
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.OpenBracketToken;
					break;

				case ']':
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.CloseBracketToken;
					break;

				case '?':
					TextWindow.AdvanceChar();
					//if (TextWindow.PeekChar() == '?')
					//{
					//	TextWindow.AdvanceChar();
					//	info.Kind = SyntaxKind.QuestionQuestionToken;
					//}
					//else
					{
						info.Kind = SyntaxKind.QuestionToken;
					}

					break;

				case '+':
					TextWindow.AdvanceChar();
					if ((character = TextWindow.PeekChar()) == '=')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.PlusEqualsToken;
					}
					else if (character == '+')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.PlusPlusToken;
					}
					else
					{
						info.Kind = SyntaxKind.PlusToken;
					}

					break;

				case '-':
					TextWindow.AdvanceChar();
					if ((character = TextWindow.PeekChar()) == '=')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.MinusEqualsToken;
					}
					else if (character == '-')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.MinusMinusToken;
					}
					else if (character == '>')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.MinusGreaterThanToken;
					}
					else
					{
						info.Kind = SyntaxKind.MinusToken;
					}

					break;

				case '%':
					TextWindow.AdvanceChar();
					if (TextWindow.PeekChar() == '=')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.PercentEqualsToken;
					}
					else
					{
						info.Kind = SyntaxKind.PercentToken;
					}

					break;

				case '&':
					TextWindow.AdvanceChar();
					if ((character = TextWindow.PeekChar()) == '=')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.AmpersandEqualsToken;
					}
					else if (TextWindow.PeekChar() == '&')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.AmpersandAmpersandToken;
					}
					else
					{
						info.Kind = SyntaxKind.AmpersandToken;
					}

					break;

				case '^':
					TextWindow.AdvanceChar();
					if (TextWindow.PeekChar() == '=')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.CaretEqualsToken;
					}
					else
					{
						info.Kind = SyntaxKind.CaretToken;
					}

					break;

				case '|':
					TextWindow.AdvanceChar();
					if (TextWindow.PeekChar() == '=')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.BarEqualsToken;
					}
					else if (TextWindow.PeekChar() == '|')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.BarBarToken;
					}
					else
					{
						info.Kind = SyntaxKind.BarToken;
					}

					break;

				case '<':
					if (this.ModeIs(LexerMode.DebuggerSyntax) && TextWindow.PeekChar(1) == '>')
					{
						// For "<>f_AnonymousType", which is an identifier in DebuggerSyntax mode
						goto case 'a';
					}

					TextWindow.AdvanceChar();
					if (TextWindow.PeekChar() == '=')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.LessThanEqualsToken;
					}
					else if (TextWindow.PeekChar() == '<')
					{
						TextWindow.AdvanceChar();
						if (TextWindow.PeekChar() == '=')
						{
							TextWindow.AdvanceChar();
							info.Kind = SyntaxKind.LessThanLessThanEqualsToken;
						}
						else
						{
							info.Kind = SyntaxKind.LessThanLessThanToken;
						}
					}
					else
					{
						info.Kind = SyntaxKind.LessThanToken;
					}

					break;

				case '>':
					TextWindow.AdvanceChar();
					if (TextWindow.PeekChar() == '=')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.GreaterThanEqualsToken;
					}
					else
					{
						info.Kind = SyntaxKind.GreaterThanToken;
					}

					break;

				case '@':
					//if (TextWindow.PeekChar(1) == '"')
					//{
					//	this.ScanVerbatimStringLiteral(ref info);
					//}
					//else
					{
						TextWindow.AdvanceChar();
						int start = TextWindow.Position;
						if (this.ScanIdentifierOrKeyword(ref info))
						{
							info = default(TokenInfo);
							TextWindow.Reset(start);
							info.Kind = SyntaxKind.AtToken;
							info.ContextualKind = SyntaxKind.None;
							info.Text = null;
						}
						else
						{
							TextWindow.Reset(start);
							info.Text = TextWindow.GetText(intern: true);
							this.AddError(ErrorCode.ERR_IdentifierExpected);
						}
					}

					break;

				//case '$':
				//	TextWindow.AdvanceChar();
				//	info.Kind = SyntaxKind.DollarToken;
				//	break;

				// All the 'common' identifier characters are represented directly in
				// these switch cases for optimal perf.  Calling IsIdentifierChar() functions is relatively
				// expensive.
				case 'a':
				case 'b':
				case 'c':
				case 'd':
				case 'e':
				case 'f':
				case 'g':
				case 'h':
				case 'i':
				case 'j':
				case 'k':
				case 'l':
				case 'm':
				case 'n':
				case 'o':
				case 'p':
				case 'q':
				case 'r':
				case 's':
				case 't':
				case 'u':
				case 'v':
				case 'w':
				case 'x':
				case 'y':
				case 'z':
				case 'A':
				case 'B':
				case 'C':
				case 'D':
				case 'E':
				case 'F':
				case 'G':
				case 'H':
				case 'I':
				case 'J':
				case 'K':
				case 'L':
				case 'M':
				case 'N':
				case 'O':
				case 'P':
				case 'Q':
				case 'R':
				case 'S':
				case 'T':
				case 'U':
				case 'V':
				case 'W':
				case 'X':
				case 'Y':
				case 'Z':
				case '_':
					this.ScanIdentifierOrKeyword(ref info);
					break;

				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9':
					this.ScanNumericLiteral(ref info);
					break;

				case '\\':
					{
						// Could be unicode escape. Try that.
						character = TextWindow.PeekCharOrUnicodeEscape(out surrogateCharacter);

						isEscaped = true;
						if (SyntaxKindFacts.IsIdentifierStartCharacter(character))
						{
							goto case 'a';
						}

						goto default;
					}

				case SlidingTextWindow.InvalidCharacter:
					if (!TextWindow.IsReallyAtEnd())
					{
						goto default;
					}

					info.Kind = SyntaxKind.EndOfFileToken;
					break;

				default:
					if (SyntaxKindFacts.IsIdentifierStartCharacter(character))
					{
						goto case 'a';
					}

					if (isEscaped)
					{
						SyntaxDiagnosticInfo error;
						TextWindow.NextCharOrUnicodeEscape(out surrogateCharacter, out error);
						AddError(error);
					}
					else
					{
						TextWindow.AdvanceChar();
					}

					info.Text = TextWindow.GetText(intern: true);

					this.AddError(ErrorCode.ERR_UnexpectedCharacter, info.Text);
					break;
			}
		}

		private bool ScanInteger()
		{
			int start = TextWindow.Position;
			char ch;
			while ((ch = TextWindow.PeekChar()) >= '0' && ch <= '9')
			{
				TextWindow.AdvanceChar();
			}

			return start < TextWindow.Position;
		}

		private bool ScanNumericLiteral(ref TokenInfo info)
		{
			int start = TextWindow.Position;
			char ch;
			bool isHex = false;
			bool hasDecimal = false;
			bool hasExponent = false;
			info.Text = null;
			info.ValueKind = SpecialType.None;
			this._builder.Clear();
			bool hasUSuffix = false;
			bool hasLSuffix = false;

			ch = TextWindow.PeekChar();
			if (ch == '0' && ((ch = TextWindow.PeekChar(1)) == 'x' || ch == 'X'))
			{
				TextWindow.AdvanceChar(2);
				isHex = true;
			}

			if (isHex)
			{
				// It's OK if it has no digits after the '0x' -- we'll catch it in ScanNumericLiteral
				// and give a proper error then.
				while (SyntaxKindFacts.IsHexDigit(ch = TextWindow.PeekChar()))
				{
					this._builder.Append(ch);
					TextWindow.AdvanceChar();
				}

				if ((ch = TextWindow.PeekChar()) == 'L' || ch == 'l')
				{
					if (ch == 'l')
					{
						this.AddError(TextWindow.Position, 1, ErrorCode.WRN_LowercaseEllSuffix);
					}

					TextWindow.AdvanceChar();
					hasLSuffix = true;
					if ((ch = TextWindow.PeekChar()) == 'u' || ch == 'U')
					{
						TextWindow.AdvanceChar();
						hasUSuffix = true;
					}
				}
				else if ((ch = TextWindow.PeekChar()) == 'u' || ch == 'U')
				{
					TextWindow.AdvanceChar();
					hasUSuffix = true;
					if ((ch = TextWindow.PeekChar()) == 'L' || ch == 'l')
					{
						TextWindow.AdvanceChar();
						hasLSuffix = true;
					}
				}
			}
			else
			{
				while ((ch = TextWindow.PeekChar()) >= '0' && ch <= '9')
				{
					this._builder.Append(ch);
					TextWindow.AdvanceChar();
				}

				if (this.ModeIs(LexerMode.DebuggerSyntax) && TextWindow.PeekChar() == '#')
				{
					// In DebuggerSyntax mode, "123#" is an identifier.
					TextWindow.AdvanceChar();
					info.StringValue = info.Text = TextWindow.GetText(intern: true);
					info.Kind = SyntaxKind.IdentifierToken;
					return true;
				}

				if ((ch = TextWindow.PeekChar()) == '.')
				{
					var ch2 = TextWindow.PeekChar(1);
					if (ch2 >= '0' && ch2 <= '9')
					{
						hasDecimal = true;
						this._builder.Append(ch);
						TextWindow.AdvanceChar();

						while ((ch = TextWindow.PeekChar()) >= '0' && ch <= '9')
						{
							this._builder.Append(ch);
							TextWindow.AdvanceChar();
						}
					}
					else if (this._builder.Length == 0)
					{
						// we only have the dot so far.. (no preceding number or following number)
						info.Kind = SyntaxKind.DotToken;
						TextWindow.Reset(start);
						return false;
					}
				}

				if ((ch = TextWindow.PeekChar()) == 'E' || ch == 'e')
				{
					this._builder.Append(ch);
					TextWindow.AdvanceChar();
					hasExponent = true;
					if ((ch = TextWindow.PeekChar()) == '-' || ch == '+')
					{
						this._builder.Append(ch);
						TextWindow.AdvanceChar();
					}

					while ((ch = TextWindow.PeekChar()) >= '0' && ch <= '9')
					{
						this._builder.Append(ch);
						TextWindow.AdvanceChar();
					}
				}

				if (hasExponent || hasDecimal)
				{
					if ((ch = TextWindow.PeekChar()) == 'f' || ch == 'F')
					{
						TextWindow.AdvanceChar();
						info.ValueKind = SpecialType.System_Single;
					}
					else if (ch == 'D' || ch == 'd')
					{
						TextWindow.AdvanceChar();
						info.ValueKind = SpecialType.System_Double;
					}
					else if (ch == 'm' || ch == 'M')
					{
						TextWindow.AdvanceChar();
						info.ValueKind = SpecialType.System_Decimal;
					}
					else
					{
						info.ValueKind = SpecialType.System_Double;
					}
				}
				else if ((ch = TextWindow.PeekChar()) == 'f' || ch == 'F')
				{
					TextWindow.AdvanceChar();
					info.ValueKind = SpecialType.System_Single;
				}
				else if (ch == 'D' || ch == 'd')
				{
					TextWindow.AdvanceChar();
					info.ValueKind = SpecialType.System_Double;
				}
				else if (ch == 'm' || ch == 'M')
				{
					TextWindow.AdvanceChar();
					info.ValueKind = SpecialType.System_Decimal;
				}
				else if (ch == 'L' || ch == 'l')
				{
					if (ch == 'l')
					{
						this.AddError(TextWindow.Position, 1, ErrorCode.WRN_LowercaseEllSuffix);
					}

					TextWindow.AdvanceChar();
					hasLSuffix = true;
					if ((ch = TextWindow.PeekChar()) == 'u' || ch == 'U')
					{
						TextWindow.AdvanceChar();
						hasUSuffix = true;
					}
				}
				else if (ch == 'u' || ch == 'U')
				{
					hasUSuffix = true;
					TextWindow.AdvanceChar();
					if ((ch = TextWindow.PeekChar()) == 'L' || ch == 'l')
					{
						TextWindow.AdvanceChar();
						hasLSuffix = true;
					}
				}
			}

			info.Kind = SyntaxKind.NumericLiteralToken;
			info.Text = TextWindow.GetText(true);
			Debug.Assert(info.Text != null);
			var valueText = TextWindow.Intern(this._builder);
			ulong val;
			switch (info.ValueKind)
			{
				case SpecialType.System_Single:
					info.FloatValue = this.GetValueSingle(valueText);
					break;
				case SpecialType.System_Double:
					info.DoubleValue = this.GetValueDouble(valueText);
					break;
				case SpecialType.System_Decimal:
					info.DecimalValue = this.GetValueDecimal(valueText, start, TextWindow.Position);
					break;
				default:
					if (string.IsNullOrEmpty(valueText))
					{
						this.AddError(MakeError(ErrorCode.ERR_InvalidNumber));
						val = 0; //safe default
					}
					else
					{
						val = this.GetValueUInt64(valueText, isHex);
					}

					// 2.4.4.2 Integer literals
					// ...
					// The type of an integer literal is determined as follows:

					// * If the literal has no suffix, it has the first of these types in which its value can be represented: int, uint, long, ulong.
					if (!hasUSuffix && !hasLSuffix)
					{
						if (val <= Int32.MaxValue)
						{
							info.ValueKind = SpecialType.System_Int32;
							info.IntValue = (int)val;
						}
						else if (val <= UInt32.MaxValue)
						{
							info.ValueKind = SpecialType.System_UInt32;
							info.UintValue = (uint)val;

							// TODO: See below, it may be desirable to mark this token
							// as special for folding if its value is 2147483648.
						}
						else if (val <= Int64.MaxValue)
						{
							info.ValueKind = SpecialType.System_Int64;
							info.LongValue = (long)val;
						}
						else
						{
							info.ValueKind = SpecialType.System_UInt64;
							info.UlongValue = val;

							// TODO: See below, it may be desirable to mark this token
							// as special for folding if its value is 9223372036854775808
						}
					}
					else if (hasUSuffix && !hasLSuffix)
					{
						// * If the literal is suffixed by U or u, it has the first of these types in which its value can be represented: uint, ulong.
						if (val <= UInt32.MaxValue)
						{
							info.ValueKind = SpecialType.System_UInt32;
							info.UintValue = (uint)val;
						}
						else
						{
							info.ValueKind = SpecialType.System_UInt64;
							info.UlongValue = val;
						}
					}

					// * If the literal is suffixed by L or l, it has the first of these types in which its value can be represented: long, ulong.
					else if (!hasUSuffix & hasLSuffix)
					{
						if (val <= Int64.MaxValue)
						{
							info.ValueKind = SpecialType.System_Int64;
							info.LongValue = (long)val;
						}
						else
						{
							info.ValueKind = SpecialType.System_UInt64;
							info.UlongValue = val;

							// TODO: See below, it may be desirable to mark this token
							// as special for folding if its value is 9223372036854775808
						}
					}

					// * If the literal is suffixed by UL, Ul, uL, ul, LU, Lu, lU, or lu, it is of type ulong.
					else
					{
						Debug.Assert(hasUSuffix && hasLSuffix);
						info.ValueKind = SpecialType.System_UInt64;
						info.UlongValue = val;
					}

					break;

				// Note, the following portion of the spec is not implemented here. It is implemented
				// in the unary minus analysis.

				// * When a decimal-integer-literal with the value 2147483648 (231) and no integer-type-suffix appears
				//   as the token immediately following a unary minus operator token (§7.7.2), the result is a constant
				//   of type int with the value −2147483648 (−231). In all other situations, such a decimal-integer-
				//   literal is of type uint.
				// * When a decimal-integer-literal with the value 9223372036854775808 (263) and no integer-type-suffix
				//   or the integer-type-suffix L or l appears as the token immediately following a unary minus operator
				//   token (§7.7.2), the result is a constant of type long with the value −9223372036854775808 (−263).
				//   In all other situations, such a decimal-integer-literal is of type ulong.
			}

			return true;
		}

		//used in directives
		private int GetValueInt32(string text, bool isHex)
		{
			int result;
			if (!Int32.TryParse(text, isHex ? NumberStyles.AllowHexSpecifier : NumberStyles.None, CultureInfo.InvariantCulture, out result))
			{
				//we've already lexed the literal, so the error must be from overflow
				this.AddError(MakeError(ErrorCode.ERR_IntOverflow));
			}

			return result;
		}

		//used for all non-directive integer literals (cast to desired type afterward)
		private ulong GetValueUInt64(string text, bool isHex)
		{
			ulong result;
			if (!UInt64.TryParse(text, isHex ? NumberStyles.AllowHexSpecifier : NumberStyles.None, CultureInfo.InvariantCulture, out result))
			{
				//we've already lexed the literal, so the error must be from overflow
				this.AddError(MakeError(ErrorCode.ERR_IntOverflow));
			}

			return result;
		}

		private double GetValueDouble(string text)
		{
			double result;
			if (!Double.TryParse(text, NumberStyles.AllowDecimalPoint | NumberStyles.AllowExponent, CultureInfo.InvariantCulture, out result))
			{
				//we've already lexed the literal, so the error must be from overflow
				this.AddError(MakeError(ErrorCode.ERR_FloatOverflow, "double"));
			}

			return result;
		}

		private float GetValueSingle(string text)
		{
			float result;
			if (!Single.TryParse(text, NumberStyles.AllowDecimalPoint | NumberStyles.AllowExponent, CultureInfo.InvariantCulture, out result))
			{
				//we've already lexed the literal, so the error must be from overflow
				this.AddError(MakeError(ErrorCode.ERR_FloatOverflow, "float"));
			}

			return result;
		}

		private decimal GetValueDecimal(string text, int start, int end)
		{
			// Use decimal.TryParse to parse value. Note: the behavior of
			// decimal.TryParse differs from Dev11 in several cases:
			//
			// 1. [-]0eNm where N > 0
			//     The native compiler ignores sign and scale and treats such cases
			//     as 0e0m. decimal.TryParse fails so these cases are compile errors.
			//     [Bug #568475]
			// 2. 1e-Nm where N >= 1000
			//     The native compiler reports CS0594 "Floating-point constant is
			//     outside the range of type 'decimal'". decimal.TryParse allows
			//     N >> 1000 but treats decimals with very small exponents as 0.
			//     [No bug.]
			// 3. Decimals with significant digits below 1e-49
			//     The native compiler considers digits below 1e-49 when rounding.
			//     decimal.TryParse ignores digits below 1e-49 when rounding. This
			//     last difference is perhaps the most significant since existing code
			//     will continue to compile but constant values may be rounded differently.
			//     (Note that the native compiler does not round in all cases either since
			//     the native compiler chops the string at 50 significant digits. For example
			//     ".100000000000000000000000000050000000000000000000001m" is not
			//     rounded up to 0.1000000000000000000000000001.)
			//     [Bug #568494]

			decimal result;
			if (!decimal.TryParse(text, NumberStyles.AllowDecimalPoint | NumberStyles.AllowExponent, CultureInfo.InvariantCulture, out result))
			{
				//we've already lexed the literal, so the error must be from overflow
				this.AddError(this.MakeError(start, end - start, ErrorCode.ERR_FloatOverflow, "decimal"));
			}

			return result;
		}

		private char ScanEscapeSequence(out char surrogateCharacter)
		{
			var start = TextWindow.Position;
			surrogateCharacter = SlidingTextWindow.InvalidCharacter;
			char ch = TextWindow.NextChar();
			Debug.Assert(ch == '\\');

			ch = TextWindow.NextChar();
			switch (ch)
			{
				case '\'':
				case '\"':
				case '\\':
					break;
				case 'a':
					ch = '\a';
					break;
				case 'b':
					ch = '\b';
					break;
				case 'f':
					ch = '\f';
					break;
				case 'n':
					ch = '\n';
					break;
				case 'r':
					ch = '\r';
					break;
				case 't':
					ch = '\t';
					break;
				case 'v':
					ch = '\v';
					break;
				case 'x':
				case 'u':
				case 'U':
					TextWindow.Reset(start);
					SyntaxDiagnosticInfo error;
					ch = TextWindow.NextUnicodeEscape(surrogateCharacter: out surrogateCharacter, info: out error);
					AddError(error);
					break;
				case '0':
					ch = '\0';
					break;
				default:
					this.AddError(start, TextWindow.Position - start, ErrorCode.ERR_IllegalEscape);
					break;
			}

			return ch;
		}

		private bool ScanStringLiteral(ref TokenInfo info, bool allowEscapes = true)
		{
			var quoteCharacter = TextWindow.PeekChar();
			if (quoteCharacter == '\'' || quoteCharacter == '"')
			{
				TextWindow.AdvanceChar();
				this._builder.Length = 0;
				while (true)
				{
					char ch;
					if ((ch = TextWindow.PeekChar()) == '\\' && allowEscapes)
					{
						// normal string & char constants can have escapes
						char c2;
						ch = this.ScanEscapeSequence(out c2);
						this._builder.Append(ch);
						if (c2 != SlidingTextWindow.InvalidCharacter)
						{
							this._builder.Append(c2);
						}
					}
					else if (ch == quoteCharacter)
					{
						TextWindow.AdvanceChar();
						break;
					}
					else if (SyntaxKindFacts.IsNewLine(ch) ||
							(ch == SlidingTextWindow.InvalidCharacter && TextWindow.IsReallyAtEnd()))
					//String and character literals can contain any Unicode character. They are not limited
					//to valid UTF-16 characters. So if we get the SlidingTextWindow's sentinel value,
					//double check that it was not real user-code contents. This will be rare.
					{
						Debug.Assert(TextWindow.Width > 0);
						this.AddError(ErrorCode.ERR_NewlineInConst);
						break;
					}
					else
					{
						TextWindow.AdvanceChar();
						this._builder.Append(ch);
					}
				}

				// text = textWindow.GetText(false);
				info.Text = TextWindow.GetText(true);
				if (quoteCharacter == '\'')
				{
					info.Kind = SyntaxKind.CharacterLiteralToken;
					if (this._builder.Length != 1)
					{
						this.AddError((this._builder.Length != 0) ? ErrorCode.ERR_TooManyCharsInConst : ErrorCode.ERR_EmptyCharConst);
					}

					if (this._builder.Length > 0)
					{
						info.StringValue = TextWindow.Intern(this._builder);
						info.CharValue = info.StringValue[0];
					}
					else
					{
						info.StringValue = string.Empty;
						info.CharValue = SlidingTextWindow.InvalidCharacter;
					}
				}
				else
				{
					info.Kind = SyntaxKind.StringLiteralToken;
					if (this._builder.Length > 0)
					{
						// unescapedText = this.builder.ToString();
						info.StringValue = TextWindow.Intern(this._builder);
					}
					else
					{
						info.StringValue = string.Empty;
					}
				}

				return true;
			}
			else
			{
				info.Kind = SyntaxKind.None;
				info.Text = null;
				return false;
			}
		}

		private void ResetIdentBuffer()
		{
			this._identLen = 0;
		}

		private void AddIdentChar(char ch)
		{
			if (this._identLen >= this._identBuffer.Length)
			{
				this.GrowIdentBuffer();
			}

			this._identBuffer[this._identLen++] = ch;
		}

		private void GrowIdentBuffer()
		{
			var tmp = new char[this._identBuffer.Length * 2];
			Array.Copy(this._identBuffer, tmp, this._identBuffer.Length);
			this._identBuffer = tmp;
		}


		private void LexSyntaxTrivia(bool afterFirstToken, bool isTrailing, ref SyntaxListBuilder triviaList)
		{
			while (true)
			{
				this.Start();
				char ch = TextWindow.PeekChar();
				if (ch == ' ')
				{
					this.AddTrivia(this.ScanWhitespace(), ref triviaList);
					continue;
				}
				else if (ch > 127)
				{
					if (SyntaxKindFacts.IsWhitespace(ch))
					{
						ch = ' ';
					}
					else if (SyntaxKindFacts.IsNewLine(ch))
					{
						ch = '\n';
					}
				}

				switch (ch)
				{
					case ' ':
					case '\t':       // Horizontal tab
					case '\v':       // Vertical Tab
					case '\f':       // Form-feed
					case '\u001A':
						this.AddTrivia(this.ScanWhitespace(), ref triviaList);
						break;
					case '/':
						if ((ch = TextWindow.PeekChar(1)) == '/')
						{
							//if (!this.SuppressDocumentationCommentParse && TextWindow.PeekChar(2) == '/' && TextWindow.PeekChar(3) != '/')
							//{
							//	// Doc comments should never be in trailing trivia.
							//	// Stop processing so that it will be leading trivia on the next token.
							//	if (isTrailing)
							//	{
							//		return;
							//	}

							//	this.AddTrivia(this.LexXmlDocComment(XmlDocCommentStyle.SingleLine), ref triviaList);
							//	break;
							//}

							// normal single line comment
							this.ScanToEndOfLine();
							var text = TextWindow.GetText(false);
							this.AddTrivia(SyntaxFactory.Comment(text), ref triviaList);
							break;
						}
						else if (ch == '*')
						{
							if (!this.SuppressDocumentationCommentParse 
								&& TextWindow.PeekChar(2) == '*' 
								//&& TextWindow.PeekChar(3) != '*' 
								//&& TextWindow.PeekChar(3) != '/'
								)
							{
								// Doc comments should never be in trailing trivia.
								// Stop processing so that it will be leading trivia on the next token.
								if (isTrailing)
								{
									return;
								}

								this.AddTrivia(this.LexXmlDocComment(XmlDocCommentStyle.Delimited), ref triviaList);
								break;
							}

							bool isTerminated;
							this.ScanMultiLineComment(out isTerminated);
							if (!isTerminated)
							{
								// The comment didn't end.  Report an error at the start point.
								this.AddError(ErrorCode.ERR_OpenEndedComment);
							}

							var text = TextWindow.GetText(false);
							this.AddTrivia(SyntaxFactory.Comment(text), ref triviaList);
							break;
						}

						// not trivia
						return;
					case '\r':
					case '\n':
						this.AddTrivia(this.ScanEndOfLine(), ref triviaList);
						if (isTrailing)
						{
							return;
						}
						break;
					default:
						return;
				}
			}
		}

		private void AddTrivia(CSharpSyntaxNode trivia, ref SyntaxListBuilder list)
		{
			if (this.HasErrors)
			{
				trivia = trivia.WithDiagnosticsGreen(this.Errors);
			}

			if (list == null)
			{
				list = new SyntaxListBuilder(TriviaListInitialCapacity);
			}

			list.Add(trivia);
		}

		private bool ScanMultiLineComment(out bool isTerminated)
		{
			if (TextWindow.PeekChar() == '/' && TextWindow.PeekChar(1) == '*')
			{
				TextWindow.AdvanceChar(2);

				char ch;
				while (true)
				{
					if ((ch = TextWindow.PeekChar()) == SlidingTextWindow.InvalidCharacter && TextWindow.IsReallyAtEnd())
					{
						isTerminated = false;
						break;
					}
					else if (ch == '*' && TextWindow.PeekChar(1) == '/')
					{
						TextWindow.AdvanceChar(2);
						isTerminated = true;
						break;
					}
					else
					{
						TextWindow.AdvanceChar();
					}
				}

				return true;
			}
			else
			{
				isTerminated = false;
				return false;
			}
		}

		private void ScanToEndOfLine()
		{
			char ch;
			while (!SyntaxKindFacts.IsNewLine(ch = TextWindow.PeekChar()) &&
				(ch != SlidingTextWindow.InvalidCharacter || !TextWindow.IsReallyAtEnd()))
			{
				TextWindow.AdvanceChar();
			}
		}

		/// <summary>
		/// Scans a new-line sequence (either a single new-line character or a CR-LF combo).
		/// </summary>
		/// <returns>A trivia node with the new-line text</returns>
		private CSharpSyntaxNode ScanEndOfLine()
		{
			char ch;
			switch (ch = TextWindow.PeekChar())
			{
				case '\r':
					TextWindow.AdvanceChar();
					if (TextWindow.PeekChar() == '\n')
					{
						TextWindow.AdvanceChar();
						return SyntaxFactory.CarriageReturnLineFeed;
					}

					return SyntaxFactory.CarriageReturn;
				case '\n':
					TextWindow.AdvanceChar();
					return SyntaxFactory.LineFeed;
				default:
					if (SyntaxKindFacts.IsNewLine(ch))
					{
						TextWindow.AdvanceChar();
						return SyntaxFactory.EndOfLine(ch.ToString());
					}

					return null;
			}
		}

		/// <summary>
		/// Scans all of the whitespace (not new-lines) into a trivia node until it runs out.
		/// </summary>
		/// <returns>A trivia node with the whitespace text</returns>
		private SyntaxTrivia ScanWhitespace()
		{
			if (this._createWhitespaceTriviaFunction == null)
			{
				this._createWhitespaceTriviaFunction = this.CreateWhitespaceTrivia;
			}

			int hashCode = Hash.FnvOffsetBias;  // FNV base
			bool onlySpaces = true;

		top:
			char ch = TextWindow.PeekChar();

			switch (ch)
			{
				case '\t':       // Horizontal tab
				case '\v':       // Vertical Tab
				case '\f':       // Form-feed
				case '\u001A':
					onlySpaces = false;
					goto case ' ';

				case ' ':
					TextWindow.AdvanceChar();
					hashCode = Hash.CombineFNVHash(hashCode, ch);
					goto top;

				case '\r':      // Carriage Return
				case '\n':      // Line-feed
					break;

				default:
					if (ch > 127 && SyntaxKindFacts.IsWhitespace(ch))
					{
						goto case '\t';
					}

					break;
			}

			if (TextWindow.Width == 1 && onlySpaces)
			{
				return SyntaxFactory.Space;
			}
			else
			{
				var width = TextWindow.Width;

				if (width < MaxCachedTokenSize)
				{
					return this._cache.LookupTrivia(
						TextWindow.CharacterWindow,
						TextWindow.LexemeRelativeStart,
						width,
						hashCode,
						this._createWhitespaceTriviaFunction);
				}
				else
				{
					return this._createWhitespaceTriviaFunction();
				}
			}
		}


		private SyntaxTrivia CreateWhitespaceTrivia()
		{
			return SyntaxFactory.Whitespace(TextWindow.GetText(intern: true));
		}

		private SyntaxToken LexDirectiveToken()
		{
			this.Start();
			TokenInfo info = default(TokenInfo);
			this.ScanDirectiveToken(ref info);
			var errors = this.Errors;
			var trailing = this.LexDirectiveTrailingTrivia(info.Kind == SyntaxKind.EndOfDirectiveToken);
			return Create(ref info, null, trailing, errors);
		}

		private bool ScanDirectiveToken(ref TokenInfo info)
		{
			char character;
			char surrogateCharacter;
			bool isEscaped = false;

			switch (character = TextWindow.PeekChar())
			{
				case SlidingTextWindow.InvalidCharacter:
					if (!TextWindow.IsReallyAtEnd())
					{
						goto default;
					}
					// don't consume end characters here
					info.Kind = SyntaxKind.EndOfDirectiveToken;
					break;

				case '\r':
				case '\n':
					// don't consume end characters here
					info.Kind = SyntaxKind.EndOfDirectiveToken;
					break;

				//case '#':
				//	TextWindow.AdvanceChar();
				//	info.Kind = SyntaxKind.HashToken;
				//	break;

				case '(':
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.OpenParenToken;
					break;

				case ')':
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.CloseParenToken;
					break;

				case ',':
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.CommaToken;
					break;

				case '!':
					TextWindow.AdvanceChar();
					if (TextWindow.PeekChar() == '=')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.ExclamationEqualsToken;
					}
					else
					{
						info.Kind = SyntaxKind.ExclamationToken;
					}

					break;

				case '=':
					TextWindow.AdvanceChar();
					if (TextWindow.PeekChar() == '=')
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.EqualsEqualsToken;
					}
					else
					{
						info.Kind = SyntaxKind.EqualsToken;
					}

					break;

				case '&':
					if (TextWindow.PeekChar(1) == '&')
					{
						TextWindow.AdvanceChar(2);
						info.Kind = SyntaxKind.AmpersandAmpersandToken;
						break;
					}

					goto default;

				case '|':
					if (TextWindow.PeekChar(1) == '|')
					{
						TextWindow.AdvanceChar(2);
						info.Kind = SyntaxKind.BarBarToken;
						break;
					}

					goto default;

				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9':
					this.ScanInteger();
					info.Kind = SyntaxKind.NumericLiteralToken;
					info.Text = TextWindow.GetText(true);
					info.ValueKind = SpecialType.System_Int32;
					info.IntValue = this.GetValueInt32(info.Text, false);
					break;

				case '\"':
					this.ScanStringLiteral(ref info, false);
					break;

				case '\\':
					{
						// Could be unicode escape. Try that.
						character = TextWindow.PeekCharOrUnicodeEscape(out surrogateCharacter);
						isEscaped = true;
						if (SyntaxKindFacts.IsIdentifierStartCharacter(character))
						{
							this.ScanIdentifierOrKeyword(ref info);
							break;
						}

						goto default;
					}

				default:
					if (!isEscaped && SyntaxKindFacts.IsNewLine(character))
					{
						goto case '\n';
					}

					if (SyntaxKindFacts.IsIdentifierStartCharacter(character))
					{
						this.ScanIdentifierOrKeyword(ref info);
					}
					else
					{
						// unknown single character
						if (isEscaped)
						{
							SyntaxDiagnosticInfo error;
							TextWindow.NextCharOrUnicodeEscape(out surrogateCharacter, out error);
							AddError(error);
						}
						else
						{
							TextWindow.AdvanceChar();
						}

						info.Kind = SyntaxKind.None;
						info.Text = TextWindow.GetText(true);
					}

					break;
			}

			Debug.Assert(info.Kind != SyntaxKind.None || info.Text != null);
			return info.Kind != SyntaxKind.None;
		}

		private SyntaxListBuilder LexDirectiveTrailingTrivia(bool includeEndOfLine)
		{
			SyntaxListBuilder trivia = null;

			CSharpSyntaxNode tr;
			while (true)
			{
				var pos = TextWindow.Position;
				tr = this.LexDirectiveTrivia();
				if (tr == null)
				{
					break;
				}
				else if (tr.Kind == SyntaxKind.EndOfLineTrivia)
				{
					if (includeEndOfLine)
					{
						AddTrivia(tr, ref trivia);
					}
					else
					{
						// don't consume end of line...
						TextWindow.Reset(pos);
					}

					break;
				}
				else
				{
					AddTrivia(tr, ref trivia);
				}
			}

			return trivia;
		}

		private CSharpSyntaxNode LexDirectiveTrivia()
		{
			CSharpSyntaxNode trivia = null;

			this.Start();
			char ch = TextWindow.PeekChar();
			switch (ch)
			{
				case '/':
					if (TextWindow.PeekChar(1) == '/')
					{
						// normal single line comment
						this.ScanToEndOfLine();
						var text = TextWindow.GetText(false);
						trivia = SyntaxFactory.Comment(text);
					}

					break;
				case '\r':
				case '\n':
					trivia = this.ScanEndOfLine();
					break;
				case ' ':
				case '\t':       // Horizontal tab
				case '\v':       // Vertical Tab
				case '\f':       // Form-feed
					trivia = this.ScanWhitespace();
					break;

				default:
					if (SyntaxKindFacts.IsWhitespace(ch))
					{
						goto case ' ';
					}

					if (SyntaxKindFacts.IsNewLine(ch))
					{
						goto case '\n';
					}

					break;
			}

			return trivia;
		}

		
	}
}
