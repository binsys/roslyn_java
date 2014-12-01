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
	/// <summary>
	/// Needs to match LexMode.XmlDocCommentLocation*
	/// </summary>
	internal enum XmlDocCommentLocation
	{
		Start = 0,
		Interior = 1,
		Exterior = 2,
		End = 4
	}

	/// <summary>
	/// Needs to match LexMode.XmlDocCommentStyle*
	/// </summary>
	internal enum XmlDocCommentStyle
	{
		SingleLine = 0,
		Delimited = 1
	}

	internal partial class Lexer : LexerBase
	{

		private DocumentationCommentParser _xmlParser;

		public bool SuppressDocumentationCommentParse
		{
			get { return this._options.DocumentationMode < DocumentationMode.Parse; }
		}

		private bool InDocumentationComment
		{
			get
			{
				switch (ModeOf(this._mode))
				{
					case LexerMode.XmlDocComment:
					case LexerMode.XmlElementTag:
					case LexerMode.XmlAttributeTextQuote:
					case LexerMode.XmlAttributeTextDoubleQuote:
					case LexerMode.XmlCrefQuote:
					case LexerMode.XmlCrefDoubleQuote:
					case LexerMode.XmlNameQuote:
					case LexerMode.XmlNameDoubleQuote:
					case LexerMode.XmlCDataSectionText:
					case LexerMode.XmlCommentText:
					case LexerMode.XmlProcessingInstructionText:
					case LexerMode.XmlCharacter:
						return true;
					default:
						return false;
				}
			}
		}



		private bool LocationIs(XmlDocCommentLocation location)
		{
			return LocationOf(this._mode) == location;
		}

		private void MutateLocation(XmlDocCommentLocation location)
		{
			this._mode &= ~LexerMode.MaskXmlDocCommentLocation;
			this._mode |= (LexerMode)((int)location << 16);
		}

		private static XmlDocCommentStyle StyleOf(LexerMode mode)
		{
			return (XmlDocCommentStyle)((int)(mode & LexerMode.MaskXmlDocCommentStyle) >> 20);
		}

		private static XmlDocCommentLocation LocationOf(LexerMode mode)
		{
			return (XmlDocCommentLocation)((int)(mode & LexerMode.MaskXmlDocCommentLocation) >> 16);
		}

		private bool StyleIs(XmlDocCommentStyle style)
		{
			return StyleOf(this._mode) == style;
		}



		private CSharpSyntaxNode LexXmlDocComment(XmlDocCommentStyle style)
		{
			var saveMode = this._mode;
			bool isTerminated;

			var mode = style == XmlDocCommentStyle.SingleLine
					? LexerMode.XmlDocCommentStyleSingleLine
					: LexerMode.XmlDocCommentStyleDelimited;
			if (_xmlParser == null)
			{
				_xmlParser = new DocumentationCommentParser(this, mode);
			}
			else
			{
				_xmlParser.ReInitialize(mode);
			}

			var docComment = _xmlParser.ParseDocumentationComment(out isTerminated);

			// We better have finished with the whole comment. There should be error
			// code in the implementation of ParseXmlDocComment that ensures this.
			Debug.Assert(this.LocationIs(XmlDocCommentLocation.End) || TextWindow.PeekChar() == SlidingTextWindow.InvalidCharacter);

			this._mode = saveMode;

			if (!isTerminated)
			{
				// The comment didn't end.  Report an error at the start point.
				// NOTE: report this error even if the DocumentationMode is less than diagnose - the comment
				// would be malformed as a non-doc comment as well.
				this.AddError(TextWindow.LexemeStartPosition, TextWindow.Width, ErrorCode.ERR_OpenEndedComment);
			}

			return docComment;
		}

		/// <summary>
		/// Lexer entry point for LexMode.XmlDocComment
		/// </summary>
		private SyntaxToken LexXmlToken()
		{
			TokenInfo xmlTokenInfo = default(TokenInfo);

			SyntaxListBuilder leading = null;
			this.LexXmlDocCommentLeadingTrivia(ref leading);

			this.Start();
			this.ScanXmlToken(ref xmlTokenInfo);
			var errors = this.Errors;

			return Create(ref xmlTokenInfo, leading, null, errors);
		}

		private bool ScanXmlToken(ref TokenInfo info)
		{
			char ch;

			Debug.Assert(!this.LocationIs(XmlDocCommentLocation.Start));
			Debug.Assert(!this.LocationIs(XmlDocCommentLocation.Exterior));

			if (this.LocationIs(XmlDocCommentLocation.End))
			{
				info.Kind = SyntaxKind.EndOfDocumentationCommentToken;
				return true;
			}

			switch (ch = TextWindow.PeekChar())
			{
				case '&':
					this.ScanXmlEntity(ref info);
					info.Kind = SyntaxKind.XmlEntityLiteralToken;
					break;

				case '<':
					this.ScanXmlTagStart(ref info);
					break;

				case '\r':
				case '\n':
					this.ScanEndOfLine();
					info.StringValue = info.Text = TextWindow.GetText(false);
					info.Kind = SyntaxKind.XmlTextLiteralNewLineToken;
					this.MutateLocation(XmlDocCommentLocation.Exterior);
					break;

				case SlidingTextWindow.InvalidCharacter:
					if (!TextWindow.IsReallyAtEnd())
					{
						goto default;
					}

					info.Kind = SyntaxKind.EndOfDocumentationCommentToken;
					break;

				default:
					if (SyntaxKindFacts.IsNewLine(ch))
					{
						goto case '\n';
					}

					this.ScanXmlText(ref info);
					info.Kind = SyntaxKind.XmlTextLiteralToken;
					break;
			}

			Debug.Assert(info.Kind != SyntaxKind.None || info.Text != null);
			return info.Kind != SyntaxKind.None;
		}

		private void ScanXmlTagStart(ref TokenInfo info)
		{
			Debug.Assert(TextWindow.PeekChar() == '<');

			if (TextWindow.PeekChar(1) == '!')
			{
				if (TextWindow.PeekChar(2) == '-'
					&& TextWindow.PeekChar(3) == '-')
				{
					TextWindow.AdvanceChar(4);
					info.Kind = SyntaxKind.XmlCommentStartToken;
				}
				else if (TextWindow.PeekChar(2) == '['
					&& TextWindow.PeekChar(3) == 'C'
					&& TextWindow.PeekChar(4) == 'D'
					&& TextWindow.PeekChar(5) == 'A'
					&& TextWindow.PeekChar(6) == 'T'
					&& TextWindow.PeekChar(7) == 'A'
					&& TextWindow.PeekChar(8) == '[')
				{
					TextWindow.AdvanceChar(9);
					info.Kind = SyntaxKind.XmlCDataStartToken;
				}
				else
				{
					// TODO: Take the < by itself, I guess?
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.LessThanToken;
				}
			}
			else if (TextWindow.PeekChar(1) == '/')
			{
				TextWindow.AdvanceChar(2);
				info.Kind = SyntaxKind.LessThanSlashToken;
			}
			else if (TextWindow.PeekChar(1) == '?')
			{
				TextWindow.AdvanceChar(2);
				info.Kind = SyntaxKind.XmlProcessingInstructionStartToken;
			}
			else
			{
				TextWindow.AdvanceChar();
				info.Kind = SyntaxKind.LessThanToken;
			}
		}

		private void ScanXmlEntity(ref TokenInfo info)
		{
			info.StringValue = null;

			Debug.Assert(TextWindow.PeekChar() == '&');
			TextWindow.AdvanceChar();
			this._builder.Clear();
			XmlParseErrorCode? error = null;
			object[] errorArgs = null;

			char ch;
			if (IsXmlNameStartChar(ch = TextWindow.PeekChar()))
			{
				while (IsXmlNameChar(ch = TextWindow.PeekChar()))
				{
					// Important bit of information here: none of \0, \r, \n, and crucially for
					// delimited comments, * are considered Xml name characters. Also, since
					// entities appear in xml text and _annotation text, it's relevant here that
					// none of <, /, >, ', ", =, are Xml name characters. Note that - and ] are
					// irrelevant--entities do not appear in comments or cdata.

					TextWindow.AdvanceChar();
					this._builder.Append(ch);
				}

				switch (this._builder.ToString())
				{
					case "lt":
						info.StringValue = "<";
						break;
					case "gt":
						info.StringValue = ">";
						break;
					case "amp":
						info.StringValue = "&";
						break;
					case "apos":
						info.StringValue = "'";
						break;
					case "quot":
						info.StringValue = "\"";
						break;
					default:
						error = XmlParseErrorCode.XML_RefUndefinedEntity_1;
						errorArgs = new[] { this._builder.ToString() };
						break;
				}
			}
			else if (ch == '#')
			{
				TextWindow.AdvanceChar();
				bool isHex = TextWindow.PeekChar() == 'x';
				uint charValue = 0;

				if (isHex)
				{
					TextWindow.AdvanceChar(); // x
					while (SyntaxKindFacts.IsHexDigit(ch = TextWindow.PeekChar()))
					{
						TextWindow.AdvanceChar();

						// disallow overflow
						if (charValue <= 0x7FFFFFF)
						{
							charValue = (charValue << 4) + (uint)SyntaxKindFacts.HexValue(ch);
						}
					}
				}
				else
				{
					while (SyntaxKindFacts.IsDecDigit(ch = TextWindow.PeekChar()))
					{
						TextWindow.AdvanceChar();

						// disallow overflow
						if (charValue <= 0x7FFFFFF)
						{
							charValue = (charValue << 3) + (charValue << 1) + (uint)SyntaxKindFacts.DecValue(ch);
						}
					}
				}

				if (TextWindow.PeekChar() != ';')
				{
					error = XmlParseErrorCode.XML_InvalidCharEntity;
				}

				if (MatchesProductionForXmlChar(charValue))
				{
					char lowSurrogate;
					char highSurrogate = SlidingTextWindow.GetCharsFromUtf32(charValue, out lowSurrogate);

					this._builder.Append(highSurrogate);
					if (lowSurrogate != SlidingTextWindow.InvalidCharacter)
					{
						this._builder.Append(lowSurrogate);
					}

					info.StringValue = this._builder.ToString();
				}
				else
				{
					if (error == null)
					{
						error = XmlParseErrorCode.XML_InvalidUnicodeChar;
					}
				}
			}
			else
			{
				if (SyntaxKindFacts.IsWhitespace(ch) || SyntaxKindFacts.IsNewLine(ch))
				{
					if (error == null)
					{
						error = XmlParseErrorCode.XML_InvalidWhitespace;
					}
				}
				else
				{
					if (error == null)
					{
						error = XmlParseErrorCode.XML_InvalidToken;
						errorArgs = new[] { ch.ToString() };
					}
				}
			}

			ch = TextWindow.PeekChar();
			if (ch == ';')
			{
				TextWindow.AdvanceChar();
			}
			else
			{
				if (error == null)
				{
					error = XmlParseErrorCode.XML_InvalidToken;
					errorArgs = new[] { ch.ToString() };
				}
			}

			// If we don't have a value computed from above, then we don't recognize the entity, in which
			// case we will simply use the text.

			info.Text = TextWindow.GetText(true);
			if (info.StringValue == null)
			{
				info.StringValue = info.Text;
			}

			if (error != null)
			{
				this.AddError(error.Value, errorArgs ?? SpecializedCollections.EmptyArray<object>());
			}
		}

		private static bool MatchesProductionForXmlChar(uint charValue)
		{
			// Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF] /* any Unicode character, excluding the surrogate blocks, FFFE, and FFFF. */

			return
				charValue == 0x9 ||
				charValue == 0xA ||
				charValue == 0xD ||
				(charValue >= 0x20 && charValue <= 0xD7FF) ||
				(charValue >= 0xE000 && charValue <= 0xFFFD) ||
				(charValue >= 0x10000 && charValue <= 0x10FFFF);
		}

		private void ScanXmlText(ref TokenInfo info)
		{
			// Collect "]]>" strings into their own XmlText.
			if (TextWindow.PeekChar() == ']' && TextWindow.PeekChar(1) == ']' && TextWindow.PeekChar(2) == '>')
			{
				TextWindow.AdvanceChar(3);
				info.StringValue = info.Text = TextWindow.GetText(false);
				this.AddError(XmlParseErrorCode.XML_CDataEndTagNotAllowed);
				return;
			}

			while (true)
			{
				var ch = TextWindow.PeekChar();
				switch (ch)
				{
					case SlidingTextWindow.InvalidCharacter:
						if (!TextWindow.IsReallyAtEnd())
						{
							goto default;
						}

						info.StringValue = info.Text = TextWindow.GetText(false);
						return;
					case '&':
					case '<':
					case '\r':
					case '\n':
						info.StringValue = info.Text = TextWindow.GetText(false);
						return;

					case '*':
						if (this.StyleIs(XmlDocCommentStyle.Delimited) && TextWindow.PeekChar(1) == '/')
						{
							// we're at the end of the comment, but don't lex it yet.
							info.StringValue = info.Text = TextWindow.GetText(false);
							return;
						}

						goto default;

					case ']':
						if (TextWindow.PeekChar(1) == ']' && TextWindow.PeekChar(2) == '>')
						{
							info.StringValue = info.Text = TextWindow.GetText(false);
							return;
						}

						goto default;

					default:
						if (SyntaxKindFacts.IsNewLine(ch))
						{
							goto case '\n';
						}

						TextWindow.AdvanceChar();
						break;
				}
			}
		}

		/// <summary>
		/// Lexer entry point for LexMode.XmlElementTag
		/// </summary>
		private SyntaxToken LexXmlElementTagToken()
		{
			TokenInfo tagInfo = default(TokenInfo);

			SyntaxListBuilder leading = null;
			this.LexXmlDocCommentLeadingTriviaWithWhitespace(ref leading);

			this.Start();
			this.ScanXmlElementTagToken(ref tagInfo);
			var errors = this.Errors;

			// PERF: De-dupe common XML element tags
			if (errors == null && tagInfo.ContextualKind == SyntaxKind.None && tagInfo.Kind == SyntaxKind.IdentifierToken)
			{
				SyntaxToken token = DocumentationCommentXmlTokens.LookupToken(tagInfo.Text, leading);
				if (token != null)
				{
					return token;
				}
			}

			return Create(ref tagInfo, leading, null, errors);
		}

		private bool ScanXmlElementTagToken(ref TokenInfo info)
		{
			char ch;

			Debug.Assert(!this.LocationIs(XmlDocCommentLocation.Start));
			Debug.Assert(!this.LocationIs(XmlDocCommentLocation.Exterior));

			if (this.LocationIs(XmlDocCommentLocation.End))
			{
				info.Kind = SyntaxKind.EndOfDocumentationCommentToken;
				return true;
			}

			switch (ch = TextWindow.PeekChar())
			{
				case '<':
					this.ScanXmlTagStart(ref info);
					break;

				case '>':
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.GreaterThanToken;
					break;

				case '/':
					if (TextWindow.PeekChar(1) == '>')
					{
						TextWindow.AdvanceChar(2);
						info.Kind = SyntaxKind.SlashGreaterThanToken;
						break;
					}

					goto default;

				case '"':
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.DoubleQuoteToken;
					break;

				case '\'':
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.SingleQuoteToken;
					break;

				case '=':
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.EqualsToken;
					break;

				case ':':
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.ColonToken;
					break;

				case '\r':
				case '\n':
					// Assert?
					break;

				case SlidingTextWindow.InvalidCharacter:
					if (!TextWindow.IsReallyAtEnd())
					{
						goto default;
					}

					info.Kind = SyntaxKind.EndOfDocumentationCommentToken;
					break;

				case '*':
					if (this.StyleIs(XmlDocCommentStyle.Delimited) && TextWindow.PeekChar(1) == '/')
					{
						// Assert? We should have gotten this in the leading trivia.
						Debug.Assert(false, "Should have picked up leading indentationTrivia, but didn't.");
						break;
					}

					goto default;

				default:
					if (IsXmlNameStartChar(ch))
					{
						this.ScanXmlName(ref info);
						info.StringValue = info.Text;
						info.Kind = SyntaxKind.IdentifierToken;
					}
					else if (SyntaxKindFacts.IsWhitespace(ch) || SyntaxKindFacts.IsNewLine(ch))
					{
						// whitespace! needed to do a better job with trivia
						Debug.Assert(false, "Should have picked up leading indentationTrivia, but didn't.");
					}
					else
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.None;
						info.StringValue = info.Text = TextWindow.GetText(false);
					}

					break;
			}

			Debug.Assert(info.Kind != SyntaxKind.None || info.Text != null);
			return info.Kind != SyntaxKind.None;
		}

		private void ScanXmlName(ref TokenInfo info)
		{
			int start = TextWindow.Position;

			while (true)
			{
				char ch = TextWindow.PeekChar();

				// Important bit of information here: none of \0, \r, \n, and crucially for
				// delimited comments, * are considered Xml name characters.
				if (ch != ':' && IsXmlNameChar(ch))
				{
					// Although ':' is a name char, we don't include it in ScanXmlName
					// since it is its own token. This enables the parser to add structure
					// to colon-separated names.

					// TODO: Could put a big switch here for common cases
					// if this is a perf bottleneck.
					TextWindow.AdvanceChar();
				}
				else
				{
					break;
				}
			}

			info.Text = TextWindow.GetText(start, TextWindow.Position - start, intern: true);
		}

		/// <summary>
		/// Determines whether this Unicode character can start a XMLName.
		/// </summary>
		/// <param name="ch">The Unicode character.</param>
		private static bool IsXmlNameStartChar(char ch)
		{
			// TODO: which is the right one?
			return XmlCharType.IsStartNCNameCharXml4e(ch);
			// return XmlCharType.IsStartNameSingleChar(ch);
		}

		/// <summary>
		/// Determines if this Unicode character can be part of an XML Name.
		/// </summary>
		/// <param name="ch">The Unicode character.</param>
		private static bool IsXmlNameChar(char ch)
		{
			// TODO: which is the right one?
			return XmlCharType.IsNCNameCharXml4e(ch);
			//return XmlCharType.IsNameSingleChar(ch);
		}

		// TODO: There is a lot of duplication between _annotation text, CDATA text, and comment text.
		// It would be nice to factor them together.

		/// <summary>
		/// Lexer entry point for LexMode.XmlAttributeText
		/// </summary>
		private SyntaxToken LexXmlAttributeTextToken()
		{
			TokenInfo info = default(TokenInfo);

			SyntaxListBuilder leading = null;
			this.LexXmlDocCommentLeadingTrivia(ref leading);

			this.Start();
			this.ScanXmlAttributeTextToken(ref info);
			var errors = this.Errors;

			return Create(ref info, leading, null, errors);
		}

		private bool ScanXmlAttributeTextToken(ref TokenInfo info)
		{
			Debug.Assert(!this.LocationIs(XmlDocCommentLocation.Start));
			Debug.Assert(!this.LocationIs(XmlDocCommentLocation.Exterior));

			if (this.LocationIs(XmlDocCommentLocation.End))
			{
				info.Kind = SyntaxKind.EndOfDocumentationCommentToken;
				return true;
			}

			char ch;
			switch (ch = TextWindow.PeekChar())
			{
				case '"':
					if (this.ModeIs(LexerMode.XmlAttributeTextDoubleQuote))
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.DoubleQuoteToken;
						break;
					}

					goto default;

				case '\'':
					if (this.ModeIs(LexerMode.XmlAttributeTextQuote))
					{
						TextWindow.AdvanceChar();
						info.Kind = SyntaxKind.SingleQuoteToken;
						break;
					}

					goto default;

				case '&':
					this.ScanXmlEntity(ref info);
					info.Kind = SyntaxKind.XmlEntityLiteralToken;
					break;

				case '<':
					TextWindow.AdvanceChar();
					info.Kind = SyntaxKind.LessThanToken;
					break;

				case '\r':
				case '\n':
					this.ScanEndOfLine();
					info.StringValue = info.Text = TextWindow.GetText(false);
					info.Kind = SyntaxKind.XmlTextLiteralNewLineToken;
					this.MutateLocation(XmlDocCommentLocation.Exterior);
					break;

				case SlidingTextWindow.InvalidCharacter:
					if (!TextWindow.IsReallyAtEnd())
					{
						goto default;
					}

					info.Kind = SyntaxKind.EndOfDocumentationCommentToken;
					break;

				default:
					if (SyntaxKindFacts.IsNewLine(ch))
					{
						goto case '\n';
					}

					this.ScanXmlAttributeText(ref info);
					info.Kind = SyntaxKind.XmlTextLiteralToken;
					break;
			}

			Debug.Assert(info.Kind != SyntaxKind.None || info.Text != null);
			return info.Kind != SyntaxKind.None;
		}

		private void ScanXmlAttributeText(ref TokenInfo info)
		{
			while (true)
			{
				var ch = TextWindow.PeekChar();
				switch (ch)
				{
					case '"':
						if (this.ModeIs(LexerMode.XmlAttributeTextDoubleQuote))
						{
							info.StringValue = info.Text = TextWindow.GetText(false);
							return;
						}

						goto default;

					case '\'':
						if (this.ModeIs(LexerMode.XmlAttributeTextQuote))
						{
							info.StringValue = info.Text = TextWindow.GetText(false);
							return;
						}

						goto default;

					case '&':
					case '<':
					case '\r':
					case '\n':
						info.StringValue = info.Text = TextWindow.GetText(false);
						return;

					case SlidingTextWindow.InvalidCharacter:
						if (!TextWindow.IsReallyAtEnd())
						{
							goto default;
						}

						info.StringValue = info.Text = TextWindow.GetText(false);
						return;

					case '*':
						if (this.StyleIs(XmlDocCommentStyle.Delimited) && TextWindow.PeekChar(1) == '/')
						{
							// we're at the end of the comment, but don't lex it yet.
							info.StringValue = info.Text = TextWindow.GetText(false);
							return;
						}

						goto default;

					default:
						if (SyntaxKindFacts.IsNewLine(ch))
						{
							goto case '\n';
						}

						TextWindow.AdvanceChar();
						break;
				}
			}
		}

		/// <summary>
		/// Lexer entry point for LexerMode.XmlCharacter.
		/// </summary>
		private SyntaxToken LexXmlCharacter()
		{
			TokenInfo info = default(TokenInfo);

			//TODO: Dev11 allows C# comments and newlines in cref trivia (DevDiv #530523).
			SyntaxListBuilder leading = null;
			this.LexXmlDocCommentLeadingTriviaWithWhitespace(ref leading);

			this.Start();
			this.ScanXmlCharacter(ref info);
			var errors = this.Errors;

			return Create(ref info, leading, null, errors);
		}

		/// <summary>
		/// Scan a single XML character (or entity).  Assumes that leading trivia has already
		/// been consumed.
		/// </summary>
		private bool ScanXmlCharacter(ref TokenInfo info)
		{
			Debug.Assert(!this.LocationIs(XmlDocCommentLocation.Start));
			Debug.Assert(!this.LocationIs(XmlDocCommentLocation.Exterior));

			if (this.LocationIs(XmlDocCommentLocation.End))
			{
				info.Kind = SyntaxKind.EndOfDocumentationCommentToken;
				return true;
			}

			switch (TextWindow.PeekChar())
			{
				case '&':
					this.ScanXmlEntity(ref info);
					info.Kind = SyntaxKind.XmlEntityLiteralToken;
					break;
				case SlidingTextWindow.InvalidCharacter:
					if (!TextWindow.IsReallyAtEnd())
					{
						goto default;
					}
					info.Kind = SyntaxKind.EndOfFileToken;
					break;
				default:
					info.Kind = SyntaxKind.XmlTextLiteralToken;
					info.Text = info.StringValue = TextWindow.NextChar().ToString();
					break;
			}

			return true;
		}

		/// <summary>
		/// Lexer entry point for LexerMode.XmlCrefQuote, LexerMode.XmlCrefDoubleQuote, 
		/// LexerMode.XmlNameQuote, and LexerMode.XmlNameDoubleQuote.
		/// </summary>
		private SyntaxToken LexXmlCrefOrNameToken()
		{
			TokenInfo info = default(TokenInfo);

			//TODO: Dev11 allows C# comments and newlines in cref trivia (DevDiv #530523).
			SyntaxListBuilder leading = null;
			this.LexXmlDocCommentLeadingTriviaWithWhitespace(ref leading);

			this.Start();
			this.ScanXmlCrefToken(ref info);
			var errors = this.Errors;

			return Create(ref info, leading, null, errors);
		}

		/// <summary>
		/// Scan a single cref _annotation token.  Assumes that leading trivia has already
		/// been consumed.
		/// </summary>
		/// <remarks>
		/// Within this method, characters that are not XML meta-characters can be seamlessly
		/// replaced with the corresponding XML entities.
		/// </remarks>
		private bool ScanXmlCrefToken(ref TokenInfo info)
		{
			Debug.Assert(!this.LocationIs(XmlDocCommentLocation.Start));
			Debug.Assert(!this.LocationIs(XmlDocCommentLocation.Exterior));

			if (this.LocationIs(XmlDocCommentLocation.End))
			{
				info.Kind = SyntaxKind.EndOfDocumentationCommentToken;
				return true;
			}

			int beforeConsumed = TextWindow.Position;
			char consumedChar = TextWindow.NextChar();
			char consumedSurrogate = SlidingTextWindow.InvalidCharacter;

			// This first switch is for special characters.  If we see the corresponding
			// XML entities, we DO NOT want to take these actions.
			switch (consumedChar)
			{
				case '"':
					if (this.ModeIs(LexerMode.XmlCrefDoubleQuote) || this.ModeIs(LexerMode.XmlNameDoubleQuote))
					{
						info.Kind = SyntaxKind.DoubleQuoteToken;
						return true;
					}

					break;

				case '\'':
					if (this.ModeIs(LexerMode.XmlCrefQuote) || this.ModeIs(LexerMode.XmlNameQuote))
					{
						info.Kind = SyntaxKind.SingleQuoteToken;
						return true;
					}

					break;

				case '<':
					info.Text = TextWindow.GetText(intern: false);
					this.AddError(XmlParseErrorCode.XML_LessThanInAttributeValue, info.Text); //ErrorCode.WRN_XMLParseError
					return true;

				case SlidingTextWindow.InvalidCharacter:
					if (!TextWindow.IsReallyAtEnd())
					{
						goto default;
					}

					info.Kind = SyntaxKind.EndOfDocumentationCommentToken;
					return true;

				case '\r':
				case '\n':
					TextWindow.Reset(beforeConsumed);
					this.ScanEndOfLine();
					info.StringValue = info.Text = TextWindow.GetText(intern: false);
					info.Kind = SyntaxKind.XmlTextLiteralNewLineToken;
					this.MutateLocation(XmlDocCommentLocation.Exterior);
					break;

				case '&':
					TextWindow.Reset(beforeConsumed);
					if (!TextWindow.TryScanXmlEntity(out consumedChar, out consumedSurrogate))
					{
						TextWindow.Reset(beforeConsumed);
						this.ScanXmlEntity(ref info);
						info.Kind = SyntaxKind.XmlEntityLiteralToken;
						return true;
					}

					// TryScanXmlEntity advances even when it returns false.
					break;

				case '{':
					consumedChar = '<';
					break;

				case '}':
					consumedChar = '>';
					break;

				default:
					if (SyntaxKindFacts.IsNewLine(consumedChar))
					{
						goto case '\n';
					}

					break;
			}

			Debug.Assert(TextWindow.Position > beforeConsumed, "First character or entity has been consumed.");

			// NOTE: None of these cases will be matched if the surrogate is non-zero (UTF-16 rules)
			// so we don't need to check for that explicitly.

			// NOTE: there's a lot of overlap between this switch and the one in
			// ScanSyntaxToken, but we probably don't want to share code because
			// ScanSyntaxToken is really hot code and this switch does some extra
			// work.
			switch (consumedChar)
			{
				//// Single-Character Punctuation/Operators ////
				case '(':
					info.Kind = SyntaxKind.OpenParenToken;
					break;
				case ')':
					info.Kind = SyntaxKind.CloseParenToken;
					break;
				case '[':
					info.Kind = SyntaxKind.OpenBracketToken;
					break;
				case ']':
					info.Kind = SyntaxKind.CloseBracketToken;
					break;
				case ',':
					info.Kind = SyntaxKind.CommaToken;
					break;
				case '.':
					info.Kind = SyntaxKind.DotToken;
					break;
				case '?':
					info.Kind = SyntaxKind.QuestionToken;
					break;
				case '&':
					info.Kind = SyntaxKind.AmpersandToken;
					break;
				case '*':
					info.Kind = SyntaxKind.AsteriskToken;
					break;
				case '|':
					info.Kind = SyntaxKind.BarToken;
					break;
				case '^':
					info.Kind = SyntaxKind.CaretToken;
					break;
				case '%':
					info.Kind = SyntaxKind.PercentToken;
					break;
				case '/':
					info.Kind = SyntaxKind.SlashToken;
					break;
				case '~':
					info.Kind = SyntaxKind.TildeToken;
					break;

				// NOTE: Special case - convert curly brackets into angle brackets.
				case '{':
					info.Kind = SyntaxKind.LessThanToken;
					break;
				case '}':
					info.Kind = SyntaxKind.GreaterThanToken;
					break;

				//// Multi-Character Punctuation/Operators ////
				case ':':
					if (AdvanceIfMatches(':')) info.Kind = SyntaxKind.ColonColonToken;
					else info.Kind = SyntaxKind.ColonToken;
					break;
				case '=':
					if (AdvanceIfMatches('=')) info.Kind = SyntaxKind.EqualsEqualsToken;
					else info.Kind = SyntaxKind.EqualsToken;
					break;
				case '!':
					if (AdvanceIfMatches('=')) info.Kind = SyntaxKind.ExclamationEqualsToken;
					else info.Kind = SyntaxKind.ExclamationToken;
					break;
				case '>':
					if (AdvanceIfMatches('=')) info.Kind = SyntaxKind.GreaterThanEqualsToken;
					// GreaterThanGreaterThanToken is synthesized in the parser since it is ambiguous (with closing nested type parameter lists)
					// else if (AdvanceIfMatches('>')) info.Kind = SyntaxKind.GreaterThanGreaterThanToken;
					else info.Kind = SyntaxKind.GreaterThanToken;
					break;
				case '<':
					if (AdvanceIfMatches('=')) info.Kind = SyntaxKind.LessThanEqualsToken;
					else if (AdvanceIfMatches('<')) info.Kind = SyntaxKind.LessThanLessThanToken;
					else info.Kind = SyntaxKind.LessThanToken;
					break;
				case '+':
					if (AdvanceIfMatches('+')) info.Kind = SyntaxKind.PlusPlusToken;
					else info.Kind = SyntaxKind.PlusToken;
					break;
				case '-':
					if (AdvanceIfMatches('-')) info.Kind = SyntaxKind.MinusMinusToken;
					else info.Kind = SyntaxKind.MinusToken;
					break;
			}

			if (info.Kind != SyntaxKind.None)
			{
				Debug.Assert(info.Text == null, "Haven't tried to set it yet.");
				Debug.Assert(info.StringValue == null, "Haven't tried to set it yet.");

				string valueText = SyntaxKindFacts.GetText(info.Kind);
				string actualText = TextWindow.GetText(intern: false);
				if (!string.IsNullOrEmpty(valueText) && actualText != valueText)
				{
					info.RequiresTextForXmlEntity = true;
					info.Text = actualText;
					info.StringValue = valueText;
				}
			}
			else
			{
				// If we didn't match any of the above cases, then we either have an
				// identifier or an unexpected character.

				TextWindow.Reset(beforeConsumed);

				if (this.ScanIdentifier(ref info) && info.Text.Length > 0)
				{
					// ACASEY:  All valid identifier characters should be valid in XML _annotation values,
					// but I don't want to add an assert because XML character classification is expensive.
					// check to see if it is an actual keyword
					// NOTE: name _annotation values don't respect keywords - everything is an identifier.
					SyntaxKind keywordKind;
					if (!InXmlNameAttributeValue && !info.IsVerbatim && !info.HasIdentifierEscapeSequence && this._cache.TryGetKeywordKind(info.StringValue, out keywordKind))
					{
						if (SyntaxKindFacts.IsContextualKeyword(keywordKind))
						{
							info.Kind = SyntaxKind.IdentifierToken;
							info.ContextualKind = keywordKind;
							// Don't need to set any special flags to store the original text of an identifier.
						}
						else
						{
							info.Kind = keywordKind;
							info.RequiresTextForXmlEntity = info.Text != info.StringValue;
						}
					}
					else
					{
						info.ContextualKind = info.Kind = SyntaxKind.IdentifierToken;
					}
				}
				else
				{
					if (consumedChar == '@')
					{
						// Saw '@', but it wasn't followed by an identifier (otherwise ScanIdentifier would have succeeded).
						if (TextWindow.PeekChar() == '@')
						{
							TextWindow.NextChar();
							info.Text = TextWindow.GetText(intern: true);
							info.StringValue = ""; // Can't be null for an identifier.
						}
						else
						{
							this.ScanXmlEntity(ref info);
						}
						info.Kind = SyntaxKind.IdentifierToken;
						this.AddError(ErrorCode.ERR_ExpectedVerbatimLiteral);
					}
					else if (TextWindow.PeekChar() == '&')
					{
						this.ScanXmlEntity(ref info);
						info.Kind = SyntaxKind.XmlEntityLiteralToken;
						this.AddCrefError(ErrorCode.ERR_UnexpectedCharacter, info.Text);
					}
					else
					{
						char bad = TextWindow.NextChar();
						info.Text = TextWindow.GetText(intern: false);

						// If it's valid in XML, then it was unexpected in cref mode.
						// Otherwise, it's just bad XML.
						if (MatchesProductionForXmlChar((uint)bad))
						{
							this.AddCrefError(ErrorCode.ERR_UnexpectedCharacter, info.Text);
						}
						else
						{
							this.AddError(XmlParseErrorCode.XML_InvalidUnicodeChar);
						}
					}
				}
			}

			Debug.Assert(info.Kind != SyntaxKind.None || info.Text != null);
			return info.Kind != SyntaxKind.None;
		}

		/// <summary>
		/// Given a character, advance the input if either the character or the
		/// corresponding XML entity appears next in the text window.
		/// </summary>
		/// <param name="ch"></param>
		/// <returns></returns>
		private bool AdvanceIfMatches(char ch)
		{
			char peekCh = TextWindow.PeekChar();
			if ((peekCh == ch) ||
				(peekCh == '{' && ch == '<') ||
				(peekCh == '}' && ch == '>'))
			{
				TextWindow.AdvanceChar();
				return true;
			}

			if (peekCh == '&')
			{
				int pos = TextWindow.Position;

				char nextChar;
				char nextSurrogate;
				if (TextWindow.TryScanXmlEntity(out nextChar, out nextSurrogate)
					&& nextChar == ch && nextSurrogate == SlidingTextWindow.InvalidCharacter)
				{
					return true;
				}

				TextWindow.Reset(pos);
			}

			return false;
		}

		/// <summary>
		/// Convenience property for determining whether we are currently lexing the
		/// value of a cref or name _annotation.
		/// </summary>
		private bool InXmlCrefOrNameAttributeValue
		{
			get
			{
				switch (this._mode & LexerMode.MaskLexMode)
				{
					case LexerMode.XmlCrefQuote:
					case LexerMode.XmlCrefDoubleQuote:
					case LexerMode.XmlNameQuote:
					case LexerMode.XmlNameDoubleQuote:
						return true;
					default:
						return false;
				}
			}
		}

		/// <summary>
		/// Convenience property for determining whether we are currently lexing the
		/// value of a name _annotation.
		/// </summary>
		private bool InXmlNameAttributeValue
		{
			get
			{
				switch (this._mode & LexerMode.MaskLexMode)
				{
					case LexerMode.XmlNameQuote:
					case LexerMode.XmlNameDoubleQuote:
						return true;
					default:
						return false;
				}
			}
		}

		/// <summary>
		/// Diagnostics that occur within cref attributes need to be
		/// wrapped with ErrorCode.WRN_ErrorOverride.
		/// </summary>
		private void AddCrefError(ErrorCode code, params object[] args)
		{
			this.AddCrefError(MakeError(code, args));
		}

		/// <summary>
		/// Diagnostics that occur within cref attributes need to be
		/// wrapped with ErrorCode.WRN_ErrorOverride.
		/// </summary>
		private void AddCrefError(DiagnosticInfo info)
		{
			if (info != null)
			{
				this.AddError(ErrorCode.WRN_ErrorOverride, info, info.Code);
			}
		}

		/// <summary>
		/// Lexer entry point for LexMode.XmlCDataSectionText
		/// </summary>
		private SyntaxToken LexXmlCDataSectionTextToken()
		{
			TokenInfo info = default(TokenInfo);

			SyntaxListBuilder leading = null;
			this.LexXmlDocCommentLeadingTrivia(ref leading);

			this.Start();
			this.ScanXmlCDataSectionTextToken(ref info);
			var errors = this.Errors;

			return Create(ref info, leading, null, errors);
		}

		private bool ScanXmlCDataSectionTextToken(ref TokenInfo info)
		{
			char ch;

			Debug.Assert(!this.LocationIs(XmlDocCommentLocation.Start));
			Debug.Assert(!this.LocationIs(XmlDocCommentLocation.Exterior));

			if (this.LocationIs(XmlDocCommentLocation.End))
			{
				info.Kind = SyntaxKind.EndOfDocumentationCommentToken;
				return true;
			}

			switch (ch = TextWindow.PeekChar())
			{
				case ']':
					if (TextWindow.PeekChar(1) == ']' && TextWindow.PeekChar(2) == '>')
					{
						TextWindow.AdvanceChar(3);
						info.Kind = SyntaxKind.XmlCDataEndToken;
						break;
					}

					goto default;

				case '\r':
				case '\n':
					this.ScanEndOfLine();
					info.StringValue = info.Text = TextWindow.GetText(false);
					info.Kind = SyntaxKind.XmlTextLiteralNewLineToken;
					this.MutateLocation(XmlDocCommentLocation.Exterior);
					break;

				case SlidingTextWindow.InvalidCharacter:
					if (!TextWindow.IsReallyAtEnd())
					{
						goto default;
					}

					info.Kind = SyntaxKind.EndOfDocumentationCommentToken;
					break;

				default:
					if (SyntaxKindFacts.IsNewLine(ch))
					{
						goto case '\n';
					}

					this.ScanXmlCDataSectionText(ref info);
					info.Kind = SyntaxKind.XmlTextLiteralToken;
					break;
			}

			return true;
		}

		private void ScanXmlCDataSectionText(ref TokenInfo info)
		{
			while (true)
			{
				var ch = TextWindow.PeekChar();
				switch (ch)
				{
					case ']':
						if (TextWindow.PeekChar(1) == ']' && TextWindow.PeekChar(2) == '>')
						{
							info.StringValue = info.Text = TextWindow.GetText(false);
							return;
						}

						goto default;

					case '\r':
					case '\n':
						info.StringValue = info.Text = TextWindow.GetText(false);
						return;

					case SlidingTextWindow.InvalidCharacter:
						if (!TextWindow.IsReallyAtEnd())
						{
							goto default;
						}

						info.StringValue = info.Text = TextWindow.GetText(false);
						return;

					case '*':
						if (this.StyleIs(XmlDocCommentStyle.Delimited) && TextWindow.PeekChar(1) == '/')
						{
							// we're at the end of the comment, but don't lex it yet.
							info.StringValue = info.Text = TextWindow.GetText(false);
							return;
						}

						goto default;

					default:
						if (SyntaxKindFacts.IsNewLine(ch))
						{
							goto case '\n';
						}

						TextWindow.AdvanceChar();
						break;
				}
			}
		}

		/// <summary>
		/// Lexer entry point for LexMode.XmlCommentText
		/// </summary>
		private SyntaxToken LexXmlCommentTextToken()
		{
			TokenInfo info = default(TokenInfo);

			SyntaxListBuilder leading = null;
			this.LexXmlDocCommentLeadingTrivia(ref leading);

			this.Start();
			this.ScanXmlCommentTextToken(ref info);
			var errors = this.Errors;

			return Create(ref info, leading, null, errors);
		}

		private bool ScanXmlCommentTextToken(ref TokenInfo info)
		{
			char ch;

			Debug.Assert(!this.LocationIs(XmlDocCommentLocation.Start));
			Debug.Assert(!this.LocationIs(XmlDocCommentLocation.Exterior));

			if (this.LocationIs(XmlDocCommentLocation.End))
			{
				info.Kind = SyntaxKind.EndOfDocumentationCommentToken;
				return true;
			}

			switch (ch = TextWindow.PeekChar())
			{
				case '-':
					if (TextWindow.PeekChar(1) == '-')
					{
						if (TextWindow.PeekChar(2) == '>')
						{
							TextWindow.AdvanceChar(3);
							info.Kind = SyntaxKind.XmlCommentEndToken;
							break;
						}
						else
						{
							TextWindow.AdvanceChar(2);
							info.Kind = SyntaxKind.MinusMinusToken;
							break;
						}
					}

					goto default;

				case '\r':
				case '\n':
					this.ScanEndOfLine();
					info.StringValue = info.Text = TextWindow.GetText(false);
					info.Kind = SyntaxKind.XmlTextLiteralNewLineToken;
					this.MutateLocation(XmlDocCommentLocation.Exterior);
					break;

				case SlidingTextWindow.InvalidCharacter:
					if (!TextWindow.IsReallyAtEnd())
					{
						goto default;
					}
					info.Kind = SyntaxKind.EndOfDocumentationCommentToken;
					break;

				default:
					if (SyntaxKindFacts.IsNewLine(ch))
					{
						goto case '\n';
					}

					this.ScanXmlCommentText(ref info);
					info.Kind = SyntaxKind.XmlTextLiteralToken;
					break;
			}

			return true;
		}

		private void ScanXmlCommentText(ref TokenInfo info)
		{
			while (true)
			{
				var ch = TextWindow.PeekChar();
				switch (ch)
				{
					case '-':
						if (TextWindow.PeekChar(1) == '-')
						{
							info.StringValue = info.Text = TextWindow.GetText(false);
							return;
						}

						goto default;

					case '\r':
					case '\n':
						info.StringValue = info.Text = TextWindow.GetText(false);
						return;

					case SlidingTextWindow.InvalidCharacter:
						if (!TextWindow.IsReallyAtEnd())
						{
							goto default;
						}

						info.StringValue = info.Text = TextWindow.GetText(false);
						return;

					case '*':
						if (this.StyleIs(XmlDocCommentStyle.Delimited) && TextWindow.PeekChar(1) == '/')
						{
							// we're at the end of the comment, but don't lex it yet.
							info.StringValue = info.Text = TextWindow.GetText(false);
							return;
						}

						goto default;

					default:
						if (SyntaxKindFacts.IsNewLine(ch))
						{
							goto case '\n';
						}

						TextWindow.AdvanceChar();
						break;
				}
			}
		}

		/// <summary>
		/// Lexer entry point for LexMode.XmlProcessingInstructionText
		/// </summary>
		private SyntaxToken LexXmlProcessingInstructionTextToken()
		{
			TokenInfo info = default(TokenInfo);

			SyntaxListBuilder leading = null;
			this.LexXmlDocCommentLeadingTrivia(ref leading);

			this.Start();
			this.ScanXmlProcessingInstructionTextToken(ref info);
			var errors = this.Errors;

			return Create(ref info, leading, null, errors);
		}

		// CONSIDER: This could easily be merged with ScanXmlCDataSectionTextToken
		private bool ScanXmlProcessingInstructionTextToken(ref TokenInfo info)
		{
			char ch;

			Debug.Assert(!this.LocationIs(XmlDocCommentLocation.Start));
			Debug.Assert(!this.LocationIs(XmlDocCommentLocation.Exterior));

			if (this.LocationIs(XmlDocCommentLocation.End))
			{
				info.Kind = SyntaxKind.EndOfDocumentationCommentToken;
				return true;
			}

			switch (ch = TextWindow.PeekChar())
			{
				case '?':
					if (TextWindow.PeekChar(1) == '>')
					{
						TextWindow.AdvanceChar(2);
						info.Kind = SyntaxKind.XmlProcessingInstructionEndToken;
						break;
					}

					goto default;

				case '\r':
				case '\n':
					this.ScanEndOfLine();
					info.StringValue = info.Text = TextWindow.GetText(false);
					info.Kind = SyntaxKind.XmlTextLiteralNewLineToken;
					this.MutateLocation(XmlDocCommentLocation.Exterior);
					break;

				case SlidingTextWindow.InvalidCharacter:
					if (!TextWindow.IsReallyAtEnd())
					{
						goto default;
					}

					info.Kind = SyntaxKind.EndOfDocumentationCommentToken;
					break;

				default:
					if (SyntaxKindFacts.IsNewLine(ch))
					{
						goto case '\n';
					}

					this.ScanXmlProcessingInstructionText(ref info);
					info.Kind = SyntaxKind.XmlTextLiteralToken;
					break;
			}

			return true;
		}

		// CONSIDER: This could easily be merged with ScanXmlCDataSectionText
		private void ScanXmlProcessingInstructionText(ref TokenInfo info)
		{
			while (true)
			{
				var ch = TextWindow.PeekChar();
				switch (ch)
				{
					case '?':
						if (TextWindow.PeekChar(1) == '>')
						{
							info.StringValue = info.Text = TextWindow.GetText(false);
							return;
						}

						goto default;

					case '\r':
					case '\n':
						info.StringValue = info.Text = TextWindow.GetText(false);
						return;

					case SlidingTextWindow.InvalidCharacter:
						if (!TextWindow.IsReallyAtEnd())
						{
							goto default;
						}

						info.StringValue = info.Text = TextWindow.GetText(false);
						return;

					case '*':
						if (this.StyleIs(XmlDocCommentStyle.Delimited) && TextWindow.PeekChar(1) == '/')
						{
							// we're at the end of the comment, but don't lex it yet.
							info.StringValue = info.Text = TextWindow.GetText(false);
							return;
						}

						goto default;

					default:
						if (SyntaxKindFacts.IsNewLine(ch))
						{
							goto case '\n';
						}

						TextWindow.AdvanceChar();
						break;
				}
			}
		}

		/// <summary>
		/// Collects XML doc comment exterior trivia, and therefore is a no op unless we are in the Start or Exterior of an XML doc comment.
		/// </summary>
		/// <param name="trivia">List in which to collect the trivia</param>
		private void LexXmlDocCommentLeadingTrivia(ref SyntaxListBuilder trivia)
		{
			var start = TextWindow.Position;
			this.Start();

			if (this.LocationIs(XmlDocCommentLocation.Start) && this.StyleIs(XmlDocCommentStyle.Delimited))
			{
				// Read the /** that begins an XML doc comment. Since these are recognized only
				// when the trailing character is not a '*', we wind up in the interior of the
				// doc comment at the end.

				if (TextWindow.PeekChar() == '/'
					&& TextWindow.PeekChar(1) == '*'
					&& TextWindow.PeekChar(2) == '*'
					&& TextWindow.PeekChar(3) != '*')
				{
					TextWindow.AdvanceChar(3);
					var text = TextWindow.GetText(true);
					this.AddTrivia(SyntaxFactory.DocumentationCommentExteriorTrivia(text), ref trivia);
					this.MutateLocation(XmlDocCommentLocation.Interior);
					return;
				}
			}
			else if (this.LocationIs(XmlDocCommentLocation.Start) || this.LocationIs(XmlDocCommentLocation.Exterior))
			{
				// We're in the exterior of an XML doc comment and need to eat the beginnings of
				// lines, for single line and delimited comments. We chew up white space until
				// a non-whitespace character, and then make the right decision depending on
				// what kind of comment we're in.

				while (true)
				{
					char ch = TextWindow.PeekChar();
					switch (ch)
					{
						case ' ':
						case '\t':
						case '\v':
						case '\f':
							TextWindow.AdvanceChar();
							break;

						case '/':
							if (this.StyleIs(XmlDocCommentStyle.SingleLine) && TextWindow.PeekChar(1) == '/' && TextWindow.PeekChar(2) == '/' && TextWindow.PeekChar(3) != '/')
							{
								TextWindow.AdvanceChar(3);
								var text = TextWindow.GetText(true);
								this.AddTrivia(SyntaxFactory.DocumentationCommentExteriorTrivia(text), ref trivia);
								this.MutateLocation(XmlDocCommentLocation.Interior);
								return;
							}

							goto default;

						case '*':
							if (this.StyleIs(XmlDocCommentStyle.Delimited))
							{
								while (TextWindow.PeekChar() == '*' && TextWindow.PeekChar(1) != '/')
								{
									TextWindow.AdvanceChar();
								}

								var text = TextWindow.GetText(true);
								if (!String.IsNullOrEmpty(text))
								{
									this.AddTrivia(SyntaxFactory.DocumentationCommentExteriorTrivia(text), ref trivia);
								}

								// This setup ensures that on the final line of a comment, if we have
								// the string "  */", the "*/" part is separated from the whitespace
								// and therefore recognizable as the end of the comment.

								if (TextWindow.PeekChar() == '*' && TextWindow.PeekChar(1) == '/')
								{
									TextWindow.AdvanceChar(2);
									this.AddTrivia(SyntaxFactory.DocumentationCommentExteriorTrivia("*/"), ref trivia);
									this.MutateLocation(XmlDocCommentLocation.End);
								}
								else
								{
									this.MutateLocation(XmlDocCommentLocation.Interior);
								}

								return;
							}

							goto default;

						default:
							if (SyntaxKindFacts.IsWhitespace(ch))
							{
								goto case ' ';
							}

							// so here we have something else. if this is a single-line xml
							// doc comment, that means we're on a line that's no longer a doc
							// comment, so we need to rewind. if we're in a delimited doc comment,
							// then that means we hit pay dirt and we're back into xml text.

							if (this.StyleIs(XmlDocCommentStyle.SingleLine))
							{
								TextWindow.Reset(start);
								this.MutateLocation(XmlDocCommentLocation.End);
							}
							else // XmlDocCommentStyle.Delimited
							{
								Debug.Assert(this.StyleIs(XmlDocCommentStyle.Delimited));

								var text = TextWindow.GetText(true);
								if (!String.IsNullOrEmpty(text))
									this.AddTrivia(SyntaxFactory.DocumentationCommentExteriorTrivia(text), ref trivia);
								this.MutateLocation(XmlDocCommentLocation.Interior);
							}

							return;
					}
				}
			}
			else if (!this.LocationIs(XmlDocCommentLocation.End) && this.StyleIs(XmlDocCommentStyle.Delimited))
			{
				if (TextWindow.PeekChar() == '*' && TextWindow.PeekChar(1) == '/')
				{
					TextWindow.AdvanceChar(2);
					var text = TextWindow.GetText(true);
					this.AddTrivia(SyntaxFactory.DocumentationCommentExteriorTrivia(text), ref trivia);
					this.MutateLocation(XmlDocCommentLocation.End);
				}
			}
		}

		private void LexXmlDocCommentLeadingTriviaWithWhitespace(ref SyntaxListBuilder trivia)
		{
			while (true)
			{
				this.LexXmlDocCommentLeadingTrivia(ref trivia);

				char ch = TextWindow.PeekChar();
				if (this.LocationIs(XmlDocCommentLocation.Interior)
					&& (SyntaxKindFacts.IsWhitespace(ch) || SyntaxKindFacts.IsNewLine(ch)))
				{
					this.LexXmlWhitespaceAndNewLineTrivia(ref trivia);
				}
				else
				{
					break;
				}
			}
		}

		/// <summary>
		/// Collects whitespace and new line trivia for XML doc comments. Does not see XML doc comment exterior trivia, and is a no op unless we are in the interior.
		/// </summary>
		/// <param name="trivia">List in which to collect the trivia</param>
		private void LexXmlWhitespaceAndNewLineTrivia(ref SyntaxListBuilder trivia)
		{
			this.Start();
			if (this.LocationIs(XmlDocCommentLocation.Interior))
			{
				char ch = TextWindow.PeekChar();
				switch (ch)
				{
					case ' ':
					case '\t':       // Horizontal tab
					case '\v':       // Vertical Tab
					case '\f':       // Form-feed
						this.AddTrivia(this.ScanWhitespace(), ref trivia);
						break;

					case '\r':
					case '\n':
						this.AddTrivia(this.ScanEndOfLine(), ref trivia);
						this.MutateLocation(XmlDocCommentLocation.Exterior);
						return;

					case '*':
						if (this.StyleIs(XmlDocCommentStyle.Delimited) && TextWindow.PeekChar(1) == '/')
						{
							// we're at the end of the comment, but don't add as trivia here.
							return;
						}

						goto default;

					default:
						if (SyntaxKindFacts.IsWhitespace(ch))
						{
							goto case ' ';
						}

						if (SyntaxKindFacts.IsNewLine(ch))
						{
							goto case '\n';
						}

						return;
				}
			}
		}


	}
}