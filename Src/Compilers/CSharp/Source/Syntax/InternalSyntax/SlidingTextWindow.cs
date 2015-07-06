// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using Microsoft.CodeAnalysis.Collections;

using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
	/// <summary>
	/// Keeps a sliding buffer over the SourceText of a file for the lexer. 
	/// Also provides the lexer with the ability to keep track of a current "lexeme"
	/// by leaving a marker and advancing ahead the offset. 
	/// The lexer can then decide to "keep" the lexeme by erasing the marker, or abandon the current
	/// lexeme by moving the offset back to the marker.
	/// </summary>
	internal sealed class SlidingTextWindow : IDisposable
	{
		/// <summary>
		/// In many cases, e.g. PeekChar, we need the ability to indicate that there are
		/// no characters left and we have reached the end of the stream, or some other
		/// invalid or not present character was asked for. Due to perf concerns, things
		/// like nullable or out variables are not viable. Instead we need to choose a
		/// char value which can never be legal.
		/// 
		/// In .NET, all characters are represented in 16 bits using the UTF-16 encoding.
		/// Fortunately for us, there are a variety of different bit patterns which
		/// are *not* legal UTF-16 characters. 0xffff (char.MaxValue) is one of these
		/// characters -- a legal Unicode code point, but not a legal UTF-16 bit pattern.
		/// </summary>
		public const char InvalidCharacter = char.MaxValue;

		private const int DefaultWindowLength = 2048;

		private readonly SourceText _text;                 // Source of text to parse.
		private int _basis;                                // Offset of the window relative to the SourceText start.
		private int _offset;                               // Offset from the start of the window.
		private readonly int _textEnd;                     // Absolute end position
		private char[] _characterWindow;                   // Moveable window of chars from source text
		private int _characterWindowCount;                 // # of valid characters in chars buffer

		private int _lexemeStart;                          // Start of current lexeme relative to the window start.

		// Example for the above variables:
		// The text starts at 0.
		// The window onto the text starts at basis.
		// The current character is at (basis + offset), AKA the current "Position".
		// The current lexeme started at (basis + lexemeStart), which is <= (basis + offset)
		// The current lexeme is the characters between the lexemeStart and the offset.

		private readonly StringTable _strings;

		private static readonly ObjectPool<char[]> WindowPool = new ObjectPool<char[]>(() => new char[DefaultWindowLength]);

		public SourceText Text
		{
			get
			{
				return this._text;
			}
		}



		/// <summary>
		/// The current absolute position in the text file.
		/// </summary>
		public int Position
		{
			get
			{
				return this._basis + this._offset;
			}
		}

		/// <summary>
		/// The current offset inside the window (relative to the window start).
		/// </summary>
		public int Offset
		{
			get
			{
				return this._offset;
			}
		}

		/// <summary>
		/// The buffer backing the current window.
		/// </summary>
		public char[] CharacterWindow
		{
			get
			{
				return this._characterWindow;
			}
		}

		/// <summary>
		/// Returns the start of the current lexeme relative to the window start.
		/// </summary>
		public int LexemeRelativeStart
		{
			get
			{
				return this._lexemeStart;
			}
		}

		/// <summary>
		/// Number of characters in the character window.
		/// </summary>
		public int CharacterWindowCount
		{
			get
			{
				return this._characterWindowCount;
			}
		}

		/// <summary>
		/// The absolute position of the start of the current lexeme in the given
		/// SourceText.
		/// </summary>
		public int LexemeStartPosition
		{
			get
			{
				return this._basis + this._lexemeStart;
			}
		}

		/// <summary>
		/// The number of characters in the current lexeme.
		/// </summary>
		public int Width
		{
			get
			{
				return this._offset - this._lexemeStart;
			}
		}

		public SlidingTextWindow(SourceText text)
		{
			this._text = text;
			this._basis = 0;
			this._offset = 0;
			this._textEnd = text.Length;
			this._strings = StringTable.GetInstance();
			this._characterWindow = WindowPool.Allocate();
			this._lexemeStart = 0;
		}

		public void Dispose()
		{
			if (this._characterWindow != null)
			{
				WindowPool.Free(this._characterWindow);
				this._characterWindow = null;
				this._strings.Free();
			}
		}

		/// <summary>
		/// Start parsing a new lexeme.
		/// </summary>
		public void Start()
		{
			this._lexemeStart = this._offset;
		}

		public void Reset(int position)
		{
			// if position is within already read character range then just use what we have
			int relative = position - this._basis;
			if (relative >= 0 && relative <= this._characterWindowCount)
			{
				this._offset = relative;
			}
			else
			{
				// we need to reread text buffer
				int amountToRead = Math.Min(this._text.Length, position + this._characterWindow.Length) - position;
				amountToRead = Math.Max(amountToRead, 0);
				if (amountToRead > 0)
				{
					this._text.CopyTo(position, this._characterWindow, 0, amountToRead);
				}

				this._lexemeStart = 0;
				this._offset = 0;
				this._basis = position;
				this._characterWindowCount = amountToRead;
			}
		}

		private bool MoreChars()
		{
			if (this._offset >= this._characterWindowCount)
			{
				if (this.Position >= this._textEnd)
				{
					return false;
				}

				// if lexeme scanning is sufficiently into the char buffer, 
				// then refocus the window onto the lexeme
				if (this._lexemeStart > (this._characterWindowCount / 4))
				{
					Array.Copy(this._characterWindow,
						this._lexemeStart,
						this._characterWindow,
						0,
						_characterWindowCount - _lexemeStart);
					this._characterWindowCount -= _lexemeStart;
					this._offset -= _lexemeStart;
					this._basis += this._lexemeStart;
					this._lexemeStart = 0;
				}

				if (this._characterWindowCount >= this._characterWindow.Length)
				{
					// grow char array, since we need more contiguous space
					char[] oldWindow = this._characterWindow;
					char[] newWindow = new char[this._characterWindow.Length * 2];
					Array.Copy(oldWindow, 0, newWindow, 0, this._characterWindowCount);
					WindowPool.ForgetTrackedObject(oldWindow, newWindow);
					this._characterWindow = newWindow;
				}

				int amountToRead = Math.Min(this._textEnd - (this._basis + this._characterWindowCount),
					this._characterWindow.Length - this._characterWindowCount);
				this._text.CopyTo(this._basis + this._characterWindowCount,
					this._characterWindow,
					this._characterWindowCount,
					amountToRead);
				this._characterWindowCount += amountToRead;
				return amountToRead > 0;
			}

			return true;
		}

		/// <summary>
		/// After reading <see cref=" InvalidCharacter"/>, a consumer can determine
		/// if the InvalidCharacter was in the user's source or a sentinel.
		/// 
		/// Comments and string literals are allowed to contain any Unicode character.
		/// </summary>
		/// <returns></returns>
		internal bool IsReallyAtEnd()
		{
			return _offset >= _characterWindowCount && Position >= _textEnd;
		}

		/// <summary>
		/// Advance the current position by one. No guarantee that this
		/// position is valid.
		/// </summary>
		public void AdvanceChar()
		{
			this._offset++;
		}

		/// <summary>
		/// Advance the current position by n. No guarantee that this position
		/// is valid.
		/// </summary>
		public void AdvanceChar(int n)
		{
			this._offset += n;
		}

		/// <summary>
		/// Grab the next character and advance the position.
		/// </summary>
		/// <returns>
		/// The next character, <see cref="InvalidCharacter" /> if there were no characters 
		/// remaining.
		/// </returns>
		public char NextChar()
		{
			char c = PeekChar();
			if (c != InvalidCharacter)
			{
				this.AdvanceChar();
			}
			return c;
		}

		/// <summary>
		/// Gets the next character if there are any characters in the 
		/// SourceText. May advance the window if we are at the end.
		/// </summary>
		/// <returns>
		/// The next character if any are available. InvalidCharacterSentinal otherwise.
		/// </returns>
		public char PeekChar()
		{
			if (this._offset >= this._characterWindowCount
				&& !MoreChars())
			{
				return InvalidCharacter;
			}

			// N.B. MoreChars may update the offset.
			return this._characterWindow[this._offset];
		}

		/// <summary>
		/// Gets the character at the given offset to the current position if
		/// the position is valid within the SourceText.
		/// </summary>
		/// <returns>
		/// The next character if any are available. InvalidCharacterSentinal otherwise.
		/// </returns>
		public char PeekChar(int delta)
		{
			int position = this.Position;
			this.AdvanceChar(delta);

			char ch;
			if (this._offset >= this._characterWindowCount
				&& !MoreChars())
			{
				ch = InvalidCharacter;
			}
			else
			{
				// N.B. MoreChars may update the offset.
				ch = this._characterWindow[this._offset];
			}

			this.Reset(position);
			return ch;
		}

		public bool IsUnicodeEscape()
		{
			if (this.PeekChar() == '\\')
			{
				var ch2 = this.PeekChar(1);
				if (ch2 == 'U' || ch2 == 'u')
				{
					return true;
				}
			}

			return false;
		}

		public char PeekCharOrUnicodeEscape(out char surrogateCharacter)
		{
			if (this.IsUnicodeEscape())
			{
				return this.PeekUnicodeEscape(out surrogateCharacter);
			}
			else
			{
				surrogateCharacter = InvalidCharacter;
				return this.PeekChar();
			}
		}

		public char PeekUnicodeEscape(out char surrogateCharacter)
		{
			int position = this.Position;

			// if we're peeking, then we don't want to change the position
			SyntaxDiagnosticInfo info;
			var ch = this.ScanUnicodeEscape(peek: true, surrogateCharacter: out surrogateCharacter, info: out info);
			Debug.Assert(info == null, "Never produce a diagnostic while peeking.");
			this.Reset(position);
			return ch;
		}

		public char NextCharOrUnicodeEscape(out char surrogateCharacter, out SyntaxDiagnosticInfo info)
		{
			var ch = this.PeekChar();
			Debug.Assert(ch != InvalidCharacter, "Precondition established by all callers; required for correctness of AdvanceChar() call.");
			if (ch == '\\')
			{
				var ch2 = this.PeekChar(1);
				if (ch2 == 'U' || ch2 == 'u')
				{
					return this.ScanUnicodeEscape(peek: false, surrogateCharacter: out surrogateCharacter, info: out info);
				}
			}

			surrogateCharacter = InvalidCharacter;
			info = null;
			this.AdvanceChar();
			return ch;
		}

		public char NextUnicodeEscape(out char surrogateCharacter, out SyntaxDiagnosticInfo info)
		{
			return ScanUnicodeEscape(peek: false, surrogateCharacter: out surrogateCharacter, info: out info);
		}

		private char ScanUnicodeEscape(bool peek, out char surrogateCharacter, out SyntaxDiagnosticInfo info)
		{
			surrogateCharacter = InvalidCharacter;
			info = null;

			int start = this.Position;
			char character = this.PeekChar();
			Debug.Assert(character == '\\');
			this.AdvanceChar();

			character = this.PeekChar();
			if (character == 'U')
			{
				uint uintChar = 0;

				this.AdvanceChar();
				if (!SyntaxKindFacts.IsHexDigit(this.PeekChar()))
				{
					if (!peek)
					{
						info = CreateIllegalEscapeDiagnostic(start);
					}
				}
				else
				{
					for (int i = 0; i < 8; i++)
					{
						character = this.PeekChar();
						if (!SyntaxKindFacts.IsHexDigit(character))
						{
							if (!peek)
							{
								info = CreateIllegalEscapeDiagnostic(start);
							}

							break;
						}

						uintChar = (uint)((uintChar << 4) + SyntaxKindFacts.HexValue(character));
						this.AdvanceChar();
					}

					if (uintChar > 0x0010FFFF)
					{
						if (!peek)
						{
							info = CreateIllegalEscapeDiagnostic(start);
						}
					}
					else
					{
						character = GetCharsFromUtf32(uintChar, out surrogateCharacter);
					}
				}
			}
			else if (character == 'u' || character == 'x')
			{
				Debug.Assert(character == 'u' || character == 'x');

				int intChar = 0;
				this.AdvanceChar();
				if (!SyntaxKindFacts.IsHexDigit(this.PeekChar()))
				{
					if (!peek)
					{
						info = CreateIllegalEscapeDiagnostic(start);
					}
				}
				else
				{
					for (int i = 0; i < 4; i++)
					{
						char ch2 = this.PeekChar();
						if (!SyntaxKindFacts.IsHexDigit(ch2))
						{
							if (character == 'u')
							{
								if (!peek)
								{
									info = CreateIllegalEscapeDiagnostic(start);
								}
							}

							break;
						}

						intChar = (intChar << 4) + SyntaxKindFacts.HexValue(ch2);
						this.AdvanceChar();
					}

					character = (char)intChar;
				}
			}
			else //Octal
			{
				Debug.Assert(SyntaxKindFacts.IsOctalDigit(character));

				List<char> chars = new List<char>();
				int intChar = 0;

				chars.Add(character);
				intChar = Convert.ToInt32(new string(chars.ToArray()), 8);


				this.AdvanceChar();
				character = this.PeekChar();// 第二个
				if (SyntaxKindFacts.IsOctalDigit(character))
				{
					chars.Add(character);
					intChar = Convert.ToInt32(new string(chars.ToArray()), 8);

					this.AdvanceChar();
					character = this.PeekChar(); //第三个
					if (SyntaxKindFacts.IsOctalDigit(character))
					{
						if (SyntaxKindFacts.IsZeroToThreeDigit(chars[0]))
						{
							chars.Add(character);
							intChar = Convert.ToInt32(new string(chars.ToArray()), 8);
							this.AdvanceChar();
						}
						else
						{
							if (!peek)
							{
								info = CreateIllegalEscapeDiagnostic(start);
							}
						}
					}
				}

				character = (char)intChar;
				//else
				//{
				//	for (int i = 0; i < 2; i++)
				//	{
				//		char ch2 = this.PeekChar();
				//		if (!SyntaxKindFacts.IsOctalDigit(ch2))
				//		{
				//			if (character == 'u')
				//			{
				//				if (!peek)
				//				{
				//					info = CreateIllegalEscapeDiagnostic(start);
				//				}
				//			}

				//			break;
				//		}

				//		intChar = (intChar << 4) + SyntaxKindFacts.HexValue(ch2);
				//		this.AdvanceChar();
				//	}

				//	character = (char)intChar;
				//}
			}

			return character;
		}

		/// <summary>
		/// Given that the next character is an ampersand ('&amp;'), attempt to interpret the
		/// following characters as an XML entity.  On success, populate the out parameters
		/// with the low and high UTF-16 surrogates for the character represented by the
		/// entity.
		/// </summary>
		/// <param name="ch">e.g. '&lt;' for &amp;lt;.</param>
		/// <param name="surrogate">e.g. '\uDC00' for &amp;#x10000; (ch == '\uD800').</param>
		/// <returns>True if a valid XML entity was consumed.</returns>
		/// <remarks>
		/// NOTE: Always advances, even on failure.
		/// </remarks>
		public bool TryScanXmlEntity(out char ch, out char surrogate)
		{
			Debug.Assert(this.PeekChar() == '&');

			ch = '&';
			this.AdvanceChar();

			surrogate = InvalidCharacter;

			switch (this.PeekChar())
			{
				case 'l':
					if (AdvanceIfMatches("lt;"))
					{
						ch = '<';
						return true;
					}
					break;
				case 'g':
					if (AdvanceIfMatches("gt;"))
					{
						ch = '>';
						return true;
					}
					break;
				case 'a':
					if (AdvanceIfMatches("amp;"))
					{
						ch = '&';
						return true;
					}
					else if (AdvanceIfMatches("apos;"))
					{
						ch = '\'';
						return true;
					}
					break;
				case 'q':
					if (AdvanceIfMatches("quot;"))
					{
						ch = '"';
						return true;
					}
					break;
				case '#':
					{
						this.AdvanceChar(); //#

						uint uintChar = 0;

						if (AdvanceIfMatches("x"))
						{
							char digit;
							while (SyntaxKindFacts.IsHexDigit(digit = this.PeekChar()))
							{
								this.AdvanceChar();

								// disallow overflow
								if (uintChar <= 0x7FFFFFF)
								{
									uintChar = (uintChar << 4) + (uint)SyntaxKindFacts.HexValue(digit);
								}
								else
								{
									return false;
								}
							}
						}
						else
						{
							char digit;
							while (SyntaxKindFacts.IsDecDigit(digit = this.PeekChar()))
							{
								this.AdvanceChar();

								// disallow overflow
								if (uintChar <= 0x7FFFFFF)
								{
									uintChar = (uintChar << 3) + (uintChar << 1) + (uint)SyntaxKindFacts.DecValue(digit);
								}
								else
								{
									return false;
								}
							}
						}

						if (AdvanceIfMatches(";"))
						{
							ch = GetCharsFromUtf32(uintChar, out surrogate);
							return true;
						}

						break;
					}
			}

			return false;
		}

		/// <summary>
		/// If the next characters in the window match the given string,
		/// then advance past those characters.  Otherwise, do nothing.
		/// </summary>
		private bool AdvanceIfMatches(string desired)
		{
			int length = desired.Length;

			for (int i = 0; i < length; i++)
			{
				if (PeekChar(i) != desired[i])
				{
					return false;
				}
			}

			AdvanceChar(length);
			return true;
		}

		private SyntaxDiagnosticInfo CreateIllegalEscapeDiagnostic(int start)
		{
			return new SyntaxDiagnosticInfo(start - this.LexemeStartPosition,
				this.Position - start,
				ErrorCode.ERR_IllegalEscape);
		}

		public string Intern(StringBuilder text)
		{
			return this._strings.Add(text);
		}

		public string Intern(char[] array, int start, int length)
		{
			return this._strings.Add(array, start, length);
		}

		public string GetInternedText()
		{
			return this.Intern(this._characterWindow, this._lexemeStart, this.Width);
		}

		public string GetText(bool intern)
		{
			return this.GetText(this.LexemeStartPosition, this.Width, intern);
		}

		public string GetText(int position, int length, bool intern)
		{
			int offset = position - this._basis;

			// PERF: Whether interning or not, there are some frequently ocurring
			// easy cases we can pick off easily.
			switch (length)
			{
				case 0:
					return string.Empty;

				case 1:
					if (this._characterWindow[offset] == ' ')
					{
						return " ";
					}
					break;

				case 2:
					char firstChar = this._characterWindow[offset];
					if (firstChar == '\r' && this._characterWindow[offset + 1] == '\n')
					{
						return "\r\n";
					}
					if (firstChar == '/' && this._characterWindow[offset + 1] == '/')
					{
						return "//";
					}
					break;

				case 3:
					if (this._characterWindow[offset] == '/' && this._characterWindow[offset + 1] == '/' && this._characterWindow[offset + 2] == ' ')
					{
						return "// ";
					}
					break;
			}

			if (intern)
			{
				return this.Intern(this._characterWindow, offset, length);
			}
			else
			{
				return new string(this._characterWindow, offset, length);
			}
		}

		internal static char GetCharsFromUtf32(uint codepoint, out char lowSurrogate)
		{
			if (codepoint < (uint)0x00010000)
			{
				lowSurrogate = InvalidCharacter;
				return (char)codepoint;
			}
			else
			{
				Debug.Assert(codepoint > 0x0000FFFF && codepoint <= 0x0010FFFF);
				lowSurrogate = (char)((codepoint - 0x00010000) % 0x0400 + 0xDC00);
				return (char)((codepoint - 0x00010000) / 0x0400 + 0xD800);
			}
		}
	}
}