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

		private bool ScanIdentifier(ref TokenInfo info)
		{
			return
				ScanIdentifier_FastPath(ref info) ||
				(InXmlCrefOrNameAttributeValue ? ScanIdentifier_CrefSlowPath(ref info) : ScanIdentifier_SlowPath(ref info));
		}

		// Implements a faster identifier lexer for the common case in the 
		// language where:
		//
		//   a) identifiers are not verbatim
		//   b) identifiers don't contain unicode characters
		//   c) identifiers don't contain unicode escapes
		//
		// Given that nearly all identifiers will contain [_a-zA-Z0-9] and will
		// be terminated by a small set of known characters (like dot, comma, 
		// etc.), we can sit in a tight loop looking for this pattern and only
		// falling back to the slower (but correct) path if we see something we
		// can't handle.
		//
		// Note: this function also only works if the identifier (and terminator)
		// can be found in the current sliding window of chars we have from our
		// source text.  With this constraint we can avoid the costly overhead 
		// incurred with peek/advance/next.  Because of this we can also avoid
		// the unecessary stores/reads from identBuffer and all other instance
		// state while lexing.  Instead we just keep track of our start, end,
		// and max positions and use those for quick checks internally.
		//
		// Note: it is critical that this method must only be called from a 
		// codepath that checked for IsIdentifierStartChar or '@' first. 
		private bool ScanIdentifier_FastPath(ref TokenInfo info)
		{
			if ((_mode & LexerMode.MaskLexMode) == LexerMode.DebuggerSyntax)
			{
				// Debugger syntax is wonky.  Can't use the fast path for it.
				return false;
			}

			var currentOffset = TextWindow.Offset;
			var characterWindow = TextWindow.CharacterWindow;
			var characterWindowCount = TextWindow.CharacterWindowCount;

			var startOffset = currentOffset;

			while (true)
			{
				if (currentOffset == characterWindowCount)
				{
					// no more contiguous characters.  Fall back to slow path
					return false;
				}

				switch (characterWindow[currentOffset])
				{
					case '&':
						// CONSIDER: This method is performance critical, so
						// it might be safer to kick out at the top (as for
						// LexerMode.DebuggerSyntax).

						// If we're in a cref, this could be the start of an
						// xml entity that belongs in the identifier.
						if (InXmlCrefOrNameAttributeValue)
						{
							// Fall back on the slow path.
							return false;
						}

						// Otherwise, end the identifier.
						goto case '\0';
					case '\0':
					case ' ':
					case '\r':
					case '\n':
					case '\t':
					case '!':
					case '%':
					case '(':
					case ')':
					case '*':
					case '+':
					case ',':
					case '-':
					case '.':
					case '/':
					case ':':
					case ';':
					case '<':
					case '=':
					case '>':
					case '?':
					case '[':
					case ']':
					case '^':
					case '{':
					case '|':
					case '}':
					case '~':
					case '"':
					case '\'':
						// All of the following characters are not valid in an 
						// identifier.  If we see any of them, then we know we're
						// done.
						var length = currentOffset - startOffset;
						TextWindow.AdvanceChar(length);
						info.Text = info.StringValue = TextWindow.Intern(characterWindow, startOffset, length);
						info.IsVerbatim = false;
						return true;
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
						if (currentOffset == startOffset)
						{
							return false;
						}
						else
						{
							goto case 'A';
						}
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
						// All of these characters are valid inside an identifier.
						// consume it and keep processing.
						currentOffset++;
						continue;

					// case '@':  verbatim identifiers are handled in the slow path
					// case '\\': unicode escapes are handled in the slow path
					default:
						// Any other character is something we cannot handle.  i.e.
						// unicode chars or an escape.  Just break out and move to
						// the fast path.
						return false;
				}
			}
		}

		private bool ScanIdentifier_SlowPath(ref TokenInfo info)
		{
			int start = TextWindow.Position;
			this.ResetIdentBuffer();

			info.IsVerbatim = TextWindow.PeekChar() == '@' && TextWindow.PeekChar(1) == '"';
			var isAnnoStart = !info.IsVerbatim && TextWindow.PeekChar() == '@';
			if (info.IsVerbatim || isAnnoStart)
			{
				TextWindow.AdvanceChar();
			}

			while (true)
			{
				char surrogateCharacter = SlidingTextWindow.InvalidCharacter;
				bool isEscaped = false;
				char ch = TextWindow.PeekChar();
			top:
				switch (ch)
				{
					case '\\':
						if (!isEscaped && TextWindow.IsUnicodeEscape())
						{
							// ^^^^^^^ otherwise \u005Cu1234 looks just like \u1234! (i.e. escape within escape)
							info.HasIdentifierEscapeSequence = true;
							isEscaped = true;
							ch = TextWindow.PeekUnicodeEscape(out surrogateCharacter);
							goto top;
						}

						goto default;
					case '$':
						goto LoopExit;

					case SlidingTextWindow.InvalidCharacter:
						if (!TextWindow.IsReallyAtEnd())
						{
							goto default;
						}

						goto LoopExit;
					case '_':
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
						{
							// Again, these are the 'common' identifier characters...
							break;
						}

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
						{
							if (this._identLen == 0)
							{
								goto LoopExit;
							}

							// Again, these are the 'common' identifier characters...
							break;
						}

					case ' ':
					case '\t':
					case '.':
					case ';':
					case '(':
					case ')':
					case ',':
						// ...and these are the 'common' stop characters.
						goto LoopExit;
					case '<':
						if (this._identLen == 0 && this.ModeIs(LexerMode.DebuggerSyntax) && TextWindow.PeekChar(1) == '>')
						{
							// In DebuggerSyntax mode, identifiers are allowed to begin with <>.
							TextWindow.AdvanceChar(2);
							this.AddIdentChar('<');
							this.AddIdentChar('>');
							continue;
						}

						goto LoopExit;
					default:
						{
							// This is the 'expensive' call
							if (this._identLen == 0 && ch > 127 && SyntaxKindFacts.IsIdentifierStartCharacter(ch))
							{
								break;
							}
							else if (this._identLen > 0 && ch > 127 && SyntaxKindFacts.IsIdentifierPartCharacter(ch))
							{
								//// BUG 424819 : Handle identifier chars > 0xFFFF via surrogate pairs
								if (SyntaxKindFacts.IsFormattingChar(ch))
								{
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

									continue; // Ignore formatting characters
								}

								break;
							}
							else
							{
								// Not a valid identifier character, so bail.
								goto LoopExit;
							}
						}
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

				this.AddIdentChar(ch);
				if (surrogateCharacter != SlidingTextWindow.InvalidCharacter)
				{
					this.AddIdentChar(surrogateCharacter);
				}
			}

		LoopExit:
			var width = TextWindow.Width; // exact size of input characters
			if (this._identLen > 0)
			{
				info.Text = TextWindow.GetInternedText();

				// id buffer is identical to width in input
				if (this._identLen == width)
				{
					info.StringValue = info.Text;
				}
				else
				{
					info.StringValue = TextWindow.Intern(this._identBuffer, 0, this._identLen);
				}

				return true;
			}
			else
			{
				info.Text = null;
				info.StringValue = null;
				TextWindow.Reset(start);
				return false;
			}
		}

		/// <summary>
		/// This method is essentially the same as ScanIdentifier_SlowPath,
		/// except that it can handle XML entities.  Since ScanIdentifier
		/// is hot code and since this method does extra work, it seem
		/// worthwhile to separate it from the common case.
		/// </summary>
		/// <param name="info"></param>
		/// <returns></returns>
		private bool ScanIdentifier_CrefSlowPath(ref TokenInfo info)
		{
			Debug.Assert(InXmlCrefOrNameAttributeValue);

			int start = TextWindow.Position;
			this.ResetIdentBuffer();

			if (AdvanceIfMatches('@'))
			{
				// In xml name _annotation values, the '@' is part of the value text of the identifier
				// (to match dev11).
				if (InXmlNameAttributeValue)
				{
					AddIdentChar('@');
				}
				else
				{
					info.IsVerbatim = true;
				}
			}

			while (true)
			{
				int beforeConsumed = TextWindow.Position;
				char consumedChar;
				char consumedSurrogate;

				if (TextWindow.PeekChar() == '&')
				{
					if (!TextWindow.TryScanXmlEntity(out consumedChar, out consumedSurrogate))
					{
						// If it's not a valid entity, then it's not part of the identifier.
						TextWindow.Reset(beforeConsumed);
						goto LoopExit;
					}
				}
				else
				{
					consumedChar = TextWindow.NextChar();
					consumedSurrogate = SlidingTextWindow.InvalidCharacter;
				}

				// NOTE: If the surrogate is non-zero, then consumedChar won't match
				// any of the cases below (UTF-16 guarantees that members of surrogate
				// pairs aren't separately valid).

				bool isEscaped = false;
			top:
				switch (consumedChar)
				{
					case '\\':
						// NOTE: For completeness, we should allow xml entities in unicode escape
						// sequences (DevDiv #16321).  Since it is not currently a priority, we will
						// try to make the interim behavior sensible: we will only attempt to scan
						// a unicode escape if NONE of the characters are XML entities (including
						// the backslash, which we have already consumed).
						// When we're ready to implement this behavior, we can drop the position
						// check and use AdvanceIfMatches instead of PeekChar.
						if (!isEscaped && (TextWindow.Position == beforeConsumed + 1) &&
							(TextWindow.PeekChar() == 'u' || TextWindow.PeekChar() == 'U'))
						{
							Debug.Assert(consumedSurrogate == SlidingTextWindow.InvalidCharacter, "Since consumedChar == '\\'");

							info.HasIdentifierEscapeSequence = true;

							TextWindow.Reset(beforeConsumed);
							// ^^^^^^^ otherwise \u005Cu1234 looks just like \u1234! (i.e. escape within escape)
							isEscaped = true;
							SyntaxDiagnosticInfo error;
							consumedChar = TextWindow.NextUnicodeEscape(out consumedSurrogate, out error);
							AddCrefError(error);
							goto top;
						}

						goto default;

					case '_':
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
						{
							// Again, these are the 'common' identifier characters...
							break;
						}

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
						{
							if (this._identLen == 0)
							{
								TextWindow.Reset(beforeConsumed);
								goto LoopExit;
							}

							// Again, these are the 'common' identifier characters...
							break;
						}

					case ' ':
					case '$':
					case '\t':
					case '.':
					case ';':
					case '(':
					case ')':
					case ',':
					case '<':
						// ...and these are the 'common' stop characters.
						TextWindow.Reset(beforeConsumed);
						goto LoopExit;
					case SlidingTextWindow.InvalidCharacter:
						if (!TextWindow.IsReallyAtEnd())
						{
							goto default;
						}

						TextWindow.Reset(beforeConsumed);
						goto LoopExit;
					default:
						{
							// This is the 'expensive' call
							if (this._identLen == 0 && consumedChar > 127 && SyntaxKindFacts.IsIdentifierStartCharacter(consumedChar))
							{
								break;
							}
							else if (this._identLen > 0 && consumedChar > 127 && SyntaxKindFacts.IsIdentifierPartCharacter(consumedChar))
							{
								//// BUG 424819 : Handle identifier chars > 0xFFFF via surrogate pairs
								if (SyntaxKindFacts.IsFormattingChar(consumedChar))
								{
									continue; // Ignore formatting characters
								}

								break;
							}
							else
							{
								// Not a valid identifier character, so bail.
								TextWindow.Reset(beforeConsumed);
								goto LoopExit;
							}
						}
				}

				this.AddIdentChar(consumedChar);
				if (consumedSurrogate != SlidingTextWindow.InvalidCharacter)
				{
					this.AddIdentChar(consumedSurrogate);
				}
			}

		LoopExit:
			if (this._identLen > 0)
			{
				// NOTE: If we don't intern the string value, then we won't get a hit
				// in the keyword dictionary!  (It searches for a key using identity.)
				// The text does not have to be interned (and probalbly shouldn't be
				// if it contains entities (else-case).

				var width = TextWindow.Width; // exact size of input characters

				// id buffer is identical to width in input
				if (this._identLen == width)
				{
					info.StringValue = TextWindow.GetInternedText();
					info.Text = info.StringValue;
				}
				else
				{
					info.StringValue = TextWindow.Intern(this._identBuffer, 0, this._identLen);
					info.Text = TextWindow.GetText(intern: false);
				}

				return true;
			}
			else
			{
				info.Text = null;
				info.StringValue = null;
				TextWindow.Reset(start);
				return false;
			}
		}

		private bool ScanIdentifierOrKeyword(ref TokenInfo info)
		{
			info.ContextualKind = SyntaxKind.None;

			if (this.ScanIdentifier(ref info))
			{
				// check to see if it is an actual keyword
				if (!info.IsVerbatim && !info.HasIdentifierEscapeSequence)
				{

					if (!this._cache.TryGetKeywordKind(info.Text, out info.Kind))
					{
						info.ContextualKind = info.Kind = SyntaxKind.IdentifierToken;
					}

					if (info.Kind == SyntaxKind.None)
					{
						info.Kind = SyntaxKind.IdentifierToken;
					}
				}
				else
				{
					info.ContextualKind = info.Kind = SyntaxKind.IdentifierToken;
				}

				return true;
			}
			else
			{
				info.Kind = SyntaxKind.None;
				return false;
			}
		}
	}
}