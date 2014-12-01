// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
	internal partial class Lexer
	{
		// Maximum size of tokens/trivia that we cache and use in quick scanner.
		// From what I see in our own codebase, tokens longer then 40-50 chars are 
		// not very common. 
		// So it seems reasonable to limit the sizes to some round number like 42.
		private const int MaxCachedTokenSize = 42;

		private enum QuickScanState : byte
		{
			Initial,
			FollowingWhite,
			FollowingCR,
			Ident,
			Number,
			Punctuation,
			Dot,
			CompoundPunctStart,
			DoneAfterNext,
			Done,
			Bad
		}

		private enum CharFlags : byte
		{
			White,      // simple whitespace (space/tab)
			CR,         // carriage return
			LF,         // line feed
			Letter,     // letter
			Digit,      // digit 0-9
			Punct,      // some simple punctuation (parens, braces, comma, equals, question)
			Dot,        // dot is different from other punctuation when foillowed by a digit (Ex: .9 )
			CompoundPunctStart, // may be a part of compound punctuation. will be used only if followed by (not white) && (not punct)
			Slash,      // /
			Complex,    // complex - causes scanning to abort
			EndOfFile,  // legal type character (except !, which is contextually dictionary lookup
		}

		private Func<SyntaxToken> createQuickTokenFunction;



		#region stateTransitions

		// PERF: Use byte instead of QuickScanState so the compiler can use array literal initialization.
		//       The most natural type choice, Enum arrays, are not blittable due to a CLR limitation.
		private static readonly byte[,] stateTransitions = new byte[,]
		{
			// Initial
			{
				(byte)QuickScanState.Initial,             // White
				(byte)QuickScanState.Initial,             // CR
				(byte)QuickScanState.Initial,             // LF
				(byte)QuickScanState.Ident,               // Letter
				(byte)QuickScanState.Number,              // Digit
				(byte)QuickScanState.Punctuation,         // Punct
				(byte)QuickScanState.Dot,                 // Dot
				(byte)QuickScanState.CompoundPunctStart,  // Compound
				(byte)QuickScanState.Bad,                 // Slash
				(byte)QuickScanState.Bad,                 // Complex
				(byte)QuickScanState.Bad,                 // EndOfFile
			},

			// Following White
			{
			    (byte)QuickScanState.FollowingWhite,      // White
			    (byte)QuickScanState.FollowingCR,         // CR
			    (byte)QuickScanState.DoneAfterNext,       // LF
			    (byte)QuickScanState.Done,                // Letter
			    (byte)QuickScanState.Done,                // Digit
			    (byte)QuickScanState.Done,                // Punct
			    (byte)QuickScanState.Done,                // Dot
			    (byte)QuickScanState.Done,                // Compound
			    (byte)QuickScanState.Bad,                 // Slash
			    (byte)QuickScanState.Bad,                 // Complex
			    (byte)QuickScanState.Done,                // EndOfFile
			},
			
			// Following CR
			{
			    (byte)QuickScanState.Done,                // White
			    (byte)QuickScanState.Done,                // CR
			    (byte)QuickScanState.DoneAfterNext,       // LF
			    (byte)QuickScanState.Done,                // Letter
			    (byte)QuickScanState.Done,                // Digit
			    (byte)QuickScanState.Done,                // Punct
			    (byte)QuickScanState.Done,                // Dot
			    (byte)QuickScanState.Done,                // Compound
			    (byte)QuickScanState.Done,                // Slash
			    (byte)QuickScanState.Done,                // Complex
			    (byte)QuickScanState.Done,                // EndOfFile
			},
			
			// Identifier
			{
			    (byte)QuickScanState.FollowingWhite,      // White
			    (byte)QuickScanState.FollowingCR,         // CR
			    (byte)QuickScanState.DoneAfterNext,       // LF
			    (byte)QuickScanState.Ident,               // Letter
			    (byte)QuickScanState.Ident,               // Digit
			    (byte)QuickScanState.Done,                // Punct
			    (byte)QuickScanState.Done,                // Dot
			    (byte)QuickScanState.Done,                // Compound
			    (byte)QuickScanState.Bad,                 // Slash
			    (byte)QuickScanState.Bad,                 // Complex
			    (byte)QuickScanState.Done,                // EndOfFile
			},
			
			// Number
			{
			    (byte)QuickScanState.FollowingWhite,      // White
			    (byte)QuickScanState.FollowingCR,         // CR
			    (byte)QuickScanState.DoneAfterNext,       // LF
			    (byte)QuickScanState.Bad,                 // Letter (might be 'e' or 'x' or suffix)
			    (byte)QuickScanState.Number,              // Digit
			    (byte)QuickScanState.Done,                // Punct
			    (byte)QuickScanState.Bad,                 // Dot (Number is followed by a dot - too complex for us to handle here).
			    (byte)QuickScanState.Done,                // Compound
			    (byte)QuickScanState.Bad,                 // Slash
			    (byte)QuickScanState.Bad,                 // Complex
			    (byte)QuickScanState.Done,                // EndOfFile
			},
			
			// Punctuation
			{
			    (byte)QuickScanState.FollowingWhite,      // White
			    (byte)QuickScanState.FollowingCR,         // CR
			    (byte)QuickScanState.DoneAfterNext,       // LF
			    (byte)QuickScanState.Done,                // Letter
			    (byte)QuickScanState.Done,                // Digit
			    (byte)QuickScanState.Done,                // Punct
			    (byte)QuickScanState.Done,                // Dot
			    (byte)QuickScanState.Done,                // Compound
			    (byte)QuickScanState.Bad,                 // Slash
			    (byte)QuickScanState.Bad,                 // Complex
			    (byte)QuickScanState.Done,                // EndOfFile
			},
			
			// Dot
			{
			    (byte)QuickScanState.FollowingWhite,      // White
			    (byte)QuickScanState.FollowingCR,         // CR
			    (byte)QuickScanState.DoneAfterNext,       // LF
			    (byte)QuickScanState.Done,                // Letter
			    (byte)QuickScanState.Number,              // Digit
			    (byte)QuickScanState.Done,                // Punct
			    (byte)QuickScanState.Done,                // Dot
			    (byte)QuickScanState.Done,                // Compound
			    (byte)QuickScanState.Bad,                 // Slash
			    (byte)QuickScanState.Bad,                 // Complex
			    (byte)QuickScanState.Done,                // EndOfFile
			},
			
			// Compound Punctuation
			{
			    (byte)QuickScanState.FollowingWhite,      // White
			    (byte)QuickScanState.FollowingCR,         // CR
			    (byte)QuickScanState.DoneAfterNext,       // LF
			    (byte)QuickScanState.Done,                // Letter
			    (byte)QuickScanState.Done,                // Digit
			    (byte)QuickScanState.Bad,                 // Punct
			    (byte)QuickScanState.Done,                // Dot
			    (byte)QuickScanState.Bad,                 // Compound
			    (byte)QuickScanState.Bad,                 // Slash
			    (byte)QuickScanState.Bad,                 // Complex
			    (byte)QuickScanState.Done,                // EndOfFile
			},
			
			// Done after next
			{
				(byte)QuickScanState.Done,                // White
				(byte)QuickScanState.Done,                // CR
				(byte)QuickScanState.Done,                // LF
				(byte)QuickScanState.Done,                // Letter
				(byte)QuickScanState.Done,                // Digit
				(byte)QuickScanState.Done,                // Punct
				(byte)QuickScanState.Done,                // Dot
				(byte)QuickScanState.Done,                // Compound
				(byte)QuickScanState.Done,                // Slash
				(byte)QuickScanState.Done,                // Complex
				(byte)QuickScanState.Done,                // EndOfFile
			},
		};

		#endregion

		private SyntaxToken QuickScanSyntaxToken()
		{
			this.Start();
			var state = QuickScanState.Initial;
			int i = TextWindow.Offset;
			int n = TextWindow.CharacterWindowCount;
			n = Math.Min(n, i + MaxCachedTokenSize);

			int hashCode = Hash.FnvOffsetBias;

			//localize frequently accessed fields
			var charWindow = TextWindow.CharacterWindow;
			var charPropLength = charProperties.Length;

			for (; i < n; i++)
			{
				char c = charWindow[i];
				int uc = unchecked((int)c);

				var flags = uc < charPropLength ? (CharFlags)charProperties[uc] : CharFlags.Complex;

				state = (QuickScanState)stateTransitions[(int)state, (int)flags];
				if (state == QuickScanState.Done || state == QuickScanState.Bad)
				{
					goto exitWhile;
				}

				hashCode = unchecked((hashCode ^ uc) * Hash.FnvPrime);
			}

			state = QuickScanState.Bad; // ran out of characters in window
		exitWhile:

			TextWindow.AdvanceChar(i - TextWindow.Offset);
			Debug.Assert(state == QuickScanState.Bad || state == QuickScanState.Done);

			if (state == QuickScanState.Done)
			{
				// this is a good token!
				var token = this._cache.LookupToken(
					TextWindow.CharacterWindow,
					TextWindow.LexemeRelativeStart,
					i - TextWindow.LexemeRelativeStart,
					hashCode,
					createQuickTokenFunction);
				return token;
			}
			else
			{
				TextWindow.Reset(TextWindow.LexemeStartPosition);
				return null;
			}
		}


		private SyntaxToken CreateQuickToken()
		{
#if DEBUG
			var quickWidth = TextWindow.Width;
#endif
			TextWindow.Reset(TextWindow.LexemeStartPosition);
			var token = this.LexSyntaxToken();
#if DEBUG
			Debug.Assert(quickWidth == token.FullWidth);
#endif
			return token;
		}


		#region charProperties
		// The following table classifies the first 0x180 (384)Unicode characters. 
		// # is marked complex as it may start directives.
		// PERF: Use byte instead of CharFlags so the compiler can use array literal initialization.
		//       The most natural type choice, Enum arrays, are not blittable due to a CLR limitation.
		private static readonly byte[] charProperties = new[]
		{
			// 0 .. 127 
			// U+0000 to U+007F: Basic Latin
			// http://inamidst.com/stuff/unidata/
			// 基本拉丁字母
			// C0 controls
			(byte)CharFlags.Complex, // Null character	NUL
			(byte)CharFlags.Complex, // Start of Heading	SOH
			(byte)CharFlags.Complex, // Start of Text	STX
			(byte)CharFlags.Complex, // End-of-text character	ETX
			(byte)CharFlags.Complex, // End-of-transmission character	EOT
			(byte)CharFlags.Complex, // Enquiry character	ENQ
			(byte)CharFlags.Complex, // Acknowledge character	ACK
			(byte)CharFlags.Complex, // Bell character	BEL
			(byte)CharFlags.Complex, // Backspace	BS
			(byte)CharFlags.White,   // Horizontal tab	HT
			(byte)CharFlags.LF,      // Line feed	LF
			(byte)CharFlags.White,   // Vertical tab	VT
			(byte)CharFlags.White,   // Form feed	FF
			(byte)CharFlags.CR,      // Carriage return	CR
			(byte)CharFlags.Complex, // Shift Out	SO
			(byte)CharFlags.Complex, // Shift In	SI
			(byte)CharFlags.Complex, // Data Link Escape	DLE
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex,
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex,
			(byte)CharFlags.Complex,
			(byte)CharFlags.Complex,
			(byte)CharFlags.Complex,
			(byte)CharFlags.Complex,
			(byte)CharFlags.Complex,
			// ASCII Punctuation and Symbols
			(byte)CharFlags.White,                 // SPC
			(byte)CharFlags.CompoundPunctStart,    // !
			(byte)CharFlags.Complex,               // "
			(byte)CharFlags.Complex,               // #
			(byte)CharFlags.Complex,               // $
			(byte)CharFlags.CompoundPunctStart,    // %
			(byte)CharFlags.CompoundPunctStart,    // &
			(byte)CharFlags.Complex,               // '
			(byte)CharFlags.Punct,                 // (
			(byte)CharFlags.Punct,                 // )
			(byte)CharFlags.CompoundPunctStart,    // *
			(byte)CharFlags.CompoundPunctStart,    // +
			(byte)CharFlags.Punct,                 // ,
			(byte)CharFlags.CompoundPunctStart,    // -
			(byte)CharFlags.Dot,                   // .
			(byte)CharFlags.Slash,                 // /
			(byte)CharFlags.Digit,                 // 0
			(byte)CharFlags.Digit,                 // 1
			(byte)CharFlags.Digit,                 // 2
			(byte)CharFlags.Digit,                 // 3
			(byte)CharFlags.Digit,                 // 4
			(byte)CharFlags.Digit,                 // 5
			(byte)CharFlags.Digit,                 // 6
			(byte)CharFlags.Digit,                 // 7
			(byte)CharFlags.Digit,                 // 8
			(byte)CharFlags.Digit,                 // 9
			(byte)CharFlags.CompoundPunctStart,    // :
			(byte)CharFlags.Punct,                 // ;
			(byte)CharFlags.CompoundPunctStart,    // <
			(byte)CharFlags.CompoundPunctStart,    // =
			(byte)CharFlags.CompoundPunctStart,    // >
			(byte)CharFlags.CompoundPunctStart,    // ?
			(byte)CharFlags.Complex,               // @
			(byte)CharFlags.Letter,                // A
			(byte)CharFlags.Letter,                // B
			(byte)CharFlags.Letter,                // C
			(byte)CharFlags.Letter,                // D
			(byte)CharFlags.Letter,                // E
			(byte)CharFlags.Letter,                // F
			(byte)CharFlags.Letter,                // G
			(byte)CharFlags.Letter,                // H
			(byte)CharFlags.Letter,                // I
			(byte)CharFlags.Letter,                // J
			(byte)CharFlags.Letter,                // K
			(byte)CharFlags.Letter,                // L
			(byte)CharFlags.Letter,                // M
			(byte)CharFlags.Letter,                // N
			(byte)CharFlags.Letter,                // O
			(byte)CharFlags.Letter,                // P
			(byte)CharFlags.Letter,                // Q
			(byte)CharFlags.Letter,                // R
			(byte)CharFlags.Letter,                // S
			(byte)CharFlags.Letter,                // T
			(byte)CharFlags.Letter,                // U
			(byte)CharFlags.Letter,                // V
			(byte)CharFlags.Letter,                // W
			(byte)CharFlags.Letter,                // X
			(byte)CharFlags.Letter,                // Y
			(byte)CharFlags.Letter,                // Z
			(byte)CharFlags.Punct,                 // [
			(byte)CharFlags.Complex,               // \
			(byte)CharFlags.Punct,                 // ]
			(byte)CharFlags.CompoundPunctStart,    // ^
			(byte)CharFlags.Letter,                // _
			(byte)CharFlags.Complex,               // `
			(byte)CharFlags.Letter,                // a
			(byte)CharFlags.Letter,                // b
			(byte)CharFlags.Letter,                // c
			(byte)CharFlags.Letter,                // d
			(byte)CharFlags.Letter,                // e
			(byte)CharFlags.Letter,                // f
			(byte)CharFlags.Letter,                // g
			(byte)CharFlags.Letter,                // h
			(byte)CharFlags.Letter,                // i
			(byte)CharFlags.Letter,                // j
			(byte)CharFlags.Letter,                // k
			(byte)CharFlags.Letter,                // l
			(byte)CharFlags.Letter,                // m
			(byte)CharFlags.Letter,                // n
			(byte)CharFlags.Letter,                // o
			(byte)CharFlags.Letter,                // p
			(byte)CharFlags.Letter,                // q
			(byte)CharFlags.Letter,                // r
			(byte)CharFlags.Letter,                // s
			(byte)CharFlags.Letter,                // t
			(byte)CharFlags.Letter,                // u
			(byte)CharFlags.Letter,                // v
			(byte)CharFlags.Letter,                // w
			(byte)CharFlags.Letter,                // x
			(byte)CharFlags.Letter,                // y
			(byte)CharFlags.Letter,                // z
			(byte)CharFlags.Punct,                 // {
			(byte)CharFlags.CompoundPunctStart,    // |
			(byte)CharFlags.Punct,                 // }
			(byte)CharFlags.CompoundPunctStart,    // ~
			(byte)CharFlags.Complex,               // Delete	DEL
			
			// 128 .. 255
			// U+0080 to U+00FF: Latin-1 Supplement
			// http://inamidst.com/stuff/unidata/
			// 拉丁字母補充-1
			(byte)CharFlags.Complex, // €
			(byte)CharFlags.Complex, // 
			(byte)CharFlags.Complex, // ‚
			(byte)CharFlags.Complex, // ƒ
			(byte)CharFlags.Complex, // „
			(byte)CharFlags.Complex, // …
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex,
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex,
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex,
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex,
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex,
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex,
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex,
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex, 
			(byte)CharFlags.Complex,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Complex,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Complex,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,

			// U+0100 to U+017F: Latin Extended-A
			// http://inamidst.com/stuff/unidata/
			// http://www.fontspace.com/unicode/block/Latin%20Extended-A
			// 拉丁字母擴充-A
			(byte)CharFlags.Letter, // Ā
			(byte)CharFlags.Letter, // ā
			(byte)CharFlags.Letter, // Ă
			(byte)CharFlags.Letter, // ă
			(byte)CharFlags.Letter, // Ą
			(byte)CharFlags.Letter, // ą
			(byte)CharFlags.Letter, // Ć
			(byte)CharFlags.Letter, // ć
			(byte)CharFlags.Letter, // Ĉ
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter,
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter, 
			(byte)CharFlags.Letter
		};
		#endregion
	}
}