// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;

namespace Microsoft.CodeAnalysis.CSharp
{
	/// <summary>
	/// Defines a set of methods to determine how Unicode characters are treated by the C# compiler.
	/// </summary>
	public static partial class SyntaxKindFacts
	{
		/// <summary>
		/// Returns true if the Unicode character is a hexadecimal digit.
		/// </summary>
		/// <param name="c">The Unicode character.</param>
		/// <returns>true if the character is a hexadecimal digit 0-9, A-F, a-f.</returns>
		internal static bool IsHexDigit(char c)
		{
			return (c >= '0' && c <= '9') ||
				   (c >= 'A' && c <= 'F') ||
				   (c >= 'a' && c <= 'f');
		}

		/// <summary>
		/// Returns true if the Unicode character is a decimal digit.
		/// </summary>
		/// <param name="c">The Unicode character.</param>
		/// <returns>true if the Unicode character is a decimal digit.</returns>
		internal static bool IsDecDigit(char c)
		{
			return c >= '0' && c <= '9';
		}

		/// <summary>
		/// Returns the value of a hexadecimal Unicode character.
		/// </summary>
		/// <param name="c">The Unicode character.</param>
		internal static int HexValue(char c)
		{
			Debug.Assert(IsHexDigit(c));
			return (c >= '0' && c <= '9') ? c - '0' : (c & 0xdf) - 'A' + 10;
		}

		/// <summary>
		/// Returns the value of a decimal Unicode character.
		/// </summary>
		/// <param name="c">The Unicode character.</param>
		internal static int DecValue(char c)
		{
			Debug.Assert(IsDecDigit(c));
			return c - '0';
		}

		// UnicodeCategory value | Unicode designation
		// -----------------------+-----------------------
		// UppercaseLetter         "Lu" (letter, uppercase)
		// LowercaseLetter         "Ll" (letter, lowercase)
		// TitlecaseLetter         "Lt" (letter, titlecase)
		// ModifierLetter          "Lm" (letter, modifier)
		// OtherLetter             "Lo" (letter, other)
		// NonSpacingMark          "Mn" (mark, nonspacing)
		// SpacingCombiningMark    "Mc" (mark, spacing combining)
		// EnclosingMark           "Me" (mark, enclosing)
		// DecimalDigitNumber      "Nd" (number, decimal digit)
		// LetterNumber            "Nl" (number, letter)
		// OtherNumber             "No" (number, other)
		// SpaceSeparator          "Zs" (separator, space)
		// LineSeparator           "Zl" (separator, line)
		// ParagraphSeparator      "Zp" (separator, paragraph)
		// Control                 "Cc" (other, control)
		// Format                  "Cf" (other, format)
		// Surrogate               "Cs" (other, surrogate)
		// PrivateUse              "Co" (other, private use)
		// ConnectorPunctuation    "Pc" (punctuation, connector)
		// DashPunctuation         "Pd" (punctuation, dash)
		// OpenPunctuation         "Ps" (punctuation, open)
		// ClosePunctuation        "Pe" (punctuation, close)
		// InitialQuotePunctuation "Pi" (punctuation, initial quote)
		// FinalQuotePunctuation   "Pf" (punctuation, final quote)
		// OtherPunctuation        "Po" (punctuation, other)
		// MathSymbol              "Sm" (symbol, math)
		// CurrencySymbol          "Sc" (symbol, currency)
		// ModifierSymbol          "Sk" (symbol, modifier)
		// OtherSymbol             "So" (symbol, other)
		// OtherNotAssigned        "Cn" (other, not assigned)

		/// <summary>
		/// Returns true if the Unicode character represents a whitespace.
		/// </summary>
		/// <param name="ch">The Unicode character.</param>
		public static bool IsWhitespace(char ch)
		{
			// whitespace:
			//   Any character with Unicode class Zs
			//   Horizontal tab character (U+0009)
			//   Vertical tab character (U+000B)
			//   Form feed character (U+000C)

			// Space and no-break space are the only space separators (Zs) in ASCII range

			return ch == ' '
				|| ch == '\t'
				|| ch == '\v'
				|| ch == '\f'
				|| ch == '\u00A0' // NO-BREAK SPACE
				// The native compiler, in ScanToken, recognized both the byte-order
				// marker '\uFEFF' as well as ^Z '\u001A' as whitespace, although
				// this is not to spec since neither of these are in Zs. For the
				// sake of compatibility, we recognize them both here. Note: '\uFEFF'
				// also happens to be a formatting character (class Cf), which means
				// that it is a legal non-initial identifier character. So it's
				// especially funny, because it will be whitespace UNLESS we happen
				// to be scanning an identifier or keyword, in which case it winds
				// up in the identifier or keyword.
				|| ch == '\uFEFF'
				|| ch == '\u001A'
				|| (ch > 255 && CharUnicodeInfo.GetUnicodeCategory(ch) == UnicodeCategory.SpaceSeparator);
		}

		/// <summary>
		/// Returns true if the Unicode character is a newline character.
		/// </summary>
		/// <param name="ch">The Unicode character.</param>
		public static bool IsNewLine(char ch)
		{
			// new-line-character:
			//   Carriage return character (U+000D)
			//   Line feed character (U+000A)
			//   Next line character (U+0085)
			//   Line separator character (U+2028)
			//   Paragraph separator character (U+2029)

			return ch == '\r'
				|| ch == '\n'
				|| ch == '\u0085'
				|| ch == '\u2028'
				|| ch == '\u2029';
		}

		/// <summary>
		/// Returns true if the Unicode character can be the starting character of a C# identifier.
		/// </summary>
		/// <param name="ch">The Unicode character.</param>
		public static bool IsIdentifierStartCharacter(char ch)
		{
			// identifier-start-character:
			//   letter-character
			//   _ (the underscore character U+005F)

			return (ch >= 'a' && ch <= 'z')
				|| (ch >= 'A' && ch <= 'Z')
				|| (ch == '_')
				|| IsLetterChar(CharUnicodeInfo.GetUnicodeCategory(ch));
		}

		/// <summary>
		/// Returns true if the Unicode character can be a part of a C# identifier.
		/// </summary>
		/// <param name="ch">The Unicode character.</param>
		public static bool IsIdentifierPartCharacter(char ch)
		{
			// identifier-part-character:
			//   letter-character
			//   decimal-digit-character
			//   connecting-character
			//   combining-character
			//   formatting-character

			UnicodeCategory cat;

			return (ch >= 'a' && ch <= 'z')
				|| (ch >= 'A' && ch <= 'Z')
				|| (ch >= '0' && ch <= '9')
				|| (ch == '_')
				|| IsLetterChar(cat = CharUnicodeInfo.GetUnicodeCategory(ch))
				|| IsDecimalDigitChar(cat)
				|| IsConnectingChar(cat)
				|| IsCombiningChar(cat)
				|| IsFormattingChar(cat);
		}

		/// <summary>
		/// Check that the name is a valid identifier.
		/// </summary>
		public static bool IsValidIdentifier(string name)
		{
			if (string.IsNullOrEmpty(name))
			{
				return false;
			}

			if (!SyntaxKindFacts.IsIdentifierStartCharacter(name[0]))
			{
				return false;
			}

			int nameLength = name.Length;
			for (int i = 1; i < nameLength; i++) //NB: start at 1
			{
				if (!SyntaxKindFacts.IsIdentifierPartCharacter(name[i]))
				{
					return false;
				}
			}

			return true;
		}

		/// <summary>
		/// Spec section 2.4.2 says that identifiers are compared without regard
		/// to leading "@" characters or unicode formatting characters.  As in dev10,
		/// this is actually accomplished by dropping such characters during parsing.
		/// Unfortunately, metadata names can still contain these characters and will
		/// not be referenceable from source if they do (lookup will fail since the
		/// characters will have been dropped from the search string).
		/// See DevDiv #14432 for more.
		/// </summary>
		internal static bool ContainsDroppedIdentifierCharacters(string name)
		{
			if (string.IsNullOrEmpty(name))
			{
				return false;
			}
			if (name[0] == '@')
			{
				return true;
			}

			int nameLength = name.Length;
			for (int i = 0; i < nameLength; i++)
			{
				if (SyntaxKindFacts.IsFormattingChar(name[i]))
				{
					return true;
				}
			}

			return false;
		}

		internal static bool IsLetterChar(UnicodeCategory cat)
		{
			// letter-character:
			//   A Unicode character of classes Lu, Ll, Lt, Lm, Lo, or Nl 
			//   A Unicode-escape-sequence representing a character of classes Lu, Ll, Lt, Lm, Lo, or Nl

			switch (cat)
			{
				case UnicodeCategory.UppercaseLetter:
				case UnicodeCategory.LowercaseLetter:
				case UnicodeCategory.TitlecaseLetter:
				case UnicodeCategory.ModifierLetter:
				case UnicodeCategory.OtherLetter:
				case UnicodeCategory.LetterNumber:
					return true;
			}

			return false;
		}

		internal static bool IsCombiningChar(UnicodeCategory cat)
		{
			// combining-character:
			//   A Unicode character of classes Mn or Mc 
			//   A Unicode-escape-sequence representing a character of classes Mn or Mc

			switch (cat)
			{
				case UnicodeCategory.NonSpacingMark:
				case UnicodeCategory.SpacingCombiningMark:
					return true;
			}

			return false;
		}

		internal static bool IsDecimalDigitChar(UnicodeCategory cat)
		{
			// decimal-digit-character:
			//   A Unicode character of the class Nd 
			//   A unicode-escape-sequence representing a character of the class Nd

			return cat == UnicodeCategory.DecimalDigitNumber;
		}

		internal static bool IsConnectingChar(UnicodeCategory cat)
		{
			// connecting-character:  
			//   A Unicode character of the class Pc
			//   A unicode-escape-sequence representing a character of the class Pc

			return cat == UnicodeCategory.ConnectorPunctuation;
		}

		/// <summary>
		/// Returns true if the Unicode character is a formatting character (Unicode class Cf).
		/// </summary>
		/// <param name="ch">The Unicode character.</param>
		internal static bool IsFormattingChar(char ch)
		{
			// There are no FormattingChars in ASCII range

			return ch > 255 && IsFormattingChar(CharUnicodeInfo.GetUnicodeCategory(ch));
		}

		/// <summary>
		/// Returns true if the Unicode character is a formatting character (Unicode class Cf).
		/// </summary>
		/// <param name="cat">The Unicode character.</param>
		internal static bool IsFormattingChar(UnicodeCategory cat)
		{
			// formatting-character:  
			//   A Unicode character of the class Cf
			//   A unicode-escape-sequence representing a character of the class Cf

			return cat == UnicodeCategory.Format;
		}

		internal static bool IsNonAsciiQuotationMark(char ch)
		{
			// CONSIDER: There are others:
			// http://en.wikipedia.org/wiki/Quotation_mark_glyphs#Quotation_marks_in_Unicode
			switch (ch)
			{
				case '\u2018': //LEFT SINGLE QUOTATION MARK
				case '\u2019': //RIGHT SINGLE QUOTATION MARK
					return true;
				case '\u201C': //LEFT DOUBLE QUOTATION MARK
				case '\u201D': //RIGHT DOUBLE QUOTATION MARK
					return true;
				default:
					return false;
			}
		}
	}



	public static partial class SyntaxKindFacts
	{
		public static bool IsKeywordKind(SyntaxKind kind)
		{
			return IsReservedKeyword(kind) || IsContextualKeyword(kind);
		}

		public static IEnumerable<SyntaxKind> GetReservedKeywordKinds()
		{
			for (int i = (int)SyntaxKind.BooleanKeyword; i <= (int)SyntaxKind.TransientKeyword; i++)
			{
				yield return (SyntaxKind)i;
			}
		}

		public static IEnumerable<SyntaxKind> GetKeywordKinds()
		{
			foreach (var reserved in GetReservedKeywordKinds())
			{
				yield return reserved;
			}

			foreach (var contextual in GetContextualKeywordKinds())
			{
				yield return contextual;
			}
		}

		public static bool IsReservedKeyword(SyntaxKind kind)
		{
			return kind >= SyntaxKind.BooleanKeyword && kind <= SyntaxKind.TransientKeyword;
		}



		public static bool IsAccessibilityModifier(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.PrivateKeyword:
				case SyntaxKind.ProtectedKeyword:
				case SyntaxKind.PublicKeyword:
					return true;
				default:
					return false;
			}
		}




		public static bool IsPunctuation(SyntaxKind kind)
		{
			return kind >= SyntaxKind.TildeToken && kind <= SyntaxKind.PercentEqualsToken;
		}

		public static bool IsLanguagePunctuation(SyntaxKind kind)
		{
			return IsPunctuation(kind);
		}

		public static bool IsPreprocessorPunctuation(SyntaxKind kind)
		{
			return kind == SyntaxKind.HashToken;
		}

		public static IEnumerable<SyntaxKind> GetPunctuationKinds()
		{
			for (int i = (int)SyntaxKind.TildeToken; i <= (int)SyntaxKind.PercentEqualsToken; i++)
			{
				yield return (SyntaxKind)i;
			}
		}

		public static bool IsPunctuationOrKeyword(SyntaxKind kind)
		{
			return kind >= SyntaxKind.TildeToken && kind <= SyntaxKind.EndOfFileToken;
		}

		internal static bool IsLiteral(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.IdentifierToken:
				case SyntaxKind.StringLiteralToken:
				case SyntaxKind.CharacterLiteralToken:
				case SyntaxKind.NumericLiteralToken:
				case SyntaxKind.XmlTextLiteralToken:
				case SyntaxKind.XmlTextLiteralNewLineToken:
				case SyntaxKind.XmlEntityLiteralToken:
					//case SyntaxKind.Unknown:
					return true;
				default:
					return false;
			}
		}

		public static bool IsAnyToken(SyntaxKind kind)
		{
			return kind >= SyntaxKind.TildeToken && kind < SyntaxKind.EndOfLineTrivia;
		}

		public static bool IsTrivia(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.EndOfLineTrivia:
				case SyntaxKind.WhitespaceTrivia:
				case SyntaxKind.SingleLineCommentTrivia:
				case SyntaxKind.MultiLineCommentTrivia:
				case SyntaxKind.SingleLineDocumentationCommentTrivia:
				case SyntaxKind.MultiLineDocumentationCommentTrivia:
				case SyntaxKind.DocumentationCommentExteriorTrivia:
					return true;
				default:
					return false;
			}
		}

		public static bool IsName(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.IdentifierName:
				case SyntaxKind.GenericName:
				case SyntaxKind.QualifiedName:
					return true;
				default:
					return false;
			}
		}

		public static bool IsPredefinedType(SyntaxKind kind)
		{
			switch (kind)
			{
				
				case SyntaxKind.ByteKeyword:
				case SyntaxKind.ShortKeyword:
				case SyntaxKind.IntKeyword:
				case SyntaxKind.LongKeyword:
				case SyntaxKind.CharKeyword:
				case SyntaxKind.FloatKeyword:
				case SyntaxKind.DoubleKeyword:
				case SyntaxKind.BooleanKeyword:
				case SyntaxKind.VoidKeyword:
					return true;
				default:
					return false;
			}
		}

		public static bool IsTypeSyntax(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.ArrayType:
				case SyntaxKind.PointerType:
				case SyntaxKind.NullableType:
				case SyntaxKind.PredefinedType:
					return true;
				default:
					return IsName(kind);
			}
		}

		public static bool IsTypeDeclaration(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.EnumDeclaration:
				case SyntaxKind.ClassDeclaration:
				case SyntaxKind.InterfaceDeclaration:
					return true;

				default:
					return false;
			}
		}

		/// <summary>
		/// Member declarations that can appear in global code (other than type declarations).
		/// </summary>
		public static bool IsGlobalMemberDeclaration(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.FieldDeclaration:
				case SyntaxKind.MethodDeclaration:
					return true;
			}
			return false;
		}

		public static bool IsNamespaceMemberDeclaration(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.ClassDeclaration:
				case SyntaxKind.InterfaceDeclaration:
				case SyntaxKind.EnumDeclaration:
				case SyntaxKind.AnnotationDeclaration:
					return true;
				default:
					return false;
			}
		}

		public static bool IsAnyUnaryExpression(SyntaxKind token)
		{
			return IsPrefixUnaryExpression(token) || IsPostfixUnaryExpression(token);
		}

		public static bool IsPrefixUnaryExpression(SyntaxKind token)
		{
			return GetPrefixUnaryExpression(token) != SyntaxKind.None;
		}

		public static bool IsPrefixUnaryExpressionOperatorToken(SyntaxKind token)
		{
			return GetPrefixUnaryExpression(token) != SyntaxKind.None;
		}

		public static SyntaxKind GetPrefixUnaryExpression(SyntaxKind token)
		{
			switch (token)
			{
				case SyntaxKind.PlusToken:
					return SyntaxKind.UnaryPlusExpression;
				case SyntaxKind.MinusToken:
					return SyntaxKind.UnaryMinusExpression;
				case SyntaxKind.TildeToken:
					return SyntaxKind.BitwiseNotExpression;
				case SyntaxKind.ExclamationToken:
					return SyntaxKind.LogicalNotExpression;
				case SyntaxKind.PlusPlusToken:
					return SyntaxKind.PreIncrementExpression;
				case SyntaxKind.MinusMinusToken:
					return SyntaxKind.PreDecrementExpression;
				case SyntaxKind.AmpersandToken:
					return SyntaxKind.AddressOfExpression;
				case SyntaxKind.AsteriskToken:
					return SyntaxKind.PointerIndirectionExpression;
				default:
					return SyntaxKind.None;
			}
		}

		public static bool IsPostfixUnaryExpression(SyntaxKind token)
		{
			return GetPostfixUnaryExpression(token) != SyntaxKind.None;
		}

		public static bool IsPostfixUnaryExpressionToken(SyntaxKind token)
		{
			return GetPostfixUnaryExpression(token) != SyntaxKind.None;
		}

		public static SyntaxKind GetPostfixUnaryExpression(SyntaxKind token)
		{
			switch (token)
			{
				case SyntaxKind.PlusPlusToken:
					return SyntaxKind.PostIncrementExpression;
				case SyntaxKind.MinusMinusToken:
					return SyntaxKind.PostDecrementExpression;
				default:
					return SyntaxKind.None;
			}
		}

		public static bool IsUnaryOperatorDeclarationToken(SyntaxKind token)
		{
			return IsPrefixUnaryExpressionOperatorToken(token) ||
				   token == SyntaxKind.TrueKeyword ||
				   token == SyntaxKind.FalseKeyword;
		}

		public static bool IsAnyOverloadableOperator(SyntaxKind kind)
		{
			return IsOverloadableBinaryOperator(kind) || IsOverloadableUnaryOperator(kind);
		}

		public static bool IsOverloadableBinaryOperator(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.PlusToken:
				case SyntaxKind.MinusToken:
				case SyntaxKind.AsteriskToken:
				case SyntaxKind.SlashToken:
				case SyntaxKind.PercentToken:
				case SyntaxKind.CaretToken:
				case SyntaxKind.AmpersandToken:
				case SyntaxKind.BarToken:
				case SyntaxKind.EqualsEqualsToken:
				case SyntaxKind.LessThanToken:
				case SyntaxKind.LessThanEqualsToken:
				case SyntaxKind.LessThanLessThanToken:
				case SyntaxKind.GreaterThanToken:
				case SyntaxKind.GreaterThanEqualsToken:
				case SyntaxKind.GreaterThanGreaterThanToken:
				case SyntaxKind.ExclamationEqualsToken:
					return true;
				default:
					return false;
			}
		}

		public static bool IsOverloadableUnaryOperator(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.PlusToken:
				case SyntaxKind.MinusToken:
				case SyntaxKind.TildeToken:
				case SyntaxKind.ExclamationToken:
				case SyntaxKind.PlusPlusToken:
				case SyntaxKind.MinusMinusToken:
				case SyntaxKind.TrueKeyword:
				case SyntaxKind.FalseKeyword:
					return true;
				default:
					return false;
			}
		}

		public static bool IsPrimaryFunction(SyntaxKind keyword)
		{
			return GetPrimaryFunction(keyword) != SyntaxKind.None;
		}

		public static SyntaxKind GetPrimaryFunction(SyntaxKind keyword)
		{
			switch (keyword)
			{
				case SyntaxKind.DefaultKeyword:
					return SyntaxKind.DefaultExpression;
				default:
					return SyntaxKind.None;
			}
		}

		public static bool IsLiteralExpression(SyntaxKind token)
		{
			return GetLiteralExpression(token) != SyntaxKind.None;
		}

		public static SyntaxKind GetLiteralExpression(SyntaxKind token)
		{
			switch (token)
			{
				case SyntaxKind.StringLiteralToken:
					return SyntaxKind.StringLiteralExpression;
				case SyntaxKind.CharacterLiteralToken:
					return SyntaxKind.CharacterLiteralExpression;
				case SyntaxKind.NumericLiteralToken:
					return SyntaxKind.NumericLiteralExpression;
				case SyntaxKind.NullKeyword:
					return SyntaxKind.NullLiteralExpression;
				case SyntaxKind.TrueKeyword:
					return SyntaxKind.TrueLiteralExpression;
				case SyntaxKind.FalseKeyword:
					return SyntaxKind.FalseLiteralExpression;
				case SyntaxKind.ArgListKeyword:
					return SyntaxKind.ArgListExpression;
				default:
					return SyntaxKind.None;
			}
		}

		public static bool IsInstanceExpression(SyntaxKind token)
		{
			return GetInstanceExpression(token) != SyntaxKind.None;
		}

		public static SyntaxKind GetInstanceExpression(SyntaxKind token)
		{
			switch (token)
			{
				case SyntaxKind.ThisKeyword:
					return SyntaxKind.ThisExpression;
				case SyntaxKind.SuperKeyword:
					return SyntaxKind.BaseExpression;
				default:
					return SyntaxKind.None;
			}
		}

		public static bool IsBinaryExpression(SyntaxKind token)
		{
			return GetBinaryExpression(token) != SyntaxKind.None;
		}

		public static bool IsBinaryExpressionOperatorToken(SyntaxKind token)
		{
			return GetBinaryExpression(token) != SyntaxKind.None;
		}

		public static SyntaxKind GetBinaryExpression(SyntaxKind token)
		{
			switch (token)
			{
				case SyntaxKind.QuestionQuestionToken:
					return SyntaxKind.CoalesceExpression;
				case SyntaxKind.InstanceOfKeyword:
					return SyntaxKind.IsExpression;
				case SyntaxKind.AsKeyword:
					return SyntaxKind.AsExpression;
				case SyntaxKind.BarToken:
					return SyntaxKind.BitwiseOrExpression;
				case SyntaxKind.CaretToken:
					return SyntaxKind.ExclusiveOrExpression;
				case SyntaxKind.AmpersandToken:
					return SyntaxKind.BitwiseAndExpression;
				case SyntaxKind.EqualsEqualsToken:
					return SyntaxKind.EqualsExpression;
				case SyntaxKind.ExclamationEqualsToken:
					return SyntaxKind.NotEqualsExpression;
				case SyntaxKind.LessThanToken:
					return SyntaxKind.LessThanExpression;
				case SyntaxKind.LessThanEqualsToken:
					return SyntaxKind.LessThanOrEqualExpression;
				case SyntaxKind.GreaterThanToken:
					return SyntaxKind.GreaterThanExpression;
				case SyntaxKind.GreaterThanEqualsToken:
					return SyntaxKind.GreaterThanOrEqualExpression;
				case SyntaxKind.LessThanLessThanToken:
					return SyntaxKind.LeftShiftExpression;
				case SyntaxKind.GreaterThanGreaterThanToken:
					return SyntaxKind.RightShiftExpression;
				case SyntaxKind.PlusToken:
					return SyntaxKind.AddExpression;
				case SyntaxKind.MinusToken:
					return SyntaxKind.SubtractExpression;
				case SyntaxKind.AsteriskToken:
					return SyntaxKind.MultiplyExpression;
				case SyntaxKind.SlashToken:
					return SyntaxKind.DivideExpression;
				case SyntaxKind.PercentToken:
					return SyntaxKind.ModuloExpression;
				case SyntaxKind.AmpersandAmpersandToken:
					return SyntaxKind.LogicalAndExpression;
				case SyntaxKind.BarBarToken:
					return SyntaxKind.LogicalOrExpression;
				case SyntaxKind.BarEqualsToken:
					return SyntaxKind.OrAssignmentExpression;
				case SyntaxKind.AmpersandEqualsToken:
					return SyntaxKind.AndAssignmentExpression;
				case SyntaxKind.CaretEqualsToken:
					return SyntaxKind.ExclusiveOrAssignmentExpression;
				case SyntaxKind.LessThanLessThanEqualsToken:
					return SyntaxKind.LeftShiftAssignmentExpression;
				case SyntaxKind.GreaterThanGreaterThanEqualsToken:
					return SyntaxKind.RightShiftAssignmentExpression;
				case SyntaxKind.PlusEqualsToken:
					return SyntaxKind.AddAssignmentExpression;
				case SyntaxKind.MinusEqualsToken:
					return SyntaxKind.SubtractAssignmentExpression;
				case SyntaxKind.AsteriskEqualsToken:
					return SyntaxKind.MultiplyAssignmentExpression;
				case SyntaxKind.SlashEqualsToken:
					return SyntaxKind.DivideAssignmentExpression;
				case SyntaxKind.PercentEqualsToken:
					return SyntaxKind.ModuloAssignmentExpression;
				case SyntaxKind.EqualsToken:
					return SyntaxKind.SimpleAssignmentExpression;
				default:
					return SyntaxKind.None;
			}
		}

		public static bool IsAssignmentExpression(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.OrAssignmentExpression:
				case SyntaxKind.AndAssignmentExpression:
				case SyntaxKind.ExclusiveOrAssignmentExpression:
				case SyntaxKind.LeftShiftAssignmentExpression:
				case SyntaxKind.RightShiftAssignmentExpression:
				case SyntaxKind.AddAssignmentExpression:
				case SyntaxKind.SubtractAssignmentExpression:
				case SyntaxKind.MultiplyAssignmentExpression:
				case SyntaxKind.DivideAssignmentExpression:
				case SyntaxKind.ModuloAssignmentExpression:
				case SyntaxKind.SimpleAssignmentExpression:
					return true;
				default:
					return false;
			}
		}

		public static bool IsAssignmentExpressionOperatorToken(SyntaxKind token)
		{
			switch (token)
			{
				case SyntaxKind.BarEqualsToken:
				case SyntaxKind.AmpersandEqualsToken:
				case SyntaxKind.CaretEqualsToken:
				case SyntaxKind.LessThanLessThanEqualsToken:
				case SyntaxKind.GreaterThanGreaterThanEqualsToken:
				case SyntaxKind.PlusEqualsToken:
				case SyntaxKind.MinusEqualsToken:
				case SyntaxKind.AsteriskEqualsToken:
				case SyntaxKind.SlashEqualsToken:
				case SyntaxKind.PercentEqualsToken:
				case SyntaxKind.EqualsToken:
					return true;
				default:
					return false;
			}
		}

		public static SyntaxKind GetCheckStatement(SyntaxKind keyword)
		{
			switch (keyword)
			{
				default:
					return SyntaxKind.None;
			}
		}


		public static SyntaxKind GetSwitchLabelKind(SyntaxKind keyword)
		{
			switch (keyword)
			{
				case SyntaxKind.CaseKeyword:
					return SyntaxKind.CaseSwitchLabel;
				case SyntaxKind.DefaultKeyword:
					return SyntaxKind.DefaultSwitchLabel;
				default:
					return SyntaxKind.None;
			}
		}

		public static SyntaxKind GetBaseTypeDeclarationKind(SyntaxKind kind)
		{
			return kind == SyntaxKind.EnumKeyword ? SyntaxKind.EnumDeclaration : GetTypeDeclarationKind(kind);
		}

		public static SyntaxKind GetTypeDeclarationKind(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.ClassKeyword:
					return SyntaxKind.ClassDeclaration;
				case SyntaxKind.InterfaceKeyword:
					return SyntaxKind.InterfaceDeclaration;
				default:
					return SyntaxKind.None;
			}
		}

		public static SyntaxKind GetKeywordKind(string text)
		{
			switch (text)
			{
				case "boolean":
					return SyntaxKind.BooleanKeyword;
				case "byte":
					return SyntaxKind.ByteKeyword;
				case "short":
					return SyntaxKind.ShortKeyword;
				case "int":
					return SyntaxKind.IntKeyword;
				case "long":
					return SyntaxKind.LongKeyword;
				case "double":
					return SyntaxKind.DoubleKeyword;
				case "float":
					return SyntaxKind.FloatKeyword;
				case "char":
					return SyntaxKind.CharKeyword;
				case "void":
					return SyntaxKind.VoidKeyword;
				case "null":
					return SyntaxKind.NullKeyword;
				case "true":
					return SyntaxKind.TrueKeyword;
				case "false":
					return SyntaxKind.FalseKeyword;
				case "if":
					return SyntaxKind.IfKeyword;
				case "else":
					return SyntaxKind.ElseKeyword;
				case "while":
					return SyntaxKind.WhileKeyword;
				case "for":
					return SyntaxKind.ForKeyword;
				case "do":
					return SyntaxKind.DoKeyword;
				case "switch":
					return SyntaxKind.SwitchKeyword;
				case "case":
					return SyntaxKind.CaseKeyword;
				case "default":
					return SyntaxKind.DefaultKeyword;
				case "try":
					return SyntaxKind.TryKeyword;
				case "throw":
					return SyntaxKind.ThrowKeyword;
				case "catch":
					return SyntaxKind.CatchKeyword;
				case "finally":
					return SyntaxKind.FinallyKeyword;
				case "goto":
					return SyntaxKind.GotoKeyword;
				case "break":
					return SyntaxKind.BreakKeyword;
				case "continue":
					return SyntaxKind.ContinueKeyword;
				case "return":
					return SyntaxKind.ReturnKeyword;
				case "public":
					return SyntaxKind.PublicKeyword;
				case "private":
					return SyntaxKind.PrivateKeyword;
				case "protected":
					return SyntaxKind.ProtectedKeyword;
				case "static":
					return SyntaxKind.StaticKeyword;
				case "const":
					return SyntaxKind.ConstKeyword;
				case "volatile":
					return SyntaxKind.VolatileKeyword;
				case "new":
					return SyntaxKind.NewKeyword;
				case "abstract":
					return SyntaxKind.AbstractKeyword;
				case "native":
					return SyntaxKind.NativeKeyword;
				case "instanceof":
					return SyntaxKind.InstanceOfKeyword;
				case "as":
					return SyntaxKind.AsKeyword;
				case "params":
					return SyntaxKind.ParamsKeyword;
				case "__arglist":
					return SyntaxKind.ArgListKeyword;
				case "this":
					return SyntaxKind.ThisKeyword;
				case "super":
					return SyntaxKind.SuperKeyword;
				case "package":
					return SyntaxKind.PackageKeyword;
				case "import":
					return SyntaxKind.ImportKeyword;
				case "class":
					return SyntaxKind.ClassKeyword;
				case "interface":
					return SyntaxKind.InterfaceKeyword;
				case "enum":
					return SyntaxKind.EnumKeyword;
				case "assert":
					return SyntaxKind.AssertKeyword;
				case "extends":
					return SyntaxKind.ExtendsKeyword;
				case "final":
					return SyntaxKind.FinalKeyword;
				case "implements":
					return SyntaxKind.ImplementsKeyword;
				case "strictfp":
					return SyntaxKind.StrictFpKeyword;
				case "synchronized":
					return SyntaxKind.SynchronizedKeyword;
				case "throws":
					return SyntaxKind.ThrowsKeyword;
				case "transient":
					return SyntaxKind.TransientKeyword;
				default:
					return SyntaxKind.None;
			}
		}

		public static SyntaxKind GetOperatorKind(string operatorMetadataName)
		{
			switch (operatorMetadataName)
			{
				case WellKnownMemberNames.AdditionOperatorName: return SyntaxKind.PlusToken;
				case WellKnownMemberNames.BitwiseAndOperatorName: return SyntaxKind.AmpersandToken;
				case WellKnownMemberNames.BitwiseOrOperatorName: return SyntaxKind.BarToken;
				// case WellKnownMemberNames.ConcatenateOperatorName:
				case WellKnownMemberNames.DecrementOperatorName: return SyntaxKind.MinusMinusToken;
				case WellKnownMemberNames.DivisionOperatorName: return SyntaxKind.SlashToken;
				case WellKnownMemberNames.EqualityOperatorName: return SyntaxKind.EqualsEqualsToken;
				case WellKnownMemberNames.ExclusiveOrOperatorName: return SyntaxKind.CaretToken;
				// case WellKnownMemberNames.ExponentOperatorName:
				case WellKnownMemberNames.FalseOperatorName: return SyntaxKind.FalseKeyword;
				case WellKnownMemberNames.GreaterThanOperatorName: return SyntaxKind.GreaterThanToken;
				case WellKnownMemberNames.GreaterThanOrEqualOperatorName: return SyntaxKind.GreaterThanEqualsToken;
				case WellKnownMemberNames.IncrementOperatorName: return SyntaxKind.PlusPlusToken;
				case WellKnownMemberNames.InequalityOperatorName: return SyntaxKind.ExclamationEqualsToken;
				//case WellKnownMemberNames.IntegerDivisionOperatorName: 
				case WellKnownMemberNames.LeftShiftOperatorName: return SyntaxKind.LessThanLessThanToken;
				case WellKnownMemberNames.LessThanOperatorName: return SyntaxKind.LessThanToken;
				case WellKnownMemberNames.LessThanOrEqualOperatorName: return SyntaxKind.LessThanEqualsToken;
				// case WellKnownMemberNames.LikeOperatorName:
				case WellKnownMemberNames.LogicalNotOperatorName: return SyntaxKind.ExclamationToken;
				case WellKnownMemberNames.ModulusOperatorName: return SyntaxKind.PercentToken;
				case WellKnownMemberNames.MultiplyOperatorName: return SyntaxKind.AsteriskToken;
				case WellKnownMemberNames.OnesComplementOperatorName: return SyntaxKind.TildeToken;
				case WellKnownMemberNames.RightShiftOperatorName: return SyntaxKind.GreaterThanGreaterThanToken;
				case WellKnownMemberNames.SubtractionOperatorName: return SyntaxKind.MinusToken;
				case WellKnownMemberNames.TrueOperatorName: return SyntaxKind.TrueKeyword;
				case WellKnownMemberNames.UnaryNegationOperatorName: return SyntaxKind.MinusToken;
				case WellKnownMemberNames.UnaryPlusOperatorName: return SyntaxKind.PlusToken;
				default:
					return SyntaxKind.None;
			}
		}

		public static IEnumerable<SyntaxKind> GetContextualKeywordKinds()
		{
			//for (int i = (int)SyntaxKind.YieldKeyword; i <= (int)SyntaxKind.AwaitKeyword; i++)
			//{
			//	yield return (SyntaxKind)i;
			//}

			return new List<SyntaxKind>();
		}

		public static bool IsContextualKeyword(SyntaxKind kind)
		{
			return false;

			//switch (kind)
			//{
			//	case SyntaxKind.YieldKeyword:
			//	case SyntaxKind.PartialKeyword:
			//	case SyntaxKind.FromKeyword:
			//	case SyntaxKind.GroupKeyword:
			//	case SyntaxKind.JoinKeyword:
			//	case SyntaxKind.IntoKeyword:
			//	case SyntaxKind.LetKeyword:
			//	case SyntaxKind.ByKeyword:
			//	case SyntaxKind.WhereKeyword:
			//	case SyntaxKind.SelectKeyword:
			//	case SyntaxKind.GetKeyword:
			//	case SyntaxKind.SetKeyword:
			//	case SyntaxKind.AddKeyword:
			//	case SyntaxKind.RemoveKeyword:
			//	case SyntaxKind.OrderByKeyword:
			//	case SyntaxKind.AliasKeyword:
			//	case SyntaxKind.OnKeyword:
			//	case SyntaxKind.EqualsKeyword:
			//	case SyntaxKind.AscendingKeyword:
			//	case SyntaxKind.DescendingKeyword:
			//	case SyntaxKind.AssemblyKeyword:
			//	case SyntaxKind.ModuleKeyword:
			//	case SyntaxKind.TypeKeyword:
			//	case SyntaxKind.GlobalKeyword:
			//	case SyntaxKind.FieldKeyword:
			//	case SyntaxKind.MethodKeyword:
			//	case SyntaxKind.ParamKeyword:
			//	case SyntaxKind.PropertyKeyword:
			//	case SyntaxKind.TypeVarKeyword:
			//	case SyntaxKind.AsyncKeyword:
			//	case SyntaxKind.AwaitKeyword:
			//		return true;
			//	default:
			//		return false;
			//}
		}

		//public static bool IsQueryContextualKeyword(SyntaxKind kind)
		//{
		//	switch (kind)
		//	{
		//		case SyntaxKind.FromKeyword:
		//		case SyntaxKind.WhereKeyword:
		//		case SyntaxKind.SelectKeyword:
		//		case SyntaxKind.GroupKeyword:
		//		case SyntaxKind.IntoKeyword:
		//		case SyntaxKind.OrderByKeyword:
		//		case SyntaxKind.JoinKeyword:
		//		case SyntaxKind.LetKeyword:
		//		case SyntaxKind.OnKeyword:
		//		case SyntaxKind.EqualsKeyword:
		//		case SyntaxKind.ByKeyword:
		//		case SyntaxKind.AscendingKeyword:
		//		case SyntaxKind.DescendingKeyword:
		//			return true;
		//		default:
		//			return false;
		//	}
		//}

		//public static SyntaxKind GetContextualKeywordKind(string text)
		//{
		//	switch (text)
		//	{
		//		//case "yield":
		//		//	return SyntaxKind.YieldKeyword;
		//		//case "partial":
		//		//	return SyntaxKind.PartialKeyword;
		//		//case "from":
		//		//	return SyntaxKind.FromKeyword;
		//		//case "group":
		//		//	return SyntaxKind.GroupKeyword;
		//		//case "join":
		//		//	return SyntaxKind.JoinKeyword;
		//		//case "into":
		//		//	return SyntaxKind.IntoKeyword;
		//		//case "let":
		//		//	return SyntaxKind.LetKeyword;
		//		//case "by":
		//		//	return SyntaxKind.ByKeyword;
		//		//case "where":
		//		//	return SyntaxKind.WhereKeyword;
		//		//case "select":
		//		//	return SyntaxKind.SelectKeyword;
		//		//case "get":
		//		//	return SyntaxKind.GetKeyword;
		//		//case "set":
		//		//	return SyntaxKind.SetKeyword;
		//		//case "add":
		//		//	return SyntaxKind.AddKeyword;
		//		//case "remove":
		//		//	return SyntaxKind.RemoveKeyword;
		//		//case "orderby":
		//		//	return SyntaxKind.OrderByKeyword;
		//		//case "alias":
		//		//	return SyntaxKind.AliasKeyword;
		//		//case "on":
		//		//	return SyntaxKind.OnKeyword;
		//		//case "equals":
		//		//	return SyntaxKind.EqualsKeyword;
		//		//case "ascending":
		//		//	return SyntaxKind.AscendingKeyword;
		//		//case "descending":
		//		//	return SyntaxKind.DescendingKeyword;
		//		//case "assembly":
		//		//	return SyntaxKind.AssemblyKeyword;
		//		//case "module":
		//		//	return SyntaxKind.ModuleKeyword;
		//		//case "type":
		//		//	return SyntaxKind.TypeKeyword;
		//		//case "field":
		//		//	return SyntaxKind.FieldKeyword;
		//		//case "method":
		//		//	return SyntaxKind.MethodKeyword;
		//		//case "param":
		//		//	return SyntaxKind.ParamKeyword;
		//		//case "property":
		//		//	return SyntaxKind.PropertyKeyword;
		//		//case "typevar":
		//		//	return SyntaxKind.TypeVarKeyword;
		//		//case "global":
		//		//	return SyntaxKind.GlobalKeyword;
		//		//case "async":
		//		//	return SyntaxKind.AsyncKeyword;
		//		//case "await":
		//		//	return SyntaxKind.AwaitKeyword;
		//		default:
		//			return SyntaxKind.None;
		//	}
		//}

		public static string GetText(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.TildeToken:
					return "~";
				case SyntaxKind.ExclamationToken:
					return "!";
				case SyntaxKind.DollarToken:
					return "$";
				case SyntaxKind.AtToken:
					return "@";
				case SyntaxKind.PercentToken:
					return "%";
				case SyntaxKind.CaretToken:
					return "^";
				case SyntaxKind.AmpersandToken:
					return "&";
				case SyntaxKind.AsteriskToken:
					return "*";
				case SyntaxKind.OpenParenToken:
					return "(";
				case SyntaxKind.CloseParenToken:
					return ")";
				case SyntaxKind.MinusToken:
					return "-";
				case SyntaxKind.PlusToken:
					return "+";
				case SyntaxKind.EqualsToken:
					return "=";
				case SyntaxKind.OpenBraceToken:
					return "{";
				case SyntaxKind.CloseBraceToken:
					return "}";
				case SyntaxKind.OpenBracketToken:
					return "[";
				case SyntaxKind.CloseBracketToken:
					return "]";
				case SyntaxKind.BarToken:
					return "|";
				case SyntaxKind.BackslashToken:
					return "\\";
				case SyntaxKind.ColonToken:
					return ":";
				case SyntaxKind.SemicolonToken:
					return ";";
				case SyntaxKind.DoubleQuoteToken:
					return "\"";
				case SyntaxKind.SingleQuoteToken:
					return "'";
				case SyntaxKind.LessThanToken:
					return "<";
				case SyntaxKind.CommaToken:
					return ",";
				case SyntaxKind.GreaterThanToken:
					return ">";
				case SyntaxKind.DotToken:
					return ".";
				case SyntaxKind.QuestionToken:
					return "?";
				case SyntaxKind.HashToken:
					return "#";
				case SyntaxKind.SlashToken:
					return "/";
				case SyntaxKind.SlashGreaterThanToken:
					return "/>";
				case SyntaxKind.LessThanSlashToken:
					return "</";
				case SyntaxKind.XmlCommentStartToken:
					return "<!--";
				case SyntaxKind.XmlCommentEndToken:
					return "-->";
				case SyntaxKind.XmlCDataStartToken:
					return "<![CDATA[";
				case SyntaxKind.XmlCDataEndToken:
					return "]]>";
				case SyntaxKind.XmlProcessingInstructionStartToken:
					return "<?";
				case SyntaxKind.XmlProcessingInstructionEndToken:
					return "?>";

				// compound
				case SyntaxKind.BarBarToken:
					return "||";
				case SyntaxKind.AmpersandAmpersandToken:
					return "&&";
				case SyntaxKind.MinusMinusToken:
					return "--";
				case SyntaxKind.PlusPlusToken:
					return "++";
				case SyntaxKind.ColonColonToken:
					return "::";
				case SyntaxKind.QuestionQuestionToken:
					return "??";
				case SyntaxKind.MinusGreaterThanToken:
					return "->";
				case SyntaxKind.ExclamationEqualsToken:
					return "!=";
				case SyntaxKind.EqualsEqualsToken:
					return "==";
				case SyntaxKind.EqualsGreaterThanToken:
					return "=>";
				case SyntaxKind.LessThanEqualsToken:
					return "<=";
				case SyntaxKind.LessThanLessThanToken:
					return "<<";
				case SyntaxKind.LessThanLessThanEqualsToken:
					return "<<=";
				case SyntaxKind.GreaterThanEqualsToken:
					return ">=";
				case SyntaxKind.GreaterThanGreaterThanToken:
					return ">>";
				case SyntaxKind.GreaterThanGreaterThanEqualsToken:
					return ">>=";
				case SyntaxKind.SlashEqualsToken:
					return "/=";
				case SyntaxKind.AsteriskEqualsToken:
					return "*=";
				case SyntaxKind.BarEqualsToken:
					return "|=";
				case SyntaxKind.AmpersandEqualsToken:
					return "&=";
				case SyntaxKind.PlusEqualsToken:
					return "+=";
				case SyntaxKind.MinusEqualsToken:
					return "-=";
				case SyntaxKind.CaretEqualsToken:
					return "^=";
				case SyntaxKind.PercentEqualsToken:
					return "%=";

				// Keywords
				case SyntaxKind.BooleanKeyword:
					return "boolean";
				case SyntaxKind.ByteKeyword:
					return "byte";
				case SyntaxKind.ShortKeyword:
					return "short";
				case SyntaxKind.IntKeyword:
					return "int";
				case SyntaxKind.LongKeyword:
					return "long";
				case SyntaxKind.DoubleKeyword:
					return "double";
				case SyntaxKind.FloatKeyword:
					return "float";
				case SyntaxKind.CharKeyword:
					return "char";
				case SyntaxKind.VoidKeyword:
					return "void";
				case SyntaxKind.NullKeyword:
					return "null";
				case SyntaxKind.TrueKeyword:
					return "true";
				case SyntaxKind.FalseKeyword:
					return "false";
				case SyntaxKind.IfKeyword:
					return "if";
				case SyntaxKind.ElseKeyword:
					return "else";
				case SyntaxKind.WhileKeyword:
					return "while";
				case SyntaxKind.ForKeyword:
					return "for";
				case SyntaxKind.DoKeyword:
					return "do";
				case SyntaxKind.SwitchKeyword:
					return "switch";
				case SyntaxKind.CaseKeyword:
					return "case";
				case SyntaxKind.DefaultKeyword:
					return "default";
				case SyntaxKind.TryKeyword:
					return "try";
				case SyntaxKind.CatchKeyword:
					return "catch";
				case SyntaxKind.FinallyKeyword:
					return "finally";
				case SyntaxKind.GotoKeyword:
					return "goto";
				case SyntaxKind.BreakKeyword:
					return "break";
				case SyntaxKind.ContinueKeyword:
					return "continue";
				case SyntaxKind.ReturnKeyword:
					return "return";
				case SyntaxKind.ThrowKeyword:
					return "throw";
				case SyntaxKind.PublicKeyword:
					return "public";
				case SyntaxKind.PrivateKeyword:
					return "private";
				case SyntaxKind.ProtectedKeyword:
					return "protected";
				case SyntaxKind.StaticKeyword:
					return "static";
				case SyntaxKind.ConstKeyword:
					return "const";
				case SyntaxKind.VolatileKeyword:
					return "volatile";
				case SyntaxKind.NewKeyword:
					return "new";
				case SyntaxKind.AbstractKeyword:
					return "abstract";
				case SyntaxKind.NativeKeyword:
					return "native";
				case SyntaxKind.InstanceOfKeyword:
					return "instanceof";
				case SyntaxKind.AsKeyword:
					return "as";
				case SyntaxKind.ParamsKeyword:
					return "params";
				case SyntaxKind.ArgListKeyword:
					return "__arglist";
				case SyntaxKind.ThisKeyword:
					return "this";
				case SyntaxKind.SuperKeyword:
					return "super";
				case SyntaxKind.PackageKeyword:
					return "package";
				case SyntaxKind.ImportKeyword:
					return "import";
				case SyntaxKind.ClassKeyword:
					return "class";
				case SyntaxKind.InterfaceKeyword:
					return "interface";
				case SyntaxKind.EnumKeyword:
					return "enum";
				case SyntaxKind.AssertKeyword:
					return "assert";
				case SyntaxKind.ExtendsKeyword:
					return "extends";
				case SyntaxKind.FinalKeyword:
					return "final";
				case SyntaxKind.ImplementsKeyword:
					return "implements";
				case SyntaxKind.StrictFpKeyword:
					return "strictfp";
				case SyntaxKind.SynchronizedKeyword:
					return "synchronized";
				case SyntaxKind.ThrowsKeyword:
					return "throws";
				case SyntaxKind.TransientKeyword:
					return "transient";
				default:
					return string.Empty;
			}
		}

		internal static bool IsStatementExpression(SyntaxKind kind, bool isMissing)
		{
			// The grammar gives:
			//
			// expression-statement:
			//     statement-expression ;
			//
			// statement-expression:
			//     invocation-expression
			//     object-creation-expression
			//     assignment
			//     post-increment-expression
			//     post-decrement-expression
			//     pre-increment-expression
			//     pre-decrement-expression
			//     await-expression

			switch (kind)
			{
				case SyntaxKind.InvocationExpression:
				case SyntaxKind.ObjectCreationExpression:
				case SyntaxKind.SimpleAssignmentExpression:
				case SyntaxKind.AddAssignmentExpression:
				case SyntaxKind.SubtractAssignmentExpression:
				case SyntaxKind.MultiplyAssignmentExpression:
				case SyntaxKind.DivideAssignmentExpression:
				case SyntaxKind.ModuloAssignmentExpression:
				case SyntaxKind.AndAssignmentExpression:
				case SyntaxKind.OrAssignmentExpression:
				case SyntaxKind.ExclusiveOrAssignmentExpression:
				case SyntaxKind.LeftShiftAssignmentExpression:
				case SyntaxKind.RightShiftAssignmentExpression:
				case SyntaxKind.PostIncrementExpression:
				case SyntaxKind.PostDecrementExpression:
				case SyntaxKind.PreIncrementExpression:
				case SyntaxKind.PreDecrementExpression:
				case SyntaxKind.AwaitExpression:
					return true;

				// Allow missing IdentifierNames; they will show up in error cases
				// where there is no statement whatsoever.

				case SyntaxKind.IdentifierName:
					return isMissing;

				// TODO: The native implementation also disallows delegate
				// creation expressions with the ERR_IllegalStatement error, 
				// so that needs to go into the semantic analysis somewhere
				// if we intend to carry it forward.

				default:
					return false;
			}
		}

		public static bool IsDocumentationCommentTrivia(SyntaxKind kind)
		{
			return kind == SyntaxKind.SingleLineDocumentationCommentTrivia ||
				kind == SyntaxKind.MultiLineDocumentationCommentTrivia;
		}
	}
}
