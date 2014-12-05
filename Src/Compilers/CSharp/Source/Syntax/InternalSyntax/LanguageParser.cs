// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading;

using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
	internal partial class LanguageParser : SyntaxParser
	{


		[Flags]
		private enum SyntaxModifier
		{
			None = 0,
			Public = 0x0001,
			Protected = 0x0002,
			Private = 0x0004,
			Volatile = 0x0008,
			Final = 0x0010,
			Abstract = 0x0020,
			Static = 0x0040,
			Strictfp = 0x0080,
			Transient = 0x0100,
			Synchronized = 0x0200,
			Native = 0x0400,
		}

		private enum PostSkipAction
		{
			Continue,
			Abort
		}

		[Flags]
		private enum ParamFlags
		{
			None = 0x00,
			This = 0x01,
			Ref = 0x02,
			Out = 0x04,
			Params = 0x08,
		}

		[Flags]
		private enum VariableFlags
		{
			Fixed = 0x01,
			Const = 0x02,
			Local = 0x04,
			Final = 0x08,
		}

		[Flags]
		private enum NameOptions
		{
			None = 0,
			InExpression = 1 << 0, // Used to influence parser ambiguity around "<" and generics vs. expressions. Used in ParseSimpleName.
			InTypeList = 1 << 1, // Allows attributes to appear within the generic type argument list. Used during ParseInstantiation.
		}

		enum ScanTypeArgumentListKind
		{
			NotTypeArgumentList,
			PossibleTypeArgumentList,
			DefiniteTypeArgumentList
		}


		private enum ScanTypeFlags
		{
			/// <summary>
			/// Definitely not a type name.
			/// </summary>
			NotType,

			/// <summary>
			/// Definitely a type name: either a predefined type (int, string, etc.) or an array type name (ending with a bracket).
			/// </summary>
			MustBeType,

			/// <summary>
			/// Might be a generic (qualified) type name or an expression.
			/// </summary>
			GenericTypeOrExpression,

			/// <summary>
			/// Might be a non-generic (qualified) type name or an expression.
			/// </summary>
			NonGenericTypeOrExpression,

			/// <summary>
			/// Nullable type (ending with ?).
			/// </summary>
			NullableType,

			/// <summary>
			/// Might be a pointer type or a multiplication.
			/// </summary>
			PointerOrMultiplication,



			JavaWildcardType,
			//ClassKeywordSuffix,
		}


		private enum NamedTypePart
		{
			NotName,
			GenericName,
			SimpleName,
			//ClassKeywordSuffix,
		}



		private const SyntaxModifier AccessModifiers = SyntaxModifier.Public | SyntaxModifier.Protected | SyntaxModifier.Private;

		// list pools - allocators for lists that are used to build sequences of nodes. The lists
		// can be reused (hence pooled) since the syntax factory methods don't keep references to
		// them

		private readonly SyntaxListPool _pool = new SyntaxListPool(); // Don't need to reset this.

		private readonly SyntaxFactoryContext _syntaxFactoryContext; // Fields are resettable.
		private readonly ContextAwareSyntax _syntaxFactory; // Has context, the fields of which are resettable.

		private TerminatorState _termState; // Resettable
		private bool _isInTry; // Resettable


		private const int LambdaPrecedence = 1;

		private bool IsIncrementalAndFactoryContextMatches
		{
			get
			{
				if (!base.IsIncremental)
				{
					return false;
				}

				CSharp.CSharpSyntaxNode current = this.CurrentNode;
				return current != null;
			}
		}


		// NOTE: If you add new state, you should probably add it to ResetPoint as well.

		internal LanguageParser(
			Lexer lexer,
			CSharp.CSharpSyntaxNode oldTree,
			IEnumerable<TextChangeRange> changes,
			CancellationToken cancellationToken = default(CancellationToken))
			: base(lexer, LexerMode.Syntax, oldTree, changes, allowModeReset: false,
				preLexIfNotIncremental: true, cancellationToken: cancellationToken)
		{
			this._syntaxFactoryContext = new SyntaxFactoryContext();
			this._syntaxFactory = new ContextAwareSyntax(_syntaxFactoryContext);
		}

		private bool NameContainsGeneric(NameSyntax name)
		{
			switch (name.Kind)
			{
				case SyntaxKind.GenericName:
					return true;
				case SyntaxKind.QualifiedName:
					var qualifedName = (QualifiedNameSyntax)name;
					return NameContainsGeneric(qualifedName.Left) || NameContainsGeneric(qualifedName.Right);
			}

			return false;
		}

		private void AddSkippedNamespaceText(
			ref CompilationUnitBodyBuilder body,
			ref SyntaxListBuilder initialBadNodes,
			CSharpSyntaxNode skippedSyntax)
		{
			if (body.Members.Count > 0)
			{
				body.Members[body.Members.Count - 1] = AddTrailingSkippedSyntax(body.Members[body.Members.Count - 1], skippedSyntax);
			}
			else if (body.Imports.Count > 0)
			{
				body.Imports[body.Imports.Count - 1] = AddTrailingSkippedSyntax(body.Imports[body.Imports.Count - 1], skippedSyntax);
			}
			else if (body.Package!=null)
			{
				body.Package = AddTrailingSkippedSyntax(body.Package, skippedSyntax);
			}
			else if (body.Annotations.Count > 0)
			{
				body.Annotations[body.Annotations.Count - 1] = AddTrailingSkippedSyntax(body.Annotations[body.Annotations.Count - 1], skippedSyntax);
			}
			else
			{
				if (initialBadNodes == null)
				{
					initialBadNodes = this._pool.Allocate();
				}

				initialBadNodes.AddRange(skippedSyntax);
			}
		}





		private void ReduceIncompleteMembers(ref SyntaxListBuilder<MemberDeclarationSyntax> incompleteMembers, ref CompilationUnitBodyBuilder body, ref SyntaxListBuilder initialBadNodes)
		{
			for (int i = 0; i < incompleteMembers.Count; i++)
			{
				this.AddSkippedNamespaceText(ref body, ref initialBadNodes, incompleteMembers[i]);
			}
			incompleteMembers.Clear();
		}

		private bool IsNamespaceMemberStartOrStop()
		{
			return this.IsEndOfNamespace()
				|| this.IsPossiblePackageMemberDeclaration();
		}



		private bool IsAttributeDeclarationTerminator()
		{
			return this.CurrentToken.Kind == SyntaxKind.CloseBracketToken
				|| this.IsPossibleAnnotationSyntax(); // start of a new one...
		}


		#region Static Method

		private static SyntaxModifier GetModifier(SyntaxToken token)
		{
			switch (token.Kind)
			{
				case SyntaxKind.PublicKeyword:
					return SyntaxModifier.Public;
				case SyntaxKind.ProtectedKeyword:
					return SyntaxModifier.Protected;
				case SyntaxKind.PrivateKeyword:
					return SyntaxModifier.Private;
				case SyntaxKind.VolatileKeyword:
					return SyntaxModifier.Volatile;
				case SyntaxKind.FinalKeyword:
					return SyntaxModifier.Final;
				case SyntaxKind.AbstractKeyword:
					return SyntaxModifier.Abstract;
				case SyntaxKind.StaticKeyword:
					return SyntaxModifier.Static;
				case SyntaxKind.StrictFpKeyword:
					return SyntaxModifier.Strictfp;
				case SyntaxKind.TransientKeyword:
					return SyntaxModifier.Transient;
				case SyntaxKind.SynchronizedKeyword:
					return SyntaxModifier.Synchronized;
				case SyntaxKind.NativeKeyword:
					return SyntaxModifier.Native;
				default:
					return SyntaxModifier.None;
			}
		}

		private static SyntaxModifier GetFieldModifier(SyntaxToken token)
		{
			switch (token.Kind)
			{
				case SyntaxKind.PublicKeyword:
					return SyntaxModifier.Public;
				case SyntaxKind.ProtectedKeyword:
					return SyntaxModifier.Protected;
				case SyntaxKind.PrivateKeyword:
					return SyntaxModifier.Private;
				case SyntaxKind.StaticKeyword:
					return SyntaxModifier.Static;
				case SyntaxKind.FinalKeyword:
					return SyntaxModifier.Final;
				case SyntaxKind.TransientKeyword:
					return SyntaxModifier.Transient;
				case SyntaxKind.VolatileKeyword:
					return SyntaxModifier.Volatile;
				default:
					return SyntaxModifier.None;
			}
		}

		private void ReportDuplicateModifiers(ref SyntaxToken modTok, SyntaxModifier newMod, SyntaxModifier mods,
			ref bool seenNoDuplicates, ref bool seenNoAccessibilityDuplicates)
		{
			if ((mods & newMod) != 0)
			{
				if (seenNoDuplicates)
				{
					modTok = this.AddError(modTok, ErrorCode.ERR_DuplicateModifier, SyntaxKindFacts.GetText(modTok.Kind));
					seenNoDuplicates = false;
				}
			}
			else
			{
				if ((mods & AccessModifiers) != 0 && (newMod & AccessModifiers) != 0)
				{
					if (seenNoAccessibilityDuplicates)
					{
						modTok = this.AddError(modTok, ErrorCode.ERR_BadMemberProtection);
					}
					seenNoAccessibilityDuplicates = false;
				}
			}
		}

		private static bool CanReuseTypeDeclaration(CSharp.Syntax.MemberDeclarationSyntax member)
		{
			if (member != null)
			{
				// on reuse valid type declaration (not bad namespace members)
				switch (member.Kind)
				{
					case SyntaxKind.JavaNormalClassDeclaration:
					case SyntaxKind.JavaNormalInterfaceDeclaration:
					case SyntaxKind.JavaAnnotationTypeDeclaration:
					case SyntaxKind.JavaEnumDeclaration:
						return true;
				}
			}

			return false;
		}

		private static bool CanStartMember(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.AbstractKeyword:
				case SyntaxKind.NativeKeyword:
				case SyntaxKind.StaticKeyword:
				case SyntaxKind.VolatileKeyword:
				case SyntaxKind.FinalKeyword:


				case SyntaxKind.PrivateKeyword:
				case SyntaxKind.ProtectedKeyword:
				case SyntaxKind.PublicKeyword:

				case SyntaxKind.BooleanKeyword:
				case SyntaxKind.ByteKeyword:
				case SyntaxKind.ShortKeyword:
				case SyntaxKind.IntKeyword:
				case SyntaxKind.LongKeyword:
				case SyntaxKind.FloatKeyword:
				case SyntaxKind.DoubleKeyword:
				case SyntaxKind.CharKeyword:

				case SyntaxKind.VoidKeyword:

				case SyntaxKind.ClassKeyword:
				case SyntaxKind.EnumKeyword:
				case SyntaxKind.InterfaceKeyword:
				case SyntaxKind.AtToken: //注解声明与注解实例

				case SyntaxKind.NewKeyword:
				case SyntaxKind.IdentifierToken:
				case SyntaxKind.OpenBracketToken:// [
				case SyntaxKind.OpenBraceToken: // { 实例初始化
				
					return true;
				default:
					return false;
			}
		}

		private static CSharp.CSharpSyntaxNode GetOldParent(CSharp.CSharpSyntaxNode node)
		{
			return node != null ? node.Parent : null;
		}

		private static void AddIncompleteMembers(ref SyntaxListBuilder<MemberDeclarationSyntax> incompleteMembers, ref CompilationUnitBodyBuilder body)
		{
			if (incompleteMembers.Count > 0)
			{
				body.Members.AddRange(incompleteMembers);
				incompleteMembers.Clear();
			}
		}


		private static bool CanReuseMemberDeclaration(CSharp.Syntax.MemberDeclarationSyntax member,string typeName)
		{
			if (member != null)
			{
				switch (member.Kind)
				{
					case SyntaxKind.JavaNormalClassDeclaration:
					case SyntaxKind.JavaNormalInterfaceDeclaration:
					case SyntaxKind.JavaAnnotationTypeDeclaration:
					case SyntaxKind.JavaEnumDeclaration:
					case SyntaxKind.FieldDeclaration:
						return true;
				}

				var parent = GetOldParent(member);
				var originalTypeDeclaration = parent as CSharp.Syntax.JavaTypeDeclarationSyntax;

				// originalTypeDeclaration can be null in the case of script code.  In that case
				// the member declaration can be a child of a namespace/copmilation-unit instead of
				// a type.
				if (originalTypeDeclaration != null)
				{
					switch (member.Kind)
					{
						case SyntaxKind.MethodDeclaration:
							// can reuse a method as long as it *doesn't* match the type name.
							//
							// TODO(cyrusn): Relax this in the case of generic methods?
							var methodDeclaration = (CSharp.Syntax.MethodDeclarationSyntax)member;
							return methodDeclaration.Identifier.ValueText != typeName;

						case SyntaxKind.ConstructorDeclaration: // fall through
						case SyntaxKind.DestructorDeclaration:
							// can reuse constructors or destructors if the name and type name still
							// match.
							return originalTypeDeclaration.Identifier.ValueText == typeName;
					}
				}
			}

			return false;
		}


		public static bool IsComplete(CSharpSyntaxNode node)
		{
			if (node == null)
			{
				return false;
			}

			foreach (var child in node.ChildNodesAndTokens().Reverse())
			{
				var token = child as SyntaxToken;
				if (token == null)
				{
					return IsComplete((CSharpSyntaxNode)child);
				}

				if (token.IsMissing)
				{
					return false;
				}

				if (token.Kind != SyntaxKind.None)
				{
					return true;
				}

				// if token was optional, consider the next one..
			}

			return true;
		}


		private static bool IsDeclarationModifier(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.ConstKeyword:
				case SyntaxKind.StaticKeyword:
				case SyntaxKind.VolatileKeyword:
				case SyntaxKind.FinalKeyword:
					return true;
				default:
					return false;
			}
		}


		private static bool IsInvalidSubExpression(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.BreakKeyword:     // break
				case SyntaxKind.CaseKeyword:      // case
				case SyntaxKind.CatchKeyword:     // catch
				case SyntaxKind.ConstKeyword:     // const
				case SyntaxKind.ContinueKeyword:  // continue
				case SyntaxKind.DoKeyword:        // do
				case SyntaxKind.FinallyKeyword:   // finally
				case SyntaxKind.ForKeyword:       // for
				case SyntaxKind.IfKeyword:        // if
				case SyntaxKind.ReturnKeyword:    // return
				case SyntaxKind.SwitchKeyword:    // switch
				case SyntaxKind.ThrowKeyword:     // throw
				case SyntaxKind.TryKeyword:       // try
				case SyntaxKind.ImportKeyword:    // import
				case SyntaxKind.WhileKeyword:     // while
				case SyntaxKind.InstanceOfKeyword:// instanceof
					return true;
				default:
					return false;
			}
		}

		internal static bool IsRightAssociative(SyntaxKind op)
		{
			switch (op)
			{
				case SyntaxKind.SimpleAssignmentExpression:     // a = b
				case SyntaxKind.AddAssignmentExpression:        // a += b
				case SyntaxKind.SubtractAssignmentExpression:   // a -= b
				case SyntaxKind.MultiplyAssignmentExpression:   // a *= b
				case SyntaxKind.DivideAssignmentExpression:     // a /= b
				case SyntaxKind.ModuloAssignmentExpression:     // a %= b
				case SyntaxKind.AndAssignmentExpression:        // a &= b
				case SyntaxKind.ExclusiveOrAssignmentExpression:// a ^= b
				case SyntaxKind.OrAssignmentExpression:         // a |= b
				case SyntaxKind.LeftShiftAssignmentExpression:  // a <<= b
				case SyntaxKind.RightShiftAssignmentExpression: // a >> = b
					return true;
				default:
					return false;
			}
		}

		/// <summary>
		/// 获取运算符优先级
		/// </summary>
		/// <param name="op"></param>
		/// <returns></returns>
		private static uint GetPrecedence(SyntaxKind op)
		{
			switch (op)
			{
				case SyntaxKind.SimpleAssignmentExpression:     // a = b
				case SyntaxKind.AddAssignmentExpression:        // a += b
				case SyntaxKind.SubtractAssignmentExpression:   // a -= b
				case SyntaxKind.MultiplyAssignmentExpression:   // a *= b
				case SyntaxKind.DivideAssignmentExpression:     // a /= b
				case SyntaxKind.ModuloAssignmentExpression:     // a %= b
				case SyntaxKind.AndAssignmentExpression:        // a &= b
				case SyntaxKind.ExclusiveOrAssignmentExpression:// a ^= b
				case SyntaxKind.OrAssignmentExpression:         // a |= b
				case SyntaxKind.LeftShiftAssignmentExpression:  // a <<= b
				case SyntaxKind.RightShiftAssignmentExpression: // a >>= b
				case SyntaxKind.UnsignedRightShiftAssignmentExpression: // a >>>= b
					return 1;
				case SyntaxKind.LogicalOrExpression:            // ||
					return 3;
				case SyntaxKind.LogicalAndExpression:           // &&
					return 4;
				case SyntaxKind.BitwiseOrExpression:            // |
					return 5;
				case SyntaxKind.ExclusiveOrExpression:          // ^
					return 6;
				case SyntaxKind.BitwiseAndExpression:           // &
					return 7;
				case SyntaxKind.EqualsExpression:               // =
				case SyntaxKind.NotEqualsExpression:            // !=
					return 8;
				case SyntaxKind.LessThanExpression:             // <
				case SyntaxKind.LessThanOrEqualExpression:      // <=
				case SyntaxKind.GreaterThanExpression:          // >
				case SyntaxKind.GreaterThanOrEqualExpression:   // >=
				case SyntaxKind.InstanceOfExpression:                   // is
				//case SyntaxKind.AsExpression:                   // as
					return 9;
				case SyntaxKind.LeftShiftExpression:            // <<
				case SyntaxKind.RightShiftExpression:           // >>
				case SyntaxKind.UnsignedRightShiftExpression:   // >>>
					return 10;
				case SyntaxKind.AddExpression:                  // +
				case SyntaxKind.SubtractExpression:             // -
					return 11;
				case SyntaxKind.MultiplyExpression:             // *
				case SyntaxKind.DivideExpression:               // /
				case SyntaxKind.ModuloExpression:               // %
					return 12;
				case SyntaxKind.UnaryPlusExpression:            // +a
				case SyntaxKind.UnaryMinusExpression:           // -a
				case SyntaxKind.BitwiseNotExpression:           // ~a
				case SyntaxKind.LogicalNotExpression:           // !a
				case SyntaxKind.PreIncrementExpression:         // ++a
				case SyntaxKind.PreDecrementExpression:         // --a
					return 13;
				case SyntaxKind.CastExpression:                 // ()
					return 14;
				default:
					return 0;
			}
		}

		private static bool IsExpectedPrefixUnaryOperator(SyntaxKind kind)
		{
			return SyntaxKindFacts.IsPrefixUnaryExpression(kind);
		}

		private static bool IsExpectedBinaryOperator(SyntaxKind kind)
		{
			return SyntaxKindFacts.IsBinaryExpression(kind) && kind != SyntaxKind.DotToken && kind != SyntaxKind.MinusGreaterThanToken;
		}


		private static bool CanReuseParameterList(CSharp.Syntax.ParameterListSyntax list)
		{
			if (list == null)
			{
				return false;
			}

			if (list.OpenParenToken.IsMissing)
			{
				return false;
			}

			if (list.CloseParenToken.IsMissing)
			{
				return false;
			}

			foreach (var parameter in list.Parameters)
			{
				if (!CanReuseParameter(parameter))
				{
					return false;
				}
			}

			return true;
		}

		private static bool CanReuseBracketedParameterList(CSharp.Syntax.BracketedParameterListSyntax list)
		{
			if (list == null)
			{
				return false;
			}

			if (list.OpenBracketToken.IsMissing)
			{
				return false;
			}

			if (list.CloseBracketToken.IsMissing)
			{
				return false;
			}

			foreach (var parameter in list.Parameters)
			{
				if (!CanReuseParameter(parameter))
				{
					return false;
				}
			}

			return true;
		}

		private static bool CanReuseParameter(CSharp.Syntax.ParameterSyntax parameter, SyntaxListBuilder<AnnotationSyntax> attributes, SyntaxListBuilder modifiers)
		{
			if (parameter == null)
			{
				return false;
			}

			// cannot reuse parameter if it had attributes.
			//
			// TODO(cyrusn): Why?  We can reuse other constructs if they have attributes.
			if (attributes.Count != 0 || parameter.AttributeLists.Count != 0)
			{
				return false;
			}

			// cannot reuse parameter if it had modifiers.
			if ((modifiers != null && modifiers.Count != 0) || parameter.Modifiers.Count != 0)
			{
				return false;
			}

			return CanReuseParameter(parameter);
		}

		private static bool CanReuseParameter(CSharp.Syntax.ParameterSyntax parameter)
		{
			// cannot reuse a node that possibly ends in an expression
			if (parameter.Default != null)
			{
				return false;
			}

			// cannot reuse lambda parameters as normal parameters (parsed with
			// different rules)
			CSharp.CSharpSyntaxNode parent = parameter.Parent;
			if (parent != null)
			{
				if (parent.Kind == SyntaxKind.SimpleLambdaExpression)
				{
					return false;
				}

				CSharp.CSharpSyntaxNode grandparent = parent.Parent;
				if (grandparent != null && grandparent.Kind == SyntaxKind.ParenthesizedLambdaExpression)
				{
					Debug.Assert(parent.Kind == SyntaxKind.ParameterList);
					return false;
				}
			}

			return true;
		}


		private static bool IsParameterModifier(SyntaxKind kind, bool allowThisKeyword)
		{
			return GetParamFlags(kind, allowThisKeyword) != ParamFlags.None;
		}


		private static ParamFlags GetParamFlags(SyntaxKind kind, bool allowThisKeyword)
		{
			switch (kind)
			{
				case SyntaxKind.ThisKeyword:
					// if (this.IsCSharp3Enabled)
					return (allowThisKeyword ? ParamFlags.This : ParamFlags.None);

				// goto default;
				//case SyntaxKind.ParamsKeyword:
				//	return ParamFlags.Params;
				default:
					return ParamFlags.None;
			}
		}

		private static SyntaxTokenList GetOriginalModifiers(CSharp.CSharpSyntaxNode decl)
		{
			if (decl != null)
			{
				switch (decl.Kind)
				{
					case SyntaxKind.FieldDeclaration:
						return ((CSharp.Syntax.FieldDeclarationSyntax)decl).Modifiers;
					case SyntaxKind.MethodDeclaration:
						return ((CSharp.Syntax.MethodDeclarationSyntax)decl).Modifiers;
					case SyntaxKind.ConstructorDeclaration:
						return ((CSharp.Syntax.ConstructorDeclarationSyntax)decl).Modifiers;
					case SyntaxKind.DestructorDeclaration:
						return ((CSharp.Syntax.DestructorDeclarationSyntax)decl).Modifiers;
					case SyntaxKind.JavaNormalClassDeclaration:
						return ((CSharp.Syntax.JavaNormalClassDeclarationSyntax)decl).Modifier.JavaModifiers;
					case SyntaxKind.JavaNormalInterfaceDeclaration:
						return ((CSharp.Syntax.JavaNormalInterfaceDeclarationSyntax)decl).Modifier.JavaModifiers;
				}
			}

			return default(SyntaxTokenList);
		}

		private static bool WasFirstVariable(CSharp.Syntax.VariableDeclaratorSyntax variable)
		{
			var parent = GetOldParent(variable) as CSharp.Syntax.VariableDeclarationSyntax;
			if (parent != null)
			{
				return parent.Variables[0] == variable;
			}

			return false;
		}

		private static VariableFlags GetOriginalVariableFlags(CSharp.Syntax.VariableDeclaratorSyntax old)
		{
			var parent = GetOldParent(old);
			var mods = GetOriginalModifiers(parent);
			VariableFlags flags = default(VariableFlags);

			if (mods.Any(SyntaxKind.ConstKeyword))
			{
				flags |= VariableFlags.Const;
			}

			if (parent != null && (parent.Kind == SyntaxKind.VariableDeclaration || parent.Kind == SyntaxKind.LocalDeclarationStatement))
			{
				flags |= VariableFlags.Local;
			}

			return flags;
		}

		private static bool CanReuseVariableDeclarator(CSharp.Syntax.VariableDeclaratorSyntax old, VariableFlags flags, bool isFirst)
		{
			if (old == null)
			{
				return false;
			}

			SyntaxKind oldKind;

			return (flags == GetOriginalVariableFlags(old))
				&& (isFirst == WasFirstVariable(old))
				&& old.Initializer == null  // can't reuse node that possibly ends in an expression
				&& (oldKind = GetOldParent(old).Kind) != SyntaxKind.VariableDeclaration // or in a method body
				&& oldKind != SyntaxKind.LocalDeclarationStatement;
		}


		private static bool IsValidEnumBaseType(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.ByteKeyword:
				case SyntaxKind.ShortKeyword:
				case SyntaxKind.IntKeyword:
				case SyntaxKind.LongKeyword:
					return true;
				default:
					return false;
			}
		}

		private static SyntaxToken CreateMissingIdentifierToken()
		{
			return SyntaxFactory.MissingToken(SyntaxKind.IdentifierToken);
		}

		private static ScanTypeFlags NamedTypePartToScanTypeFlags(NamedTypePart st)
		{
			Debug.Assert(st != NamedTypePart.NotName);
			return st == NamedTypePart.GenericName ? ScanTypeFlags.GenericTypeOrExpression : ScanTypeFlags.NonGenericTypeOrExpression;
		}

		private static bool IsPredefinedType(SyntaxKind keyword)
		{
			return SyntaxKindFacts.IsPredefinedType(keyword);
		}

		#endregion

		private bool CanStartTypeDeclaration(SyntaxKind kind)
		{
			var istype = false;
			switch (kind)
			{
				case SyntaxKind.ClassKeyword:
				case SyntaxKind.EnumKeyword:
				case SyntaxKind.InterfaceKeyword:
					istype = true;
					break;
				default:
					istype = false;
					break;
			}

			return istype || IsPossibilityAnnotationTypeDefine();
		}

		private TypeArgumentListSyntax TypeArgumentFromTypeParameters(TypeParameterListSyntax typeParameterList)
		{
			var types = this._pool.AllocateSeparated<TypeSyntax>();
			foreach (var p in typeParameterList.Parameters.GetWithSeparators())
			{
				switch (p.Kind)
				{
					case SyntaxKind.TypeParameter:
						var typeParameter = (TypeParameterSyntax)p;
						var typeArgument = _syntaxFactory.IdentifierName(typeParameter.Identifier);
						throw new NotImplementedException();
						// NOTE: reverse order of variance keyword and attributes list so they come out in the right order.
						//if (typeParameter.TypeParameterModifiers.Node != null)
						//{
						//	// This only happens in error scenarios, so don't bother to produce a diagnostic about
						//	// having an _annotation on a type argument.
						//	typeArgument = AddLeadingSkippedSyntax(typeArgument, typeParameter.TypeParameterModifiers.Node);
						//}
						types.Add(typeArgument);
						break;
					case SyntaxKind.CommaToken:
						types.AddSeparator((SyntaxToken)p);
						break;
					default:
						Debug.Assert(false);
						break;
				}
			}

			var result = _syntaxFactory.TypeArgumentList(typeParameterList.LessThanToken, types.ToList(), typeParameterList.GreaterThanToken);
			this._pool.Free(types);
			return result;
		}

		


		private TNode EatUnexpectedTrailingSemicolon<TNode>(TNode decl) where TNode : CSharpSyntaxNode
		{
			// allow for case of one unexpected semicolon...
			if (this.CurrentToken.Kind == SyntaxKind.SemicolonToken)
			{
				var semi = this.EatToken();
				semi = this.AddError(semi, ErrorCode.ERR_UnexpectedSemicolon);
				decl = AddTrailingSkippedSyntax(decl, semi);
			}

			return decl;
		}

		private bool IsDotOrColonColon()
		{
			return this.CurrentToken.Kind == SyntaxKind.DotToken || this.CurrentToken.Kind == SyntaxKind.ColonColonToken;
		}

		private bool IsDot()
		{
			return this.CurrentToken.Kind == SyntaxKind.DotToken;
		}


		private IdentifierNameSyntax CreateMissingIdentifierName()
		{
			return _syntaxFactory.IdentifierName(CreateMissingIdentifierToken());
		}

		

		/// <summary>
		/// True if current identifier token is not really some contextual keyword
		/// </summary>
		/// <returns></returns>
		private bool IsTrueIdentifier()
		{
			if (this.CurrentToken.Kind == SyntaxKind.IdentifierToken)
			{
				return true;
			}
			return false;
		}



		private bool IsOpenName()
		{
			bool isOpen = true;
			int n = 0;
			while (this.PeekToken(n).Kind == SyntaxKind.CommaToken)
			{
				n++;
			}

			if (this.PeekToken(n).Kind != SyntaxKind.GreaterThanToken)
			{
				isOpen = false;
			}

			return isOpen;
		}


		private SyntaxToken ConvertToMissingWithTrailingTrivia(SyntaxToken token, SyntaxKind expectedKind)
		{
			var newToken = SyntaxFactory.MissingToken(expectedKind);
			newToken = AddTrailingSkippedSyntax(newToken, token);
			return newToken;
		}


		private bool IsTerm()
		{
			switch (this.CurrentToken.Kind)
			{
				case SyntaxKind.ArgListKeyword:
				case SyntaxKind.SuperKeyword:
				case SyntaxKind.DefaultKeyword:
				case SyntaxKind.FalseKeyword:
				case SyntaxKind.NewKeyword:
				case SyntaxKind.NullKeyword:
				case SyntaxKind.ThisKeyword:
				case SyntaxKind.TrueKeyword:
				case SyntaxKind.NumericLiteralToken:
				case SyntaxKind.StringLiteralToken:
				case SyntaxKind.CharacterLiteralToken:
				case SyntaxKind.OpenParenToken:
				case SyntaxKind.MinusGreaterThanToken:
					return true;
				case SyntaxKind.IdentifierToken:
					return this.IsTrueIdentifier();
				default:
					return false;
			}
		}


		// Is this statement list non-empty, and large enough to make using weak children beneficial?
		private static bool IsLargeEnoughNonEmptyStatementList(SyntaxListBuilder<StatementSyntax> statements)
		{
			if (statements.Count == 0)
			{
				return false;
			}
			else if (statements.Count == 1)
			{
				// If we have a single statement, it might be small, like "return null", or large,
				// like a loop or if or switch with many statements inside. Use the width as a proxy for
				// how big it is. If it's small, its better to forgoe a many children list anyway, since the
				// weak reference would consume as much memory as is saved.
				return statements[0].Width > 60;
			}
			else
			{
				// For 2 or more statements, go ahead and create a many-children lists.
				return true;
			}
		}

		


		private bool IsUsingStatementVariableDeclaration(ScanTypeFlags st)
		{
			bool condition1 = st == ScanTypeFlags.MustBeType && this.CurrentToken.Kind != SyntaxKind.DotToken;
			bool condition2 = st != ScanTypeFlags.NotType && this.CurrentToken.Kind == SyntaxKind.IdentifierToken;
			bool condition3 = st == ScanTypeFlags.NonGenericTypeOrExpression || this.PeekToken(1).Kind == SyntaxKind.EqualsToken;

			return condition1 || (condition2 && condition3);
		}




		private static bool IsAnyTypeOrExpr(ScanTypeFlags st)
		{
			return st == ScanTypeFlags.GenericTypeOrExpression || st == ScanTypeFlags.NonGenericTypeOrExpression;
		}


		private static bool CanFollowCast(SyntaxKind kind)
		{
			switch (kind)
			{
				case SyntaxKind.InstanceOfKeyword:
				case SyntaxKind.SemicolonToken:
				case SyntaxKind.CloseParenToken:
				case SyntaxKind.CloseBracketToken:
				case SyntaxKind.OpenBraceToken:
				case SyntaxKind.CloseBraceToken:
				case SyntaxKind.CommaToken:
				case SyntaxKind.EqualsToken:
				case SyntaxKind.PlusEqualsToken:
				case SyntaxKind.MinusEqualsToken:
				case SyntaxKind.AsteriskEqualsToken:
				case SyntaxKind.SlashEqualsToken:
				case SyntaxKind.PercentEqualsToken:
				case SyntaxKind.AmpersandEqualsToken:
				case SyntaxKind.CaretEqualsToken:
				case SyntaxKind.BarEqualsToken:
				case SyntaxKind.LessThanLessThanEqualsToken:
				case SyntaxKind.GreaterThanGreaterThanEqualsToken:
				case SyntaxKind.QuestionToken:
				case SyntaxKind.ColonToken:
				case SyntaxKind.BarBarToken:
				case SyntaxKind.AmpersandAmpersandToken:
				case SyntaxKind.BarToken:
				case SyntaxKind.CaretToken:
				case SyntaxKind.AmpersandToken:
				case SyntaxKind.EqualsEqualsToken:
				case SyntaxKind.ExclamationEqualsToken:
				case SyntaxKind.LessThanToken:
				case SyntaxKind.LessThanEqualsToken:
				case SyntaxKind.GreaterThanToken:
				case SyntaxKind.GreaterThanEqualsToken:
				case SyntaxKind.LessThanLessThanToken:
				case SyntaxKind.GreaterThanGreaterThanToken:
				case SyntaxKind.PlusToken:
				case SyntaxKind.MinusToken:
				case SyntaxKind.AsteriskToken:
				case SyntaxKind.SlashToken:
				case SyntaxKind.PercentToken:
				case SyntaxKind.PlusPlusToken:
				case SyntaxKind.MinusMinusToken:
				case SyntaxKind.OpenBracketToken:
				case SyntaxKind.DotToken:
				case SyntaxKind.MinusGreaterThanToken:
				//case SyntaxKind.QuestionQuestionToken:
				case SyntaxKind.EndOfFileToken:
					return false;
				default:
					return true;
			}
		}

		private bool IsAnonymousType()
		{
			return this.CurrentToken.Kind == SyntaxKind.NewKeyword && this.PeekToken(1).Kind == SyntaxKind.OpenBraceToken;
		}

		private bool IsAnonymousTypeMemberExpression(ExpressionSyntax expr)
		{
			if (expr.Kind == SyntaxKind.QualifiedName)
			{
				return IsAnonymousTypeMemberExpression(((QualifiedNameSyntax)expr).Right);
			}

			return expr.Kind == SyntaxKind.IdentifierName || expr.Kind == SyntaxKind.SimpleMemberAccessExpression;
		}

		private bool IsInitializerMember()
		{
			return this.IsComplexElementInitializer() ||
				this.IsNamedAssignment() ||
				this.IsDictionaryInitializer() ||
				this.IsPossibleExpression();
		}

		private bool IsComplexElementInitializer()
		{
			return this.CurrentToken.Kind == SyntaxKind.OpenBraceToken;
		}

		private bool IsDictionaryInitializer()
		{
			return this.CurrentToken.Kind == SyntaxKind.OpenBracketToken;
		}

		private bool IsNamedAssignment()
		{
			return IsTrueIdentifier() && this.PeekToken(1).Kind == SyntaxKind.EqualsToken;
		}

		private static int GetNumberOfNonOmittedArraySizes(ArrayRankSpecifierSyntax rankSpec)
		{
			int count = rankSpec.Sizes.Count;
			int result = 0;
			for (int i = 0; i < count; i++)
			{
				if (rankSpec.Sizes[i].Kind != SyntaxKind.OmittedArraySizeExpression)
				{
					result++;
				}
			}
			return result;
		}




		private new ResetPoint GetResetPoint()
		{
			return new ResetPoint(base.GetResetPoint(), 
				_termState, 
				_isInTry
				);
		}

		private void Reset(ref ResetPoint state)
		{
			this._termState = state.TerminatorState;
			this._isInTry = state.IsInTry;
			base.Reset(ref state.BaseResetPoint);
		}

		private void Release(ref ResetPoint state)
		{
			base.Release(ref state.BaseResetPoint);
		}


		internal TNode ConsumeUnexpectedTokens<TNode>(TNode node) where TNode : CSharpSyntaxNode
		{
			if (this.CurrentToken.Kind == SyntaxKind.EndOfFileToken) return node;
			SyntaxListBuilder<SyntaxToken> b = this._pool.Allocate<SyntaxToken>();

			while (this.CurrentToken.Kind != SyntaxKind.EndOfFileToken)
			{
				b.Add(this.EatToken());
			}

			var trailingTrash = b.ToList();

			this._pool.Free(b);


			node = this.AddError(node, ErrorCode.ERR_UnexpectedCharacter, trailingTrash[0].ToString());
				// TODO: better diagnostic?
			node = this.AddTrailingSkippedSyntax(node, trailingTrash.Node);
			return node;
		}
	}
}