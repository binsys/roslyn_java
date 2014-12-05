// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp
{
	public static partial class SyntaxKindFacts
	{
		public static bool IsAttributeName(SyntaxNode node)
		{
			var parent = node.Parent;
			if (parent == null || !IsName(node.CSharpKind()))
			{
				return false;
			}

			switch (parent.CSharpKind())
			{
				case SyntaxKind.QualifiedName:
					var qn = (QualifiedNameSyntax)parent;
					return qn.Right == node ? IsAttributeName(parent) : false;

			}

			var p = node.Parent as AnnotationSyntax;
			return p != null && p.Name == node;
		}

		/// <summary>
		/// Returns true if the node is the object of an invocation expression.
		/// </summary>
		public static bool IsInvoked(ExpressionSyntax node)
		{
			node = (ExpressionSyntax)SyntaxFactory.GetStandaloneExpression(node);
			var inv = node.Parent as InvocationExpressionSyntax;
			return inv != null && inv.Expression == node;
		}

		/// <summary>
		/// Returns true if the node is the object of an element access expression.
		/// </summary>
		public static bool IsIndexed(ExpressionSyntax node)
		{
			node = (ExpressionSyntax)SyntaxFactory.GetStandaloneExpression(node);
			var indexer = node.Parent as ElementAccessExpressionSyntax;
			return indexer != null && indexer.Expression == node;
		}


		/// <summary>
		/// Returns true if the node is in a tree location that is expected to be a type
		/// </summary>
		/// <param name="node"></param>
		/// <returns></returns>
		public static bool IsInTypeOnlyContext(ExpressionSyntax node)
		{
			node = (ExpressionSyntax)SyntaxFactory.GetStandaloneExpression(node);
			var parent = node.Parent;
			if (parent != null)
			{
				switch (parent.Kind)
				{
					case SyntaxKind.Annotation:
						return ((AnnotationSyntax)parent).Name == node;

					case SyntaxKind.ArrayType:
						return ((ArrayTypeSyntax)parent).ElementType == node;

					case SyntaxKind.PredefinedType:
						return true;

					//case SyntaxKind.NullableType:
					//	return ((NullableTypeSyntax)parent).ElementType == node;

					case SyntaxKind.TypeArgumentList:
						// all children of GenericNames are type arguments
						return true;

					case SyntaxKind.CastExpression:
						return ((CastExpressionSyntax)parent).Type == node;

					case SyntaxKind.ObjectCreationExpression:
						return ((ObjectCreationExpressionSyntax)parent).Type == node;


					case SyntaxKind.VariableDeclaration:
						return ((VariableDeclarationSyntax)parent).Type == node;


					case SyntaxKind.CatchDeclaration:
						return ((CatchDeclarationSyntax)parent).Type == node;


					case SyntaxKind.InstanceOfExpression:
						return ((BinaryExpressionSyntax)parent).Right == node;
					case SyntaxKind.DefaultExpression:
						return ((DefaultExpressionSyntax)parent).Type == node;

					case SyntaxKind.Parameter:
						return ((ParameterSyntax)parent).Type == node;

					case SyntaxKind.TypeConstraint:
						return ((TypeConstraintSyntax)parent).Type == node;

					case SyntaxKind.MethodDeclaration:
						return ((MethodDeclarationSyntax)parent).ReturnType == node;


					case SyntaxKind.ImplementsListClause:
						return true;  // children of BaseListSyntax are only types

					case SyntaxKind.CrefParameter:
						return true;
				}
			}

			return false;
		}

		/// <summary>
		/// Returns true if a node is in a tree location that is expected to be either a namespace or type
		/// </summary>
		/// <param name="node"></param>
		/// <returns></returns>
		public static bool IsInNamespaceOrTypeContext(ExpressionSyntax node)
		{
			if (node != null)
			{
				node = (ExpressionSyntax)SyntaxFactory.GetStandaloneExpression(node);
				var parent = node.Parent;
				if (parent != null)
				{
					switch (parent.Kind)
					{
						case SyntaxKind.ImportDeclaration:
							return ((ImportDeclarationSyntax)parent).Name == node;

						case SyntaxKind.QualifiedName:
							// left of QN is namespace or type.  Note: when you have "a.b.c()", then
							// "a.b" is not a qualified name, it is a member access expression.
							// Qualified names are only parsed when the parser knows it's a type only
							// context.
							return ((QualifiedNameSyntax)parent).Left == node;

						default:
							return IsInTypeOnlyContext(node);
					}
				}
			}

			return false;
		}

		/// <summary>
		/// Is the node the name of a named argument of an invocation, object creation expression, 
		/// constructor initializer, or element access, but not an _annotation.
		/// </summary>
		public static bool IsNamedArgumentName(SyntaxNode node)
		{
			// An argument name is an IdentifierName inside a NameColon, inside an Argument, inside an ArgumentList, inside an
			// Invocation, ObjectCreation, ObjectInitializer, or ElementAccess.

			if (!node.IsKind(SyntaxKind.IdentifierName))
				return false;

			var parent1 = node.Parent;
			if (parent1 == null || !parent1.IsKind(SyntaxKind.NameColon))
				return false;

			var parent2 = parent1.Parent;
			if (parent2 == null || !(parent2.IsKind(SyntaxKind.Argument) || parent2.IsKind(SyntaxKind.AnnotationArgument)))
				return false;

			var parent3 = parent2.Parent;
			if (parent3 == null || !(parent3 is BaseArgumentListSyntax || parent3.IsKind(SyntaxKind.AnnotationArgumentList)))
				return false;

			var parent4 = parent3.Parent;
			if (parent4 == null)
				return false;

			switch (parent4.CSharpKind())
			{
				case SyntaxKind.InvocationExpression:
				case SyntaxKind.ObjectCreationExpression:
				case SyntaxKind.ObjectInitializerExpression:
				case SyntaxKind.ElementAccessExpression:
				case SyntaxKind.Annotation:
				case SyntaxKind.BaseConstructorInitializer:
				case SyntaxKind.ThisConstructorInitializer:
					return true;
				default:
					return false;
			}
		}

		/// <summary>
		/// Is the expression the initializer in a fixed statement?
		/// </summary>
		public static bool IsFixedStatementExpression(SyntaxNode node)
		{
			node = node.Parent;
			// Dig through parens because dev10 does (even though the spec doesn't say so)
			// Dig through casts because there's a special error code (CS0254) for such casts.
			while (node != null && (node.IsKind(SyntaxKind.ParenthesizedExpression) || node.IsKind(SyntaxKind.CastExpression))) node = node.Parent;
			if (node == null || !node.IsKind(SyntaxKind.EqualsValueClause)) return false;
			node = node.Parent;
			if (node == null || !node.IsKind(SyntaxKind.VariableDeclarator)) return false;
			node = node.Parent;
			if (node == null || !node.IsKind(SyntaxKind.VariableDeclaration)) return false;
			node = node.Parent;
			return node != null && node.IsKind(SyntaxKind.FixedStatement);
		}

		public static string GetText(Accessibility accessibility)
		{
			switch (accessibility)
			{
				case Accessibility.NotApplicable:
					return string.Empty;
				case Accessibility.Private:
					return SyntaxKindFacts.GetText(SyntaxKind.PrivateKeyword);
				case Accessibility.Protected:
					return SyntaxKindFacts.GetText(SyntaxKind.ProtectedKeyword);
				case Accessibility.Public:
					return SyntaxKindFacts.GetText(SyntaxKind.PublicKeyword);
				default:
					System.Diagnostics.Debug.Assert(false, string.Format("Unknown accessibility '{0}'", accessibility));
					return null;
			}
		}
	}
}