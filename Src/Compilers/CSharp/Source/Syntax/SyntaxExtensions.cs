﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
	public static class SyntaxExtensions
	{
		internal const string DefaultIndentation = "    ";

		/// <summary>
		/// Creates a new syntax token with all whitespace and end of line trivia replaced with
		/// regularly formatted trivia.
		/// </summary>
		/// <param name="token">The token to normalize.</param>
		/// <param name="indentation">An optional sequence of whitespace characters that defines a
		/// single level of indentation.</param>
		/// <param name="elasticTrivia">If true the replaced trivia is elastic trivia.</param>
		public static SyntaxToken NormalizeWhitespace(this SyntaxToken token, string indentation = DefaultIndentation, bool elasticTrivia = false)
		{
			return SyntaxFormatter.Format(token, indentation, elasticTrivia);
		}

		/// <summary>
		/// Creates a new syntax trivia list with all whitespace and end of line trivia replaced with
		/// regularly formatted trivia.
		/// </summary>
		/// <param name="list">The trivia list to normalize.</param>
		/// <param name="indentation">An optional sequence of whitespace characters that defines a
		/// single level of indentation.</param>
		/// <param name="elasticTrivia">If true the replaced trivia is elastic trivia.</param>
		public static SyntaxTriviaList NormalizeWhitespace(this SyntaxTriviaList list, string indentation = DefaultIndentation, bool elasticTrivia = false)
		{
			return SyntaxFormatter.Format(list, indentation, elasticTrivia);
		}

		public static SyntaxTriviaList ToSyntaxTriviaList(this IEnumerable<SyntaxTrivia> sequence)
		{
			return SyntaxFactory.TriviaList(sequence);
		}

		internal static XmlNameAttributeElementKind GetElementKind(this XmlNameAttributeSyntax attributeSyntax)
		{
			CSharpSyntaxNode parentSyntax = attributeSyntax.Parent;
			SyntaxKind parentKind = parentSyntax.Kind;

			string parentName;
			if (parentKind == SyntaxKind.XmlEmptyElement)
			{
				var parent = (XmlEmptyElementSyntax)parentSyntax;
				parentName = parent.Name.LocalName.ValueText;
				Debug.Assert((object)parent.Name.Prefix == null);
			}
			else if (parentKind == SyntaxKind.XmlElementStartTag)
			{
				var parent = (XmlElementStartTagSyntax)parentSyntax;
				parentName = parent.Name.LocalName.ValueText;
				Debug.Assert((object)parent.Name.Prefix == null);
			}
			else
			{
				throw ExceptionUtilities.UnexpectedValue(parentKind);
			}

			if (DocumentationCommentXmlNames.ElementEquals(parentName, DocumentationCommentXmlNames.ParameterElementName))
			{
				return XmlNameAttributeElementKind.Parameter;
			}
			else if (DocumentationCommentXmlNames.ElementEquals(parentName, DocumentationCommentXmlNames.ParameterReferenceElementName))
			{
				return XmlNameAttributeElementKind.ParameterReference;
			}
			else if (DocumentationCommentXmlNames.ElementEquals(parentName, DocumentationCommentXmlNames.TypeParameterElementName))
			{
				return XmlNameAttributeElementKind.TypeParameter;
			}
			else if (DocumentationCommentXmlNames.ElementEquals(parentName, DocumentationCommentXmlNames.TypeParameterReferenceElementName))
			{
				return XmlNameAttributeElementKind.TypeParameterReference;
			}
			else
			{
				throw ExceptionUtilities.UnexpectedValue(parentName);
			}
		}

		internal static bool ReportDocumentationCommentDiagnostics(this SyntaxTree tree)
		{
			return tree.Options.DocumentationMode >= DocumentationMode.Diagnose;
		}

		/// <summary>
		/// Updates the given SimpleNameSyntax node with the given identifier token.
		/// This function is a wrapper that calls WithIdentifier on derived syntax nodes.
		/// </summary>
		/// <param name="simpleName"></param>
		/// <param name="identifier"></param>
		/// <returns>The given simple name updated with the given identifier.</returns>
		public static SimpleNameSyntax WithIdentifier(this SimpleNameSyntax simpleName, SyntaxToken identifier)
		{
			return simpleName.Kind == SyntaxKind.IdentifierName
				? (SimpleNameSyntax)((IdentifierNameSyntax)simpleName).WithIdentifier(identifier)
				: (SimpleNameSyntax)((GenericNameSyntax)simpleName).WithIdentifier(identifier);
		}

		internal static bool IsTypeInContextWhichNeedsDynamicAttribute(this IdentifierNameSyntax typeNode)
		{
			Debug.Assert(typeNode != null);
			Debug.Assert(SyntaxKindFacts.IsInTypeOnlyContext(typeNode));

			return IsInContextWhichNeedsDynamicAttribute(typeNode);
		}

		internal static CSharpSyntaxNode SkipParens(this CSharpSyntaxNode expression)
		{
			while (expression != null && expression.Kind == SyntaxKind.ParenthesizedExpression)
			{
				expression = ((ParenthesizedExpressionSyntax)expression).Expression;
			}

			return expression;
		}

		private static bool IsInContextWhichNeedsDynamicAttribute(CSharpSyntaxNode node)
		{
			Debug.Assert(node != null);

			switch (node.Kind)
			{
				case SyntaxKind.Parameter:
				case SyntaxKind.FieldDeclaration:
				case SyntaxKind.MethodDeclaration:
				case SyntaxKind.ImplementsListClause:
					return true;

				case SyntaxKind.Block:
				case SyntaxKind.VariableDeclarator:
				case SyntaxKind.TypeBound:
				case SyntaxKind.Annotation:
				case SyntaxKind.EqualsValueClause:
					return false;

				default:
					return node.Parent != null && IsInContextWhichNeedsDynamicAttribute(node.Parent);
			}
		}
	}
}
