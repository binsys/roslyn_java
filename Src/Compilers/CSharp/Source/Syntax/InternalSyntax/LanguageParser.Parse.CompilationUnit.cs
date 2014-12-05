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
		private enum PackageParts
		{
			None = 0,
			Package = 2,
			Imports = 3,
			Members = 4,
		}

		internal CompilationUnitSyntax ParseCompilationUnit()
		{
			SyntaxListBuilder initialBadNodes = null;
			var body = new CompilationUnitBodyBuilder(this._pool);
			try
			{
				this.ParseCompilationUnitBody(ref body, ref initialBadNodes, SyntaxKind.CompilationUnit);
				var eof = this.EatToken(SyntaxKind.EndOfFileToken);
				var result = _syntaxFactory.CompilationUnit(body.Package, body.Imports, body.Members, eof);
				if (initialBadNodes != null)
				{
					// attach initial bad nodes as leading trivia on first token
					result = AddLeadingSkippedSyntax(result, initialBadNodes.ToListNode());
					this._pool.Free(initialBadNodes);
				}
				return result;
			}
			finally
			{
				body.Free(this._pool);
			}
		}

		private void ParseCompilationUnitBody(ref CompilationUnitBodyBuilder body, ref SyntaxListBuilder initialBadNodes, SyntaxKind parentKind)
		{

			var saveTerm = this._termState;
			this._termState |= TerminatorState.IsPackageMemberStartOrStop;
			PackageParts seen = PackageParts.None;
			bool reportUnexpectedToken = true;
			var pendingIncompleteMembers = _pool.Allocate<MemberDeclarationSyntax>();
			var pendingAnnotations = _pool.Allocate<AnnotationSyntax>();
			try
			{
				while (true)
				{
					switch (this.CurrentToken.Kind)
					{
						case SyntaxKind.AtToken:
							if (this.IsPossiblePackageAnnotations())
							{
								// incomplete members must be processed before we add any nodes to the body:
								ReduceIncompleteMembers(ref pendingIncompleteMembers, ref body, ref initialBadNodes);
								this.ParseAnnotationDeclarations(pendingAnnotations);
								reportUnexpectedToken = true;
								break;
							}
							goto default;
						case SyntaxKind.PackageKeyword:

							// incomplete members must be processed before we add any nodes to the body:
							ReduceIncompleteMembers(ref pendingIncompleteMembers, ref body, ref initialBadNodes);

							var package = this.ParsePackageDeclaration(ref pendingAnnotations);
							pendingAnnotations.Clear();
							if (seen > PackageParts.Package)
							{
								package = this.AddError(package, ErrorCode.ERR_UsingAfterElements);

								//package 必须在所有using之前
								this.AddSkippedNamespaceText(ref body, ref initialBadNodes, package);
							}
							else if (seen == PackageParts.Package)
							{
								//package 重复
								package = this.AddError(package, ErrorCode.ERR_NamespaceUnexpected);
								this.AddSkippedNamespaceText(ref body, ref initialBadNodes, package);
							}
							else
							{
								body.Package = package;
								seen = PackageParts.Package;
							}

							reportUnexpectedToken = true;
							break;
						case SyntaxKind.ImportKeyword:

							// incomplete members must be processed before we add any nodes to the body:
							ReduceIncompleteMembers(ref pendingIncompleteMembers, ref body, ref initialBadNodes);
							var import = this.ParseImportDeclaration();
							if (seen > PackageParts.Imports)
							{
								import = this.AddError(import, ErrorCode.ERR_UsingAfterElements);
								this.AddSkippedNamespaceText(ref body, ref initialBadNodes, import);
							}
							else
							{
								body.Imports.Add(import);
								seen = PackageParts.Imports;
							}
							reportUnexpectedToken = true;
							break;
						case SyntaxKind.EndOfFileToken:
							// This token marks the end of a namespace body
							return;
						default:
							var memberDeclaration = this.ParseMemberDeclaration(parentKind);
							if (memberDeclaration == null)
							{
								// incomplete members must be processed before we add any nodes to the body:
								ReduceIncompleteMembers(ref pendingIncompleteMembers, ref body, ref initialBadNodes);

								// eat one token and try to parse declaration or statement again:
								var skippedToken = EatToken();
								if (reportUnexpectedToken && !skippedToken.ContainsDiagnostics)
								{
									skippedToken = this.AddError(skippedToken, ErrorCode.ERR_EOFExpected);

									// do not report the error multiple times for subsequent tokens:
									reportUnexpectedToken = false;
								}

								this.AddSkippedNamespaceText(ref body, ref initialBadNodes, skippedToken);
							}
							else if (memberDeclaration.Kind == SyntaxKind.IncompleteMember && seen < PackageParts.Members)
							{
								pendingIncompleteMembers.Add(memberDeclaration);
								reportUnexpectedToken = true;
							}
							else
							{
								// incomplete members must be processed before we add any nodes to the body:
								AddIncompleteMembers(ref pendingIncompleteMembers, ref body);

								body.Members.Add(memberDeclaration);
								seen = PackageParts.Members;
								reportUnexpectedToken = true;
							}
							break;
					}
				}
			}
			finally
			{
				this._termState = saveTerm;

				// adds pending incomplete nodes:
				AddIncompleteMembers(ref pendingIncompleteMembers, ref body);
				_pool.Free(pendingIncompleteMembers);
				_pool.Free(pendingAnnotations);
			}
		}

		private JavaPackageDeclarationSyntax ParsePackageDeclaration(ref SyntaxListBuilder<AnnotationSyntax> pendingAnnotations)
		{
			if (this.IsIncrementalAndFactoryContextMatches && this.CurrentNodeKind == SyntaxKind.JavaPackageDeclaration)
			{
				return (JavaPackageDeclarationSyntax)this.EatNode();
			}

			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.PackageKeyword);

			var usingToken = this.EatToken(SyntaxKind.PackageKeyword);

			NameSyntax name;
			SyntaxToken semicolon;

			if (IsPossiblePackageMemberDeclaration())
			{
				//We're worried about the case where someone already has a correct program
				//and they've gone back to add a using directive, but have not finished the
				//new directive.  e.g.
				//
				//    using 
				//    namespace Foo {
				//        //...
				//    }
				//
				//If the token we see after "using" could be its own top-level construct, then
				//we just want to insert a missing identifier and semicolon and then return to
				//parsing at the top-level.
				//
				//NB: there's no way this could be true for a set of tokens that form a valid 
				//using directive, so there's no danger in checking the error case first.

				name = WithAdditionalDiagnostics(CreateMissingIdentifierName(), GetExpectedTokenError(SyntaxKind.IdentifierToken, this.CurrentToken.Kind));
				semicolon = SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken);
			}
			else
			{
				name = this.ParseQualifiedName();

				if (NameContainsGeneric(name))
				{
					// We're not allowed to have generics.
					name = this.AddError(name, ErrorCode.ERR_UnexpectedGenericName);
				}

				if (name.IsMissing && this.PeekToken(1).Kind == SyntaxKind.SemicolonToken)
				{
					//if we can see a semicolon ahead, then the current token was
					//probably supposed to be an identifier
					name = AddTrailingSkippedSyntax(name, this.EatToken());
				}
				semicolon = this.EatToken(SyntaxKind.SemicolonToken);
			}

			return _syntaxFactory.JavaPackageDeclaration(pendingAnnotations.ToList(), usingToken, name, semicolon);
		}

		private ImportDeclarationSyntax ParseImportDeclaration()
		{
			if (this.IsIncrementalAndFactoryContextMatches && this.CurrentNodeKind == SyntaxKind.ImportDeclaration)
			{
				return (ImportDeclarationSyntax)this.EatNode();
			}

			Debug.Assert(this.CurrentToken.Kind == SyntaxKind.ImportKeyword);

			var importKeywordToken = this.EatToken(SyntaxKind.ImportKeyword);

			SyntaxToken staticKeyword = null;
			if (this.CurrentToken.Kind == SyntaxKind.StaticKeyword)
			{
				staticKeyword = this.EatToken(SyntaxKind.StaticKeyword);
			}

			NameSyntax name;
			SyntaxToken semicolon;
			ImportOnDemandSuffixSyntax importOnDemand = null;
			//this.ParseTypeName()

			if (IsPossiblePackageMemberDeclaration())
			{
				//We're worried about the case where someone already has a correct program
				//and they've gone back to add a using directive, but have not finished the
				//new directive.  e.g.
				//
				//    import 
				//    class Foo {
				//        //...
				//    }
				//
				//If the token we see after "using" could be its own top-level construct, then
				//we just want to insert a missing identifier and semicolon and then return to
				//parsing at the top-level.
				//
				//NB: there's no way this could be true for a set of tokens that form a valid 
				//using directive, so there's no danger in checking the error case first.

				name = WithAdditionalDiagnostics(CreateMissingIdentifierName(), GetExpectedTokenError(SyntaxKind.IdentifierToken, this.CurrentToken.Kind));

				if (this.CurrentToken.Kind == SyntaxKind.DotToken && this.PeekToken(1).Kind == SyntaxKind.AsteriskToken)
				{
					importOnDemand = _syntaxFactory.ImportOnDemandSuffix(this.EatToken(SyntaxKind.DotToken),
						this.EatToken(SyntaxKind.AsteriskToken));
				}
				semicolon = SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken);
			}
			else
			{
				//var a = this.ParseType(false);
				name = this.ParseQualifiedName();

				if (name.IsMissing && this.PeekToken(1).Kind == SyntaxKind.SemicolonToken)
				{
					//if we can see a semicolon ahead, then the current token was
					//probably supposed to be an identifier
					name = AddTrailingSkippedSyntax(name, this.EatToken());
				}

				if (this.CurrentToken.Kind == SyntaxKind.DotToken && this.PeekToken(1).Kind == SyntaxKind.AsteriskToken)
				{
					importOnDemand = _syntaxFactory.ImportOnDemandSuffix(this.EatToken(SyntaxKind.DotToken),
						this.EatToken(SyntaxKind.AsteriskToken));
				}


				semicolon = this.EatToken(SyntaxKind.SemicolonToken);
			}

			return _syntaxFactory.ImportDeclaration(importKeywordToken, staticKeyword, name, importOnDemand, semicolon);
		}

	}
}