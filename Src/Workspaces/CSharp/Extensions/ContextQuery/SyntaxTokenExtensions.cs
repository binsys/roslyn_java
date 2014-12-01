// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.Extensions.ContextQuery
{
    internal static class SyntaxTokenExtensions
    {
        public static bool IsUsingOrExternKeyword(this SyntaxToken token)
        {
            return
                token.CSharpKind() == SyntaxKind.ImportKeyword ||
                token.CSharpKind() == SyntaxKind.NativeKeyword;
        }

        public static bool IsBeginningOfStatementContext(this SyntaxToken token)
        {
            // cases:
            //    {
            //      |

            // }
            // |

            // Note, the following is *not* a legal statement context: 
            //    do { } |

            // ...;
            // |

            // case 0:
            //   |

            // default:
            //   |

            // label:
            //   |

            // if (foo)
            //   |

            // while (true)
            //   |

            // do
            //   |

            // for (;;)
            //   |

            // foreach (var v in c)
            //   |

            // else
            //   |

            // using (expr)
            //   |

            // lock (expr)
            //   |

            // for ( ; ; Foo(), |

            if (token.CSharpKind() == SyntaxKind.OpenBraceToken &&
                token.IsParentKind(SyntaxKind.Block))
            {
                return true;
            }

            if (token.CSharpKind() == SyntaxKind.SemicolonToken)
            {
                var statement = token.GetAncestor<StatementSyntax>();
                if (statement != null && !statement.IsParentKind(SyntaxKind.GlobalStatement) &&
                    statement.GetLastToken(includeZeroWidth: true) == token)
                {
                    return true;
                }
            }

            if (token.CSharpKind() == SyntaxKind.CloseBraceToken &&
                token.IsParentKind(SyntaxKind.Block))
            {
                if (token.Parent.Parent is StatementSyntax)
                {
                    // Most blocks that are the child of statement are places
                    // that we can follow with another statement.  i.e.:
                    // if { }
                    // while () { }
                    // There are two exceptions.
                    // try {}
                    // do {}
                    if (!token.Parent.IsParentKind(SyntaxKind.TryStatement) &&
                        !token.Parent.IsParentKind(SyntaxKind.DoStatement))
                    {
                        return true;
                    }
                }
                else if (
                    token.Parent.IsParentKind(SyntaxKind.ElseClause) ||
                    token.Parent.IsParentKind(SyntaxKind.FinallyClause) ||
                    token.Parent.IsParentKind(SyntaxKind.CatchClause) ||
                    token.Parent.IsParentKind(SyntaxKind.SwitchSection))
                {
                    return true;
                }
            }

            if (token.CSharpKind() == SyntaxKind.CloseBraceToken &&
                token.IsParentKind(SyntaxKind.SwitchStatement))
            {
                return true;
            }

            if (token.CSharpKind() == SyntaxKind.ColonToken)
            {
                if (token.IsParentKind(SyntaxKind.CaseSwitchLabel) ||
                    token.IsParentKind(SyntaxKind.DefaultSwitchLabel) ||
                    token.IsParentKind(SyntaxKind.LabeledStatement))
                {
                    return true;
                }
            }

            if (token.CSharpKind() == SyntaxKind.DoKeyword &&
                token.IsParentKind(SyntaxKind.DoStatement))
            {
                return true;
            }

            if (token.CSharpKind() == SyntaxKind.CloseParenToken)
            {
                var parent = token.Parent;
                if (parent.IsKind(SyntaxKind.ForStatement) ||
                    parent.IsKind(SyntaxKind.ForEachStatement) ||
                    parent.IsKind(SyntaxKind.WhileStatement) ||
                    parent.IsKind(SyntaxKind.IfStatement) ||
                    parent.IsKind(SyntaxKind.LockStatement) ||
                    parent.IsKind(SyntaxKind.UsingStatement))
                {
                    return true;
                }
            }

            if (token.CSharpKind() == SyntaxKind.ElseKeyword)
            {
                return true;
            }

            return false;
        }

        public static bool IsBeginningOfGlobalStatementContext(this SyntaxToken token)
        {
            // cases:
            // }
            // |

            // ...;
            // |

            // extern alias Foo;
            // using System;
            // |

            // [asssembly: Foo]
            // |

            if (token.CSharpKind() == SyntaxKind.CloseBraceToken)
            {
                var memberDeclaration = token.GetAncestor<MemberDeclarationSyntax>();
                if (memberDeclaration != null && memberDeclaration.GetLastToken(includeZeroWidth: true) == token &&
                    memberDeclaration.IsParentKind(SyntaxKind.CompilationUnit))
                {
                    return true;
                }
            }

            if (token.CSharpKind() == SyntaxKind.SemicolonToken)
            {
                var globalStatement = token.GetAncestor<GlobalStatementSyntax>();
                if (globalStatement != null && globalStatement.GetLastToken(includeZeroWidth: true) == token)
                {
                    return true;
                }

                var memberDeclaration = token.GetAncestor<MemberDeclarationSyntax>();
                if (memberDeclaration != null && memberDeclaration.GetLastToken(includeZeroWidth: true) == token &&
                    memberDeclaration.IsParentKind(SyntaxKind.CompilationUnit))
                {
                    return true;
                }

                var compUnit = token.GetAncestor<CompilationUnitSyntax>();
                if (compUnit != null)
                {
                    if (compUnit.Usings.Count > 0 && compUnit.Usings.Last().GetLastToken(includeZeroWidth: true) == token)
                    {
                        return true;
                    }

                    if (compUnit.Externs.Count > 0 && compUnit.Externs.Last().GetLastToken(includeZeroWidth: true) == token)
                    {
                        return true;
                    }
                }
            }

            if (token.CSharpKind() == SyntaxKind.CloseBracketToken)
            {
                var compUnit = token.GetAncestor<CompilationUnitSyntax>();
                if (compUnit != null)
                {
                    if (compUnit.AttributeLists.Count > 0 && compUnit.AttributeLists.Last().GetLastToken(includeZeroWidth: true) == token)
                    {
                        return true;
                    }
                }
            }

            return false;
        }

        public static bool IsAfterPossibleCast(this SyntaxToken token)
        {
            if (token.CSharpKind() == SyntaxKind.CloseParenToken)
            {
                if (token.IsParentKind(SyntaxKind.CastExpression))
                {
                    return true;
                }

                if (token.IsParentKind(SyntaxKind.ParenthesizedExpression))
                {
                    var parenExpr = token.Parent as ParenthesizedExpressionSyntax;
                    var expr = parenExpr.Expression;

                    if (expr is TypeSyntax)
                    {
                        return true;
                    }
                }
            }

            return false;
        }

        public static bool IsLastTokenOfNode<T>(this SyntaxToken token)
            where T : SyntaxNode
        {
            var node = token.GetAncestor<T>();
            return node != null && token == node.GetLastToken(includeZeroWidth: true);
        }

        public static bool IsLastTokenOfQueryClause(this SyntaxToken token)
        {
            if (token.IsLastTokenOfNode<QueryClauseSyntax>())
            {
                return true;
            }

            if (token.CSharpKind() == SyntaxKind.IdentifierToken &&
                token.GetPreviousToken(includeSkipped: true).CSharpKind() == SyntaxKind.IntoKeyword)
            {
                return true;
            }

            // Check to see if the parser has treated this as a partially typed declaration expression.
            if (token.IsKind(SyntaxKind.IdentifierToken) &&
                token.IsParentKind(SyntaxKind.IdentifierName) &&
                token.Parent.IsParentKind(SyntaxKind.DeclarationExpression))
            {
                var queryClause = token.Parent.FirstAncestorOrSelf<QueryClauseSyntax>();
                if (queryClause == null)
                {
                    return false;
                }

                var declarationExpression = (DeclarationExpressionSyntax)token.Parent.Parent;
                if (declarationExpression.Type == token.Parent &&
                    declarationExpression.Variable != null &&
                    declarationExpression.Variable.Initializer == null)
                {
                    return true;
                }
            }

            return false;
        }

        public static bool IsPreProcessorExpressionContext(this SyntaxToken targetToken)
        {
            // cases:
            //   #if |
            //   #if foo || |
            //   #if foo && |
            //   #if ( |
            //   #if ! |
            // Same for elif

            if (targetToken.GetAncestor<ConditionalDirectiveTriviaSyntax>() == null)
            {
                return false;
            }

            // #if
            // #elif
            if (targetToken.CSharpKind() == SyntaxKind.IfKeyword ||
                targetToken.CSharpKind() == SyntaxKind.ElifKeyword)
            {
                return true;
            }

            // ( |
            if (targetToken.CSharpKind() == SyntaxKind.OpenParenToken &&
                targetToken.IsParentKind(SyntaxKind.ParenthesizedExpression))
            {
                return true;
            }

            // ! |
            if (targetToken.Parent is PrefixUnaryExpressionSyntax)
            {
                var prefix = targetToken.Parent as PrefixUnaryExpressionSyntax;
                return prefix.OperatorToken == targetToken;
            }

            // a &&
            // a ||
            if (targetToken.Parent is BinaryExpressionSyntax)
            {
                var binary = targetToken.Parent as BinaryExpressionSyntax;
                return binary.OperatorToken == targetToken;
            }

            return false;
        }

        public static bool IsOrderByDirectionContext(this SyntaxToken targetToken)
        {
            // cases:
            //   orderby a |
            //   orderby a a|
            //   orderby a, b |
            //   orderby a, b a|

            if (targetToken.CSharpKind() != SyntaxKind.IdentifierToken)
            {
                return false;
            }

            var ordering = targetToken.GetAncestor<OrderingSyntax>();
            if (ordering == null)
            {
                return false;
            }

            // orderby a |
            // orderby a, b |
            var lastToken = ordering.Expression.GetLastToken(includeSkipped: true);

            // In the following case, the parser may treat a partially typed keyword as part of a
            // declaration expression:
            //   orderby a a|
            if (ordering.Expression.IsKind(SyntaxKind.DeclarationExpression))
            {
                lastToken = lastToken.GetPreviousToken(includeSkipped: true);
            }

            if (targetToken == lastToken)
            {
                return true;
            }

            return false;
        }

        public static bool IsSwitchLabelContext(this SyntaxToken targetToken)
        {
            // cases:
            //   case X: |
            //   default: |
            //   switch (e) { |
            //
            //   case X: Statement(); |

            if (targetToken.CSharpKind() == SyntaxKind.OpenBraceToken &&
                targetToken.IsParentKind(SyntaxKind.SwitchStatement))
            {
                return true;
            }

            if (targetToken.CSharpKind() == SyntaxKind.ColonToken)
            {
                if (targetToken.IsParentKind(SyntaxKind.CaseSwitchLabel) ||
                    targetToken.IsParentKind(SyntaxKind.DefaultSwitchLabel))
                {
                    return true;
                }
            }

            if (targetToken.CSharpKind() == SyntaxKind.SemicolonToken ||
                targetToken.CSharpKind() == SyntaxKind.CloseBraceToken)
            {
                var section = targetToken.GetAncestor<SwitchSectionSyntax>();
                if (section != null)
                {
                    foreach (var statement in section.Statements)
                    {
                        if (targetToken == statement.GetLastToken(includeSkipped: true))
                        {
                            return true;
                        }
                    }
                }
            }

            return false;
        }

        public static bool IsXmlCrefParameterModifierContext(this SyntaxToken targetToken)
        {
            return targetToken.MatchesKind(SyntaxKind.CommaToken, SyntaxKind.OpenParenToken)
                && targetToken.Parent.MatchesKind(SyntaxKind.CrefBracketedParameterList, SyntaxKind.CrefParameterList);
        }

        public static bool IsConstructorOrMethodParameterArgumentContext(this SyntaxToken targetToken)
        {
            // cases:
            //   Foo( |
            //   Foo(expr, |
            //   Foo(bar: |
            //   new Foo( |
            //   new Foo(expr, |
            //   new Foo(bar: |

            // Foo(bar: |
            if (targetToken.CSharpKind() == SyntaxKind.ColonToken &&
                targetToken.IsParentKind(SyntaxKind.NameColon) &&
                targetToken.Parent.IsParentKind(SyntaxKind.Argument) &&
                targetToken.Parent.GetParent().IsParentKind(SyntaxKind.ArgumentList))
            {
                var owner = targetToken.Parent.GetParent().GetParent().GetParent();
                if (owner.IsKind(SyntaxKind.InvocationExpression) ||
                    owner.IsKind(SyntaxKind.ObjectCreationExpression))
                {
                    return true;
                }
            }

            if (targetToken.CSharpKind() == SyntaxKind.OpenParenToken ||
                targetToken.CSharpKind() == SyntaxKind.CommaToken)
            {
                if (targetToken.IsParentKind(SyntaxKind.ArgumentList))
                {
                    if (targetToken.Parent.IsParentKind(SyntaxKind.InvocationExpression) ||
                        targetToken.Parent.IsParentKind(SyntaxKind.ObjectCreationExpression))
                    {
                        return true;
                    }
                }
            }

            return false;
        }

        public static bool IsUnaryOperatorContext(this SyntaxToken targetToken)
        {
            if (targetToken.CSharpKind() == SyntaxKind.OperatorKeyword &&
                targetToken.GetPreviousToken(includeSkipped: true).IsLastTokenOfNode<TypeSyntax>())
            {
                return true;
            }

            return false;
        }

        public static bool IsUnsafeContext(this SyntaxToken targetToken)
        {
            return
                targetToken.GetAncestors<StatementSyntax>().Any(s => s.IsKind(SyntaxKind.UnsafeStatement)) ||
                targetToken.GetAncestors<MemberDeclarationSyntax>().Any(m => m.GetModifiers().Any(SyntaxKind.UnsafeKeyword));
        }

        public static bool IsAfterYieldKeyword(this SyntaxToken targetToken)
        {
            // yield |
            // yield r|

            if (targetToken.IsKindOrHasMatchingText(SyntaxKind.YieldKeyword))
            {
                return true;
            }

            return false;
        }

        public static bool IsAccessorDeclarationContext<TMemberNode>(this SyntaxToken targetToken, int position, SyntaxKind kind = SyntaxKind.None)
            where TMemberNode : SyntaxNode
        {
            if (!IsAccessorDeclarationContextWorker(targetToken))
            {
                return false;
            }

            var list = targetToken.GetAncestor<AccessorListSyntax>();
            if (list == null)
            {
                return false;
            }

            // Check if we already have this accessor.  (however, don't count it
            // if the user is *on* that accessor.
            var existingAccessor = list.Accessors
                .Select(a => a.Keyword)
                .FirstOrDefault(a => !a.IsMissing && a.IsKindOrHasMatchingText(kind));

            if (existingAccessor.CSharpKind() != SyntaxKind.None)
            {
                var existingAccessorSpan = existingAccessor.Span;
                if (!existingAccessorSpan.IntersectsWith(position))
                {
                    return false;
                }
            }

            var decl = targetToken.GetAncestor<TMemberNode>();
            return decl != null;
        }

        private static bool IsAccessorDeclarationContextWorker(SyntaxToken targetToken)
        {
            // cases:
            //   int Foo { |
            //   int Foo { private |
            //   int Foo { set { } |
            //   int Foo { set; |
            //   int Foo { [Bar]|

            // Consume all preceding access modifiers
            while (targetToken.CSharpKind() == SyntaxKind.InternalKeyword ||
                targetToken.CSharpKind() == SyntaxKind.PublicKeyword ||
                targetToken.CSharpKind() == SyntaxKind.ProtectedKeyword ||
                targetToken.CSharpKind() == SyntaxKind.PrivateKeyword)
            {
                targetToken = targetToken.GetPreviousToken(includeSkipped: true);
            }

            // int Foo { |
            // int Foo { private |
            if (targetToken.CSharpKind() == SyntaxKind.OpenBraceToken &&
                targetToken.IsParentKind(SyntaxKind.AccessorList))
            {
                return true;
            }

            // int Foo { set { } |
            // int Foo { set { } private |
            if (targetToken.CSharpKind() == SyntaxKind.CloseBraceToken &&
                targetToken.IsParentKind(SyntaxKind.Block) &&
                targetToken.Parent.GetParent() is AccessorDeclarationSyntax)
            {
                return true;
            }

            // int Foo { set; |
            if (targetToken.CSharpKind() == SyntaxKind.SemicolonToken &&
                targetToken.Parent is AccessorDeclarationSyntax)
            {
                return true;
            }

            // int Foo { [Bar]|
            if (targetToken.CSharpKind() == SyntaxKind.CloseBracketToken &&
                targetToken.IsParentKind(SyntaxKind.AttributeList) &&
                targetToken.Parent.GetParent() is AccessorDeclarationSyntax)
            {
                return true;
            }

            return false;
        }

        private static bool IsGenericInterfaceOrDelegateTypeParameterList(SyntaxNode node)
        {
            if (node.IsKind(SyntaxKind.TypeParameterList))
            {
                if (node.IsParentKind(SyntaxKind.InterfaceDeclaration))
                {
                    var decl = node.Parent as TypeDeclarationSyntax;
                    return decl.TypeParameterList == node;
                }
                else if (node.IsParentKind(SyntaxKind.DelegateDeclaration))
                {
                    var decl = node.Parent as DelegateDeclarationSyntax;
                    return decl.TypeParameterList == node;
                }
            }

            return false;
        }

        public static bool IsTypeParameterVarianceContext(this SyntaxToken targetToken)
        {
            // cases:
            // interface IFoo<|
            // interface IFoo<A,|
            // interface IFoo<[Bar]|

            // deletate X D<|
            // deletate X D<A,|
            // deletate X D<[Bar]|
            if (targetToken.CSharpKind() == SyntaxKind.LessThanToken &&
                IsGenericInterfaceOrDelegateTypeParameterList(targetToken.Parent))
            {
                return true;
            }

            if (targetToken.CSharpKind() == SyntaxKind.CommaToken &&
                IsGenericInterfaceOrDelegateTypeParameterList(targetToken.Parent))
            {
                return true;
            }

            if (targetToken.CSharpKind() == SyntaxKind.CloseBracketToken &&
                targetToken.IsParentKind(SyntaxKind.AttributeList) &&
                targetToken.Parent.IsParentKind(SyntaxKind.TypeParameter) &&
                IsGenericInterfaceOrDelegateTypeParameterList(targetToken.Parent.GetParent().GetParent()))
            {
                return true;
            }

            return false;
        }
    }
}
