
namespace Microsoft.CodeAnalysis.CSharp.Syntax
{
	public partial class ClassDeclarationSyntax
	{
		public ClassDeclarationSyntax Update(JavaMemberModifierSyntax modifier, 
			SyntaxToken keyword, 
			SyntaxToken identifier, 
			TypeParameterListSyntax typeParameterList,
			ExtendsClauseSyntax extendsClauseSyntaxs,
			ImplementsListClauseSyntax baseList,
			SyntaxToken openBraceToken, 
			SyntaxList<MemberDeclarationSyntax> members, 
			SyntaxToken closeBraceToken, 
			SyntaxToken semicolonToken)
		{
			return this.Update(
				modifier,
				keyword, 
				identifier, 
				typeParameterList, 
				this.ParameterList,
				extendsClauseSyntaxs, 
				baseList, 
				openBraceToken, 
				members, 
				closeBraceToken, 
				semicolonToken);
		}
	}
}