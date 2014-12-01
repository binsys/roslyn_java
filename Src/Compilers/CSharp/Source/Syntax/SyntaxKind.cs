// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.CodeAnalysis.CSharp
{
	public enum SyntaxKind : ushort
	{
		None,
		List = GreenNode.ListKind,

		// punctuation
		TildeToken = 8193,// ~
		ExclamationToken, // !
		DollarToken,      // $
		AtToken,          // @
		PercentToken,     // %
		CaretToken,       // ^
		AmpersandToken,   // &
		AsteriskToken,    // *
		OpenParenToken,   // (
		CloseParenToken,  // )
		MinusToken,       // -
		PlusToken,        // +
		EqualsToken,      // =
		OpenBraceToken,   // {
		CloseBraceToken,  // }
		OpenBracketToken, // [
		CloseBracketToken,// ]
		BarToken,         // |
		BackslashToken,   // \\
		ColonToken,       // :
		SemicolonToken,   // ;
		DoubleQuoteToken, // "
		SingleQuoteToken, // '
		LessThanToken,    // <
		CommaToken,       // ,
		GreaterThanToken, // >
		DotToken,         // .
		QuestionToken,    // ?
		HashToken,        // #
		SlashToken,       // /


		// additional xml tokens
		SlashGreaterThanToken,              // xml empty element end
		LessThanSlashToken,                 // element end tag start token
		XmlCommentStartToken,               // <!--
		XmlCommentEndToken,                 // -->
		XmlCDataStartToken,                 // <![CDATA[
		XmlCDataEndToken,                   // ]]>
		XmlProcessingInstructionStartToken, // <?
		XmlProcessingInstructionEndToken,   // ?>

		// compound punctuation
		BarBarToken,                      // ||
		AmpersandAmpersandToken,          // &&
		MinusMinusToken,                  // --
		PlusPlusToken,                    // ++
		ColonColonToken,                  // ::
		QuestionQuestionToken,            // ??
		MinusGreaterThanToken,            // ->
		ExclamationEqualsToken,           // !=
		EqualsEqualsToken,                // ==
		EqualsGreaterThanToken,           // =>
		LessThanEqualsToken,              // <=
		LessThanLessThanToken,            // <<
		LessThanLessThanEqualsToken,      // <<=
		GreaterThanEqualsToken,           // >=
		GreaterThanGreaterThanToken,      // >>
		GreaterThanGreaterThanEqualsToken,// >>=
		SlashEqualsToken,                 // /=
		AsteriskEqualsToken,              // *=
		BarEqualsToken,                   // |=
		AmpersandEqualsToken,             // &=
		PlusEqualsToken,                  // +=
		MinusEqualsToken,                 // -=
		CaretEqualsToken,                 // ^=
		PercentEqualsToken,               // %=

		// Keywords
		BooleanKeyword,     // boolean bool         CJ
		ByteKeyword,        // byte                 J
		ShortKeyword,       // short                J
		IntKeyword,         // int                  J
		LongKeyword,        // long                 J
		DoubleKeyword,      // double               J
		FloatKeyword,       // float                J
		CharKeyword,        // char                 J
		VoidKeyword,        // void                 J
		NullKeyword,        // null                 J ������ �ǹؼ���
		TrueKeyword,        // true                 J ������ �ǹؼ���
		FalseKeyword,       // false                J ������ �ǹؼ���
		IfKeyword,          // if                   J
		ElseKeyword,        // else                 J
		WhileKeyword,       // while                J
		ForKeyword,         // for                  J
		DoKeyword,          // do                   J
		SwitchKeyword,      // switch               J
		CaseKeyword,        // case                 J
		DefaultKeyword,     // default              J
		TryKeyword,         // try                  J
		CatchKeyword,       // catch                J
		FinallyKeyword,     // finally              J
		GotoKeyword,        // goto                 J ����δ��
		BreakKeyword,       // break                J
		ContinueKeyword,    // continue             J
		ReturnKeyword,      // return               J
		ThrowKeyword,       // throw                J
		PublicKeyword,      // public               J
		PrivateKeyword,     // private              J
		ProtectedKeyword,   // protected            J
		StaticKeyword,      // static               J
		ConstKeyword,       // const                J ����δ��
		VolatileKeyword,    // volatile             J
		NewKeyword,         // new                  J
		AbstractKeyword,    // abstract             J
		NativeKeyword,      // native extern        CJ
		InstanceOfKeyword,  // instanceof is        CJ
		AsKeyword,          // as                   
		ParamsKeyword,      // params               
		ArgListKeyword,     // __arglist            
		ThisKeyword,        // this                 J
		SuperKeyword,       // super base           CJ
		PackageKeyword,     // package namespace    CJ
		ImportKeyword,      // import using         CJ
		ClassKeyword,       // class                J
		InterfaceKeyword,   // interface            J
		EnumKeyword,        // enum                 J
		AssertKeyword,      // assert               J
		ExtendsKeyword,     // extends              J
		FinalKeyword,       // final                J
		ImplementsKeyword,  // implements           J
		StrictFpKeyword,    // strictfp            �� strict float point (��ȷ����)����Ӧ�����ࡢ�ӿڻ򷽷�
		SynchronizedKeyword,// synchronized      lock ��� Java���ԵĹؼ��֣���lock�������ڷ�����������������ͷ������ߴ�����������������һ����������һ��������ʱ��ͬһʱ�����ֻ��һ���߳�ִ������δ��롣
		ThrowsKeyword,      // throws            ���ڷ������棬ָ����ĳЩ�쳣�׸����÷�
		TransientKeyword,   // transient         �������η��������transient����һ��ʵ��������������洢ʱ������ֵ����Ҫά�֡������л���������

		// Other
		
		/// <summary>
		/// ʡ�Ե����Ͳ�������
		/// </summary>
		OmittedTypeArgumentToken,

		/// <summary>
		/// ʡ�Ե����ִ�С���ʽ����
		/// </summary>
		OmittedArraySizeExpressionToken,
		EndOfDirectiveToken,

		EndOfDocumentationCommentToken,

		EndOfFileToken, //NB: this is assumed to be the last textless token

		// tokens with text
		BadToken,
		IdentifierToken,
		NumericLiteralToken,
		CharacterLiteralToken,
		StringLiteralToken,
		XmlEntityLiteralToken,  // &lt; &gt; &quot; &amp; &apos; or &name; or &#nnnn; or &#xhhhh;
		XmlTextLiteralToken,    // xml text node text
		XmlTextLiteralNewLineToken,

		// trivia ��ô����ģ����飿���飿���
		EndOfLineTrivia,
		WhitespaceTrivia,
		SingleLineCommentTrivia,
		MultiLineCommentTrivia,
		DocumentationCommentExteriorTrivia,
		SingleLineDocumentationCommentTrivia,
		MultiLineDocumentationCommentTrivia,
		SkippedTokensTrivia,

		// xml nodes (for xml doc comment structure)
		XmlElement,
		XmlElementStartTag,
		XmlElementEndTag,
		XmlEmptyElement,
		XmlTextAttribute,
		XmlCrefAttribute,
		XmlNameAttribute,
		XmlName,
		XmlPrefix,
		XmlText,
		XmlCDataSection,
		XmlComment,
		XmlProcessingInstruction,

		// documentation comment nodes (structure inside DocumentationCommentTrivia)
		TypeCref,
		QualifiedCref,
		NameMemberCref,
		//IndexerMemberCref,
		CrefParameterList,
		CrefBracketedParameterList,
		CrefParameter,

		

		// names & type-names


		IdentifierName,
		QualifiedName,
		GenericName,
		IndexedName,
		TypeArgumentList,
		PredefinedType,
		ArrayType,
		ArrayRankSpecifier,
		PointerType,
		NullableType,
		OmittedTypeArgument,
		BaseClassWithArguments,

		// expressions
		ParenthesizedExpression,
		ConditionalExpression,
		InvocationExpression,
		ElementAccessExpression,
		ArgumentList,
		BracketedArgumentList,
		Argument,
		NameColon,
		CastExpression,
		AnonymousMethodExpression,
		SimpleLambdaExpression,
		ParenthesizedLambdaExpression,
		ObjectInitializerExpression,
		CollectionInitializerExpression,
		ArrayInitializerExpression,
		AnonymousObjectMemberDeclarator,
		ComplexElementInitializerExpression,
		ObjectCreationExpression,
		AnnotationCreationExpression,
		AnonymousObjectCreationExpression,
		ArrayCreationExpression,
		ImplicitArrayCreationExpression,
		ImplicitElementAccess,
		StackAllocArrayCreationExpression,
		OmittedArraySizeExpression,
		DeclarationExpression,

		// binary expressions

		/// <summary>
		/// ��Ԫ���ʽ ��
		/// </summary>
		AddExpression,

		/// <summary>
		/// ��Ԫ���ʽ ��
		/// </summary>
		SubtractExpression,

		/// <summary>
		/// ��Ԫ���ʽ ��
		/// </summary>
		MultiplyExpression,

		/// <summary>
		/// ��Ԫ���ʽ ��
		/// </summary>
		DivideExpression,

		/// <summary>
		/// ��Ԫ���ʽ ����
		/// </summary>
		ModuloExpression,

		/// <summary>
		/// ��Ԫ���ʽ ����
		/// </summary>
		LeftShiftExpression,

		/// <summary>
		/// ��Ԫ���ʽ ����
		/// </summary>
		RightShiftExpression,

		/// <summary>
		/// ��Ԫ���ʽ �߼���
		/// </summary>
		LogicalOrExpression,

		/// <summary>
		/// ��Ԫ���ʽ �߼���
		/// </summary>
		LogicalAndExpression,

		/// <summary>
		/// ��Ԫ���ʽ λ��
		/// </summary>
		BitwiseOrExpression,

		/// <summary>
		/// ��Ԫ���ʽ λ��
		/// </summary>
		BitwiseAndExpression,
		ExclusiveOrExpression,

		/// <summary>
		/// ��Ԫ���ʽ ����
		/// </summary>
		EqualsExpression,

		/// <summary>
		/// ��Ԫ���ʽ ������
		/// </summary>
		NotEqualsExpression,


		/// <summary>
		/// ��Ԫ���ʽ С��
		/// </summary>
		LessThanExpression,

		/// <summary>
		/// ��Ԫ���ʽ С�ڻ����
		/// </summary>
		LessThanOrEqualExpression,

		/// <summary>
		/// ��Ԫ���ʽ  ����
		/// </summary>
		GreaterThanExpression,

		/// <summary>
		/// ��Ԫ���ʽ ���ڻ����
		/// </summary>
		GreaterThanOrEqualExpression,

		/// <summary>
		/// is ���ʽ
		/// </summary>
		IsExpression,

		/// <summary>
		/// as ���ʽ
		/// </summary>
		AsExpression,
		CoalesceExpression,

		/// <summary>
		/// �򵥳�Ա����
		/// </summary>
		SimpleMemberAccessExpression,  // dot .

		/// <summary>
		/// ָ���Ա����
		/// </summary>
		PointerMemberAccessExpression,  // arrow ->

		/// <summary>
		/// ������Ա����
		/// </summary>
		IndexedMemberAccessExpression,  // dot dollar   . $

		// binary assignment expressions
		/// <summary>
		/// ��Ԫ��ֵ���ʽ �򵥸�ֵ
		/// </summary>
		SimpleAssignmentExpression,

		/// <summary>
		/// ��Ԫ��ֵ���ʽ +=
		/// </summary>
		AddAssignmentExpression,

		/// <summary>
		/// ��Ԫ��ֵ���ʽ -=
		/// </summary>
		SubtractAssignmentExpression,

		/// <summary>
		/// ��Ԫ��ֵ���ʽ *=
		/// </summary>
		MultiplyAssignmentExpression,

		/// <summary>
		/// ��Ԫ��ֵ���ʽ /=
		/// </summary>
		DivideAssignmentExpression,

		/// <summary>
		/// %=
		/// </summary>
		ModuloAssignmentExpression,

		/// <summary>
		/// <![CDATA[&=]]>
		/// </summary>
		AndAssignmentExpression,

		/// <summary>
		/// ^=
		/// </summary>
		ExclusiveOrAssignmentExpression,

		/// <summary>
		/// |=
		/// </summary>
		OrAssignmentExpression,

		/// <summary>
		/// <![CDATA[<<=]]>
		/// </summary>
		LeftShiftAssignmentExpression,

		/// <summary>
		/// >>=
		/// </summary>
		RightShiftAssignmentExpression,

		// unary expressions

		/// <summary>
		/// һԪ���ʽ +
		/// </summary>
		UnaryPlusExpression,

		/// <summary>
		/// һԪ���ʽ -
		/// </summary>
		UnaryMinusExpression,

		/// <summary>
		/// һԪ���ʽ λ��
		/// </summary>
		BitwiseNotExpression,

		/// <summary>
		/// һԪ���ʽ �߼���
		/// </summary>
		LogicalNotExpression,

		/// <summary>
		/// һԪ���ʽ ++*
		/// </summary>
		PreIncrementExpression,

		/// <summary>
		/// һԪ���ʽ --*
		/// </summary>
		PreDecrementExpression,

		/// <summary>
		/// һԪ���ʽ 
		/// </summary>
		PointerIndirectionExpression,

		/// <summary>
		/// һԪ���ʽ 
		/// </summary>
		AddressOfExpression,

		/// <summary>
		/// һԪ���ʽ *++
		/// </summary>
		PostIncrementExpression,

		/// <summary>
		/// һԪ���ʽ *--
		/// </summary>
		PostDecrementExpression,

		/// <summary>
		/// һԪ���ʽ await
		/// </summary>
		AwaitExpression,

		// primary expression

		/// <summary>
		/// this
		/// </summary>
		ThisExpression,

		/// <summary>
		/// base
		/// </summary>
		BaseExpression,

		/// <summary>
		/// ...
		/// </summary>
		ArgListExpression,

		/// <summary>
		/// ����������
		/// </summary>
		NumericLiteralExpression,

		/// <summary>
		/// �ַ���������
		/// </summary>
		StringLiteralExpression,

		/// <summary>
		/// �ַ�������
		/// </summary>
		CharacterLiteralExpression,

		/// <summary>
		/// true ������
		/// </summary>
		TrueLiteralExpression,

		/// <summary>
		/// false ������
		/// </summary>
		FalseLiteralExpression,

		/// <summary>
		/// .class ������
		/// </summary>
		ClassLiteralExpression,

		/// <summary>
		/// null ������
		/// </summary>
		NullLiteralExpression,


		// primary function expressions

		/// <summary>
		/// typeof
		/// </summary>
		TypeOfExpression,

		/// <summary>
		/// sizeof
		/// </summary>
		SizeOfExpression,

		/// <summary>
		/// checked
		/// </summary>
		CheckedExpression,

		/// <summary>
		/// unchecked
		/// </summary>
		UncheckedExpression,

		/// <summary>
		/// default
		/// </summary>
		DefaultExpression,



		// query expressions
		QueryExpression,
		QueryBody,
		FromClause,
		LetClause,
		JoinClause,
		JoinIntoClause,
		WhereClause,
		OrderByClause,
		AscendingOrdering,
		DescendingOrdering,
		SelectClause,
		GroupClause,
		QueryContinuation,

		// statements
		Block,

		/// <summary>
		/// ������������
		/// </summary>
		LocalDeclarationStatement,

		/// <summary>
		/// ��������
		/// </summary>
		VariableDeclaration,

		/// <summary>
		/// ����������
		/// </summary>
		VariableDeclarator,

		/// <summary>
		/// ����ֵ
		/// </summary>
		EqualsValueClause,

		/// <summary>
		/// ���ʽ����
		/// </summary>
		ExpressionStatement,

		/// <summary>
		/// ������
		/// </summary>
		EmptyStatement,

		/// <summary>
		/// ��ǩ������
		/// </summary>
		LabeledStatement,

		// jump statements
		GotoStatement,
		GotoCaseStatement,
		GotoDefaultStatement,
		BreakStatement,
		ContinueStatement,
		ReturnStatement,
		YieldReturnStatement,
		YieldBreakStatement,
		ThrowStatement,

		WhileStatement,
		DoStatement,
		ForStatement,
		ForEachStatement,
		UsingStatement,
		FixedStatement,

		// checked statements
		CheckedStatement,
		UncheckedStatement,

		UnsafeStatement,
		LockStatement,
		IfStatement,
		ElseClause,
		SwitchStatement,
		SwitchSection,
		CaseSwitchLabel,
		DefaultSwitchLabel,
		TryStatement,
		CatchClause,
		CatchDeclaration,
		CatchFilterClause,
		FinallyClause,

		// declarations

		CompilationUnit,
		PackageDeclaration,
		//JavaPackageModifier,
		ImportDeclaration,
		ImportOnDemandSuffix,

		JavaTypeParameterModifier,

		JavaNormalClassDeclaration,
		JavaEnumDeclaration,
		JavaNormalInterfaceDeclaration,
		JavaAnnotationTypeDeclaration,

		JavaEnumConstant,
		JavaEnumClassBody,

		//ExternAliasDirective,

		// ע�� Annotation
		Annotation,
		AnnotationArgumentList,
		AnnotationArgument,
		NameEquals,

		JavaMemberModifier,

		// type declarations
		ClassDeclaration,
		InterfaceDeclaration,
		AnnotationDeclaration,
		EnumDeclaration,

		//BaseList,
		ImplementsListClause,
		ExtendsListClause,
		ExtendsClause,

		TypeBound,
		ConstructorConstraint,
		ClassConstraint,
		StructConstraint,
		TypeConstraint,
		AdditionalTypeConstraint,
		//ExplicitInterfaceSpecifier,
		//EnumMemberDeclaration,
		FieldDeclaration,
		//EventFieldDeclaration,
		MethodDeclaration,
		//OperatorDeclaration,
		//ConversionOperatorDeclaration,
		ConstructorDeclaration,
		BaseConstructorInitializer,
		ThisConstructorInitializer,
		DestructorDeclaration,
		//PropertyDeclaration,
		//EventDeclaration,
		//IndexerDeclaration,
		AccessorList,
		//GetAccessorDeclaration,
		//SetAccessorDeclaration,
		//AddAccessorDeclaration,
		//RemoveAccessorDeclaration,
		UnknownAccessorDeclaration,
		ParameterList,
		BracketedParameterList,
		Parameter,
		TypeParameterList,
		TypeParameter,
		IncompleteMember
	}
}
