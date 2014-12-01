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
		NullKeyword,        // null                 J 字面量 非关键字
		TrueKeyword,        // true                 J 字面量 非关键字
		FalseKeyword,       // false                J 字面量 非关键字
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
		GotoKeyword,        // goto                 J 保留未用
		BreakKeyword,       // break                J
		ContinueKeyword,    // continue             J
		ReturnKeyword,      // return               J
		ThrowKeyword,       // throw                J
		PublicKeyword,      // public               J
		PrivateKeyword,     // private              J
		ProtectedKeyword,   // protected            J
		StaticKeyword,      // static               J
		ConstKeyword,       // const                J 保留未用
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
		StrictFpKeyword,    // strictfp            即 strict float point (精确浮点)。可应用于类、接口或方法
		SynchronizedKeyword,// synchronized      lock 替代 Java语言的关键字，但lock不可用于方法，可用来给对象和方法或者代码块加锁，当它锁定一个方法或者一个代码块的时候，同一时刻最多只有一个线程执行这个段代码。
		ThrowsKeyword,      // throws            用于方法后面，指定把某些异常抛给调用方
		TransientKeyword,   // transient         变量修饰符，如果用transient声明一个实例变量，当对象存储时，它的值不需要维持。（序列化是跳过）

		// Other
		
		/// <summary>
		/// 省略的类型参数令牌
		/// </summary>
		OmittedTypeArgumentToken,

		/// <summary>
		/// 省略的数字大小表达式令牌
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

		// trivia 怎么翻译的？琐碎？杂碎？杂项？
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
		/// 二元表达式 加
		/// </summary>
		AddExpression,

		/// <summary>
		/// 二元表达式 减
		/// </summary>
		SubtractExpression,

		/// <summary>
		/// 二元表达式 乘
		/// </summary>
		MultiplyExpression,

		/// <summary>
		/// 二元表达式 除
		/// </summary>
		DivideExpression,

		/// <summary>
		/// 二元表达式 求余
		/// </summary>
		ModuloExpression,

		/// <summary>
		/// 二元表达式 左移
		/// </summary>
		LeftShiftExpression,

		/// <summary>
		/// 二元表达式 右移
		/// </summary>
		RightShiftExpression,

		/// <summary>
		/// 二元表达式 逻辑或
		/// </summary>
		LogicalOrExpression,

		/// <summary>
		/// 二元表达式 逻辑与
		/// </summary>
		LogicalAndExpression,

		/// <summary>
		/// 二元表达式 位或
		/// </summary>
		BitwiseOrExpression,

		/// <summary>
		/// 二元表达式 位与
		/// </summary>
		BitwiseAndExpression,
		ExclusiveOrExpression,

		/// <summary>
		/// 二元表达式 等于
		/// </summary>
		EqualsExpression,

		/// <summary>
		/// 二元表达式 不等于
		/// </summary>
		NotEqualsExpression,


		/// <summary>
		/// 二元表达式 小于
		/// </summary>
		LessThanExpression,

		/// <summary>
		/// 二元表达式 小于或等于
		/// </summary>
		LessThanOrEqualExpression,

		/// <summary>
		/// 二元表达式  大于
		/// </summary>
		GreaterThanExpression,

		/// <summary>
		/// 二元表达式 大于或等于
		/// </summary>
		GreaterThanOrEqualExpression,

		/// <summary>
		/// is 表达式
		/// </summary>
		IsExpression,

		/// <summary>
		/// as 表达式
		/// </summary>
		AsExpression,
		CoalesceExpression,

		/// <summary>
		/// 简单成员访问
		/// </summary>
		SimpleMemberAccessExpression,  // dot .

		/// <summary>
		/// 指针成员访问
		/// </summary>
		PointerMemberAccessExpression,  // arrow ->

		/// <summary>
		/// 索引成员访问
		/// </summary>
		IndexedMemberAccessExpression,  // dot dollar   . $

		// binary assignment expressions
		/// <summary>
		/// 二元赋值表达式 简单赋值
		/// </summary>
		SimpleAssignmentExpression,

		/// <summary>
		/// 二元赋值表达式 +=
		/// </summary>
		AddAssignmentExpression,

		/// <summary>
		/// 二元赋值表达式 -=
		/// </summary>
		SubtractAssignmentExpression,

		/// <summary>
		/// 二元赋值表达式 *=
		/// </summary>
		MultiplyAssignmentExpression,

		/// <summary>
		/// 二元赋值表达式 /=
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
		/// 一元表达式 +
		/// </summary>
		UnaryPlusExpression,

		/// <summary>
		/// 一元表达式 -
		/// </summary>
		UnaryMinusExpression,

		/// <summary>
		/// 一元表达式 位非
		/// </summary>
		BitwiseNotExpression,

		/// <summary>
		/// 一元表达式 逻辑非
		/// </summary>
		LogicalNotExpression,

		/// <summary>
		/// 一元表达式 ++*
		/// </summary>
		PreIncrementExpression,

		/// <summary>
		/// 一元表达式 --*
		/// </summary>
		PreDecrementExpression,

		/// <summary>
		/// 一元表达式 
		/// </summary>
		PointerIndirectionExpression,

		/// <summary>
		/// 一元表达式 
		/// </summary>
		AddressOfExpression,

		/// <summary>
		/// 一元表达式 *++
		/// </summary>
		PostIncrementExpression,

		/// <summary>
		/// 一元表达式 *--
		/// </summary>
		PostDecrementExpression,

		/// <summary>
		/// 一元表达式 await
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
		/// 数字字面量
		/// </summary>
		NumericLiteralExpression,

		/// <summary>
		/// 字符串字面量
		/// </summary>
		StringLiteralExpression,

		/// <summary>
		/// 字符字面量
		/// </summary>
		CharacterLiteralExpression,

		/// <summary>
		/// true 字面量
		/// </summary>
		TrueLiteralExpression,

		/// <summary>
		/// false 字面量
		/// </summary>
		FalseLiteralExpression,

		/// <summary>
		/// .class 字面量
		/// </summary>
		ClassLiteralExpression,

		/// <summary>
		/// null 字面量
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
		/// 变量声明语句段
		/// </summary>
		LocalDeclarationStatement,

		/// <summary>
		/// 变量声明
		/// </summary>
		VariableDeclaration,

		/// <summary>
		/// 变量声明？
		/// </summary>
		VariableDeclarator,

		/// <summary>
		/// 等于值
		/// </summary>
		EqualsValueClause,

		/// <summary>
		/// 表达式语句段
		/// </summary>
		ExpressionStatement,

		/// <summary>
		/// 空语句段
		/// </summary>
		EmptyStatement,

		/// <summary>
		/// 标签化语句段
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

		// 注解 Annotation
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
