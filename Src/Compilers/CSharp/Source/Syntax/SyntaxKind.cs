// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.CodeAnalysis.CSharp
{
	public enum SyntaxKind : ushort
	{
		None,
		List = GreenNode.ListKind,

		// punctuation
		TildeToken = 8193,// ~                 J
		ExclamationToken, // !                 J
		//DollarToken,      // $
		AtToken,          // @                 J
		PercentToken,     // %                 J
		CaretToken,       // ^                 J
		AmpersandToken,   // &                 J
		AsteriskToken,    // *                 J
		OpenParenToken,   // (                 J
		CloseParenToken,  // )                 J
		MinusToken,       // -                 J
		PlusToken,        // +                 J
		EqualsToken,      // =                 J
		OpenBraceToken,   // {                 J
		CloseBraceToken,  // }                 J
		OpenBracketToken, // [                 J
		CloseBracketToken,// ]                 J
		BarToken,         // |                 J
		ColonToken,       // :                 J
		SemicolonToken,   // ;                 J
		DoubleQuoteToken, // "
		SingleQuoteToken, // '
		LessThanToken,    // <                 J
		CommaToken,       // ,                 J
		GreaterThanToken, // >                 J
		DotToken,         // .                 J
		QuestionToken,    // ?                 J
		SlashToken,       // /                 J


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
		BarBarToken,                                 // ||                 J
		AmpersandAmpersandToken,                     // &&                 J
		MinusMinusToken,                             // --                 J
		PlusPlusToken,                               // ++                 J
		ColonColonToken,                             // ::                 J
		MinusGreaterThanToken,                       // ->                 J
		ExclamationEqualsToken,                      // !=                 J
		EqualsEqualsToken,                           // ==                 J
		LessThanEqualsToken,                         // <=                 J
		LessThanLessThanToken,                       // <<                 J
		LessThanLessThanEqualsToken,                 // <<=                J
		GreaterThanEqualsToken,                      // >=                 J
		GreaterThanGreaterThanToken,                 // >>                 J
		GreaterThanGreaterThanEqualsToken,           // >>=                J
		SlashEqualsToken,                            // /=                 J
		AsteriskEqualsToken,                         // *=                 J
		BarEqualsToken,                              // |=                 J
		AmpersandEqualsToken,                        // &=                 J
		PlusEqualsToken,                             // +=                 J
		MinusEqualsToken,                            // -=                 J
		CaretEqualsToken,                            // ^=                 J
		DotDotDotToken,                              // ...                J
		GreaterThanGreaterThanGreaterThanEqualsToken,// >>>=               J
		GreaterThanGreaterThanGreaterThanToken,      // >>>                J
		PercentEqualsToken,                          // %=                 J
		
		

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
		StrictFpKeyword,    // strictfp             即 strict float point (精确浮点)。可应用于类、接口或方法
		SynchronizedKeyword,// synchronized         lock 替代 Java语言的关键字，但lock不可用于方法，可用来给对象和方法或者代码块加锁，当它锁定一个方法或者一个代码块的时候，同一时刻最多只有一个线程执行这个段代码。
		ThrowsKeyword,      // throws               用于方法后面，指定把某些异常抛给调用方
		TransientKeyword,   // transient            变量修饰符，如果用transient声明一个实例变量，当对象存储时，它的值不需要维持。（序列化是跳过）

		// Other
		OmittedTypeArgumentToken,
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
		//PointerType,
		//NullableType,
		JavaWildcardType,
		OmittedTypeArgument,
		BaseClassWithArguments,
		JavaWildcardTypeBound,



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
		//ImplicitArrayCreationExpression,
		ImplicitElementAccess,
		StackAllocArrayCreationExpression,
		OmittedArraySizeExpression,
		DeclarationExpression,

		// binary expressions
		AddExpression,                  // +
		SubtractExpression,             // -
		MultiplyExpression,             // *
		DivideExpression,               // /
		ModuloExpression,               // %
		LeftShiftExpression,            // <<
		RightShiftExpression,           // >>
		UnsignedRightShiftExpression,   // >>>
		LogicalOrExpression,            // ||
		LogicalAndExpression,           // &&
		BitwiseOrExpression,            // |
		BitwiseAndExpression,           // &
		ExclusiveOrExpression,          // ^
		EqualsExpression,               // =
		NotEqualsExpression,            // !=
		LessThanExpression,             // <
		LessThanOrEqualExpression,      // <=
		GreaterThanExpression,          // >
		GreaterThanOrEqualExpression,   // >=
		InstanceOfExpression,                   // is
		//AsExpression,                   // as
		SimpleMemberAccessExpression,   // dot .
		SimpleAssignmentExpression,     // a = b
		AddAssignmentExpression,        // a +=b
		SubtractAssignmentExpression,   // a-=b
		MultiplyAssignmentExpression,   // a*= b
		DivideAssignmentExpression,     // a/=b
		ModuloAssignmentExpression,     // a%=b
		AndAssignmentExpression,        // a&=b
		ExclusiveOrAssignmentExpression,// a^=b
		OrAssignmentExpression,         // |=
		LeftShiftAssignmentExpression,  // a<<= b
		RightShiftAssignmentExpression, // a>>= b
		UnsignedRightShiftAssignmentExpression, // a >>>= b

		// unary expressions
		UnaryPlusExpression,    // +a
		UnaryMinusExpression,   // -a
		BitwiseNotExpression,   // !
		LogicalNotExpression,   // !
		PreIncrementExpression, // ++a
		PreDecrementExpression, // --a
		PostIncrementExpression,// a++
		PostDecrementExpression,// a--

		// primary expression
		ThisExpression,              // this
		JavaQualifiedThisExpression, /// a.this
		BaseExpression,              // base super
		JavaQualifiedSuperExpression,// a.base a.super
		ArgListExpression,
		NumericLiteralExpression,    // 123
		StringLiteralExpression,     // "asdfa"
		CharacterLiteralExpression,  // 'a'
		TrueLiteralExpression,       // true
		FalseLiteralExpression,      // false
		ClassLiteralExpression,      // abc.class
		NullLiteralExpression,       // null


		// primary function expressions
		DefaultExpression,  // default


		// statements
		Block,
		LocalDeclarationStatement,
		VariableDeclaration,
		VariableDeclarator,
		EqualsValueClause,
		ExpressionStatement,
		EmptyStatement,
		LabeledStatement,

		// jump statements
		BreakStatement,
		ContinueStatement,
		ReturnStatement,
		YieldReturnStatement,
		YieldBreakStatement,
		ThrowStatement,

		WhileStatement,
		DoStatement,
		ForStatement,
		JavaEnhancedForStatement,
		UsingStatement,
		JavaSynchronizedStatement,
		FixedStatement,

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
		JavaAssertStatement,

		// declarations
		CompilationUnit,
		JavaPackageDeclaration,
		ImportDeclaration,
		ImportOnDemandSuffix,

		//JavaTypeParameterModifier,

		JavaNormalClassDeclaration,
		JavaEnumDeclaration,
		JavaNormalInterfaceDeclaration,
		JavaAnnotationTypeDeclaration,
		JavaClassBody,


		JavaInitializerMethodDeclaration,
		JavaAnonymousClassInitializerExpression,
		JavaThrowsListClause,

		JavaEnumConstant,
		JavaEnumBodyDeclarations,
		JavaEnumBody,
		JavaMemberModifier,

		// 注解 Annotation
		Annotation,
		AnnotationArgumentList,
		AnnotationArgument,
		NameEquals,

	
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
		FieldDeclaration,
		MethodDeclaration,
		ConstructorDeclaration,
		BaseConstructorInitializer,
		ThisConstructorInitializer,
		DestructorDeclaration,
		AccessorList,
		UnknownAccessorDeclaration,
		ParameterList,
		BracketedParameterList,
		Parameter,
		TypeParameterList,
		TypeParameter,
		IncompleteMember
	}
}
