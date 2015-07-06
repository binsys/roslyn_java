using System;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
namespace JavaTest
{
	[TestClass]
	public class UnitTestCompilationUnitSyntax
	{
		[TestMethod]
		public void TestMethod1()
		{
			var tree = CSharpSyntaxTree.ParseText("class aa{}");
			var diags = tree.GetDiagnostics();
			Assert.IsTrue(!diags.Any());
		}


		[TestMethod]
		public void TestPackageDecl()
		{
			var tree = CSharpSyntaxTree.ParseText("package aaa;");
			var diags = tree.GetDiagnostics();
			Assert.IsTrue(!diags.Any());
		}

		[TestMethod]
		public void TestImportDecl()
		{
			var tree = CSharpSyntaxTree.ParseText("import aaa;");
			var diags = tree.GetDiagnostics();
			Assert.IsTrue(!diags.Any());


			tree = CSharpSyntaxTree.ParseText("import aaa.bbbb;");
			diags = tree.GetDiagnostics();
			Assert.IsTrue(!diags.Any());

			tree = CSharpSyntaxTree.ParseText("import aaa.bbbb.*;");
			diags = tree.GetDiagnostics();
			Assert.IsTrue(!diags.Any());

			tree = CSharpSyntaxTree.ParseText("import static aaa.bbbb;");
			diags = tree.GetDiagnostics();
			Assert.IsTrue(!diags.Any());

			tree = CSharpSyntaxTree.ParseText("import static aaa.bbbb.*;");
			diags = tree.GetDiagnostics();
			Assert.IsTrue(!diags.Any());
		}
	}
}
