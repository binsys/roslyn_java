using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Emit;

namespace TestCUI
{

	/*
Keyword:
	 Keyword:
		abstract
		assert
		boolean
		break
		byte
		case
		catch
		char
		class
		//const
		continue
		default
		do
		double
		else
		enum
		extends
		final
		finally
		float
		for
		if
		//goto
		implements
		import
		instanceof
		int
		interface
		long
		native
		new
		package
		private
		protected
		public
		return
		short
		static
		strictfp
		super
		switch
		synchronized
		this
		throw
		throws
		transient
		try
		void
		volatile
		while
	BooleanLiteral:
		true 
		false
	NullLiteral:
		null

Identifiers:
	Identifier:
		IdentifierChars but not a Keyword or BooleanLiteral or NullLiteral

	IdentifierChars:
		JavaLetter
		IdentifierChars JavaLetterOrDigit

	JavaLetter:
		any Unicode character that is a Java letter (see below)

	JavaLetterOrDigit:
		any Unicode character that is a Java letter-or-digit (see below)

	The "Java letters" include uppercase and lowercase ASCII Latin letters 
	 * A-Z (\u0041-\u005a), and 
	 * a-z (\u0061-\u007a), and, for historical reasons, the ASCII underscore 
	 * (_, or \u005f) and dollar sign 
	 * ($, or \u0024). 
	 * The $ character should be used only in mechanically generated source code or, 
	 * rarely, to access pre-existing names on legacy systems.The "Java digits" include the ASCII digits 0-9 (\u0030-\u0039).
	 * 


	 */




	/*
	 * Versions of the Java programming language prior to 1.1 used Unicode version 1.1.5. 
	 * Upgrades to newer versions of the Unicode Standard occurred in JDK 1.1 (to Unicode 2.0), 
	 * JDK 1.1.7 (to Unicode 2.1), 
	 * Java SE 1.4 (to Unicode 3.0), and 
	 * Java SE 5.0 (to Unicode 4.0).
	 * 
	 *  U+0000 to U+FFFF
	 */
	class Program
	{


		static string CSharpSample = @"//using System;
//using System.IO;
//namespace testNamespace
//{
	using System;
	using System.IO;
	
	public class testClass
	{
		public void TestMethod()
		{
		
		}

		public int TestMethodAdd(int a, int b)
		{
			return a + b;
		}
	}
//}
";

		static void Main(string[] args)
		{


			//JavaSyntaxTree.

			//var tree = Roslyn.Compilers.CSharp.SyntaxTree.ParseCompilationUnit(@"class c1 { }");
			//var token = tree.Root.DescendentTokens().First();
			//Console.WriteLine(token.Kind);

			//var tree = CSharpSyntaxTree.ParseText(CSharpSample);
			//var cus = tree.GetCompilationUnitRoot();


            aaaa();

			Console.WriteLine("Press [Enter] to exit!");
			Console.ReadLine();
		}

		public static void Method2()
		{
//			var tree = JavaSyntaxTree.ParseText(@"
//				public class MyClass {
//						 int Method1() { return 0; }
//						 void Method2()
//						 {
//							int x = Method1();
//						 }
//					}
//				}");
 
//			var Mscorlib = new MetadataFileReference(typeof(object).Assembly.Location);
//			var compilation = JavaCompilation.Create("MyCompilation",
//				syntaxTrees: new[] { tree }, references: new[] { Mscorlib });
//			var model = compilation.GetSemanticModel(tree);
 
//			//Looking at the first method symbol
//			var methodSyntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().First();
//			var methodSymbol = model.GetDeclaredSymbol(methodSyntax);
 
//			Console.WriteLine(methodSymbol.ToString());         //MyClass.Method1()
//			Console.WriteLine(methodSymbol.ContainingSymbol);   //MyClass
//			Console.WriteLine(methodSymbol.IsAbstract);         //false
 
//			//Looking at the first invocation
//			var invocationSyntax = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().First();
//			var invokedSymbol = model.GetSymbolInfo(invocationSyntax).Symbol; //Same as MyClass.Method1
 
//			Console.WriteLine(invokedSymbol.ToString());         //MyClass.Method1()
//			Console.WriteLine(invokedSymbol.ContainingSymbol);   //MyClass
//			Console.WriteLine(invokedSymbol.IsAbstract);         //false
 
//			Console.WriteLine(invokedSymbol.Equals(methodSymbol)); //true
		}

	    public static void aaaa()
	    {
            var syntaxTree = CSharpSyntaxTree.ParseText(@"
import System;
package HelloWorld
{
    class Greeter
    {
        public static void Greet()
        {
            Console.WriteLine(""Hello, World"");
        }
    }
}");

 
            string dllPath = Path.Combine(Directory.GetCurrentDirectory(), "Greeter.dll");
            string pdbPath = Path.Combine(Directory.GetCurrentDirectory(), "Greeter.pdb");


            var compilation = CSharpCompilation.Create("Greeter", options:
                new CSharpCompilationOptions(
                    outputKind: OutputKind.DynamicallyLinkedLibrary
                ))
                .AddSyntaxTrees()
                .AddSyntaxTrees(new[] { syntaxTree })
                .AddReferences(new MetadataFileReference(typeof(object).Assembly.Location))
                .AddReferences(new MetadataFileReference(typeof(Enumerable).Assembly.Location));

            EmitResult result;

            using (FileStream dllStream = new FileStream(dllPath, FileMode.OpenOrCreate))
            using (FileStream pdbStream = new FileStream(pdbPath, FileMode.OpenOrCreate))
            {
     
                result = compilation.Emit(
                    outputStream: dllStream,
                    pdbStream: pdbStream);
            }

            if (result.Success)
            {
                //assembly = Assembly.LoadFile(Path.Combine(Directory.GetCurrentDirectory(), @"Greeter.dll"));
                Assembly assembly = Assembly.LoadFrom(@"Greeter.dll");

                Type type = assembly.GetType("HelloWorld.Greeter");
                var obj = Activator.CreateInstance(type);

                type.InvokeMember("Greet",
                    BindingFlags.Default | BindingFlags.InvokeMethod,
                    null,
                    obj,
                    null);
            }
            else
            {
                Console.WriteLine("No Go");
                Console.WriteLine(result.Diagnostics.ToString());
            }

            Console.WriteLine("<ENTER> to continue");
            Console.ReadLine();
	    }
	}


}
