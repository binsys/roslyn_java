// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace Microsoft.CodeAnalysis.CSharp.Internal.CSharpErrorFactsGenerator
{
	public static class Program
	{
		public static void Main()
		{
			var args = Environment.GetCommandLineArgs();

			if (args.Length != 3)
			{
				Console.WriteLine(
@"Usage: {0} input output
  input     The path to ErrorCode.cs
  output    The path to GeneratedErrorFacts.cs",
				Path.GetFileNameWithoutExtension(args[0]));

				Environment.Exit(-1);
			}

			string inputPath = args[1];
			string outputPath = args[2];

			var outputText = new StringBuilder();

			outputText.AppendLine();
			outputText.AppendLine("namespace Microsoft.CodeAnalysis.CSharp");
			outputText.AppendLine("{");
			outputText.AppendLine("\tinternal static partial class ErrorFacts");
			outputText.AppendLine("\t{");

			var warningCodeNames = new List<string>();
			var fatalCodeNames = new List<string>();
			var infoCodeNames = new List<string>();
			foreach (var line in File.ReadAllLines(inputPath).Select(l => l.Trim()))
			{
				if (line.StartsWith("WRN_"))
				{
					warningCodeNames.Add(line.Substring(0, line.IndexOf(' ')));
				}
				else if (line.StartsWith("FTL_"))
				{
					fatalCodeNames.Add(line.Substring(0, line.IndexOf(' ')));
				}
				else if (line.StartsWith("INF_"))
				{
					infoCodeNames.Add(line.Substring(0, line.IndexOf(' ')));
				}
			}

			outputText.AppendLine("\t\tpublic static bool IsWarning(ErrorCode code)");
			outputText.AppendLine("\t\t{");
			outputText.AppendLine("\t\t\tswitch (code)");
			outputText.AppendLine("\t\t\t{");
			foreach (var name in warningCodeNames)
			{
				outputText.Append("\t\t\t\tcase ErrorCode.");
				outputText.Append(name);
				outputText.AppendLine(":");
			}
			outputText.AppendLine("\t\t\t\t\treturn true;");
			outputText.AppendLine("\t\t\t\tdefault:");
			outputText.AppendLine("\t\t\t\t\treturn false;");
			outputText.AppendLine("\t\t\t\t}");
			outputText.AppendLine("\t\t\t}");

			outputText.AppendLine();

			outputText.AppendLine("\t\tpublic static bool IsFatal(ErrorCode code)");
			outputText.AppendLine("\t\t{");
			outputText.AppendLine("\t\t\tswitch (code)");
			outputText.AppendLine("\t\t\t{");
			foreach (var name in fatalCodeNames)
			{
				outputText.Append("\t\t\t\tcase ErrorCode.");
				outputText.Append(name);
				outputText.AppendLine(":");
			}
			outputText.AppendLine("\t\t\t\t\treturn true;");
			outputText.AppendLine("\t\t\t\tdefault:");
			outputText.AppendLine("\t\t\t\t\treturn false;");
			outputText.AppendLine("\t\t\t}");
			outputText.AppendLine("\t\t}");

			outputText.AppendLine();

			outputText.AppendLine("\t\tpublic static bool IsInfo(ErrorCode code)");
			outputText.AppendLine("\t\t{");
			outputText.AppendLine("\t\t\tswitch (code)");
			outputText.AppendLine("\t\t\t{");
			foreach (var name in infoCodeNames)
			{
				outputText.Append("\t\t\t\tcase ErrorCode.");
				outputText.Append(name);
				outputText.AppendLine(":");
			}
			outputText.AppendLine("\t\t\t\t\treturn true;");
			outputText.AppendLine("\t\t\t\tdefault:");
			outputText.AppendLine("\t\t\t\t\treturn false;");
			outputText.AppendLine("\t\t\t}");
			outputText.AppendLine("\t\t}");

			outputText.AppendLine("\t}");
			outputText.AppendLine("}");

			File.WriteAllText(outputPath, outputText.ToString());
		}
	}
}
