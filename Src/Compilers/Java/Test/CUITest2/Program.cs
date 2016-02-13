using Microsoft.CodeAnalysis.CSharp;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CUITest2
{
	class Program
	{
		static void Main(string[] args)
		{

			var src_path = @"C:\Program Files (x86)\Java\jdk1.7.0_45\src.zip";

			using (ZipArchive zipArchive = ZipFile.OpenRead(src_path))
			{
				foreach (ZipArchiveEntry entry in zipArchive.Entries)
				{
					if (string.IsNullOrEmpty(entry.Name) && entry.Length == 0)
					{

					}
					else
					{
						if (entry.FullName.ToLower().EndsWith(".java"))
						{

							//if (!entry.FullName.StartsWith(@"com/sun/corba"))
							{
								DoOneFile(entry);
							}
							
						}
						
					}
				}

			}
			


			int bb = 6;
		}

		static void DoOneFile(ZipArchiveEntry entry)
		{
			using (var aa = entry.Open())
			using (StreamReader sr = new StreamReader(aa))
			{
				var text = sr.ReadToEnd();


				Trace.TraceInformation(entry.FullName);
				var result = (CSharpSyntaxTree)CSharpSyntaxTree.ParseText(text);
				var digs = result.GetDiagnostics();
				if (digs.Any())
				{
					bool pass = true;
					foreach (var diag in digs)
					{
						
						if (diag.Id == "CS1552")
						{
						}
						else
						{
							pass = false;
						}
					}

					if (!pass)
					{
						Trace.TraceError(entry.FullName);
						
					}
					
				}
			}
		}
	}
}
