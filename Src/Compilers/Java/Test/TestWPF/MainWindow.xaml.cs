using System.Windows.Threading;
using ICSharpCode.AvalonEdit.Document;
using ICSharpCode.AvalonEdit.Editing;
using ICSharpCode.AvalonEdit.Folding;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using Microsoft.CodeAnalysis.Text;
using Roslyn.SyntaxVisualizer.Control;

namespace TestWPF
{
	/// <summary>
	/// MainWindow.xaml 的交互逻辑
	/// </summary>
	public partial class MainWindow : Window
	{

		FoldingManager _foldingManager;
		object _foldingStrategy;
		private CSharpParseOptions opt;
		private CSharpSyntaxTree currentTree;

		public MainWindow()
		{
			this.SetValue(TextOptions.TextFormattingModeProperty, TextFormattingMode.Display);

			InitializeComponent();

			DispatcherTimer foldingUpdateTimer = new DispatcherTimer();
			foldingUpdateTimer.Interval = TimeSpan.FromSeconds(2);
			foldingUpdateTimer.Tick += delegate { UpdateFoldings(); };
			foldingUpdateTimer.Start();



			this.syntaxVisualizer.SyntaxNodeNavigationToSourceRequested += node => this.NavigateToSource(node.Span);
			this.syntaxVisualizer.SyntaxTokenNavigationToSourceRequested += token => this.NavigateToSource(token.Span);
			this.syntaxVisualizer.SyntaxTriviaNavigationToSourceRequested += trivia => this.NavigateToSource(trivia.Span);


			this.txt2.TextArea.SelectionChanged += (sender, args) =>
			{
				//this.txt2.sele

				//TextSpan ts = new TextSpan(this.txt2.SelectionStart,this.txt2.SelectionLength);

				this.syntaxVisualizer.NavigateToBestMatch(this.txt2.SelectionStart, this.txt2.SelectionLength, null,
					SyntaxCategory.None, true, null);

			};


			this.txt2.TextArea.TextEntered += (sender, args) =>
			{
				//if (this.currentTree != null)
				//{
				//	this.currentTree = (CSharpSyntaxTree)this.currentTree.WithChangedText(SourceText.From(this.txt2.Text));
				//	this.syntaxVisualizer.DisplaySyntaxTree(this.currentTree);
				//}
				//else
				//{
				//	this.currentTree = (CSharpSyntaxTree)CSharpSyntaxTree.ParseText(this.txt2.Text, options: opt);
				//	this.syntaxVisualizer.DisplaySyntaxTree(this.currentTree);
				//}


			};

			opt = CSharpParseOptions.Default;
			opt = opt.WithDocumentationMode(DocumentationMode.Diagnose);
			
		}


		private void Window_Loaded(object sender, RoutedEventArgs e)
		{
			propertyGridComboBox.SelectedIndex = 2;
			this.txt2.TextArea.IndentationStrategy = new ICSharpCode.AvalonEdit.Indentation.CSharp.CSharpIndentationStrategy(this.txt2.Options);
			_foldingStrategy = new BraceFoldingStrategy();
			_foldingManager = FoldingManager.Install(this.txt2.TextArea);


			#region Test Source
			this.txt2.Text = @"/*
 * Copyright 2012, Google Inc.
 * All rights reserved.
 */

@Deprecated
package com.intertech.cms.domain;

import com.google.common.io.ByteStreams;
import org.jf.dexlib2.Opcodes;
import org.jf.dexlib2.dexbacked.raw.*;
import org.jf.dexlib2.dexbacked.util.FixedSizeSet;
import org.jf.dexlib2.iface.DexFile;
import org.jf.util.ExceptionWithContext;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Set;

public class DexBackedDexFile extends BaseDexBuffer implements DexFile {
    private final Opcodes opcodes;

    private final int stringCount;
    private final int stringStartOffset;
    private final int typeCount;
    private final int typeStartOffset;
    private final int protoCount;
    private final int protoStartOffset;
    private final int fieldCount;
    private final int fieldStartOffset;
    private final int methodCount;
    private final int methodStartOffset;
    private final int classCount;
    private final int classStartOffset;

    private DexBackedDexFile(Opcodes opcodes, @Nonnull byte[] buf, int offset, boolean verifyMagic) {
        super(buf);

        this.opcodes = opcodes;

        if (verifyMagic) {
            verifyMagicAndByteOrder(buf, offset);
        }

        stringCount = readSmallUint(HeaderItem.STRING_COUNT_OFFSET);
        stringStartOffset = readSmallUint(HeaderItem.STRING_START_OFFSET);
        typeCount = readSmallUint(HeaderItem.TYPE_COUNT_OFFSET);
        typeStartOffset = readSmallUint(HeaderItem.TYPE_START_OFFSET);
        protoCount = readSmallUint(HeaderItem.PROTO_COUNT_OFFSET);
        protoStartOffset = readSmallUint(HeaderItem.PROTO_START_OFFSET);
        fieldCount = readSmallUint(HeaderItem.FIELD_COUNT_OFFSET);
        fieldStartOffset = readSmallUint(HeaderItem.FIELD_START_OFFSET);
        methodCount = readSmallUint(HeaderItem.METHOD_COUNT_OFFSET);
        methodStartOffset = readSmallUint(HeaderItem.METHOD_START_OFFSET);
        classCount = readSmallUint(HeaderItem.CLASS_COUNT_OFFSET);
        classStartOffset = readSmallUint(HeaderItem.CLASS_START_OFFSET);
    }

    public DexBackedDexFile(@Nonnull Opcodes opcodes, @Nonnull BaseDexBuffer buf) {
        this(opcodes, buf.buf);
    }

    public DexBackedDexFile(@Nonnull Opcodes opcodes, @Nonnull byte[] buf, int offset) {
        this(opcodes, buf, offset, false);
    }

    public DexBackedDexFile(@Nonnull Opcodes opcodes, @Nonnull byte[] buf) {
        this(opcodes, buf, 0, true);
    }

    /**
    * Validates a chess move.
    *
    * Use {@link #doMove(int theFromFile, int theFromRank, int theToFile, int theToRank)} to move a piece.
    *
    * @param opcodes file from which a piece is being moved
    * @param is rank from which a piece is being moved
    * @return true if the move is valid, otherwise false
    */
    public static DexBackedDexFile fromInputStream(@Nonnull Opcodes opcodes, @Nonnull InputStream is)
            throws IOException {
        if (!is.markSupported()) {
            throw new IllegalArgumentException(""InputStream must support mark"");
        }
        is.mark(44);
        byte[] partialHeader = new byte[44];
        try {
            ByteStreams.readFully(is, partialHeader);
        } catch (EOFException ex) {
            throw new NotADexFile(""File is too short"");
        } finally {
            is.reset();
        }

        verifyMagicAndByteOrder(partialHeader, 0);

        byte[] buf = ByteStreams.toByteArray(is);
        return new DexBackedDexFile(opcodes, buf, 0, false);
    }

    @Nonnull
    @Override
    public Set<? extends DexBackedClassDef> getClasses() {
        return new FixedSizeSet<DexBackedClassDef>() {
            @Nonnull
            @Override
            public DexBackedClassDef readItem(int index) {
                return new DexBackedClassDef(DexBackedDexFile.this, getClassDefItemOffset(index));
            }

            @Override
            public int size() {
                return classCount;
            }
        };
    }

    @Nonnull
    public String getString(int stringIndex) {
        int stringOffset = getStringIdItemOffset(stringIndex);
        int stringDataOffset = readSmallUint(stringOffset);
        DexReader reader = readerAt(stringDataOffset);
        int utf16Length = reader.readSmallUleb128();
        return reader.readString(utf16Length);
    }
}
";
			#endregion
		}


		private void NavigateToSource(TextSpan span)
		{

			try
			{
				this.txt2.Select(span.Start, span.Length);

				//this.txt2.SelectionStart
				//DocumentLine line = this.txt2.Document.GetLineByOffset(this.txt2.CaretOffset);
				//this.txt2.Select(line.Offset, line.Length);
				var line = this.txt2.Document.GetLineByOffset(this.txt2.SelectionStart);
				this.txt2.ScrollToLine(line.LineNumber);
			}
			catch (Exception)
			{

			}

		}

		void UpdateFoldings()
		{
			if (_foldingStrategy is BraceFoldingStrategy)
			{
				((BraceFoldingStrategy)_foldingStrategy).UpdateFoldings(_foldingManager, this.txt2.Document);
			}
		}





		private void btnShow_Click(object sender, RoutedEventArgs e)
		{
			
			//
			this.currentTree = (CSharpSyntaxTree)CSharpSyntaxTree.ParseText(this.txt2.Text, options: opt);


			//var cus = tree.GetCompilationUnitRoot();
			this.syntaxVisualizer.DisplaySyntaxTree(this.currentTree);

			//TextSpan 

			//this.syntaxVisualizer.SyntaxTree.GetRoot().DescendantNodesAndTokens()

			//foreach (object item in this.syntaxVisualizer.treeView.Items)
			//{
			//	TreeViewItem treeItem = this.syntaxVisualizer.treeView.ItemContainerGenerator.ContainerFromItem(item) as TreeViewItem;
			//	if (treeItem != null)
			//		ExpandAll(treeItem, true);
			//	treeItem.IsExpanded = true;
			//}

			//this.syntaxVisualizer.treeView.
		}

		private void ExpandAll(ItemsControl items, bool expand)
		{
			foreach (object obj in items.Items)
			{
				ItemsControl childControl = items.ItemContainerGenerator.ContainerFromItem(obj) as ItemsControl;
				if (childControl != null)
				{
					ExpandAll(childControl, expand);
				}
				TreeViewItem item = childControl as TreeViewItem;
				if (item != null)
					item.IsExpanded = true;
			}
		}



		private void propertyGridComboBoxSelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			if (propertyGrid == null)
				return;
			switch (propertyGridComboBox.SelectedIndex)
			{
				case 0:
					propertyGrid.SelectedObject = this.txt2;
					break;
				case 1:
					propertyGrid.SelectedObject = this.txt2.TextArea;
					break;
				case 2:
					propertyGrid.SelectedObject = this.txt2.Options;
					break;
			}
		}
	}
}
