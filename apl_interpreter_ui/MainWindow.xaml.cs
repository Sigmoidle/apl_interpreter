﻿using System;
using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using apl_compiler;
using Microsoft.FSharp.Collections;
using Microsoft.Win32;

namespace apl_compiler_ui;

/// <summary>
/// Interaction logic for MainWindow.xaml
/// </summary>
public partial class MainWindow
{
    private bool _modifiedSinceLastSave;
    private string _filePath = "";
    private bool _replFocused;

    // list of apl symbols to stick in the toolbar
    private readonly string[] _symbols =
    {
        "+", "-", "×", "÷", "?", "~", "⍳", "⍋", "⍒", "≢", "…", "⊇", "/", "←", "⍝"
    };

    public MainWindow()
    {
        InitializeComponent();

        SetWindowName();

        foreach (var symbol in _symbols)
        {
            var btn = new Button
            {
                Content = symbol,
                FontFamily = new FontFamily("Monospace")
            };
            btn.Click += SymbolOnClick;
            btn.Width = 32;

            SymbolToolbar.Items.Add(btn);
        }
    }

    private void SetWindowName()
    {
        Title = "APL Interpreter - " + (string.IsNullOrEmpty(_filePath) ? "Untitled" : _filePath);
    }

    private bool Save(bool forceSaveAs)
    {
        if (forceSaveAs || string.IsNullOrEmpty(_filePath))
        {
            var dialog = new SaveFileDialog
            {
                Filter = "APL programs (*.apl)|*.apl|All files (*.*)|*.*",
                FileName = _filePath
            };

            if (dialog.ShowDialog() == true)
            {
                _filePath = dialog.FileName;
            }
            else
            {
                return false;
            }
        }

        File.WriteAllText(_filePath, Apl.Text);
        _modifiedSinceLastSave = false;
        SetWindowName();
        return true;
    }

    private bool SaveIfModified()
    {
        if (!_modifiedSinceLastSave) return true;

        var result = MessageBox.Show(
            "Your program has been modified since you last saved. Would you like to save your changes?",
            "APL Interpreter", MessageBoxButton.YesNoCancel);

        return result switch
        {
            MessageBoxResult.Yes => Save(false),
            MessageBoxResult.No => true,
            MessageBoxResult.Cancel => false,
            _ => true
        };
    }

    private void InsertText(string character)
    {
        var box = _replFocused ? ReplInput : Apl;
        var oldCaretIndex = box.CaretIndex;
        box.Text = box.Text.Insert(box.CaretIndex, character);
        box.CaretIndex = oldCaretIndex + 1;
        box.Focus();
    }

    private void RunCode(string code)
    {
        LexerOutput.Items.Clear();

        AddOutput(" > " + code.Trim(), Brushes.Gray);

        FSharpList<Lexer.Token> tokens;

        try
        {
            tokens = main.getTokens(code);
        }
        catch (Exception ex)
        {
            AddOutput(" < " + ex.Message, Brushes.Red);
            return;
        }

        foreach (var token in tokens)
        {
            var item = new ListBoxItem
            {
                Content = token.ToString()
            };
            LexerOutput.Items.Add(item);
        }

        try
        {
            var output = main.interpret(code);
            AddOutput(" < " + "[ " + string.Join("; ", output.Item2) + " ]", Brushes.Black);
        }
        catch (Exception ex)
        {
            AddOutput(" < " + ex.Message, Brushes.Red);
        }
    }

    private void RunOnClick(object sender, RoutedEventArgs e)
    {
        AddOutput("[RUNNING PROGRAM]", Brushes.Gray);
        RunCode(Apl.Text);
        AddOutput("[PROGRAM FINISHED]", Brushes.Gray);
    }

    private void NewOnClick(object sender, RoutedEventArgs e)
    {
        if (!SaveIfModified()) return;
        _filePath = "";
        Apl.Text = "";
        SetWindowName();
    }

    private void OpenOnClick(object sender, RoutedEventArgs e)
    {
        // if cancelled, don't open
        if (!SaveIfModified()) return;

        var dialog = new OpenFileDialog
        {
            Filter = "APL programs (*.apl)|*.apl|All files (*.*)|*.*"
        };

        if (dialog.ShowDialog() != true) return;

        Apl.Text = File.ReadAllText(dialog.FileName);
        _filePath = dialog.FileName;
        SetWindowName();
    }

    private void SaveOnClick(object sender, RoutedEventArgs e)
    {
        Save(false);
    }

    private void SaveAsOnClick(object sender, RoutedEventArgs e)
    {
        Save(true);
    }

    private void SymbolOnClick(object sender, RoutedEventArgs e)
    {
        var button = (Button)sender;
        InsertText((string)button.Content);
    }

    private void AddOutput(string text, Brush colour)
    {
        var para = new Paragraph
        {
            Foreground = colour
        };
        para.Inlines.Add(new Run(text));
        Output.Document.Blocks.Add(para);
        Output.ScrollToEnd();
    }

    private void ReplInputOnKeyDown(object sender, KeyEventArgs e)
    {
        if (e.Key != Key.Return) return;
        RunCode(ReplInput.Text);
        ReplInput.Text = "";
    }

    private void Apl_OnGotFocus(object sender, RoutedEventArgs e)
    {
        _replFocused = false;
    }

    private void ReplInput_OnGotFocus(object sender, RoutedEventArgs e)
    {
        _replFocused = true;
    }
}