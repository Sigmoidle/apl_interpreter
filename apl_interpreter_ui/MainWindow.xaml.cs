using System;
using System.Collections.Generic;
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
        "+", "-", "×", "÷", "?", "~", "⍳", "⍋", "⍒", "≢", "…", "⊇", "←", "⍝", "∧", "∨", "⍲", "⍱", "≤",
        "≥", "≠", "|", "¯", "⌊", "∊", "⌈", "*"
    };

    public MainWindow()
    {
        InitializeComponent();

        SetWindowName();

        PopulateHelpTable();

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

    public struct Help
    {
        public string Symbol { set; get; }
        public string Type { set; get; }
        public string Name { set; get; }
        public string Information { set; get; }
    }

    private void PopulateHelpTable()
    {
        HelpTable.Items.Add(new Help
        {
            Symbol = ":If :Else :End", Type = "Statement", Name = "If statement",
            Information =
                "The APL if statement must contain all 3 parts. There should be a boolean expression to the right of the :If token."
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = ":While :EndWhile", Type = "Statement", Name = "While loop statement",
            Information =
                "The APL while statement must contain all 2 parts. There should be a boolean expression to the right of the :While token."
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "~", Type = "Monadic Function", Name = "Not",
            Information =
                "Not only takes in boolean numbers. Not will turn a 1 to a 0 and a 0 to a 1 in a scalar or vector"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "≢", Type = "Monadic Function", Name = "Tally",
            Information = "Tally takes in a scalar or vector and returns its length."
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "-", Type = "Monadic Function", Name = "Negate",
            Information = "Negate will multiply each number in a scalar or vector by -1"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "?", Type = "Monadic Function", Name = "Roll",
            Information = "Roll will produce a random number between 1 and its RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "×", Type = "Monadic Function", Name = "Sign Of",
            Information =
                "Sign Of will produce a scalar or vector of equal size to its RHS where -1, 0 or 1 denotes the sign of the RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "÷", Type = "Monadic Function", Name = "Reciprocal",
            Information = "Reciprocal gets the Reciprocal of every number in its RHS vector"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "÷", Type = "Monadic Function", Name = "Reciprocal",
            Information = "Reciprocal gets the Reciprocal of every number in its RHS vector"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "×/", Type = "Monadic Function", Name = "Multiply Reduce",
            Information = "Multiply reduce"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "÷/", Type = "Monadic Function", Name = "Divide Reduce",
            Information = "Divide reduce"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "+/", Type = "Monadic Function", Name = "Add Reduce",
            Information = "Add reduce"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "-/", Type = "Monadic Function", Name = "Subtract Reduce",
            Information = "Subtract reduce"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "⍳", Type = "Monadic Function", Name = "Index Generator",
            Information = "Produces a vector from 1 to the scalar on it's RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "⍋", Type = "Monadic Function", Name = "Grade Up",
            Information = "Produces a vector of indices that are the sorted version of the RHS vector"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "⍒", Type = "Monadic Function", Name = "Grade Down",
            Information = "Produces a vector of indices that are the reverse-sorted version of the RHS vector"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "|", Type = "Monadic Function", Name = "Magnitude",
            Information = "Makes every number positive in the vector/array to its RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "⌈", Type = "Monadic Function", Name = "Ceiling",
            Information = "Rounds all floats in a scalar/vector up"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "*", Type = "Monadic Function", Name = "Exponential",
            Information = "Computes the exponential function to the power of Euler's constant e"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "⌊", Type = "Monadic Function", Name = "Floor",
            Information = "Rounds all floats in a scalar/vector down"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "←", Type = "Assign Expression", Name = "Assign",
            Information = "Assigns the RHS to a variable. It also outputs the RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "+", Type = "Dyadic Function", Name = "Add",
            Information =
                "Adds the LHS to the RHS. Works on scalars or vectors of the same size. It will also work with a scalar on the LHS and a vector on the RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "?", Type = "Dyadic Function", Name = "Deal",
            Information = "Deal produces a random scalar number in-between the scalars on its LHS and RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "×", Type = "Dyadic Function", Name = "Multiply",
            Information =
                "Multiplies the LHS to the RHS. Works on scalars or vectors of the same size. It will also work with a scalar on the LHS and a vector on the RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "÷", Type = "Dyadic Function", Name = "Divide",
            Information =
                "Divides the LHS to the RHS. Works on scalars or vectors of the same size. It will also work with a scalar on the LHS and a vector on the RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "-", Type = "Dyadic Function", Name = "Subtract",
            Information =
                "Subtracts the LHS to the RHS. Works on scalars or vectors of the same size. It will also work with a scalar on the LHS and a vector on the RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "…", Type = "Dyadic Function", Name = "Range",
            Information = "Produces a range of numbers in a vector from and to the scalars on its LHS and RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "⊇", Type = "Dyadic Function", Name = "Select",
            Information =
                "Selects the numbers out of the vector to its RHS using the indices in the vector to its LHS. Produces a new vector."
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "∧", Type = "Dyadic Function", Name = "Logical And",
            Information = "Acts as a logical and on binary numbers"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "∨", Type = "Dyadic Function", Name = "Logical Or",
            Information = "Acts as a logical or on binary numbers"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "⍲", Type = "Dyadic Function", Name = "Logical Not-And",
            Information = "Acts as a logical NAND on binary numbers"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "⍱", Type = "Dyadic Function", Name = "Logical Not-Or",
            Information = "Acts as a logical NOR on binary numbers"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "<", Type = "Dyadic Function", Name = "Less Than",
            Information =
                "Produces a boolean representing if the scalar to its LHS is smaller than the scalar on its RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "≤", Type = "Dyadic Function", Name = "Less or Equal",
            Information =
                "Produces a boolean representing if the scalar to its LHS is smaller than or equal to the scalar on its RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "=", Type = "Dyadic Function", Name = "Equals",
            Information =
                "Produces a boolean representing if the scalar or vector to its LHS is equal to the scalar or vector on its RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "≥", Type = "Dyadic Function", Name = "Greater or Equal",
            Information =
                "Produces a boolean representing if the scalar to its LHS is greater than or equal to the scalar on its RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = ">", Type = "Dyadic Function", Name = "Greater Than",
            Information =
                "Produces a boolean representing if the scalar to its LHS is greater than or equal to the scalar on its RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "≠", Type = "Dyadic Function", Name = "Equals",
            Information =
                "Produces a boolean representing if the scalar or vector to its LHS is not equal to the scalar or vector on its RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "|", Type = "Dyadic Function", Name = "Modulus",
            Information = "Takes a scalar to its LHS then finds the modulus of each number in the vector to its RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = ",", Type = "Dyadic Function", Name = "Catenate",
            Information = "Concatenates the vector to its LHS to the vector on its RHS"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "∊", Type = "Dyadic Function", Name = "Membership",
            Information =
                "Membership checks to see if the vector elements to its LHS are in the vector to its RHS. It produces 1 for yes and 0 for no"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "⌈", Type = "Dyadic Function", Name = "Maximum",
            Information = "Produces the scalar of which scalar to its LHS or RHS was larger (maybe works with vectors)"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "⌊", Type = "Dyadic Function", Name = "Minimum",
            Information = "Produces the scalar of which scalar to its LHS or RHS was smaller (maybe works with vectors)"
        });
        HelpTable.Items.Add(new Help
        {
            Symbol = "*", Type = "Dyadic Function", Name = "Power",
            Information = "Raises the scalar to its RHS to the power of the scalar on the LHS"
        });
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

        Parser.Program program;

        try
        {
            program = Parser.parse(tokens);
        }
        catch (Exception ex)
        {
            AddOutput(" < " + ex.Message, Brushes.Red);
            return;
        }


        SyntaxTree.Document.Blocks.Clear();
        SyntaxTree.Document.Blocks.Add(new Paragraph(new Run(main.getParseTreeAsString(code))));

        Symbols.RuntimeData symbols;

        try
        {
            symbols = Symbols.createSymbols(program);
        }
        catch (Exception ex)
        {
            AddOutput(" < " + ex.Message, Brushes.Red);
            return;
        }

        Tuple<Symbols.RuntimeData, FSharpList<double>> output;

        try
        {
            output = Runtime.runtime(symbols);
            AddOutput(" < " + "[ " + string.Join("; ", output.Item2) + " ]", Brushes.Black);
        }
        catch (Exception ex)
        {
            AddOutput(" < " + ex.Message, Brushes.Red);
            return;
        }

        var symbolTableSource = new List<SymbolTableEntry>();

        foreach (var entry in output.Item1._symbolTable)
        {
            symbolTableSource.Add(new SymbolTableEntry()
                { Identifier = entry.Key, Value = string.Join(",", entry.Value) });
        }

        SymbolTable.ItemsSource = symbolTableSource;
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

class SymbolTableEntry
{
    public string Identifier { get; set; } = null!;
    public string Value { get; set; } = null!;
}