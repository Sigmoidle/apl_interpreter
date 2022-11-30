using System.IO;
using System.Windows;
using System.Windows.Controls;
using Microsoft.Win32;

namespace apl_compiler_ui
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private bool _modifiedSinceLastSave = false;
        private string _filePath = "";

        private readonly string[] _symbols = { "+", "-", "×", "÷", "⌈", "⌊", "*", "⍟", "|", "?", "○", "!" };

        public MainWindow()
        {
            InitializeComponent();

            SetWindowName();

            foreach (var symbol in _symbols)
            {
                var btn = new Button();
                btn.Content = symbol;
                btn.Click += SymbolOnClick;
                btn.Width = 32;

                SymbolToolbar.Items.Add(btn);
            }
        }

        private void SetWindowName()
        {
            Title = "APL Transpiler - " + (string.IsNullOrEmpty(_filePath) ? "Untitled" : _filePath);
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
                "APL Transpiler", MessageBoxButton.YesNoCancel);

            switch (result)
            {
                case MessageBoxResult.Yes:
                    return Save(false);
                case MessageBoxResult.No:
                    return true;
                case MessageBoxResult.Cancel:
                    return false;
            }

            return true;
        }

        private void InsertText(string character)
        {
            Apl.Text = Apl.Text.Insert(Apl.CaretIndex, character);
            Apl.Focus();
        }

        private void TranspileOnClick(object sender, RoutedEventArgs e)
        {
            // todo
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
    }
}