module apl_compiler.main

open apl_compiler.Parser
open apl_compiler.Lexer

let getInputString () : string =
    System.Console.Write("Enter an expression: ")
    System.Console.ReadLine()

[<EntryPoint>]
let main _ =
    System.Console.WriteLine("Simple Interpreter for APL")
    System.Console.WriteLine("-----")
    let out = getInputString() |> lex |> parseAndEval
    System.Console.WriteLine("-----")
    System.Console.WriteLine("Result = {0}", snd out)
    0
