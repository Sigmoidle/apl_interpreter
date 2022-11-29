module apl_compiler.main
open apl_compiler.Parser
open apl_compiler.Lexer

let getInputString() : string = 
    System.Console.Write("Enter an expression: ")
    System.Console.ReadLine()

[<EntryPoint>]
let main _ =
    System.Console.WriteLine("Simple Interpreter for APL")
    //let input = "1 + 1"
    let input = getInputString()
    let tokList = lex input
    System.Console.WriteLine("-----")
    let out = parseAndEval tokList
    System.Console.WriteLine("Result = {0}", snd out)
    0