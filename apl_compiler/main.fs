module apl_compiler.main

open apl_compiler.Parser
open apl_compiler.Lexer
open System.IO

let getInputString () : string =
    System.Console.Write("Enter an expression: ")
    System.Console.ReadLine()

let fullListToString values : string = values |> Seq.map (fun n -> n.ToString()) |> String.concat ", " |> sprintf "[ %s ]"

[<EntryPoint>]
let main _ =
    let aplProgram = File.ReadAllText("test_program.apl")
    let out = aplProgram |> lex |> parse
    printfn "Simple Interpreter for APL"
    printfn "-----"
    printfn $"Input: \n %A{aplProgram}"
    printfn "-----"
    printfn $"Parse Tree: %A{out}"
    0
