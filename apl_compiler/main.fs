module apl_compiler.main

open apl_compiler.Parser
open apl_compiler.Lexer
open apl_compiler.Symbols
open apl_compiler.Runtime

open System.IO

[<EntryPoint>]
let main _ =
    let aplProgram = File.ReadAllText("test_program.apl")
    let parseTree = aplProgram |> lex |> parse
    let symbolTable = parseTree |> createSymbols
    //let out = parseTree |> runtime
    printfn "Simple Interpreter for APL"
    printfn "-----"
    printfn $"Input: \n %A{aplProgram}"
    printfn "-----"
    printfn $"ParseTree: \n %A{parseTree}"
    printfn "-----"
    printfn $"SymbolTable: \n %A{symbolTable._symbolTable}"
    //printfn "-----"
    //printfn $"Output: %A{snd out}"
    0
