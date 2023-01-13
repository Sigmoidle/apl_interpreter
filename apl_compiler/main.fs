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
    let runtimeData = parseTree |> createSymbols
    let out = runtimeData |> runtime
    printfn "Simple Interpreter for APL"
    printfn "-----"
    printfn $"Input: \n%A{aplProgram}"
    printfn "-----"
    printfn $"ParseTree: \n%A{parseTree}"
    printfn "-----"
    printfn $"SymbolTable before running: \n%A{runtimeData._symbolTable}"
    printfn "-----"
    printfn $"Output: %A{snd out}"
    printfn "-----"
    printfn $"Finished Symbol Table: \n%A{(fst out)._symbolTable}"
    0
