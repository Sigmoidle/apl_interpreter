module apl_compiler.Symbols

open apl_compiler.Parser

type Symbol = { symbolType: SymbolType; shape: int * int; value: float list }

and SymbolType =
    | Scalar
    | Vector
    | Unknown


type RuntimeData = { _program: Program; _symbolTable: Map<string, Symbol> }

let createSymbols (program: Program): RuntimeData =
    
    let originalProgram = program
    
    let rec _Program (data: RuntimeData) =
        match data._program with
        | Program.EndOfFile -> {data with _program = originalProgram}
        | Program.NewLine newProgram -> _Program {data with _program = newProgram}
        | Program.Statement (statement, program) ->
            let newSymbolTable = _Statement (statement, data._symbolTable)
            _Program {data with _program = program; _symbolTable = newSymbolTable}
            
    and _Statement (statement, symbolTable) =
        match statement with
        | Statement.Assign(symbolName, _) ->
            symbolTable.Add(symbolName, {symbolType = Unknown; shape = (0,0); value = [0]})
        | _ -> symbolTable
        
    
    _Program {_program = program; _symbolTable = Map.empty }