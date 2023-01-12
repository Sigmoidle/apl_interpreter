module apl_compiler.Parser

open Lexer

(*
<PROGRAM> ::=
            | EndOfFile
            | <STATEMENT> <Program>

<STATEMENT> ::= 
            | <MonadicFn>
            | <DyadicFn>
            
<MonadicFn> ::=
            | Not <MaybeChain>
            
<DyadicFn> ::=
            | Add <MaybeChain>

<MaybeChain> ::= 
            | <NList>
            | <Statement>

<NList> ::=
            | list of floats
 *)

let functionSymbolList = [Token.Plus; Token.Tilde]

type Program =
    | Statement of Statement * Program
    | NewLine of Program
    | EndOfFile

and Statement =
    | MonadicFn of MonadicFn
    | DyadicFn of DyadicFn

and MonadicFn =
    | Not of MaybeChain

and DyadicFn =
    | Add of NList * MaybeChain

and MaybeChain =
    | NoChain of NList
    | Chain of Statement

and NList = float list

let private parseError error = System.Exception(error)

let parse tokens =
    let rec _Program tokens =
        match tokens with
        | Token.EndOfFile :: _ -> Program.EndOfFile
        | Token.NewLine :: tail -> Program.NewLine(_Program tail)
        | _ ->
            let tokens, statement = _Statement tokens
            Program.Statement(statement, _Program tokens)
      
    and _Statement tokens =
        match tokens with
        | Token.Number _ :: _ ->
            let tokens, dyadicFn = (_NList >> _DyadicFn) tokens
            (tokens, Statement.DyadicFn(dyadicFn))
        | _ ->
            let tokens, monadicFn = _MonadicFn tokens
            (tokens, Statement.MonadicFn(monadicFn))
    
    and _DyadicFn (tokens, numList1st) =
        match tokens with
        | Token.Plus :: tail ->
            let tokens, maybeChain = _MaybeChain tail
            (tokens, DyadicFn.Add(numList1st, maybeChain))
        | token :: _ -> raise <| parseError($"%A{token} is not a recognised dyadic function")
        | _ -> raise <| parseError("Empty token list when processing dyadic function")
        
    and _MonadicFn tokens =
        match tokens with
        | Token.Tilde :: tail ->
            let tokens, maybeChain = _MaybeChain tail
            (tokens, MonadicFn.Not(maybeChain))
        | token :: _ -> raise <| parseError($"%A{token} is not a recognised monadic function")
        | _ -> raise <| parseError("Empty token list when processing monadic function")
        
    and _MaybeChain tokens =
        match tokens with
        | Token.Number _ :: _ ->
            let newTokens, numList = _NList tokens
            match newTokens with
            | token :: _ when List.contains token functionSymbolList ->
                let tokens, statement = _Statement tokens
                (tokens, MaybeChain.Chain(statement))
            | _ -> (newTokens, MaybeChain.NoChain(numList))
        | _ ->
            let tokens, statement = _Statement tokens
            (tokens, MaybeChain.Chain(statement))
    
    and _NList tokens =
        match tokens with
        | Token.Number value :: tail ->
            let tokens, numList = _NList tail
            (tokens, value :: numList)
        | _ -> (tokens, [])
    
    _Program tokens