module apl_compiler.Parser

open Lexer

(*
<PROGRAM> ::=
            | EndOfFile
            | <STATEMENT> EndOfFile

<STATEMENT> ::= Scalar <FUNCTION> Scalar

<FUNCTION> ::= "+"

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

let private parseError = System.Exception("parse error")

let private Add (list1: float list, list2: float list) : float list =
    if list1.Length <> list2.Length then
        // TODO: check if uneven array addition in APL is valid
        raise parseError

    let rec AddList (list1: float list, list2: float list) : float list =
        if list1.IsEmpty then
            [] // return empty results list
        else
            let tail1 = list1.Tail
            let tail2 = list2.Tail
            let result = list1.Head + list2.Head
            let resultList = AddList(tail1, tail2)
            result :: resultList

    AddList(list1, list2)

let rec private Not (numList: float list) : float list =
    let ContainsOnlyBinaryValue (numList: float list) : bool = numList |> Seq.forall (fun n -> n = 1.0 || n = 0.0)

    if not (ContainsOnlyBinaryValue numList) then
        raise parseError

    elif numList.IsEmpty then
        numList
    else
        numList |> Seq.map (fun n -> if n = 1.0 then 0.0 else 1.0) |> Seq.toList

// TODO: make copy of this fn that adds the parts to a parse tree as it
//       goes instead of eval
let parseAndEval (tokens: Token list) : Token list * float list =
    let rec Program (tokens: Token list) : Token list * float list =
        match tokens with
        | Token.EndOfFile :: tail -> (tail, []) // Probably shouldn't return an empty array?
        | _ -> Statement tokens

    and Statement (tokens: Token list) : Token list * float list =
        match tokens with
        | Token.Number _ :: _ -> (NList >> DyadicFn) tokens
        | _ -> MonadicFn tokens // could change to list of all monadics

    and MonadicFn (tokens: Token list) : Token list * float list =
        // Currently does not handle stacking multiple Fns
        //  e.g. `~~ 0 1` should apply "not" two times
        match tokens with
        | Token.Tilde :: tail ->
            let tokens, numList = NList tail
            let result = Not numList
            (tokens, result)
        | _ -> raise parseError

    and DyadicFn (tokens, list1) : Token list * float list =
        match tokens with
        | Token.Plus :: tail ->
            let tokens, list2 = NList tail
            let summed = Add(list1, list2)
            // EXIT
            (tokens, summed)
        | _ -> raise parseError

    and NList (tokens: Token list) : Token list * float list =
        match tokens with
        | Token.Number value :: tail ->
            let tokens, numList = NList tail
            (tokens, value :: numList)
        | _ -> (tokens, [])

    Program tokens

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
        | _ -> raise parseError
        
    and _MonadicFn tokens =
        match tokens with
        | Token.Tilde :: tail ->
            let tokens, maybeChain = _MaybeChain tail
            (tokens, MonadicFn.Not(maybeChain))
        | _ -> raise parseError
        
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