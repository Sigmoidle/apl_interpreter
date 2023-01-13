module apl_compiler.Parser

open System
open Lexer

(*
<PROGRAM> ::=
            | EndOfFile
            | <EXPRESSION> <PROGRAM>

<EXPRESSION> ::= 
            | <MonadicFn>
            | <DyadicFn>
            | <NList>
            
<MonadicFn> ::=
            | Not <MaybeChain>
            
<DyadicFn> ::=
            | Add <NList> <MaybeChain>

<MaybeChain> ::= 
            | <NList>
            | <EXPRESSION>

<NList> ::=
            | list of floats
 *)

let functionTokenList = [ Token.Plus; Token.Tilde ]

type Program =
    | Expression of Expression * Program
    | NewLine of Program
    | EndOfFile

and Expression =
    | Assign of string * Expression
    | MonadicFn of MonadicFn
    | DyadicFn of DyadicFn
    | NList of NList

and MonadicFn =
    | Not of MaybeChain
    | Roll of MaybeChain

and DyadicFn =
    | Add of NList * MaybeChain
    | Deal of NList * MaybeChain

and MaybeChain =
    | NoChain of NList
    | Chain of Expression

and NList =
    | NListIdentifier of string
    | NListValue of float list

let private parseError error = Exception(error)

let rec private isNewLineNext tokens =
    match tokens with
    | Token.Number _ :: tail -> isNewLineNext tail
    | Token.NewLine :: _ -> true
    | Token.EndOfFile :: _ -> true
    | _ -> false

let parse tokens =
    let rec _Program tokens =
        match tokens with
        | Token.EndOfFile :: _ -> Program.EndOfFile
        | Token.NewLine :: tail -> Program.NewLine(_Program tail)
        | _ ->
            let newTokens, expression = _Expression tokens
            Program.Expression(expression, _Program newTokens)

    and _Expression tokens =
        match tokens with
        | Token.Identifier name :: Token.Assign :: tail ->
            let newTokens, expression = _Expression tail
            (newTokens, Expression.Assign(name, expression))
        | Token.Identifier name :: tail when isNewLineNext tail -> (tail, Expression.NList(NList.NListIdentifier name))
        | Token.Number _ :: tail when isNewLineNext tail ->
            let newTokens, nList = _NList tokens
            (newTokens, Expression.NList(nList))
        | Token.Number _ :: _ ->
            let newTokens, dyadicFn = (_NList >> _DyadicFn) tokens
            (newTokens, Expression.DyadicFn(dyadicFn))
        | Token.Identifier string :: tail ->
            let newTokens, dyadicFn = (tail, NList.NListIdentifier string) |> _DyadicFn
            (newTokens, Expression.DyadicFn(dyadicFn))
        | _ ->
            let newTokens, monadicFn = _MonadicFn tokens
            (newTokens, Expression.MonadicFn(monadicFn))

    and _DyadicFn (tokens, nList1st) =
        match tokens with
        | Token.Plus :: tail ->
            let newTokens, maybeChain = _MaybeChain tail
            (newTokens, DyadicFn.Add(nList1st, maybeChain))
        | Token.QuestionMark :: tail ->
            let newTokens, maybeChain = _MaybeChain tail
            (newTokens, DyadicFn.Deal(nList1st, maybeChain))
        | token :: _ -> raise <| parseError $"%A{token} is not a recognised dyadic function"
        | _ -> raise <| parseError "Empty token list when processing dyadic function"

    and _MonadicFn tokens =
        match tokens with
        | Token.Tilde :: tail ->
            let newTokens, maybeChain = _MaybeChain tail
            (newTokens, MonadicFn.Not(maybeChain))
        | Token.QuestionMark :: tail ->
            let newTokens, maybeChain = _MaybeChain tail
            (newTokens, MonadicFn.Roll(maybeChain))
        | token :: _ -> raise <| parseError $"%A{token} is not a recognised monadic function"
        | _ -> raise <| parseError "Empty token list when processing monadic function"

    and _MaybeChain tokens =
        match tokens with
        | Token.Number _ :: _ ->
            let newTokens, nList = _NList tokens

            match newTokens with
            | token :: _ when List.contains token functionTokenList ->
                let tokens, expression = _Expression tokens
                (tokens, MaybeChain.Chain(expression))
            | _ -> (newTokens, MaybeChain.NoChain(nList))
        | _ ->
            let newTokens, expression = _Expression tokens
            (newTokens, MaybeChain.Chain(expression))

    and _NList tokens =
        match tokens with
        | Token.Number value :: tail ->
            let newTokens, nList = _NList tail

            match nList with
            | NListValue nList -> (newTokens, NList.NListValue(value :: nList))
            | NListIdentifier _ -> raise <| parseError "Identifier found while attempting to parse NList value"
        | _ -> (tokens, NList.NListValue [])

    _Program tokens
