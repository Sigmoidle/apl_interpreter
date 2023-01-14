module apl_compiler.Parser

open System
open Lexer

(*
<PROGRAM> ::=
            | EndOfFile
            | NewLine: <PROGRAM>
            | <EXPRESSION> <PROGRAM>

<EXPRESSION> ::= 
            | Assign: string * <EXPRESSION>
            | <MonadicFn>
            | <DyadicFn>
            | <NList>
            
<MonadicFn> ::=
            | Not <EXPRESSION>
            | Roll <EXPRESSION>
            
<DyadicFn> ::=
            | Add <EXPRESSION> <EXPRESSION>
            | Deal <EXPRESSION> <EXPRESSION>

<NList> ::=
            | list of floats
            | string
 *)

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
    | Not of Expression
    | Roll of Expression

and DyadicFn =
    | Add of Expression * Expression
    | Deal of Expression * Expression

and NList =
    | NListIdentifier of string
    | NListValue of float list

let dyadicFunctionTokenList = [ Token.Plus; Token.QuestionMark ]

let private parseError error = Exception(error)

let rec private isNewLineOrEndNext tokens =
    match tokens with
    | Token.Number _ :: tail -> isNewLineOrEndNext tail
    | Token.NewLine :: _ -> true
    | Token.EndOfFile :: _ -> true
    | [] -> true
    | _ -> false

let private gotoMatchingBracket tokens =
    let rec go tokens acc depth =
        match tokens with
        | Token.RightBracket :: _ when depth = 0 -> (tokens, List.rev acc)
        | Token.RightBracket :: tail when depth <> 0 -> go tail (Token.RightBracket :: acc) (depth - 1)
        | Token.LeftBracket :: tail -> go tail (Token.LeftBracket :: acc) (depth + 1)
        | token :: tail -> go tail (token :: acc) depth
        | _ -> raise <| parseError "The brackets are unbalanced!"

    go tokens [] 0

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
        | Token.LeftBracket :: tail ->
            let newTokens, accumulation = gotoMatchingBracket tail

            match newTokens with
            | _ :: token :: tail when List.contains token dyadicFunctionTokenList ->
                let tokens, dyadicFn = (token :: tail, snd <| _Expression accumulation) |> _DyadicFn
                (tokens, Expression.DyadicFn(dyadicFn))
            | _ :: tail -> (tail, snd <| _Expression accumulation)
            | _ -> ([], snd <| _Expression accumulation)
        | Token.Identifier name :: Token.Assign :: tail ->
            let newTokens, expression = _Expression tail
            (newTokens, Expression.Assign(name, expression))
        | Token.Identifier name :: tail when isNewLineOrEndNext tail -> (tail, Expression.NList(NList.NListIdentifier name))
        | Token.Number _ :: tail when isNewLineOrEndNext tail ->
            let newTokens, nList = _NList tokens
            (newTokens, Expression.NList(nList))
        | Token.Number _ :: _ ->
            let newTokens, nList = tokens |> _NList
            let newTokens, dyadicFn = (newTokens, Expression.NList nList) |> _DyadicFn
            (newTokens, Expression.DyadicFn(dyadicFn))
        | Token.Identifier string :: tail ->
            let newTokens, dyadicFn = (tail, Expression.NList(NList.NListIdentifier string)) |> _DyadicFn
            (newTokens, Expression.DyadicFn(dyadicFn))
        | _ ->
            let newTokens, monadicFn = _MonadicFn tokens
            (newTokens, Expression.MonadicFn(monadicFn))

    and _DyadicFn (tokens, expression1) =
        match tokens with
        | Token.Plus :: tail ->
            let newTokens, expression2 = _Expression tail
            (newTokens, DyadicFn.Add(expression1, expression2))
        | Token.QuestionMark :: tail ->
            let newTokens, expression2 = _Expression tail
            (newTokens, DyadicFn.Deal(expression1, expression2))
        | token :: _ -> raise <| parseError $"%A{token} is not a recognised dyadic function"
        | _ -> raise <| parseError "Empty token list when processing dyadic function"

    and _MonadicFn tokens =
        match tokens with
        | Token.Tilde :: tail ->
            let newTokens, expression = _Expression tail
            (newTokens, MonadicFn.Not(expression))
        | Token.QuestionMark :: tail ->
            let newTokens, expression = _Expression tail
            (newTokens, MonadicFn.Roll(expression))
        | token :: _ -> raise <| parseError $"%A{token} is not a recognised monadic function"
        | _ -> raise <| parseError "Empty token list when processing monadic function"

    and _NList tokens =
        match tokens with
        | Token.Number value :: tail ->
            let newTokens, nList = _NList tail

            match nList with
            | NListValue nList -> (newTokens, NList.NListValue(value :: nList))
            | NListIdentifier _ -> raise <| parseError "Identifier found while attempting to parse NList value"
        | _ -> (tokens, NList.NListValue [])

    _Program tokens
