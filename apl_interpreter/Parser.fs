﻿module apl_compiler.Parser

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
    | Tally of Expression
    | Negate of Expression
    | Roll of Expression
    | SignOf of Expression
    | Reciprocal of Expression
    | DivideReduce of Expression
    | MultiplyReduce of Expression
    | AddReduce of Expression
    | SubtractReduce of Expression
    | IndexGenerator of Expression
    | GradeUp of Expression
    | GradeDown of Expression

and DyadicFn =
    | Add of Expression * Expression
    | Deal of Expression * Expression
    | Multiply of Expression * Expression
    | Divide of Expression * Expression
    | Subtract of Expression * Expression
    | Range of Expression * Expression
    | Select of Expression * Expression
    | LogicalAnd of Expression * Expression
    | LogicalOr of Expression * Expression
    | LogicalNand of Expression * Expression
    | LogicalNor of Expression * Expression
    | LessThan of Expression * Expression
    | LessOrEqual of Expression * Expression
    | Equals of Expression * Expression
    | GreaterOrEqual of Expression * Expression
    | GreaterThan of Expression * Expression
    | NotEqual of Expression * Expression

and NList =
    | NListIdentifier of string
    | NListValue of float list

let dyadicFunctionTokenList =
    [ Token.Plus
      Token.QuestionMark
      Token.Multiplication
      Token.Division
      Token.Hyphen
      Token.Range
      Token.Select
      Token.LogicalAnd
      Token.LogicalOr
      Token.LogicalNand
      Token.LogicalNor
      Token.LessThan
      Token.LessOrEqual
      Token.Equals
      Token.GreaterOrEqual
      Token.GreaterThan
      Token.NotEqual ]

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
        | Token.Multiplication :: tail ->
            let newTokens, expression2 = _Expression tail
            (newTokens, DyadicFn.Multiply(expression1, expression2))
        | Token.Division :: tail ->
            let newTokens, expression2 = _Expression tail
            (newTokens, DyadicFn.Divide(expression1, expression2))
        | Token.Hyphen :: tail ->
            let newTokens, expression2 = _Expression tail
            (newTokens, DyadicFn.Subtract(expression1, expression2))
        | Token.Range :: tail ->
            let newTokens, expression2 = _Expression tail
            (newTokens, DyadicFn.Range(expression1, expression2))
        | Token.Select :: tail ->
            let newTokens, expression2 = _Expression tail
            (newTokens, DyadicFn.Select(expression1, expression2))
        | Token.LogicalAnd :: tail ->
            let newTokens, expression2 = _Expression tail
            (newTokens, DyadicFn.LogicalAnd(expression1, expression2))
        | Token.LogicalOr :: tail ->
            let newTokens, expression2 = _Expression tail
            (newTokens, DyadicFn.LogicalOr(expression1, expression2))
        | Token.LogicalNand :: tail ->
            let newTokens, expression2 = _Expression tail
            (newTokens, DyadicFn.LogicalNand(expression1, expression2))
        | Token.LogicalNor :: tail ->
            let newTokens, expression2 = _Expression tail
            (newTokens, DyadicFn.LogicalNor(expression1, expression2))
        | Token.LessThan :: tail ->
            let newTokens, expression2 = _Expression tail
            (newTokens, DyadicFn.LessThan(expression1, expression2))
        | Token.LessOrEqual :: tail ->
            let newTokens, expression2 = _Expression tail
            (newTokens, DyadicFn.LessOrEqual(expression1, expression2))
        | Token.Equals :: tail ->
            let newTokens, expression2 = _Expression tail
            (newTokens, DyadicFn.Equals(expression1, expression2))
        | Token.GreaterOrEqual :: tail ->
            let newTokens, expression2 = _Expression tail
            (newTokens, DyadicFn.GreaterOrEqual(expression1, expression2))
        | Token.GreaterThan :: tail ->
            let newTokens, expression2 = _Expression tail
            (newTokens, DyadicFn.GreaterThan(expression1, expression2))
        | Token.NotEqual :: tail ->
            let newTokens, expression2 = _Expression tail
            (newTokens, DyadicFn.NotEqual(expression1, expression2))
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
        | Token.Tally :: tail ->
            let newTokens, expression = _Expression tail
            (newTokens, MonadicFn.Tally(expression))
        | Token.Multiplication :: tail ->
            let newTokens, expression = _Expression tail
            (newTokens, MonadicFn.SignOf(expression))
        | Token.Division :: tail ->
            let newTokens, expression = _Expression tail
            (newTokens, MonadicFn.Reciprocal(expression))
        | Token.Hyphen :: tail ->
            let newTokens, expression = _Expression tail
            (newTokens, MonadicFn.Negate(expression))
        | Token.MultiplyReduce :: tail ->
            let newTokens, expression = _Expression tail
            (newTokens, MonadicFn.MultiplyReduce(expression))
        | Token.DivideReduce :: tail ->
            let newTokens, expression = _Expression tail
            (newTokens, MonadicFn.DivideReduce(expression))
        | Token.AddReduce :: tail ->
            let newTokens, expression = _Expression tail
            (newTokens, MonadicFn.AddReduce(expression))
        | Token.SubtractReduce :: tail ->
            let newTokens, expression = _Expression tail
            (newTokens, MonadicFn.SubtractReduce(expression))
        | Token.Iota :: tail ->
            let newTokens, expression = _Expression tail
            (newTokens, MonadicFn.IndexGenerator(expression))
        | Token.GradeUp :: tail ->
            let newTokens, expression = _Expression tail
            (newTokens, MonadicFn.GradeUp(expression))
        | Token.GradeDown :: tail ->
            let newTokens, expression = _Expression tail
            (newTokens, MonadicFn.GradeDown(expression))
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
