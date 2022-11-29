module apl_compiler.Parser

open Lexer

(*
<PROGRAM> ::=
            | EndOfFile
            | <STATEMENT> EndOfFile

<STATEMENT> ::= Scalar <FUNCTION> Scalar

<FUNCTION> ::= "+"

 *)

type Program =
    | EndOfFile
    | Statement of Statement

and Statement = Function of float * Function * float

and Function = Add

let private parseError = System.Exception("parse error")

// TODO: make copy of this fn that adds the parts to a parse tree as it
//       goes instead of eval
let parseAndEval tokens =
    let rec Program tokens =
        match tokens with
        | Token.EndOfFile :: tail -> (tail, 0.0) // Probably shouldn't return 0 here but not sure what val to return
        | _ -> Statement tokens

    and Statement tokens = (Number >> Function) tokens

    and Function (tokens, value) =
        match tokens with
        | Token.Plus :: tail ->
            let tokens, tokenVal = Number tail
            Function(tokens, value + tokenVal)
        | _ -> (tokens, value)

    and Number tokens =
        match tokens with
        | Token.Number value :: tail -> (tail, value)
        | _ -> raise parseError

    Program tokens
