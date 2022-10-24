module apl_compiler.Parser

open Lexer

// Backus Nuar Form for APL
(*
<PROGRAM> ::= 
                | (<STATEMENT> | <EXPRESSION>) "End of Tokens" 
<EXPRESSION> ::= 
                | (( <ARRAY> <Dyadic F> | <Monadic F> ) <ARRAY>) <EXPRESSION>
                | (( <ARRAY> <Dyadic F> | <Monadic F> ) <ARRAY>) 
                | <ARRAY>
<STATEMENT> ::= 
                | <ASSIGN>
<ARRAY> ::= 
                | "Number" <ARRAY>
                | "Number"
<ASSIGN> ::= 
                | "IDENTIFIER" "←" <EXPRESSION>
<Dyadic F> ::= "Plus(+)"
<Monadic F> ::= "Conjugate(+)"
*)

[<EntryPoint>]
let main _ =
    testLexer
    0