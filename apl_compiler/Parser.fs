module apl_compiler.Parser

open Lexer

// Backus Nuar From for APL

// <PROGRAM> := <STATEMENT> * <EOF> 
// <STATEMENT> := ( <ARRAY> * <Dyadic F> | <Monadic F> ) * <ARRAY>
// <ARRAY> := ( "(" <STATEMENT> ")" | <FLOAT> ) +
// <ASSIGN> := <IDENTIFIER> "←" <ARRAY> | <STATEMENT>
// <IDENTIFIER> := "a".."z" | "A".."Z"
// <Dyadic F> := "Plus(+)"
// <Monadic F> := "Conjugate(+)"

[<EntryPoint>]
let main _ =
    testLexer
    0