module apl_compiler.Parser

open Lexer

// Backus Nuar Form for APL
(*
<ExpressionList> ::= 
                | "NewLine"
                | <Expression>
                | <Expression> "NewLine"  
                | <Expression> "NewLine" <ExpressionList>          
<Expression> ::=  
                | <ArrayExpression>
                | "(" <ExpressionList> ")"
<ArrayExpression> ::=
                | <ArrayAtom>
                | <Function> <ArrayExpression>
                | <ArrayNaming>
<ArrayAtom> ::=
                | "Number"
                | Strand
                | "(" <ArrayExpression ")"
<ArrayNaming> ::= "Identifier" "←" <ArrayExpression>
<Strand> ::= <ArrayAtom> <Strand>
<Function> ::= <FunctionAtom>
<FunctionAtom> ::= <F>
<F> ::= "+"
*)



[<EntryPoint>]
let main _ =
    testLexer
    0