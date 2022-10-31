module apl_compiler.Parser
open apl_compiler.Lexer

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
 
and Statement =
    Function of float * Function * float

and Function =
    Add
     
