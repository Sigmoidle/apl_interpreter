module apl_compiler.Parser
open apl_compiler.Lexer

// Backus Nuar Form for APL
(*
<ExpressionList> ::= 
                | "NewLine"
                | <Expression>
                | <Expression> "NewLine"  
                | <Expression> "NewLine" <ExpressionList>          
<Expression> ::=  
                | <ArrayExpression>
<ArrayExpression> ::=
                | <ArrayAtom>
                | <Function> <ArrayExpression>
                | <ArrayNaming>
<ArrayAtom> ::=
                | "Number"
                | Strand
<ArrayNaming> ::= "Identifier" "←" <ArrayExpression>
<Strand> ::= <ArrayAtom> <Strand>
<Function> ::= <FunctionAtom>
<FunctionAtom> ::= <F>
<F> ::= "+"
*)

type ExpressionList =
    | NewLine
    | Expression of Expression
    | ExpressionNewLine of Expression
    | ExpressionNewLineExpressionList of Expression * ExpressionList
    
and Expression =
    | ArrayExpression of ArrayExpression

and ArrayExpression =
    | ArrayAtom of ArrayAtom
    | FunctionArrayExpression of Function * ArrayExpression
    | ArrayNaming of ArrayNaming
    
and ArrayAtom =
    | Number of float
    | Strand of Strand
    
and ArrayNaming = Naming of string * ArrayExpression

and Strand = Strand of ArrayAtom * Strand

and Function = Function of FunctionAtom

and FunctionAtom = FunctionAtom of F

and F = Token of PrimitiveFunction

and PrimitiveFunction =
    | Plus // +
    // | Hyphen // -
    // | Multiplication // ×
    // | Division // ÷
    // | Asterisk // *
    // | CircleStar // ⍟
    // | LeftCeiling // ⌈
    // | LeftFloor // ⌊
    // | VerticalBar // |
    // | ExclamationMark // !
    // | WhiteCircle // ○
    // | LessThan // <
    // | NotGreaterThan // ≤
    // | Equals // =
    // | NotLessThan // ≥
    // | GreaterThan // >
    // | NotEqual // ≠
    // | LogicalAnd // ∧
    // | LogicalOr // ∨
    // | UpCaretTilde // ⍲
    // | DownCaretTilde // ⍱
    // | Tilde // ~
    // | QuestionMark // ?
    // | Rho // ⍴
    // | Iota // ⍳
    // | SmallElementOf // ∊
    // | UpwardPointingArrow // ↑
    // | DownwardPointingArrow // ↓
    // | DeltaStile // ⍋
    // | DelStile // ⍒
    // | CircleStile // ⌽
    // | CircleBackslash // ⍉
    // | CircledMinus // ⊖
    // | Slash

// and Slash =
//     | Slash // /
//     | SlashBar // ⌿
//     | Backslash // \
//     | BackslashBar // ⍀


[<EntryPoint>]
let main _ =
    testLexer
    0