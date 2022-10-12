open System
open Microsoft.VisualBasic

let isBlank c = Char.IsWhiteSpace c
let isDigit c = Char.IsDigit c

type MonadicOperatorToken =
    | Reduction // /
    | ReductionAllowAxis // ⌿
    
type DyadicOperatorToken =
    | InnerProduct // .
    | OuterProduct // ∘.

type OperatorToken =
    | AxisLeft // [
    | AxisRight // ]

// Dyadic Tokens refer to the the tokens which can have a "Dyadic"
// Form, this means that the function can only, or is able to
// Take two argument (from it's left and right hand side).
// Depending on Monadic or Dyadic form, the function of the token changes
type DyadicToken =
    | Plus // +
    | Minus // -
    | Times // ×
    | Divide // ÷
    | Maximum // ⌈
    | Minimum // ⌊
    | Power // *
    | Logarithm // ⍟
    | Residue // |
    | Deal // ?
    | Circular // ○
    | BinomialCoefficient // !
    | And // ∧
    | Or // ∨
    | Nand // ⍲
    | Nor // ⍱
    | Less // <
    | NotGreater // ≤
    | Equal // =
    | NotLess // ≥
    | Greater // >
    | NotEqual // ≠
    | Reshape // ⍴
    | Catenate // ,
    | IndexingLeft // [
    | IndexingRight // ]
    | IndexOf // ⍳
    | Take // ↑
    | Drop // ↓
    | Compress // /
    | CompressAllowAxis // ⌿
    | Expand // \
    | ExpandAllowAxis // ⍀
    | Rotate // ⌽
    | RotateAllowAxis // ⊖
    | Membership // ∊
    | Decode // ⊥
    | Encode // ⊤

// Monadic Tokens refer to the the tokens which can have a "monadic"
// Form, this means that the function can only, or is able to
// Take one argument (to it's right).
// Depending on Monadic or Dyadic form, the function of the token changes
type MonadicToken =
    | Conjugate // +
    | Negative // -
    | Signum // ×
    | Reciprocal // ÷
    | Ceiling // ⌈
    | Floor // ⌊
    | Exponential // *
    | NaturalLogarithm // ⍟
    | Magnitude // |
    | Roll // ?
    | PiTimes // ○
    | Factorial // !
    | Not // ~
    | Size // ⍴
    | Ravel // ,
    | IndexGenerator // ⍳
    | GradeUp // ⍋
    | GradeDown // ⍒
    | Reverse // ⌽
    | ReverseAllowAxis // ⊖

// All apl Tokens
type Token =
    | Operator of OperatorToken
    | DyadicOperator of DyadicOperatorToken
    | MonadicOperator of MonadicOperatorToken
    | Dyadic of DyadicToken
    | Monadic of MonadicToken
    | Comment // ⍝
    | Number of float
    | String of string


let rec makeNumberToken float characters =
    match characters with
    // Number characters
    | '0' :: rest -> makeNumberToken (float * 10.0) rest
    | '1' :: rest -> makeNumberToken (float * 10.0 + 1.0) rest
    | '2' :: rest -> makeNumberToken (float * 10.0 + 2.0) rest
    | '3' :: rest -> makeNumberToken (float * 10.0 + 3.0) rest
    | '4' :: rest -> makeNumberToken (float * 10.0 + 4.0) rest
    | '5' :: rest -> makeNumberToken (float * 10.0 + 5.0) rest
    | '6' :: rest -> makeNumberToken (float * 10.0 + 6.0) rest
    | '7' :: rest -> makeNumberToken (float * 10.0 + 7.0) rest
    | '8' :: rest -> makeNumberToken (float * 10.0 + 8.0) rest
    | '9' :: rest -> makeNumberToken (float * 10.0 + 9.0) rest
    // Empty character array
    | [] -> ([], float)
    // Finished finding numbers
    | _ -> (characters, float)


let rec makeTokens tokenList characters =
    match characters with
    // Tokens
    // Numbers (And whitespaces)
    | whitespace :: '-' :: digit :: rest when isBlank whitespace && isDigit digit ->
        let newRest, number =
            makeNumberToken 0 (digit :: rest)

        makeTokens (Token.Number(-number) :: tokenList) newRest
    | whitespace :: rest when isBlank whitespace -> makeTokens tokenList rest
    | '+' :: digit :: rest
    | digit :: rest when isDigit digit ->
        let newRest, number =
            makeNumberToken 0 (digit :: rest)

        makeTokens (Token.Number(number) :: tokenList) newRest
    // Empty character array
    | [] -> tokenList |> List.rev
    // Error, no matches
    | _ -> failwith "tokenization error"

let tokenize (inputString: string) =
    inputString |> Seq.toList |> makeTokens []

[<EntryPoint>]
let main _ =
    Console.ReadLine() |> tokenize |> printfn "%A"
    0
