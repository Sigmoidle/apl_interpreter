open System
open System.IO

type MonadicOperatorToken =
    | Reduction // /
    | ReductionAllowAxis // ⌿
    | Branch // →

type DyadicOperatorToken =
    | InnerProduct // .
    | OuterProduct // ∘.
    | Assign // ←

type OperatorToken =
    | AxisLeft // [
    | AxisRight // ]
    | LeftBracket // (
    | RightBracket // )

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
    | NewLine // \n
    | Number of float
    | String of string
    | Identifier of string

let isIndicationOfArray t =
    match t with
    | Token.Identifier _ -> true
    | Token.Operator OperatorToken.RightBracket -> true
    | _ -> false

let isNewLine c = c = '\n'

let isLetter c = Char.IsLetter c

let isBlank c = Char.IsWhiteSpace c && not (c.Equals('\n'))

let isDigit c = Char.IsDigit c

let rec calculateAfterDecimal float scale characters =
    match characters with
    | '0' :: rest -> calculateAfterDecimal (float * 10.0) (scale * 10.0) rest
    | '1' :: rest -> calculateAfterDecimal (float * 10.0 + 1.0) (scale * 10.0) rest
    | '2' :: rest -> calculateAfterDecimal (float * 10.0 + 2.0) (scale * 10.0) rest
    | '3' :: rest -> calculateAfterDecimal (float * 10.0 + 3.0) (scale * 10.0) rest
    | '4' :: rest -> calculateAfterDecimal (float * 10.0 + 4.0) (scale * 10.0) rest
    | '5' :: rest -> calculateAfterDecimal (float * 10.0 + 5.0) (scale * 10.0) rest
    | '6' :: rest -> calculateAfterDecimal (float * 10.0 + 6.0) (scale * 10.0) rest
    | '7' :: rest -> calculateAfterDecimal (float * 10.0 + 7.0) (scale * 10.0) rest
    | '8' :: rest -> calculateAfterDecimal (float * 10.0 + 8.0) (scale * 10.0) rest
    | '9' :: rest -> calculateAfterDecimal (float * 10.0 + 9.0) (scale * 10.0) rest
    // Empty character array
    | [] -> ([], float / scale)
    // Finished finding numbers
    | _ -> (characters, float / scale)


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
    // Detect and calculate float number
    | '.' :: digit :: rest when isDigit digit ->
        let newRest, number = calculateAfterDecimal 0 1 (digit :: rest)

        makeNumberToken (float + number) newRest
    // Empty character array
    | [] -> ([], float)
    // Finished finding numbers
    | _ -> (characters, float)

let rec handleComment characters =
    match characters with
    | newline :: rest when isNewLine newline -> rest
    | [] -> characters
    | _ :: rest -> handleComment rest

let rec makeStringToken (calculatedString: string) characters =
    match characters with
    | letter :: rest when isLetter letter -> makeStringToken (calculatedString + string letter) rest
    | [] -> ([], calculatedString)
    | _ -> (characters, calculatedString)

let rec makeTokens tokenList characters =
    match characters with
    // Tokens
    // DyadicOperatorTokens
    | '←' :: rest -> makeTokens (Token.DyadicOperator DyadicOperatorToken.Assign :: tokenList) rest
    // DyadicTokens
    | '+' :: rest when isIndicationOfArray tokenList.Head -> makeTokens (Token.Dyadic DyadicToken.Plus :: tokenList) rest
    // MonadicTokens
    | '+' :: rest when not <| isIndicationOfArray tokenList.Head -> makeTokens (Token.Monadic MonadicToken.Conjugate :: tokenList) rest
    // Identifiers
    | letter :: rest when isLetter letter ->
        let newRest, calculatedString = makeStringToken "" (letter :: rest)

        makeTokens (Token.Identifier(calculatedString) :: tokenList) newRest
    // Numbers
    | '¯' :: digit :: rest when isDigit digit ->
        let newRest, number = makeNumberToken 0 (digit :: rest)

        makeTokens (Token.Number(-number) :: tokenList) newRest
    | digit :: rest when isDigit digit ->
        let newRest, number = makeNumberToken 0 (digit :: rest)

        makeTokens (Token.Number(number) :: tokenList) newRest
    // Whitespaces
    | whitespace :: rest when isBlank whitespace -> makeTokens tokenList rest
    // NewLines
    | newLine :: rest when isNewLine newLine -> makeTokens (Token.NewLine :: tokenList) rest
    // Comments
    | '⍝' :: rest ->
        let newRest = handleComment rest

        makeTokens tokenList newRest
    // Empty character array
    | [] -> tokenList |> List.rev
    // Error, no matches
    | error :: _ -> failwith $"tokenization error at character: {error} | After token: {tokenList.Head}"

let tokenize (inputString: string) = inputString |> Seq.toList |> makeTokens []

[<EntryPoint>]
let main _ =
    let testString = File.ReadAllText("C:\Users\\benjo\OneDrive\Documents\university\\advanced programming\project\learning\compiler_learning\\apl_compiler\\test_program.apl")

    testString |> tokenize |> printfn "%A"
    0
