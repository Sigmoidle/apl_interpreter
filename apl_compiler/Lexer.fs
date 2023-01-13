module apl_compiler.Lexer

open System
open System.IO

// All Apl tokens
type Token =
    | Plus // +
    | Hyphen // -
    | Multiplication // ×
    | Division // ÷
    | LeftCeiling // ⌈
    | LeftFloor // ⌊
    | Asterisk // *
    | CircleStar // ⍟
    | VerticalBar // |
    | QuestionMark // ?
    | WhiteCircle // ○
    | ExclamationMark // !
    | Tilde // ~
    | LogicalAnd // ∧
    | LogicalOr // ∨
    | UpCaretTilde // ⍲
    | DownCaretTilde // ⍱
    | LessThan // <
    | NotGreaterThan // ≤
    | Equals // =
    | NotLessThan // ≥
    | GreaterThan // >
    | NotEqual // ≠
    | Rho // ⍴
    | Comma // ,
    | LeftSquareBracket // [
    | RightSquareBracket // ]
    | Iota // ⍳
    | UpwardPointingArrow // ↑
    | DownwardPointingArrow // ↓
    | DeltaStile // ⍋
    | DelStile // ⍒
    | Slash // /
    | SlashBar // ⌿
    | Backslash // \
    | BackslashBar // ⍀
    | CircleStile // ⌽
    | CircledMinus // ⊖
    | CircleBackslash // ⍉
    | SmallElementOf // ∊
    | Decode // ⊥
    | Encode // ⊤
    | FullStop // .
    | OuterProduct // ∘.
    | LeftBracket // (
    | RightBracket // )
    | Assign // ←
    // Misc
    | Comment // ⍝
    | NewLine // \n
    // Types
    | Number of float //
    | String of string
    // Identifiers
    | Identifier of string //
    // End of file
    | EndOfFile

let private isIndicationOfArray t =
    match t with
    | Token.Identifier _ -> true
    | Token.RightBracket -> true
    | _ -> false

let private isNewLine c = c = '\n'

let private isLetter c = Char.IsLetter c

let private isBlank c = Char.IsWhiteSpace c && not (c.Equals('\n'))

let private isDigit c = Char.IsDigit c

let rec private makeTokens tokenList characters =
    match characters with
    // Tokens
    | '←' :: rest -> makeTokens (Assign :: tokenList) rest
    | '+' :: rest -> makeTokens (Plus :: tokenList) rest
    | '~' :: rest -> makeTokens (Tilde :: tokenList) rest
    | '?' :: rest -> makeTokens (QuestionMark :: tokenList) rest
    // Identifiers
    | letter :: rest when isLetter letter ->
        let newRest, calculatedString = makeStringToken "" (letter :: rest)

        makeTokens (Identifier(calculatedString) :: tokenList) newRest
    // Numbers
    | '¯' :: digit :: rest when isDigit digit ->
        let newRest, number = makeNumberToken 0.0 (digit :: rest)

        makeTokens (Number(-number) :: tokenList) newRest
    | digit :: rest when isDigit digit ->
        let newRest, number = makeNumberToken 0.0 (digit :: rest)

        makeTokens (Number(number) :: tokenList) newRest
    // Whitespaces
    | whitespace :: rest when isBlank whitespace -> makeTokens tokenList rest
    // NewLines
    | newLine :: rest when isNewLine newLine -> makeTokens (NewLine :: tokenList) rest
    // Comments
    | '⍝' :: rest ->
        let newRest = handleComment rest

        makeTokens tokenList newRest
    // Empty character array
    | [] -> EndOfFile :: tokenList |> List.rev
    // Error, no matches
    | error :: _ -> failwith $"tokenization error at character: {error} | After token: {tokenList.Head}"

and calculateAfterDecimal float scale characters =
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

and makeNumberToken float characters =
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
        let newRest, number = calculateAfterDecimal 0.0 1.0 (digit :: rest)

        makeNumberToken (float + number) newRest
    // Empty character array
    | [] -> ([], float)
    // Finished finding numbers
    | _ -> (characters, float)

and handleComment characters =
    match characters with
    | newline :: rest when isNewLine newline -> newline :: rest
    | [] -> characters
    | _ :: rest -> handleComment rest

and makeStringToken (calculatedString: string) characters =
    match characters with
    | letter :: rest when isLetter letter || letter = '_' -> makeStringToken (calculatedString + string letter) rest
    | [] -> ([], calculatedString)
    | _ -> (characters, calculatedString)

let public lex (inputString: string) = inputString |> Seq.toList |> makeTokens []


let public testLexer =
    let aplProgram = File.ReadAllText("test_program.apl")

    aplProgram |> lex |> printfn "%A"
