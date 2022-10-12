open System

let isBlank c = Char.IsWhiteSpace c
let isDigit c = Char.IsDigit c

// The names of the tokens are based purely on their unicode names
// This is because the context of the position of the token defines
// What role the token takes in APL
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
    | '+' :: rest -> makeTokens (Token.Plus :: tokenList) rest
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
