open System

let isBlank c = Char.IsWhiteSpace c
let isDigit c = Char.IsDigit c

// The names of the tokens are based purely on their unicode names
// This is because the context of the position of the token defines
// What role the token takes in APL
type Token =
    | Plus
    | Hyphen
    | Multiplication
    | Division
    | LeftCeiling
    | LeftFloor
    | Asterisk
    | APLFunctionalSymbolCircleStar
    | VerticalBar
    | QuestionMark
    | WhiteCircle
    | ExclamationMark
    | Tilde
    | LogicalAnd
    | LogicalOr
    | APLFunctionalSymbolUpCaretTilde
    | APLFunctionalSymbolDownCaretTilde
    | LessThan
    | Number of float
    | String of string


let rec makeNumberToken (characters: char list) (float: float) =
    match characters with
    | '0' :: rest -> makeNumberToken rest (float * 10.0)
    | '1' :: rest -> makeNumberToken rest (float * 10.0 + 1.0)
    | '2' :: rest -> makeNumberToken rest (float * 10.0 + 2.0)
    | '3' :: rest -> makeNumberToken rest (float * 10.0 + 3.0)
    | '4' :: rest -> makeNumberToken rest (float * 10.0 + 4.0)
    | '5' :: rest -> makeNumberToken rest (float * 10.0 + 5.0)
    | '6' :: rest -> makeNumberToken rest (float * 10.0 + 6.0)
    | '7' :: rest -> makeNumberToken rest (float * 10.0 + 7.0)
    | '8' :: rest -> makeNumberToken rest (float * 10.0 + 8.0)
    | '9' :: rest -> makeNumberToken rest (float * 10.0 + 9.0)
    | [] -> ([], float)
    | _ -> (characters, float)


let rec makeTokens (characters: char list) (tokenList: Token list) : Token list =
    match characters with
    | '+' :: rest -> makeTokens rest (Token.Plus :: tokenList)
    | whitespace :: '-' :: digit :: rest when isBlank whitespace && isDigit digit ->
        let newRest, number =
            makeNumberToken (digit :: rest) 0

        makeTokens newRest (Token.Number(-number) :: tokenList)
    | whitespace :: rest when isBlank whitespace -> makeTokens rest tokenList
    | '+' :: digit :: rest
    | digit :: rest when isDigit digit ->
        let newRest, number =
            makeNumberToken (digit :: rest) 0

        makeTokens newRest (Token.Number(number) :: tokenList)
    | [] -> tokenList |> List.rev
    | _ -> failwith "tokenization error"

let tokenize (inputString: string) =
    let characters =
        inputString.ToCharArray() |> List.ofArray

    makeTokens characters []

[<EntryPoint>]
let main args =
    Console.ReadLine() |> tokenize |> printfn "%A"
    0
