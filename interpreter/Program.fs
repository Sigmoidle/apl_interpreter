open System

let isBlank c = Char.IsWhiteSpace c
let isDigit c = Char.IsDigit c
    
type Token = Plus | Multiply | LeftBracket | RightBracket | Number

let rec makeTokens (characters: char list) (tokenList: Token list) =
    match characters with
    | '+' :: rest -> makeTokens rest (Token.Plus :: tokenList)
    | '*' :: rest -> makeTokens rest (Token.Multiply :: tokenList)
    | '(' :: rest -> makeTokens rest (Token.LeftBracket :: tokenList)
    | ')' :: rest -> makeTokens rest (Token.RightBracket :: tokenList)
    | whitespace :: rest when isBlank whitespace -> makeTokens rest tokenList
    | [] -> List.rev(tokenList)
    | err -> failwith "tokenization error"

let tokenize (inputString: string) =
    let characters = List.ofArray(inputString.ToCharArray())
    let tokens = makeTokens characters []
    tokens
  
[<EntryPoint>]  
let main args=
    Console.ReadLine() |> tokenize |> printfn "%A"
    0
    


