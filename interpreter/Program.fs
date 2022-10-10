open System

let isBlank c = Char.IsWhiteSpace c
let isDigit c = Char.IsDigit c
    
type Token =
    Plus
    | Multiply
    | Subtract
    | Divide
    | LeftBracket
    | RightBracket
    | For
    | Integer of int


let rec makeIntegerToken (characters: char list) (integer: int) =
    match characters with
    | '0' :: rest -> makeIntegerToken rest (integer * 10)
    | '1' :: rest -> makeIntegerToken rest (integer * 10 + 1)
    | '2' :: rest -> makeIntegerToken rest (integer * 10 + 2)
    | '3' :: rest -> makeIntegerToken rest (integer * 10 + 3)
    | '4' :: rest -> makeIntegerToken rest (integer * 10 + 4)
    | '5' :: rest -> makeIntegerToken rest (integer * 10 + 5)
    | '6' :: rest -> makeIntegerToken rest (integer * 10 + 6)
    | '7' :: rest -> makeIntegerToken rest (integer * 10 + 7)
    | '8' :: rest -> makeIntegerToken rest (integer * 10 + 8)
    | '9' :: rest -> makeIntegerToken rest (integer * 10 + 9)
    | [] -> ([], integer)
    | _ -> (characters, integer)
    

let rec makeTokens (characters: char list) (tokenList: Token list): Token list =
    match characters with
    | '+' :: rest -> makeTokens rest (Token.Plus :: tokenList)
    | '*' :: rest -> makeTokens rest (Token.Multiply :: tokenList)
    | '(' :: rest -> makeTokens rest (Token.LeftBracket :: tokenList)
    | ')' :: rest -> makeTokens rest (Token.RightBracket :: tokenList)
    | '-' :: rest -> makeTokens rest (Token.Subtract :: tokenList)
    | '/' :: rest -> makeTokens rest (Token.Divide :: tokenList)
    | 'f' :: 'o' :: 'r' :: rest -> makeTokens rest (Token.For :: tokenList)
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
    


