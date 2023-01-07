module apl_compiler.Parser

open Lexer

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

and Statement = Function of float * Function * float

and Function = Add

let private parseError = System.Exception("parse error")

let private Add (list1: float list, list2: float list) : float list =
    if list1.Length <> list2.Length then
        // TODO: check if uneven array addition in APL is valid
        raise parseError

    let rec AddList (list1: float list, list2: float list) : float list =
        if list1.IsEmpty then
            [] // return empty results list
        else
            let tail1 = list1.Tail
            let tail2 = list2.Tail
            let result = list1.Head + list2.Head
            let resultList = AddList(tail1, tail2)
            result :: resultList

    AddList(list1, list2)

let rec private Not (numList: float list) : float list =
    let ContainsOnlyBinaryValue (numList: float list) : bool = numList |> Seq.forall (fun n -> n = 1.0 || n = 0.0)

    if not (ContainsOnlyBinaryValue numList) then
        raise parseError

    elif numList.IsEmpty then
        numList
    else
        numList |> Seq.map (fun n -> if n = 1.0 then 0.0 else 1.0) |> Seq.toList

// TODO: make copy of this fn that adds the parts to a parse tree as it
//       goes instead of eval
let parseAndEval (tokens: Token list) : Token list * float list =
    let rec Program (tokens: Token list) : Token list * float list =
        match tokens with
        | Token.EndOfFile :: tail -> (tail, []) // Probably shouldn't return an empty array?
        | _ -> Statement tokens

    and Statement (tokens: Token list) : Token list * float list =
        match tokens with
        | Token.Number _ :: _ -> (NList >> DyadicFn) tokens
        | _ -> MonadicFn tokens // could change to list of all monadics

    and MonadicFn (tokens: Token list) : Token list * float list =
        // Currently does not handle stacking multiple Fns
        //  e.g. `~~ 0 1` should apply not two times
        match tokens with
        | Token.Tilde :: tail ->
            let tokens, numList = NList tail
            let result = Not numList
            (tokens, result)
        | _ -> raise parseError

    and DyadicFn (tokens, list1) : Token list * float list =
        match tokens with
        | Token.Plus :: tail ->
            let tokens, list2 = NList tail
            let summed = Add(list1, list2)
            // EXIT
            (tokens, summed)
        | _ -> raise parseError

    and NList (tokens: Token list) : Token list * float list =
        match tokens with
        | Token.Number value :: tail ->
            let tokens, numList = NList tail
            (tokens, value :: numList)
        | _ -> (tokens, [])

    Program tokens


let mutable userVariableList = []
let main (tokens: Token list) =
    
    let floatToString(float: float) =
        string float

    let rec checkList(userVariableList:list<string*float>,variable:string) =
        if fst userVariableList.Head = variable then 
            floatToString(snd userVariableList.Head) |> lex
        else checkList(userVariableList.Tail,variable)

    let rec addTail(tail:Token list,head:Token list) : Token list =
        let list = head.Head::tail
        list

    let rec identifierExists(tokens: Token list) = 
        match tokens with
        | Token.Identifier _::_ -> true
        | Token.EndOfFile _::_ -> false
        | _ -> identifierExists(tokens.Tail)


    let rec checkExpression(tokens: Token list,userVariableList:list<string*float>) : Token list =
        match tokens with
        | Token.EndOfFile ::_ -> [Token.EndOfFile]
        | Token.Identifier variable::tail -> 
            let newNumber = checkList(userVariableList,variable)
            let newTokenList = addTail(tail,newNumber) 
            let newExpression = checkExpression(newTokenList.Tail,userVariableList)
            [newTokenList.Head]@newExpression
        | _ -> if not (identifierExists(tokens)) then tokens
                else let newExpression = checkExpression(tokens.Tail,userVariableList)
                     [tokens.Head]@newExpression

    let addToList(userVariableList:list<string*float>,variable:string,number:float list) = 
        let list = userVariableList@[(variable,number.Head)]
        list


    let checkAssign(tokens: Token list, userVariableList:list<string*float>,variable:string) = 
        if tokens.Head = Token.Assign then 
            let newExpression = checkExpression(tokens.Tail,userVariableList)
            let output = parseAndEval(newExpression)
            let newList = addToList(userVariableList,variable,snd output)
            newList
        else
            userVariableList

    let rec createVariableList(tokens: Token list, userVariableList:list<string*float>) = 
        match tokens with
        | Token.EndOfFile _::_ -> userVariableList
        | Token.Identifier variable::tail -> checkAssign(tail,userVariableList,variable)
        | _::tail -> createVariableList(tail,userVariableList)
    
    let rec replaceVariables(tokens:Token list) = 
        match tokens with
        | Token.Identifier variable::tail -> 
            let newNumber = checkList(userVariableList,variable)
            let newTokenList = addTail(tail,newNumber)
            let finalTokenList = newTokenList@replaceVariables(newTokenList.Tail)
            finalTokenList
        | Token.EndOfFile ::_ -> [Token.EndOfFile]
        | head::tail -> [head]@replaceVariables(tail)

    let rec assignExists(tokens:Token list) = 
        match tokens with 
        | Token.Assign _::_ -> true
        | Token.EndOfFile ::_ -> false
        | _::tail -> assignExists(tail)


    //←
    if not(assignExists(tokens)) then 
        let newTokenList = replaceVariables(tokens)
        parseAndEval(newTokenList)
    else 
        userVariableList <- createVariableList(tokens, userVariableList)
        ([],[])