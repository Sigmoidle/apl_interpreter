module apl_compiler.Runtime

open System
open Parser
open apl_compiler.Symbols

let private runtimeError error = Exception(error)

let random = Random()

let rec private _ConvertNListToValue (symbolTable: Map<string, float list>) nList =
    match nList with
    | NListValue value -> value
    | NListIdentifier string -> symbolTable.Item(string)

let rec private _ConvertNListsToValues (symbolTable: Map<string, float list>) (nList1, nList2) =
    match (nList1, nList2) with
    | NListValue value1, NListValue value2 -> (value1, value2)
    | NListIdentifier string1, NListValue value2 -> (symbolTable.Item(string1), value2)
    | NListValue value1, NListIdentifier string2 -> (value1, symbolTable.Item(string2))
    | NListIdentifier string1, NListIdentifier string2 -> (symbolTable.Item(string1), symbolTable.Item(string2))

let private _Add (list1: float list, list2: float list) =
    match list1.Length with
    | 1 -> list2 |> Seq.map (fun float -> float + list1.Head) |> Seq.toList
    | _ when list1.Length = list2.Length -> Seq.map2 (+) list1 list2 |> Seq.toList
    | _ ->
        raise
        <| runtimeError $"The array: %A{list1} and %A{list2} are not compatible lengths for the add operation (+)"

let private _Subtract (list1: float list, list2: float list) =
    match list1.Length with
    | 1 when list2.Length <> 1 -> list2 |> Seq.map (fun float -> float - list1.Head) |> Seq.toList
    | _ when list1.Length = list2.Length -> Seq.map2 (-) list1 list2 |> Seq.toList
    | _ ->
        raise
        <| runtimeError $"The array: %A{list1} and %A{list2} are not compatible lengths for the subtract operation (-)"

let private _Multiply (list1: float list, list2: float list) =
    match list1.Length with
    | 1 -> list2 |> Seq.map (fun float -> float * list1.Head) |> Seq.toList
    | _ when list1.Length = list2.Length -> Seq.map2 (*) list1 list2 |> Seq.toList
    | _ ->
        raise
        <| runtimeError $"The array: %A{list1} and %A{list2} are not compatible lengths for the multiply operation (×)"

let private _Divide (list1: float list, list2: float list) =
    match list1.Length with
    | 1 when list1.Head <> 0 && list2.Length <> 1 -> list2 |> Seq.map (fun float -> float / list1.Head) |> Seq.toList
    | _ when List.contains 0.0 list2 ->
        raise
        <| runtimeError $"The array: %A{list2} contains one or more 0s which will cause a divide by 0 error for the division operation (÷)"
    | _ when list1.Length = list2.Length -> Seq.map2 (/) list1 list2 |> Seq.toList
    | _ ->
        raise
        <| runtimeError $"The array: %A{list1} and %A{list2} are not compatible lengths for the division operation (÷)"

let rec private _Not (numList: float list) =
    let ContainsOnlyBinaryValue numList = numList |> Seq.forall (fun n -> n = 1.0 || n = 0.0)

    if not (ContainsOnlyBinaryValue numList) then
        raise
        <| runtimeError $"The value(s): %A{numList} are not all binary numbers, not(~) requires binary number"

    elif numList.IsEmpty then
        numList
    else
        numList |> Seq.map (fun n -> if n = 1.0 then 0.0 else 1.0) |> Seq.toList

let private _Roll (numList: float list) =
    if numList.Length <> 1 then
        raise <| runtimeError $"The array: %A{numList} is not a scalar and roll(?) requires a scalar"

    if numList.Head < 1 then
        raise <| runtimeError $"The array: %A{numList} is less than 1 and roll(?) requires integers above 0"

    if numList.IsEmpty then
        raise <| runtimeError $"The array: %A{numList} is empty and roll(?) requires a scalar"

    [ Convert.ToDouble(random.Next(1, Convert.ToInt32 numList.Head)) ]

let private _Deal (list1: float list, list2: float list) =
    if list1.Length <> 1 || list2.Length <> 1 then
        raise
        <| runtimeError $"The array: %A{list1} or %A{list2} is not a scalar and deal(?) requires a scalar"

    if list1.Head < 1 || list2.Head < 1 then
        raise
        <| runtimeError $"The array: %A{list1} or %A{list2} is less than 1 and deal(?) requires integers above 0"

    if list1.IsEmpty || list2.IsEmpty then
        raise <| runtimeError $"The array: %A{list1} or %A{list2} is empty and deal(?) requires a scalar"

    let list1 = Convert.ToInt32 list1.Head
    let list2 = Convert.ToInt32 list2.Head

    Array.zeroCreate list1 |> Seq.map (fun _ -> Convert.ToDouble(random.Next(1, list2))) |> Seq.toList

let private _SignOf (list: float list) =
    match list with
    | [] -> raise <| runtimeError $"The array: %A{list} is empty and the SignOf operation requires numbers (×)"
    | _ ->
        list
        |> Seq.map (function
            | 0.0 -> 0.0
            | x when x > 0.0 -> 1.0
            | x when x < 0.0 -> -1.0
            | _ -> raise <| runtimeError "maths has failed")
        |> Seq.toList

let private _AddReduce (list: float list) =
    match list with
    | [] -> raise <| runtimeError $"The array: %A{list} is empty and the reduce operation requires numbers"
    | _ -> [ list |> List.reduceBack (fun a b -> a + b) ]

let private _MultiplyReduce (list: float list) =
    match list with
    | [] -> raise <| runtimeError $"The array: %A{list} is empty and the reduce operation requires numbers"
    | _ -> [ list |> List.reduceBack (fun a b -> a * b) ]

let private _DivideReduce (list: float list) =
    match list with
    | [] -> raise <| runtimeError $"The array: %A{list} is empty and the reduce operation requires numbers"
    | _ -> [ list |> List.reduceBack (fun a b -> a / b) ]

let private _SubtractReduce (list: float list) =
    match list with
    | [] -> raise <| runtimeError $"The array: %A{list} is empty and the reduce operation requires numbers"
    | _ -> [ list |> List.reduceBack (fun a b -> a - b) ]

let private _Reciprocal (list: float list) =
    match list with
    | [] ->
        raise
        <| runtimeError $"The array: %A{list} is empty and the Reciprocal operation requires numbers (÷)"
    | _ when List.contains 0.0 list ->
        raise
        <| runtimeError $"The array: %A{list} contains one or more 0s which will cause a divide by 0 error for the Reciprocal function(÷)"
    | _ -> list |> Seq.map (fun x -> 1.0 / x) |> Seq.toList

let private _Negate (list: float list) =
    match list with
    | [] -> raise <| runtimeError $"The array: %A{list} is empty and the Negate operation requires numbers (-)"
    | _ -> list |> Seq.map (fun x -> x * -1.0) |> Seq.toList

let private _Tally (list: float list) = [ Convert.ToDouble list.Length ]

let runtime data =
    let rec _Program (data, out) =
        match data._program with
        | EndOfFile -> (data, out)
        | NewLine newProgram -> _Program ({ data with _program = newProgram }, out)
        | Expression (expression, newProgram) ->
            let newSymbolTable, newOut = _Expression (expression, data._symbolTable, out)
            _Program ({ data with _program = newProgram; _symbolTable = newSymbolTable }, newOut)

    and _Expression (expression, symbolTable, out) =
        match expression with
        | Assign (symbolName, newExpression) ->
            let symbolTable, newOut = _Expression (newExpression, symbolTable, out)
            let newSymbolTable = symbolTable.Add(symbolName, newOut)
            (newSymbolTable, newOut)
        | MonadicFn func -> (symbolTable, _MonadicFn (func, symbolTable, out))
        | DyadicFn func -> (symbolTable, _DyadicFn (func, symbolTable, out))
        | Expression.NList nListType ->
            match nListType with
            | NListIdentifier _ -> (symbolTable, _ConvertNListToValue symbolTable nListType)
            | NListValue value -> (symbolTable, value)

    and _MonadicFn (monadicFn, symbolTable, out) =
        match monadicFn with
        | Not expression -> _Expression (expression, symbolTable, out) |> snd |> _Not
        | Roll expression -> _Expression (expression, symbolTable, out) |> snd |> _Roll
        | Tally expression -> _Expression (expression, symbolTable, out) |> snd |> _Tally
        | Negate expression -> _Expression (expression, symbolTable, out) |> snd |> _Negate
        | SignOf expression -> _Expression (expression, symbolTable, out) |> snd |> _SignOf
        | Reciprocal expression -> _Expression (expression, symbolTable, out) |> snd |> _Reciprocal
        | AddReduce expression -> _Expression (expression, symbolTable, out) |> snd |> _AddReduce
        | MultiplyReduce expression -> _Expression (expression, symbolTable, out) |> snd |> _MultiplyReduce
        | DivideReduce expression -> _Expression (expression, symbolTable, out) |> snd |> _DivideReduce
        | SubtractReduce expression -> _Expression (expression, symbolTable, out) |> snd |> _SubtractReduce

    and _DyadicFn (dyadicFn, symbolTable, out) =
        match dyadicFn with
        | Add (expression1, expression2) ->
            (_Expression (expression1, symbolTable, out) |> snd, _Expression (expression2, symbolTable, out) |> snd)
            |> _Add
        | Deal (expression1, expression2) ->
            (_Expression (expression1, symbolTable, out) |> snd, _Expression (expression2, symbolTable, out) |> snd)
            |> _Deal
        | Multiply (expression1, expression2) ->
            (_Expression (expression1, symbolTable, out) |> snd, _Expression (expression2, symbolTable, out) |> snd)
            |> _Multiply
        | Divide (expression1, expression2) ->
            (_Expression (expression1, symbolTable, out) |> snd, _Expression (expression2, symbolTable, out) |> snd)
            |> _Divide
        | Subtract (expression1, expression2) ->
            (_Expression (expression1, symbolTable, out) |> snd, _Expression (expression2, symbolTable, out) |> snd)
            |> _Subtract

    _Program (data, [ 0 ])
