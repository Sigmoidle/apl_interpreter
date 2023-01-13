module apl_compiler.Runtime

open System
open Parser
open apl_compiler.Symbols

let private runtimeError error = Exception(error)

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
    if list1.Length <> list2.Length then
        // TODO: check if uneven array addition in APL is valid
        // TODO: I've checked and they are, but only between a scalar and a vector
        raise <| runtimeError "During a dyadic add operation the two APL lists weren't even"

    let rec AddList (list1: float list, list2: float list) =
        if list1.IsEmpty then
            [] // return empty results list
        else
            let tail1 = list1.Tail
            let tail2 = list2.Tail
            let result = list1.Head + list2.Head
            let resultList = AddList(tail1, tail2)
            result :: resultList

    AddList(list1, list2)

let rec private _Not (numList: float list) =
    let ContainsOnlyBinaryValue numList = numList |> Seq.forall (fun n -> n = 1.0 || n = 0.0)

    if not (ContainsOnlyBinaryValue numList) then
        raise <| runtimeError $"The value(s): %A{numList} are not all binary numbers"

    elif numList.IsEmpty then
        numList
    else
        numList |> Seq.map (fun n -> if n = 1.0 then 0.0 else 1.0) |> Seq.toList

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
        | Not maybeChain -> _MaybeChain (maybeChain, symbolTable, out) |> _ConvertNListToValue symbolTable |> _Not

    and _DyadicFn (dyadicFn, symbolTable, out) =
        match dyadicFn with
        | Add (nList, maybeChain) -> (nList, _MaybeChain (maybeChain, symbolTable, out)) |> _ConvertNListsToValues symbolTable |> _Add

    and _MaybeChain (maybeChain, symbolTable, out) =
        match maybeChain with
        | NoChain numList -> numList
        | Chain expression ->
            let _, nList = _Expression (expression, symbolTable, out)
            NList.NListValue nList

    _Program (data, [ 0 ])
