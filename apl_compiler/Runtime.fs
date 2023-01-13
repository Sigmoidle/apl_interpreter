module apl_compiler.Runtime

open System
open Parser
open apl_compiler.Symbols

let private runtimeError error = Exception(error)

let private _Add (list1: float list, list2: float list) =
    if list1.Length <> list2.Length then
        // TODO: check if uneven array addition in APL is valid
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

let rec private _Not numList =
    let ContainsOnlyBinaryValue (numList: float list) = numList |> Seq.forall (fun n -> n = 1.0 || n = 0.0)

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
        | Expression.NList value -> (symbolTable, value)

    and _MonadicFn (monadicFn, symbolTable, out) =
        match monadicFn with
        | Not maybeChain -> _MaybeChain (maybeChain, symbolTable, out) |> _Not

    and _DyadicFn (dyadicFn, symbolTable, out) =
        match dyadicFn with
        | Add (nList, maybeChain) -> (nList, _MaybeChain (maybeChain, symbolTable, out)) |> _Add

    and _MaybeChain (maybeChain, symbolTable, out) =
        match maybeChain with
        | NoChain numList -> numList
        | Chain expression ->
            let _, nList = _Expression (expression, symbolTable, out)
            nList

    _Program (data, [ 0 ])
