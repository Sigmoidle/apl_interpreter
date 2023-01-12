module apl_compiler.Runtime

open Parser

let private runtimeError error = System.Exception(error)

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


let runtime program =
    let rec _Program (program, out) =
        match program with
        | Program.EndOfFile -> (program, out)
        | Program.NewLine program -> _Program (program, out)
        | Program.Statement (statement, program) ->
            let newOut = _Statement (statement, out)
            _Program (program, newOut)

    and _Statement (statement, out) =
        match statement with
        | MonadicFn func -> _MonadicFn (func, out)
        | DyadicFn func -> _DyadicFn (func, out)

    and _MonadicFn (monadicFn, out) =
        match monadicFn with
        | Not maybeChain -> _MaybeChain (maybeChain, out) |> _Not

    and _DyadicFn (dyadicFn, out) =
        match dyadicFn with
        | Add (nList, maybeChain) -> (nList, _MaybeChain (maybeChain, out)) |> _Add

    and _MaybeChain (maybeChain, out) =
        match maybeChain with
        | NoChain numList -> numList
        | Chain statement -> _Statement (statement, out)

    _Program (program, [ 0 ])
