namespace Interpreter

module Semantics =

    open Syntax

    type Signature =
        | StaticState of int
        | StaticGate of int
        | DynamicGate

    type SymbolTable = Map<Identifier, Signature>

    let defaultSymbolTable =
        [ Identifier "H", StaticGate 1
          Identifier "CNOT", StaticGate 1
          Identifier "log", DynamicGate ]
        |> Map.ofList

    let optToResult err =
        function
        | Some x -> Ok x
        | None -> Error err

    let getStateSize (table: SymbolTable) =
        function
        | Zero -> Ok 1
        | One -> Ok 1
        | StateExp id ->
            table.TryFind(id)
            |> optToResult (sprintf "Identifier '%O' is undefined!" id)
            |> Result.bind
                (function
                | StaticState x -> Ok x
                | _ ->
                    sprintf "Identifier '%O' is not a statically sized state!" id
                    |> Error)

    let getStatesSizes (table: SymbolTable) states =
        states
        |> List.fold
            (fun acc y ->
                acc
                |> Result.bind (fun x -> getStateSize table y |> Result.map ((+) x)))
            (Ok 0)

    let getGateSize (table: SymbolTable) (id: Identifier) : Result<int, string> = Error ""

    let getGatesSize (table: SymbolTable) =
        function
        | Log -> Ok DynamicGate
        | ParallelGates g ->
            g
            |> List.fold
                (fun (acc: Result<int, string>) y ->
                    acc
                    |> Result.bind (fun acc -> getGateSize table y |> Result.map ((+) acc)))
                (Ok 0)
            |> Result.map StaticGate

    let verifyFunq (table: SymbolTable) (SequentialGates gates) =
        let results = List.map (getGatesSize table) gates

        Seq.choose
            (function
            | Error e -> Some(Error e)
            | Ok _ -> None)
            results
        |> Seq.tryHead
        |> Option.defaultWith
            (fun () ->
                let sizes =
                    results
                    |> Seq.choose
                        (function
                        | Error _ -> None
                        | Ok size ->
                            match size with
                            | StaticGate size -> Some size
                            | _ -> None)

                match Seq.tryHead sizes with
                | None -> Ok DynamicGate
                | Some size ->
                    match Seq.forall ((=) size) sizes with
                    | false -> Error "Inconsistent sizing!"
                    | true -> Ok(StaticGate size))

    let verifyLet (table: SymbolTable) (ParallelStates states, SequentialGates gates) =
        getStatesSizes table states
        |> Result.map StaticState

    let verifyDefinition (table: SymbolTable) (identifier: Identifier) =
        function
        | Let (x, y) -> verifyLet table (x, y)
        | Funq x -> verifyFunq table x
        >> Result.mapError (fun e -> sprintf "Definition of '%O': " identifier + e)

    let analyseSemantics definitions =
        let rec innerFn table =
            function
            | [] -> Ok Map.empty
            | head :: tail ->
                verifyDefinition table (fst head) (snd head)
                |> Result.bind
                    (fun signature ->
                        match innerFn (table.Add(fst head, signature)) tail with
                        | Error e -> Error e
                        | Ok definitions -> Ok(definitions.Add head))

        innerFn defaultSymbolTable definitions
