namespace Interpreter

open Syntax

module Semantics =

    type Signature =
        | StaticState of int
        | StaticGate of int
        | DynamicGate

    type SymbolTable = Map<Identifier, Signature>

    let defaultSymbolTable =
        [ Identifier "I", StaticGate 1
          Identifier "H", StaticGate 1
          Identifier "CNOT", StaticGate 2
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
        | StateExp (Identifier id) ->
            table.TryFind(Identifier id)
            |> optToResult (sprintf "Identifier '%s' is undefined at this point." id)
            |> Result.bind
                (function
                | StaticState x -> Ok x
                | _ ->
                    sprintf "Identifier '%s' is not a statically sized state." id
                    |> Error)

    let getStatesSizes (table: SymbolTable) states =
        states
        |> List.fold
            (fun acc y ->
                acc
                |> Result.bind (fun x -> getStateSize table y |> Result.map ((+) x)))
            (Ok 0)

    let getGateSize (table: SymbolTable) (Identifier id) =
        table.TryFind(Identifier id)
        |> optToResult (sprintf "Identifier '%s' is undefined at this point." id)
        |> Result.bind
            (function
            | StaticGate x -> Ok(StaticGate x)
            | DynamicGate -> Ok DynamicGate
            | _ ->
                sprintf "Identifier '%s' is not a statically sized gate." id
                |> Error)

    let getGatesSize (table: SymbolTable) =
        function
        | Log -> Ok DynamicGate
        | ParallelGates g ->
            match g with
            | [ y ] -> getGateSize table y
            | g ->
                g
                |> List.fold
                    (fun (acc: Result<int, string>) y ->
                        acc
                        |> Result.bind
                            (fun acc ->
                                getGateSize table y
                                |> Result.bind
                                    (function
                                    | StaticGate x -> Ok x
                                    | DynamicGate -> Error "Dynamically sized gates must appear on their own line."
                                    | _ -> failwith "Impossible state!")
                                |> Result.map ((+) acc)))
                    (Ok 0)
                |> Result.map StaticGate

    let private firstError results =
        results
        |> Seq.choose
            (function
            | Error e -> Some(Error e)
            | Ok _ -> None)
        |> Seq.tryHead

    let private extractGateSizes results =
        results
        |> Seq.choose
            (function
            | Error _ -> failwith "Impossible state!"
            | Ok size ->
                match size with
                | StaticGate size -> Some size
                | _ -> None)

    let verifyFunq (table: SymbolTable) (SequentialGates gates) =
        let results = List.map (getGatesSize table) gates

        firstError results
        |> Option.defaultWith
            (fun () ->
                let sizes = extractGateSizes results

                match Seq.tryHead sizes with
                | None -> Ok DynamicGate
                | Some size ->
                    if Seq.forall ((=) size) sizes then
                        Ok(StaticGate size)
                    else
                        sprintf "Expected to find gates of width %i." size
                        |> Error)


    let verifyLet (table: SymbolTable) (ParallelStates states, SequentialGates gates) =
        getStatesSizes table states
        |> Result.bind
            (fun size ->
                let results = List.map (getGatesSize table) gates

                firstError results
                |> Option.defaultWith
                    (fun () ->
                        let sizes = extractGateSizes results

                        if Seq.forall ((=) size) sizes then
                            Ok(StaticState size)
                        else
                            sprintf "Expected to find gates of width %i." size
                            |> Error))

    let verifyDefinition (table: SymbolTable) (Identifier identifier) =
        function
        | Let (x, y) -> verifyLet table (x, y)
        | Funq x -> verifyFunq table x
        >> Result.mapError (fun e -> sprintf "('%s' definition) " identifier + e)

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
        |> Result.bind
            (fun m ->
                match Map.tryFind (Identifier "main") m with
                | None -> Error "Could not find 'main' state definition."
                | Some s ->
                    match s with
                    | Funq _ -> Error "'main' is a gate, not a state."
                    | Let _ -> Ok m)
