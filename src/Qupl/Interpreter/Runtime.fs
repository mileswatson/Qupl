namespace Interpreter

open System.Collections.Generic

open Syntax
open Quantum

module Runtime =

    type RunTable = Dictionary<Identifier, Cacheable>

    and Cacheable =
        | CachedState of State._State
        | CachedGate of Operator._Operator
        | Computable of Definition

    let rec computeFunq (table: RunTable) (SequentialGates gates) =
        gates
        |> List.choose
            (function
            | Log _ -> None
            | ParallelGates x -> Some x)
        |> List.map (List.map (getGate table))
        |> Operator.chainAll

    and getGate (table: RunTable) id =
        match table.[id] with
        | CachedGate g -> g
        | Computable (Funq gates) ->
            let gate = computeFunq table gates
            table.[id] <- CachedGate gate
            gate
        | _ -> failwithf "%O is not a state" id

    let logState msg states =
        Core.tostring states |> printfn "%s\n%s\n" msg
        states

    let rec computeState table (ParallelStates states, SequentialGates gates) =
        states
        |> List.map
            (function
            | Zero -> State.zero
            | One -> State.one
            | StateExp id -> getState table id)
        |> State.entangle
        |> (gates
            |> List.map
                (function
                | Log msg -> logState msg
                | ParallelGates g ->
                    g
                    |> List.map (getGate table)
                    |> Operator.combine
                    |> Operator.apply)
            |> List.fold (>>) id)

    and getState (table: RunTable) id =
        match table.[id] with
        | CachedState s -> s
        | Computable (Let (states, gates)) ->
            let state = computeState table (states, gates)
            table.[id] <- CachedState state
            state
        | _ -> failwithf "%O is not a state!" id

    let prepareRuntable m =
        let d =
            m
            |> Map.map (fun _ x -> Computable x)
            |> Dictionary

        d.[Identifier "I"] <- CachedGate Operator.IDENTITY
        d.[Identifier "H"] <- CachedGate Operator.HADAMARD
        d.[Identifier "CNOT"] <- CachedGate Operator.CNOT

        d

    let run input =
        input |> prepareRuntable |> getState
        <| (Identifier "main")
        |> State.probabilities
