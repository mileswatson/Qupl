namespace Interpreter

module Semantics =

    open Syntax

    type Signature =
        | StateSize of int
        | GateSize of int

    type SymbolTable = Map<Identifier, Signature>

    let defaultSymbolTable =
        [ Identifier "H", GateSize 1
          Identifier "CNOT", GateSize 1 ]
        |> Map.ofList

    let verifyDefinition (table: SymbolTable) (def: Definition) : Result<Signature, string> = Ok(StateSize 0)

    let analyseSemantics definitions =
        let rec innerFn table =
            function
            | [] -> Ok Map.empty
            | head :: tail ->
                match verifyDefinition table (snd head) with
                | Error e -> Error e
                | Ok signature ->
                    match innerFn (table.Add(fst head, signature)) tail with
                    | Error e -> Error e
                    | Ok definitions -> Ok(definitions.Add head)

        innerFn defaultSymbolTable definitions
