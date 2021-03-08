open Quantum
open Quantum.Core

// some constant and variable functions
let c1 = combine [IDENTITY; IDENTITY]
let c2 = combine [IDENTITY; NOT]
let v1 = CNOT
let v2 = combine [IDENTITY; NOT]
        |> chain [CNOT]

type DeutschResult =
    | Constant of double // probability of constant
    | Variable of double // probability of variable

let deutsch blackbox =
    entangle [State.zero; State.one]
    |> apply [HADAMARD; HADAMARD]
    |> apply [blackbox]
    |> apply [HADAMARD; IDENTITY]
    |> measure 0
    |> function
        | x when x >= 0.5 -> Constant x
        | x (*when x < 0.5*) -> Variable (1. - x)

[<EntryPoint>]
let main argv =
    entangle [zero; zero]
    |> apply [HADAMARD; HADAMARD]
    |> tostring
    |> printf "%O"
    0 // return an integer exit code
