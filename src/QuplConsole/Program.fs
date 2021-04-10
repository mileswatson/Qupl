open Interpreter

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "File path is required!"
        -1
    else
        System.IO.File.ReadAllText argv.[0]
        |> Syntax.generateSyntaxTree
        |> Result.bind Semantics.analyseSemantics
        |> Result.map Runtime.run
        |> function
        | Ok s -> printfn "%s" s
        | Error msg -> printfn "%s" msg

        0
