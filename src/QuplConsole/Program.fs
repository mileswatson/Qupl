open Interpreter.Parsing

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "File path is required!"
        -1
    else
        System.IO.File.ReadAllText argv.[0]
        |> generateSyntaxTree
        |> function
        | Ok _ -> printfn "Success!"
        | Error msg -> printfn "%s" msg

        0
