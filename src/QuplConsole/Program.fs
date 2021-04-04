open Interpreter.Parser

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "File path is required!"
        -1
    else
        System.IO.File.ReadAllText argv.[0]
        |> parse
        |> function
        | Ok _ -> printfn "Success!"
        | Error msg -> printfn "%s" msg

        0
