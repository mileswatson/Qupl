open Interpreter.Lexer

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "File path is required!"
        -1
    else
        System.IO.File.ReadAllText argv.[0]
        |> lex
        |> String.concat "\n"
        |> printf "%s"

        0
