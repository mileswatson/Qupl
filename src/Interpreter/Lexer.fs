namespace Interpreter

module Lexer =

    let splitLines (code: string) =
        code.Replace("\r", "").Split '\n' |> seq

    let removeBlankLines =
        Seq.where
            (fun (x: string) ->
                (x.Length = 0 || System.String.IsNullOrWhiteSpace x)
                |> not)

    let lex = splitLines >> removeBlankLines
