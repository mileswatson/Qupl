namespace Interpreter

type Keyword =
    | Let
    | Funq
    | Log

type Gate =
    | H
    | CNOT
    | NOT
    | Identifier of string

type Token =
    | Keyword of Keyword
    | Gate of Gate
    | Equal
    | Colon
    | OpenBracket
    | CloseBracket
    | Identifier of string

module Lexer =

    let splitLines (code: string) =
        code.Replace("\r", "").Split '\n' |> seq

    let removeBlankLines =
        Seq.where
            (fun (x: string) ->
                (x.Length = 0 || System.String.IsNullOrWhiteSpace x)
                |> not)

    let lex = splitLines >> removeBlankLines
