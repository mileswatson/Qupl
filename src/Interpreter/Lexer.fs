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

    let removeCarriageReturns (code: string) = code.Replace("\r", "")

    let removeComments (code: string) =
        code.Split("\n")
        |> Seq.map (fun line -> line.Split("//").[0])
        |> String.concat "\n"

    let characterise (code: string) =
        code.Split("\n")
        |> Seq.mapi
            (fun lineNum line ->
                if String.length line = 0
                   || System.String.IsNullOrWhiteSpace line then
                    Seq.empty
                else
                    line + "\n"
                    |> Seq.mapi (fun charNum char -> Parsing.Char(char, (lineNum + 1, charNum + 1))))
        |> Seq.concat

    let lex (input: string) =
        input
        |> removeCarriageReturns
        |> removeComments
        |> characterise
