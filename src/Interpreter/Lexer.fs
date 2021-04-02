namespace Interpreter

type Keyword =
    | Let
    | Funq
    | Log

type Token =
    | Keyword of Keyword
    | Identifier of string
    | Space
    | Newline

module Lexer =

    let removeCarriageReturns (code: string) = code.Replace("\r", "")

    let removeComments (code: string) =
        code.Split("\n")
        |> Seq.map (fun line -> line.Split("//").[0])
        |> String.concat "\n"

    let characterise (code: string) =
        code.Split("\n")
        |> Array.toList
        |> List.mapi
            (fun lineNum line ->
                if String.length line = 0
                   || System.String.IsNullOrWhiteSpace line then
                    List.empty
                else
                    (Seq.toList line) @ [ '\n' ]
                    |> List.mapi (fun charNum char -> Parsing.Char(char, (lineNum + 1, charNum + 1))))
        |> List.concat

    let pWhitespace =
        Parsing.any [ Parsing.pChar '\n'
                      Parsing.pChar '\t' ]

    let tokenise = Parsing.pString "funq"

    let lex (input: string) =
        input
        |> removeCarriageReturns
        |> removeComments
        |> characterise
        |> Parsing.runFmt tokenise
