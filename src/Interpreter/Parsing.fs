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

    open ParserPrimitives

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
                    |> List.mapi (fun charNum char -> Char(char, (lineNum + 1, charNum + 1))))
        |> List.concat

    let whitespace = many1 (pAnyChar " 	") <?> "whitespace"

    /// Matches a new line, even if surrounded by whitespace
    let newline =
        maybe whitespace >>. (pChar '\n')
        .>> maybe whitespace
        <?> "a new line"

    let tokenise =
        pString "never gonna give you up" .>> newline
        >>. pString "never gonna let you down"

    let lex (input: string) =
        input
        |> removeCarriageReturns
        |> removeComments
        |> characterise
        |> runFmt tokenise
