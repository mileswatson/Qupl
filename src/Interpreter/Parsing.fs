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

    let characterise (code: string) =
        code.Split("\n")
        |> Array.toList
        |> List.mapi
            (fun lineNum line ->
                (Seq.toList line) @ [ '\n' ]
                |> List.mapi (fun charNum char -> Char(char, (lineNum + 1, charNum + 1))))
        |> List.concat

    let whitespace =
        atleast 1 (pAnyChar " 	") <?> "whitespace"

    // Matches a comment until a new line is reached
    let comment =
        pString "//" >>. many (pAnyOtherChar "\n")
        |>> System.String.Concat

    /// Matches a new line, even if there is whitespace / a comment
    /// before or whitespace after the '\n' char
    let newline =
        let _newline =
            maybe whitespace >>. maybe comment >>. pChar '\n'
            .>> maybe whitespace

        atleast 1 _newline <?> "a new line"

    let tokenise =
        newline >>. pString "never gonna give you up"
        .>> newline
        >>. pString "never gonna let you down"

    let lex (input: string) =
        input
        |> removeCarriageReturns
        |> characterise
        |> runFmt tokenise
