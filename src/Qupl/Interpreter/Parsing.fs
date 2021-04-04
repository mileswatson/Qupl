namespace Interpreter

type Identifier = Identifier of string

type State =
    | Zero
    | One
    | StateExp of Identifier

type ParallelStates = ParallelStates of State list

type ParallelGates =
    | ParallelGates of Identifier list
    | Log

type SequentialGates = SequentialGates of ParallelGates list

type Definition =
    | Let of Identifier * ParallelStates * SequentialGates option
    | Funq of Identifier * SequentialGates

type Program = Program of Definition list

module Parser =

    open ParserPrimitives

    /// Removes characters that increase complexity of parsing.
    let removeCarriageReturns (code: string) = code.Replace("\r", "")

    /// Splits string into a list of Chars
    let characterise (code: string) =
        code.Split('\n')
        |> Array.toList
        |> List.mapi
            (fun lineNum line ->
                (Seq.toList line) @ [ '\n' ]
                |> List.mapi (fun charNum char -> Char(char, (lineNum + 1, charNum + 1))))
        |> List.concat

    /// Matches whitespace (except new lines).
    let whitespace =
        atleast 1 (pAnyChar " 	") <?> "whitespace"

    // Matches a comment until a new line is reached.
    let comment =
        pString "//" >>. many (pAnyOtherChar "\n")
        |>> System.String.Concat

    /// Matches a new line, even if there is whitespace / a comment
    /// before or whitespace after the '\n' char.
    let newline =
        let _newline =
            maybe whitespace >>. maybe comment >>. pChar '\n'

        atleast 1 _newline <?> "a new line"

    /// Matches an alphabetic identifier.
    let identifier =
        pAnyChar [ 'a' .. 'z' ]
        <|> pAnyChar [ 'A' .. 'Z' ]
        |> atleast 1
        |>> (System.String.Concat >> Identifier)
        <?> "an identifier (alphabetic string)"

    /// Matches a state expression (0, 1, or an identifer).
    let state =
        (pChar '0' |>> (fun _ -> Zero))
        <|> (pChar '1' |>> (fun _ -> One))
        <|> (identifier |>> StateExp)
        <?> "a state expression (0, 1, or an identifier)"

    /// Matches multiple states on the same line, separated by whitespace.
    let parallelStates =
        state .>>. many (whitespace >>. state)
        |>> function
        | (a, m) -> a :: m
        |>> ParallelStates
        <?> "at least one state expression (separated by whitespace)"

    /// Matches multiple gates on the same line, separated by whitespace.
    let parallelGates =
        (pString "log" |>> (fun _ -> Log))
        <|> (identifier .>>. many (whitespace >>. identifier)
             |>> function
             | (a, m) -> a :: m
             |>> ParallelGates)
        <?> "either a 'log' keyword or at least one gate (separated by whitespace)"

    /// Matches multiple parallel gate expressions, separated by newlines.
    let sequentialGates =
        separated whitespace (parallelGates .>> newline)
        |>> SequentialGates
        <?> "at least 1 parallel gate expression"

    /// Matches a 'let' definition (not including the keyword).
    let letDefinition =
        whitespace >>. identifier
        .>> maybe whitespace
        .>> pChar '='
        .>> maybe whitespace
        .>> maybe (newline .>>. whitespace)
        .>>. parallelStates
        .>> (newline .>>. whitespace)
        .>>. maybe sequentialGates
        |>> function
        | ((name, states), gates) -> Let(name, states, gates)

    /// Matches a 'funq' definition (not including keyword).
    let funqDefinition =
        whitespace >>. identifier
        .>> maybe whitespace
        .>> pChar '='
        .>> maybe whitespace
        .>> maybe (newline .>>. whitespace)
        .>>. sequentialGates
        |>> Funq

    /// Matches either a 'funq' or a 'let' definition.
    let definition =
        let innerFn input =
            let letOrFunq =
                pString "let" <|> pString "funq"
                <?> "expected 'let' or 'funq' keyword"

            match run letOrFunq input with
            | Failure (p, e, f) -> Failure(p, e, f)
            | Success ("let", remaining) -> run letDefinition remaining
            | Success ("funq", remaining) -> run funqDefinition remaining
            | Success (matched, _) -> failwithf "Unexpected match '%s'" matched

        Parser innerFn

    /// Matches definitions until the end of the stream is reached.
    let parse =
        maybe newline >>. untilEnd definition |>> Program

    /// Generates an abstract syntax tree.
    let generateSyntaxTree (input: string) =
        input
        |> removeCarriageReturns
        |> characterise
        |> runFmt parse
