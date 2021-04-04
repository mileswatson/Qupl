namespace Interpreter

module ParserPrimitives =

    type Char = char * (int * int)

    type Position = int * int

    type Expected = string

    type Found = string

    type Result<'a> =
        | Success of 'a * Char list
        | Failure of Position * Expected * Found

    type Parser<'a> = Parser of (Char list -> Result<'a>)

    /// Applies a parser to a given Char list.
    let run (Parser p) = p

    /// Applies a parser to a given Char list, and returns either
    /// the result or a formatted error string.
    let runFmt p cs =
        run p cs
        |> function
        | Success (a, _) -> Ok a
        | Failure (p, e, f) ->
            let lineNum, charNum = p

            if lineNum <= 0 || charNum <= 0 then
                sprintf "Expected %s but found %s" e f |> Error
            else
                let msg =
                    sprintf "(%i, %i) Expected %s but found %s instead." (fst p) (snd p) e f

                let line =
                    cs
                    |> Seq.where
                        (function
                        | (_, p) when fst p = lineNum -> true
                        | _ -> false)
                    |> Seq.map fst
                    |> System.String.Concat

                let pointer = String.replicate (charNum - 1) " " + "^"

                msg + "\n" + line + pointer |> Error

    /// Matches two parsers, and tuples the results.
    let (.>>.) p1 p2 =
        let innerFn input =
            match run p1 input with
            | Failure (p, e, f) -> Failure(p, e, f)
            | Success (a1, remaining) ->
                match run p2 remaining with
                | Failure (p, e, f) -> Failure(p, e, f)
                | Success (a2, remaining) -> Success((a1, a2), remaining)

        Parser innerFn

    /// Matches two parsers, ignoring the result of the first.
    let (>>.) p1 p2 =
        let innerFn input =
            match run p1 input with
            | Failure (p, e, f) -> Failure(p, e, f)
            | Success (_, remaining) ->
                match run p2 remaining with
                | Failure (p, e, f) -> Failure(p, e, f)
                | Success (a2, remaining) -> Success(a2, remaining)

        Parser innerFn

    /// Matches two parsers, ignoring the result of the second.
    let (.>>) p1 p2 =
        let innerFn input =
            match run p1 input with
            | Failure (p, e, f) -> Failure(p, e, f)
            | Success (a1, remaining) ->
                match run p2 remaining with
                | Failure (p, e, f) -> Failure(p, e, f)
                | Success (_, remaining) -> Success(a1, remaining)

        Parser innerFn

    /// Matches either of the parsers.
    let (<|>) p1 p2 =
        let innerFn input =
            match run p1 input with
            | Success (a, remaining) -> Success(a, remaining)
            | Failure _ -> run p2 input

        Parser innerFn

    /// Applies a function to the successful output of a parser.
    let map f p1 =
        let innerFn input =
            match run p1 input with
            | Success (a, remaining) -> Success(f a, remaining)
            | Failure (p, e, f) -> Failure(p, e, f)

        Parser innerFn

    /// Applies a function to the successful output of a parser.
    let (|>>) p1 f = map f p1

    /// Replaces the expected value of a failure.
    let (<?>) parser label =
        let innerFn input =
            match run parser input with
            | Success (a, remaining) -> Success(a, remaining)
            | Failure (position, _, f) -> Failure(position, label, f)

        Parser innerFn

    /// Matches any parser in the list.
    let any parsers = parsers |> Seq.reduce (<|>)

    /// Matches a list of items in sequence.
    let sequence parsers =
        parsers
        |> Seq.map (map List.singleton)
        |> Seq.reduce (fun x y -> (x .>>. y) |>> ((<||) List.append))

    /// Matches Some or None.
    let maybe parser =
        let innerFn input =
            match run parser input with
            | Success (a, remaining) -> Success(Some a, remaining)
            | Failure _ -> Success(None, input)

        Parser innerFn

    /// Matches zero or more parsers.
    let many parser =
        let rec innerFn matched input =
            match run parser input with
            | Failure _ -> Success(List.rev matched, input)
            | Success (a, remaining) -> innerFn (a :: matched) remaining

        Parser(innerFn [])

    let untilEnd parser =
        let rec innerFn matched =
            function
            | [] -> Success(List.rev matched, [])
            | input ->
                match run parser input with
                | Failure (p, e, f) -> Failure(p, e, f)
                | Success (a, remaining) -> innerFn (a :: matched) remaining

        Parser(innerFn [])

    /// Matches exactly n parsers, where n >= 1.
    let exactly n parser =
        Seq.init n (fun _ -> parser) |> sequence

    /// Matches n or more, where n >= 1.
    /// Use 'many' when n = 0.
    let atleast n parser =
        exactly n parser .>>. many parser
        |>> ((<||) List.append)

    let private charToString =
        function
        | '\n' -> "'\\n' (a new line)"
        | '\t' -> "'\\t' (a tab)"
        | ' ' -> "' ' (a space)"
        | c -> sprintf "'%c'" c


    /// Matches one character exactly.
    let pChar a =
        let innerFn input =
            match input with
            | [] -> Failure((0, 0), string a, "EOF")
            | (head: Char) :: remaining ->
                if fst head = a then
                    Success(head, remaining)
                else
                    let found = fst head |> charToString
                    Failure(snd head, charToString a, found)

        Parser innerFn

    /// Matches any character that doesn't appear in the list.
    let pAnyOtherChar str =
        let innerFn input =
            match input with
            | [] -> Failure((0, 0), sprintf "a character not in %A" str, "EOF")
            | (head: Char) :: remaining ->
                if Seq.contains (fst head) str then
                    let found = fst head |> charToString
                    Failure(snd head, sprintf "a character not in %A" str, found)
                else
                    Success(head, remaining)

        Parser innerFn

    /// Matches any character in the list.
    let pAnyChar str = str |> Seq.map pChar |> any

    /// Matches a string.
    let pString (str: string) =
        str |> Seq.map pChar |> sequence
        |>> (List.map fst >> System.String.Concat)
        <?> sprintf "'%s'" str
