namespace Interpreter

module Parsing =

    type Char = char * (int * int)

    type Position = int * int

    type Expected = string

    type Found = string

    type Result<'a> =
        | Success of 'a * Char list
        | Failure of Position * Expected * Found

    type Parser<'a> = Parser of (Char list -> Result<'a>)

    let run (Parser p) = p

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
                    sprintf "(%i, %i) Expected %s but found %s" (fst p) (snd p) e f

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

    /// andThen
    let (.>>.) p1 p2 =
        let innerFn input =
            match run p1 input with
            | Failure (p, e, f) -> Failure(p, e, f)
            | Success (a1, remaining) ->
                match run p2 remaining with
                | Failure (p, e, f) -> Failure(p, e, f)
                | Success (a2, remaining) -> Success((a1, a2), remaining)

        Parser innerFn

    let (>>.) p1 p2 =
        let innerFn input =
            match run p1 input with
            | Failure (p, e, f) -> Failure(p, e, f)
            | Success (_, remaining) ->
                match run p2 remaining with
                | Failure (p, e, f) -> Failure(p, e, f)
                | Success (a2, remaining) -> Success(a2, remaining)

        Parser innerFn

    let (.>>) p1 p2 =
        let innerFn input =
            match run p1 input with
            | Failure (p, e, f) -> Failure(p, e, f)
            | Success (a1, remaining) ->
                match run p2 remaining with
                | Failure (p, e, f) -> Failure(p, e, f)
                | Success (_, remaining) -> Success(a1, remaining)

        Parser innerFn

    /// orElse
    let (<|>) p1 p2 =
        let innerFn input =
            match run p1 input with
            | Success (a, remaining) -> Success(a, remaining)
            | Failure _ -> run p2 input

        Parser innerFn

    /// map
    let map f p1 =
        let innerFn input =
            match run p1 input with
            | Success (a, remaining) -> Success(f a, remaining)
            | Failure (p, e, f) -> Failure(p, e, f)

        Parser innerFn

    let (|>>) p1 f = map f p1

    /// Replaces the expected value of a failure
    let (<?>) parser label =
        let innerFn input =
            match run parser input with
            | Success (a, remaining) -> Success(a, remaining)
            | Failure (position, _, f) -> Failure(position, label, f)

        Parser innerFn

    /// Matches any parser
    let any parsers = parsers |> Seq.reduce (<|>)

    /// Matches a list of items in sequence
    let sequence parsers =
        parsers
        |> Seq.map (map List.singleton)
        |> Seq.reduce (fun x y -> (x .>>. y) |>> ((<||) List.append))

    /// Matches Some or None
    let maybe parser =
        let innerFn input =
            match run parser input with
            | Success (a, remaining) -> Success(Some a, remaining)
            | Failure _ -> Success(None, input)

        Parser innerFn

    /// Matches zero or more
    let many parser =
        let rec innerFn matched input =
            match run parser input with
            | Failure _ -> Success(List.rev matched, input)
            | Success (a, remaining) -> innerFn (a :: matched) remaining

        Parser(innerFn [])

    /// Matches one or more
    let many1 parser = parser .>>. (many parser)

    /// Matches one character exactly
    let pChar a =
        let innerFn input =
            match input with
            | [] -> Failure((0, 0), string a, "EOF")
            | (head: Char) :: remaining ->
                if fst head = a then
                    Success(head, remaining)
                else
                    Failure(snd head, string a, sprintf "'%c'" (fst head))

        Parser innerFn

    /// Matches any char
    let pAnyChar str = str |> Seq.map pChar |> any

    /// Matches a string
    let pString (str: string) =
        str |> Seq.map pChar |> sequence |>> string
        <?> str
