namespace Interpreter

module Parsing =

    type Char = char * (int * int)

    type Failure = (int * int) * string

    type Result<'a> =
        | Success of 'a * seq<Char>
        | Failure of Failure

    type Parser<'a> = Parser of (seq<Char> -> Result<'a>)

    let run (Parser p) = p

    /// andThen
    let (.>>.) (p1: Parser<seq<'a>>) (p2: Parser<seq<'a>>) =
        let innerFn input =
            match run p1 input with
            | Failure f -> Failure f
            | Success (a1, remaining) ->
                match run p2 remaining with
                | Failure f -> Failure f
                | Success (a2, remaining) -> Success(Seq.append a1 a2, remaining)

        Parser innerFn

    /// orElse
    let (<|>) p1 p2 =
        let innerFn input =
            match run p1 input with
            | Success (a, remaining) -> Success(a, remaining)
            | Failure _ -> run p2 input

        Parser innerFn

    /// map
    let (|>>) f p1 =
        let innerFn input =
            match run p1 input with
            | Success (a, remaining) -> Success(f a, remaining)
            | Failure f -> Failure f

        Parser innerFn

    let maybe parser =
        let innerFn input =
            match run parser input with
            | Success (a, remaining) -> Success(Seq.singleton a, remaining)
            | Failure f -> Success(Seq.empty, input)

        Parser innerFn

    let maybeMany parser =
        let rec innerFn matched input =
            match run parser input with
            | Failure f -> Success(matched, input)
            | Success (a, remaining) -> innerFn (Seq.append a matched) remaining

        Parser(innerFn Seq.empty)

    let many parser = parser .>>. (maybeMany parser)

    let pChar a =
        let innerFn input =
            match Seq.tryHead input with
            | None -> Failure((0, 0), "Unexpected end of file.")
            | Some (c: Char) ->
                if fst c = a then
                    Success(c, Seq.tail input)
                else
                    Failure(snd c, sprintf "Expected to find '%c', but found '%c'." a (fst c))

        Parser innerFn

    let pAnyChar aSeq =
        let innerFn input =
            match Seq.tryHead input with
            | None -> Failure((0, 0), "Unexpected end of file!")
            | Some (c: Char) ->
                if Seq.contains (fst c) aSeq then
                    Success(c, Seq.tail input)
                else
                    Failure(snd c, sprintf "Unexpected character '%c'." (fst c))

        Parser innerFn

    let pString str =
        str
        |> Seq.map pChar
        |> Seq.map ((|>>) Seq.singleton)
        |> Seq.reduce ((.>>.))
