namespace Interpreter

module Parsing =

    type Result<'a, 'b> =
        | Success of 'a * seq<'b>
        | Failure of string

    type Parser<'a, 'b> = Parser of (seq<'b> -> Result<'a, 'b>)

    let run (Parser p) = p

    /// andThen
    let (.>>.) p1 p2 =
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
            match p1 input with
            | Success (a, remaining) -> Success(f a, remaining)
            | Failure f -> Failure f

        Parser innerFn

    let matchOne a =
        let innerFn (input: seq<'b>) =
            match Seq.tryHead input with
            | None -> Failure "Unexpected termination!"
            | Some c ->
                if a = c then
                    Success(c, Seq.tail input)
                else
                    Failure "Character did not match!"

        Parser innerFn

    let matchAny aSeq =
        let innerFn (input: seq<'b>) =
            match Seq.tryHead input with
            | None -> Failure "Unexpected termination!"
            | Some c ->
                if Seq.contains c aSeq then
                    Success(c, Seq.tail input)
                else
                    Failure "Unexpected character!"

        Parser innerFn
