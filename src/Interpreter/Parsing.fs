namespace Interpreter

module Parsing =

    type Result<'a> =
        | Success of 'a * string
        | Failure of string

    type Parser<'a> = Parser of (string -> Result<'a>)

    let run (Parser p) = p

    /// andThen
    let (.>>.) p1 p2 =
        let innerFn input =
            match run p1 input with
            | Failure f -> Failure f
            | Success (a1, remaining) ->
                match run p2 remaining with
                | Failure f -> Failure f
                | Success (a2, remaining) -> Success((a1, a2), remaining)

        Parser innerFn

    /// orElse
    let (<|>) p1 p2 =
        let innerFn input =
            match run p1 input with
            | Success (a, remaining) -> Success(a, remaining)
            | Failure f -> run p2 input

        Parser innerFn

    /// map
    let (|>>) f p1 =
        let innerFn input =
            match p1 input with
            | Success (a, remaining) -> Success(f a, remaining)
            | Failure f -> Failure f

        Parser innerFn
