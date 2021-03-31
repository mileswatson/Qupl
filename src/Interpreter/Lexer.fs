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

    let lex = removeCarriageReturns
