namespace Language.Core

type Token =
    | LParen
    | RParen
    | Question
    | Colon
    | Bang
    | Atom of string
