namespace Language

type Token =
    | LParen
    | RParen
    | Question
    | Colon
    | Bang
    | Atom of string
