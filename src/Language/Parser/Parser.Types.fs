namespace Language.Parser

type internal Sexp =
    | SAtom of string
    | SList of Sexp list
    | STernary of Sexp * Sexp * Sexp
    | SRaise of Sexp
