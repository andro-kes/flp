namespace Language.Parser

open Language.Core

module internal ArrowLoopParser =
    let parseArrowLoop (toExpr: Sexp -> Expr) (sexp: Sexp) : Expr option =
        match sexp with
        | SList [ SAtom "->"; cond; body ] -> Some(ArrowLoop(toExpr cond, toExpr body))
        | _ -> None
