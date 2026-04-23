namespace Language

type Expr =
    | Number of int
    | Bool of bool
    | Var of string
    | If of Expr * Expr * Expr
    | Lambda of string * Expr
    | App of Expr * Expr
    | Let of string * Expr * Expr
    | LetRec of string * string * Expr * Expr
    | Set of string * Expr
    | Raise of Expr
    | ArrowLoop of Expr * Expr
    | Seq of Expr list
