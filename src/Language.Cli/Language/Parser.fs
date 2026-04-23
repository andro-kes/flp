namespace Language

module Parser =

    type private Sexp =
        | SAtom of string
        | SList of Sexp list
        | STernary of Sexp * Sexp * Sexp
        | SRaise of Sexp

    let private parseSexps (tokens: Token list) : Sexp list =
        let rec parseOne (toks: Token list) : Sexp * Token list =
            match toks with
            | [] -> failwith "Unexpected end of input"

            | Bang :: rest ->
                let (node, tail) = parseOne rest
                (SRaise node, tail)

            | LParen :: rest ->
                let rec parseList acc ts =
                    match ts with
                    | [] -> failwith "Unclosed '('"
                    | RParen :: tail -> (SList(List.rev acc), tail)
                    | _ ->
                        let (node, tail) = parseOne ts
                        parseList (node :: acc) tail
                parseList [] rest

            | RParen :: _ -> failwith "Unexpected ')'"

            | Atom a :: Question :: rest ->
                let cond = SAtom a
                let (tBranch, afterT) = parseOne rest
                match afterT with
                | Colon :: afterColon ->
                    let (fBranch, tail) = parseOne afterColon
                    (STernary(cond, tBranch, fBranch), tail)
                | _ -> failwith "Expected ':' after then-branch in ternary expression"

            | Atom a :: rest -> (SAtom a, rest)

            | Question :: _ -> failwith "Unexpected '?'"
            | Colon :: _ -> failwith "Unexpected ':'"

        let rec loop acc ts =
            match ts with
            | [] -> List.rev acc
            | _ ->
                let (node, tail) = parseOne ts
                loop (node :: acc) tail

        loop [] tokens

    let rec private toExpr (sexp: Sexp) : Expr =
        let atomToExpr (a: string) =
            match System.Int32.TryParse a with
            | true, n -> Number n
            | _ ->
                match a with
                | "true" -> Bool true
                | "false" -> Bool false
                | _ -> Var a

        match sexp with
        | SAtom a -> atomToExpr a
        | STernary(c, t, f) -> If(toExpr c, toExpr t, toExpr f)
        | SRaise e -> Raise(toExpr e)

        | SList [] -> failwith "Empty list is not an expression"

        | SList [ SAtom "if"; c; t; f ] -> If(toExpr c, toExpr t, toExpr f)

        | SList [ SAtom "lambda"; SList [ SAtom arg ]; body ] -> Lambda(arg, toExpr body)

        | SList [ SAtom "let"; SAtom name; value; body ] -> Let(name, toExpr value, toExpr body)

        | SList [ SAtom "letrec"; SAtom fname; SList [ SAtom arg ]; fbody; inExpr ] ->
            LetRec(fname, arg, toExpr fbody, toExpr inExpr)

        | SList [ SAtom "set"; SAtom name; value ] -> Set(name, toExpr value)

        | SList(SAtom "begin" :: exprs) -> exprs |> List.map toExpr |> Seq

        | SList [ SAtom "->"; cond; body ] -> ArrowLoop(toExpr cond, toExpr body)

        | SList(f :: args) ->
            let f0 = toExpr f
            args |> List.map toExpr |> List.fold (fun acc x -> App(acc, x)) f0

    let parseProgram (text: string) : Expr =
        let tokens = Lexer.tokenize text
        let sexps = parseSexps tokens
        match sexps with
        | [] -> failwith "Empty program"
        | [ one ] -> toExpr one
        | many -> many |> List.map toExpr |> Seq
