namespace Language.Parser

open Language.Core

module internal TernaryRaiseParser =
    let parseRaise (parsePrimary: Token list -> Sexp * Token list) (tokens: Token list) : Sexp * Token list =
        match tokens with
        | Bang :: rest ->
            let node, tail = parsePrimary rest
            (SRaise node, tail)
        | _ -> parsePrimary tokens

    let parseTernary (parseOne: Token list -> Sexp * Token list) (cond: Sexp) (rest: Token list) : Sexp * Token list =
        match rest with
        | Question :: afterQ ->
            let tBranch, afterT = parseOne afterQ
            match afterT with
            | Colon :: afterColon ->
                let fBranch, tail = parseOne afterColon
                (STernary(cond, tBranch, fBranch), tail)
            | _ -> failwith "Expected ':' after then-branch in ternary expression"
        | _ -> (cond, rest)
