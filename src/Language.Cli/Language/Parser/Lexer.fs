namespace Language.Parser

open System
open Language.Core

module Lexer =
    let private isWhitespace (c: char) = Char.IsWhiteSpace c

    let tokenize (text: string) : Token list =
        let rec loop i acc =
            if i >= text.Length then List.rev acc
            else
                let c = text[i]
                if isWhitespace c then loop (i + 1) acc
                elif c = '(' then loop (i + 1) (LParen :: acc)
                elif c = ')' then loop (i + 1) (RParen :: acc)
                elif c = '?' then loop (i + 1) (Question :: acc)
                elif c = ':' then loop (i + 1) (Colon :: acc)
                elif c = '!' then loop (i + 1) (Bang :: acc)
                else
                    let mutable j = i
                    while j < text.Length
                          && not (isWhitespace text[j])
                          && text[j] <> '('
                          && text[j] <> ')'
                          && text[j] <> '?'
                          && text[j] <> ':'
                          && text[j] <> '!' do
                        j <- j + 1
                    let atom = text.Substring(i, j - i)
                    loop j (Atom atom :: acc)
        loop 0 []
