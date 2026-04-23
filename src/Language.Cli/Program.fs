module Program

open Language

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        eprintfn "Usage: flp <file.lang>"
        1
    else
        let path = argv[0]
        if not (System.IO.File.Exists(path)) then
            eprintfn "File not found: %s" path
            1
        else
            try
                let source = System.IO.File.ReadAllText(path)
                let expr = Parser.parseProgram source
                let env = Eval.baseEnv ()
                let result = Eval.eval env expr
                printfn "%A" result
                0
            with
            | LangError err ->
                eprintfn "Error: %s" err.Message
                1
            | ex ->
                eprintfn "Error: %s" ex.Message
                1
