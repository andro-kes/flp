namespace Language.Runtime

open System.Collections.Generic
open Language.Core

type Value =
    | VNumber of int
    | VBool of bool
    | VUnit
    | VClosure of string * Expr * Env
    | VPrim of (Value -> Value)

and Env(parent: Env option) =
    let store = Dictionary<string, Value>()

    member _.Get(name: string) : Value =
        match store.TryGetValue(name) with
        | true, v -> v
        | _ ->
            match parent with
            | Some p -> p.Get(name)
            | None -> failwith $"Unbound variable: {name}"

    member _.Define(name: string, value: Value) : unit =
        store[name] <- value

    member this.Set(name: string, value: Value) : unit =
        if store.ContainsKey(name) then
            store[name] <- value
        else
            match parent with
            | Some p -> p.Set(name, value)
            | None -> failwith $"Unbound variable: {name}"

module Env =
    let extend (parent: Env) : Env = Env(Some parent)
    let empty () : Env = Env(None)

module Eval =
    let private truthy (v: Value) : bool =
        match v with
        | VBool b -> b
        | _ -> failwith "Condition must be a boolean value"

    let rec eval (env: Env) (expr: Expr) : Value =
        match expr with
        | Number n -> VNumber n
        | Bool b -> VBool b
        | Var x -> env.Get(x)
        | If(c, t, f) -> if eval env c |> truthy then eval env t else eval env f
        | Lambda(arg, body) -> VClosure(arg, body, env)
        | App(fexp, aexp) ->
            let fv = eval env fexp
            let av = eval env aexp
            match fv with
            | VClosure(arg, body, closEnv) ->
                let callEnv = Env.extend closEnv
                callEnv.Define(arg, av)
                eval callEnv body
            | VPrim p -> p av
            | _ -> failwith "Attempt to call a non-function value"
        | Let(name, value, body) ->
            let v = eval env value
            let env2 = Env.extend env
            env2.Define(name, v)
            eval env2 body
        | LetRec(fname, arg, fbody, inExpr) ->
            let env2 = Env.extend env
            env2.Define(fname, VUnit)
            let closure = VClosure(arg, fbody, env2)
            env2.Set(fname, closure)
            eval env2 inExpr
        | Set(name, value) ->
            let v = eval env value
            env.Set(name, v)
            VUnit
        | Raise e ->
            let v = eval env e
            raise (LangError { Message = $"Raised: %A{v}" })
        | ArrowLoop(cond, body) ->
            let mutable last = VUnit
            while eval env cond |> truthy do
                last <- eval env body
            last
        | Seq exprs ->
            let mutable last = VUnit
            for e in exprs do
                last <- eval env e
            last

    let baseEnv () : Env =
        let env = Env.empty ()
        env.Define("+", VPrim(fun a -> VPrim(fun b ->
            match a, b with
            | VNumber x, VNumber y -> VNumber(x + y)
            | _ -> failwith "'+' expects two numbers")))
        env.Define("-", VPrim(fun a -> VPrim(fun b ->
            match a, b with
            | VNumber x, VNumber y -> VNumber(x - y)
            | _ -> failwith "'-' expects two numbers")))
        env.Define("*", VPrim(fun a -> VPrim(fun b ->
            match a, b with
            | VNumber x, VNumber y -> VNumber(x * y)
            | _ -> failwith "'*' expects two numbers")))
        env.Define("=", VPrim(fun a -> VPrim(fun b ->
            match a, b with
            | VNumber x, VNumber y -> VBool(x = y)
            | VBool x, VBool y -> VBool(x = y)
            | _ -> failwith "'=' expects comparable values")))
        env.Define("<", VPrim(fun a -> VPrim(fun b ->
            match a, b with
            | VNumber x, VNumber y -> VBool(x < y)
            | _ -> failwith "'<' expects two numbers")))
        env.Define(">", VPrim(fun a -> VPrim(fun b ->
            match a, b with
            | VNumber x, VNumber y -> VBool(x > y)
            | _ -> failwith "'>' expects two numbers")))
        env.Define("not", VPrim(fun a ->
            match a with
            | VBool b -> VBool(not b)
            | _ -> failwith "'not' expects a boolean"))
        env
