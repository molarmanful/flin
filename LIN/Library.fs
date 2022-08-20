module rec LIN.ENV

open System.IO
open Spectre.Console
open FSharpx.Collections
open FSharpPlus
open MathNet.Numerics

module PVec = PersistentVector
module PMap = PersistentHashMap

module PVec =
    let lconj ys xs = fold (flip PVec.conj) xs ys

let (|C|Nil|) = PVec.(|Conj|Nil|)

[<AutoOpen>]
module HELP =
    let join s = map string >> String.intercalate s

    let toForm a =
        match a with
        | ARR x -> map toForm x |> string |> sprintf "[%s]"
        | MAP x ->
            PMap.toSeq x
            |> map (fun (a, b) -> $"{toForm a}=>{toForm b}")
            |> join " "
            |> sprintf "{%s}"
        | NUM _ -> string a
        | STR x ->
            [ "\\", "\\\\"; "\"", "\\\"" ]
            |> fold (flip <| (<||) replace) x
            |> sprintf "\"%s\""
        | CMD x -> x
        | FN (x, (f, l)) ->
            toSeq x
            |> Seq.map toForm
            |> join " "
            |> fun a -> $"""( {a} )@{l}"""
        | UN _ -> "$U"

    let pprint = AnsiConsole.MarkupLine
    let ppprint = AnsiConsole.MarkupLineInterpolated

    let pTrace (c, env, d) =
        pprint "[dim grey]———>[/]"

        let xs, (f, l) = env.code
        let c = toForm c
        let code = map toForm xs

        if length code > 7 then
            let cs = take 7 code |> join " "
            ppprint $"[bold yellow]{c}[/] [grey]{cs} ...[/]"
        elif length code = 0 then
            ppprint (
                if d then
                    $"[dim green](EMPTY)[/]"
                else
                    $"[bold yellow]{c}[/]"
            )
        else
            let cs = join " " code
            ppprint $"[bold yellow]{c}[/] [grey]{cs}[/]"

        let f = defaultArg f "?"
        ppprint $"[dim grey]———({f}:{l})[/]"

        PVec.map (toForm >> printfn "%s") env.stack
        |> ignore

    let stk env st = { env with stack = st }
    let mkE env e s = e (s, snd env.code)

    let cod env xs =
        let (_, l) = env.code
        { env with code = (xs, l) }

    let code env xs =
        let (cs, l) = env.code
        { env with code = (xs ++ cs, l) }

    let codl env (xs, l) =
        let (cs, _) = env.code
        { env with code = (xs ++ cs, l) }

    let arg1 env f =
        match env.stack with
        | C (xs, x) -> f x <| stk env xs
        | _ -> mkE env ERR_ST_LEN 1 |> raise

    let arg2 env f =
        match env.stack with
        | C (C (xs, x), y) -> f x y <| stk env xs
        | _ -> mkE env ERR_ST_LEN 2 |> raise

    let arg3 env f =
        match env.stack with
        | C (C (C (xs, x), y), z) -> f x y z <| stk env xs
        | _ -> mkE env ERR_ST_LEN 3 |> raise

    let push x env = env.stack.Conj x |> stk env
    let pushs x env = PVec.lconj x env.stack |> stk env

    let eval env s =
        match s with
        | FN x -> codl env x
        | _ -> ANY.toCode (snd env.code) s |> FN |> eval env

module LIB =
    let form env = arg1 env <| (push << STR << toForm)

    let out env =
        arg1 env
        <| fun x xs ->
            printf $"{string x}"
            xs

    let outn env =
        arg1 env
        <| fun x xs ->
            printfn $"{string x}"
            xs

    let pop env = arg1 env <| fun _ xs -> xs

    let dup env = arg1 env <| fun x -> pushs [ x; x ]

    let swap env = arg2 env <| fun x y -> pushs [ y; x ]

    let Lplus env = arg2 env <| fun x -> push << ANY.plus x

    let startFN env =
        let rec fn env i res =
            match i, env.code with
            | 0, (_, l)
            | _, ([], l) -> FN(res, l) |> flip push env
            | _, (c :: cs, _) ->
                match c with
                | CMD x when x.Contains "(" -> res ++ [ c ] |> fn (cod env cs) (i + 1)
                | CMD x when x.Contains ")" ->
                    let i = i - 1

                    (if i > 0 then res ++ [ c ] else res)
                    |> fn (cod env cs) i
                | _ -> res ++ [ c ] |> fn (cod env cs) i

        fn env 1 []

    let es env = arg1 env <| flip eval

    let quar env = failwith "TODO"

    let typ env =
        arg1 env
        <| fun x -> ANY.typ x |> option STR (NUM 0N) |> push

    let SL =
        dict [ ("typ", typ)
               ("form", form)
               ("out", out)
               ("outn", outn)
               ("dup", dup)
               ("pop", pop)
               ("swap", swap)
               ("(", startFN)
               (")", id)
               ("$", es)
               ("Q", quar)
               ("+", Lplus)
               (".", id) ]

let exec env =
    match env.code with
    | ([], _) ->
        if env.STEP || env.VERB || env.IMPL then
            pTrace (UN(), env, true)

        env
    | (c :: cs, p) -> execA { env with code = (cs, p) } c

let execA env c =
    if env.STEP || env.VERB then
        pTrace (c, env, false)

    match c with
    | NUM _
    | STR _ -> push c env |> exec
    | CMD x ->
        match x with
        | a when a.StartsWith '\\' && a.Length > 1 ->
            drop 1 x
            |> CMD
            |> ANY.toCode (snd env.code)
            |> FN
            |> flip push env
            |> exec
        | a when LIB.SL.ContainsKey a -> LIB.SL[a](env) |> exec
        | _ -> mkE env ERR_UNK_FN x |> raise

let run (s, v, i) file lines =
    exec
        { stack = PVec.empty
          code = head lines |> ANY.toCode (file, 0)
          lines = lines
          scope = PMap.empty
          STEP = s
          VERB = v
          IMPL = i }

let runf o p =
    File.ReadLines p
    |> map STR
    |> toList
    |> run o (Some p)

let runs o s = run o None [ STR s ]
