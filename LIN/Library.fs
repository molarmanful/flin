module rec LIN.ENV

open System.IO
open Spectre.Console
open FSharpx.Collections
open FSharpPlus
open FSharpPlus.Lens
open Ficus.RRBVector
open MathNet.Numerics

module RVec = RRBVector
module PVec = PersistentVector
module PMap = PersistentHashMap

module PVec =
    let lconj ys xs = fold (flip PVec.conj) xs ys

let (|C|Nil|) = PVec.(|Conj|Nil|)

[<AutoOpen>]
module HELP =
    let lines = String.split [ "\n"; "\r\n" ]

    let toForm a =
        match a with
        | ARR x -> RVec.map toForm x |> string |> sprintf "[%s]"
        | MAP x ->
            Seq.map (fun (a, b) -> $"{toForm a}=>{toForm b}") x
            |> String.intercalate " "
            |> sprintf "{%s}"
        | NUM _ -> string a
        | STR x ->
            [ "\\", "\\\\"; "\"", "\\\"" ]
            |> fold (flip <| (<||) replace) x
            |> sprintf "\"%s\""
        | CMD x -> x
        | FN (_, x) ->
            List.map toForm x
            |> String.intercalate " "
            |> fun a -> $"( {a} )"
        | UN _ -> "$U"

    let pprint = AnsiConsole.MarkupLine
    let ppprint = AnsiConsole.MarkupLineInterpolated

    let pTrace (c, env, d) =
        pprint "[dim grey]———>[/]"

        let (l, f), xs = env.code
        let c = toForm c
        let fcs = map toForm xs

        if length fcs > 7 then
            let cs = take 7 fcs |> String.intercalate " "
            ppprint $"[bold yellow]{c}[/] [grey]{cs} ...[/]"
        elif length fcs = 0 then
            ppprint (
                if d then
                    $"[dim green](EMPTY)[/]"
                else
                    $"[bold yellow]{c}[/]"
            )
        else
            let cs = fcs |> String.intercalate " "
            ppprint $"[bold yellow]{c}[/] [grey]{cs}[/]"

        ppprint $"[dim grey]———({f}:{l})[/]"

        PVec.map (toForm >> printfn "%s") env.stack
        |> ignore

    let stk env st = { env with stack = st }
    let mkE env e s = e (fst env.code, s)

    let cod env xs =
        let l, _ = env.code
        { env with code = (l, xs) }

    let code env xs =
        let l, cs = env.code
        { env with code = (l, xs ++ cs) }

    let codl env (p, xs) =
        let _, cs = env.code
        { env with code = (p, xs ++ cs) }

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
    let push' = flip push
    let pushs x env = PVec.lconj x env.stack |> stk env
    let pushs' = flip pushs

    let eval env s =
        match s with
        | FN x -> codl env x |> exec
        | _ -> ANY.toFN env s |> eval env

    let evalr env s = (eval env s).stack

    let pline env i =
        let (f, _), _ = env.code

        { env with
            lines =
                if PMap.containsKey (f, i) env.lines then
                    let s = env.lines[f, i]

                    PMap.add
                        (f, i)
                        (match s with
                         | STR _ -> ANY.iFN env i s
                         | _ -> s)
                        env.lines
                else
                    env.lines }

    let eline env i =
        let (f, _), _ = env.code

        if PMap.containsKey (f, i) env.lines then
            let env = pline env i
            eval env env.lines[f, i]
        else
            env

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

    let dups env =
        RVec.ofSeq env.stack |> ARR |> push' env

    let swap env = arg2 env <| fun x y -> pushs [ y; x ]

    let neg env = arg1 env <| fun x -> ANY.neg x |> push

    let Lplus env = arg2 env <| fun x -> ANY.plus x >> push

    let startFN env =
        let rec fn env i res =
            match i, env.code with
            | 0, (l, _)
            | _, (l, []) -> FN(l, res) |> push' env
            | _, (_, c :: cs) ->
                match c with
                | CMD x when x.Contains "(" -> res ++ [ c ] |> fn (cod env cs) (i + 1)
                | CMD x when x.Contains ")" ->
                    let i = i - 1

                    (if i > 0 then res ++ [ c ] else res)
                    |> fn (cod env cs) i
                | _ -> res ++ [ c ] |> fn (cod env cs) i

        fn env 1 []

    let es env = arg1 env <| flip eval

    let quar env =
        arg1 env
        <| fun x env ->
            ANY.toFN env x |> eval env
            </ arg1 /> fun y _ -> push y env

    let typ env =
        arg1 env
        <| fun x -> ANY.typ x |> option STR (NUM 0N) |> push

    let dot env =
        if snd env.code |> length = 0 then
            let (_, l), _ = env.code
            eline env (l + 1)
        else
            failwith "TODO"

    let SL =
        dict [ ("type", typ)
               ("form", form)
               ("out", out)
               ("outn", outn)
               ("dup", dup)
               ("pop", pop)
               ("swap", swap)
               ("(", startFN)
               (")", id)
               ("#", es)
               ("Q", quar)
               ("_", neg)
               ("+", Lplus)
               (".", dot) ]

let exec env =
    match env.code with
    | (_, []) ->
        if env.STEP || env.VERB || env.IMPL then
            pTrace (UN(), env, true)

        env
    | (p, c :: cs) -> execA { env with code = (p, cs) } c

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
            |> ANY.toFN env
            |> push' env
            |> exec
        | a when a.StartsWith '#' && a.Length > 1 -> env
        | a when LIB.SL.ContainsKey a -> LIB.SL[a](env) |> exec
        | _ -> mkE env ERR_UNK_FN x |> raise

let run (s, v, i) file lines =
    let env =
        { stack = PVec.empty
          code = ((file, 0), [])
          lines = lines
          scope = PMap.empty
          STEP = s
          VERB = v
          IMPL = i }

    eline env 0

let runf o f =
    File.ReadLines f
    |> mapi (fun l x -> ((f, l), STR x))
    |> PMap.ofSeq
    |> run o f

let runs o s =
    lines s
    |> mapi (fun l x -> (("", l), STR x))
    |> PMap.ofSeq
    |> run o ""
