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
    let TODO x = failwith "TODO"

    let lines = String.split [ "\n"; "\r\n" ]

    let pprint = AnsiConsole.MarkupLine
    let ppprint = AnsiConsole.MarkupLineInterpolated

    let pTrace c env d =
        let (l, f), xs = env.code
        let c = ANY.toForm c
        let fcs = map ANY.toForm xs

        PVec.map (ANY.toForm >> printfn "%s") env.stack
        |> ignore

        pprint "[dim grey]———>[/]"

        if length fcs > 7 then
            let cs = take 7 fcs |> String.intercalate " "
            ppprint $"[bold yellow]{c}[/] [grey]{cs} ...[/]"
        elif length fcs = 0 then
            ppprint (
                if d then
                    $"[dim green](END)[/]"
                else
                    $"[bold yellow]{c}[/]"
            )
        else
            let cs = fcs |> String.intercalate " "
            ppprint $"[bold yellow]{c}[/] [grey]{cs}[/]"

        ppprint $"[dim grey]———({f}:{l})[/]"

    let stk env st = { env with stack = st }
    let mkE env e s = e (fst env.code, s)

    let code env xs =
        let l, _ = env.code
        { env with code = (l, xs) }

    let coda env xs =
        let l, cs = env.code
        { env with code = (l, xs ++ cs) }

    let lcode env fn = { env with code = fn }

    let lcoda env (p, xs) =
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
        | FN x ->
            let (_, c) = env.code

            match c with
            | [] -> lcoda env x
            | _ ->
                lcode env x
                |> exec
                |> fun env1 -> stk env env1.stack
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
    let form env = arg1 env <| (push << STR << ANY.toForm)

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

    let show = form >> outn

    let dup env = arg1 env <| fun x -> pushs [ x; x ]

    let dups env =
        RVec.ofSeq env.stack |> ARR |> push' env

    let over env =
        arg2 env <| fun x y -> pushs [ x; y; x ]

    let pop env = arg1 env <| konst id

    let clr env = stk env PVec.empty

    let nip env = arg2 env <| konst push

    let swap env = arg2 env <| fun x y -> pushs [ y; x ]

    let rot env =
        arg3 env <| fun x y z -> pushs [ y; z; x ]

    let rot_ env =
        arg3 env <| fun x y z -> pushs [ z; x; y ]

    let neg env = arg1 env <| fun x -> ANY.neg x |> push

    let Lplus env = arg2 env <| fun x -> ANY.plus x >> push

    let plusp env =
        arg2 env <| fun x -> ANY.plus' x >> push

    let startFN env =
        let rec fn env i res =
            match i, env.code with
            | 0, (l, _)
            | _, (l, []) -> FN(l, res) |> push' env
            | _, (_, c :: cs) ->
                match c with
                | CMD x when x.Contains "(" -> res ++ [ c ] |> fn (code env cs) (i + 1)
                | CMD x when x.Contains ")" ->
                    let i = i - 1

                    (if i > 0 then res ++ [ c ] else res)
                    |> fn (code env cs) i
                | _ -> res ++ [ c ] |> fn (code env cs) i

        fn env 1 []

    let es env = arg1 env <| flip eval

    let quar env =
        arg1 env
        <| fun x env ->
            ANY.toFN env x
            |> evalr env
            |> PVec.tryLast
            |> Option.defaultValue (UN())
            |> push' env

    let typ env =
        arg1 env
        <| fun x -> ANY.typ x |> option STR (NUM 0N) |> push

    let Lstr env =
        arg1 env <| fun x -> ANY.toSTR x |> push

    let Lnum env =
        arg1 env <| fun x -> ANY.toNUM x |> push

    let Larr env =
        arg1 env <| fun x -> ANY.toNUM x |> push

    let eln env =
        arg1 env
        <| fun x env ->
            let (_, l), _ = env.code
            eline env (l + ANY.toInt x)

    let eprev = NUM -1N |> push >> eln
    let ehere = NUM 0N |> push >> eln
    let enext = NUM 1N |> push >> eln

    let dot env =
        if snd env.code |> length = 0 then
            enext env
        else
            failwith "TODO"

    let SL =
        dict [ "type", typ
               ">S", Lstr
               ">N", Lnum
               ">F", TODO
               ">A", Larr
               ">M", TODO
               ">Q", TODO
               "form", form

               ">O", out
               "n>O", outn
               "f>O", show

               "dup", dup
               "dups", dups
               "over", over
               "pick", TODO
               "pop", pop
               "clr", clr
               "nip", nip
               "nix", TODO
               "swap", swap
               "rot", rot
               "rot_", rot_
               "roll", TODO
               "roll_", TODO

               "_", neg
               "+", Lplus
               "-", TODO
               "*", TODO
               "**", TODO
               "/", TODO
               "%", TODO
               "/%", TODO

               "++", plusp

               "(", startFN
               ")", id // TODO?
               "#", es
               "Q", quar
               "@", ehere
               ";", enext
               ";;", eprev
               "e@", eln

               ".", dot ]

let exec env =
    match env.code with
    | (_, []) -> env
    | (p, c :: cs) -> execA { env with code = (p, cs) } c |> exec

let execA env c =
    if env.STEP || env.VERB then
        pTrace c env false

    match c with
    | NUM _
    | STR _ -> push c env
    | CMD x ->
        match x with
        | a when a.StartsWith '\\' && a.Length > 1 -> drop 1 x |> CMD |> ANY.toFN env |> push' env
        | a when a.StartsWith '#' && a.Length > 1 -> env
        | a when LIB.SL.ContainsKey a -> LIB.SL[a](env)
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
    |> exec
    |> tap (fun env ->
        if env.STEP || env.VERB || env.IMPL then
            pTrace (UN()) env true)

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
