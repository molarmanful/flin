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

    let mod1 f env = arg1 env <| (f >> push)
    let mod1s f env = arg1 env <| (f >> pushs)

    let arg2 env f =
        match env.stack with
        | C (C (xs, x), y) -> f x y <| stk env xs
        | _ -> mkE env ERR_ST_LEN 2 |> raise

    let mod2 f env = arg2 env <| fun x -> f x >> push
    let mod2s f env = arg2 env <| fun x -> f x >> pushs

    let arg3 env f =
        match env.stack with
        | C (C (C (xs, x), y), z) -> f x y z <| stk env xs
        | _ -> mkE env ERR_ST_LEN 3 |> raise

    let mod3 f env = arg3 env <| fun x y -> f x y >> push
    let mod3s f env = arg3 env <| fun x y -> f x y >> pushs

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
    let form = mod1 (ANY.toForm >> STR)

    let out =
        mod1s
        <| fun x ->
            printf $"{string x}"
            []

    let outn =
        mod1s
        <| fun x ->
            printfn $"{string x}"
            []

    let show = form >> outn

    let dup = mod1s <| fun x -> [ x; x ]

    let dups env =
        RVec.ofSeq env.stack |> ARR |> push' env

    let over = mod2s <| fun x y -> [ x; y; x ]

    let pop = mod1s <| konst []
    let clr = flip stk PVec.empty
    let nip = mod2 <| fun x _ -> x

    let swap = mod2s <| fun x y -> [ y; x ]
    let rot = mod3s <| fun x y z -> [ y; z; x ]
    let rot_ = mod3s <| fun x y z -> [ z; x; y ]

    let neg = mod1 ANY.neg
    let Lplus = mod2 ANY.plus
    let Lplus' = mod2 ANY.plus'
    let Lplus'' = mod2 ANY.plus''

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

    let typ = mod1 (ANY.typ >> option STR (NUM 0N))

    let Lstr = mod1 ANY.toSTR
    let Lnum = mod1 ANY.toNUM
    let Lseq = mod1 ANY.toSEQ
    let Larr = mod1 ANY.toARR

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
               ">Q", Lseq
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
               "++", Lplus'
               "+'", Lplus''
               "-", TODO
               "*", TODO
               "^", TODO
               "/", TODO
               "%", TODO
               "/%", TODO

               "(", startFN
               ")", id // TODO?
               "#", es
               "Q", quar
               "@", ehere
               ";", enext
               ";;", eprev
               "e@", eln

               "[", TODO
               "]", TODO

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
