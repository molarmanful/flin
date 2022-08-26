module rec LIN.ENV

open System.IO
open Spectre.Console
open FSharpx.Collections
open FSharpPlus
open Ficus.RRBVector

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
        | _ -> ANY.mkE env ERR_ST_LEN 1

    let mod1 f env = arg1 env <| (f >> push)
    let mod1s f env = arg1 env <| (f >> pushs)

    let arg2 env f =
        match env.stack with
        | C (C (xs, x), y) -> f x y <| stk env xs
        | _ -> ANY.mkE env ERR_ST_LEN 2

    let mod2 f env = arg2 env <| fun x -> f x >> push
    let mod2s f env = arg2 env <| fun x -> f x >> pushs

    let arg3 env f =
        match env.stack with
        | C (C (C (xs, x), y), z) -> f x y z <| stk env xs
        | _ -> ANY.mkE env ERR_ST_LEN 3

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
        | NUM _ -> push s env
        | ANY.Itr _ ->
            ANY.map
                (evale env
                 >> exec
                 >> fun env1 -> RVec.ofSeq env1.stack |> ARR)
                s
            |> push' env
        | _ -> ANY.toFN env s |> eval env

    let evale env = eval env >> exec
    let evalr env s = (evale env s).stack

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

    let dups env = ANY.fromStack env.stack |> push' env

    let over = mod2s <| fun x y -> [ x; y; x ]

    // TODO: make sure index is length - i - 1
    // use ANY fns
    let pick env =
        arg1 env
        <| fun i ->
            ANY.modStack' (fun x -> x </ ANY.Lplus'' /> ANY.get i x)
            >> stk env

    let pop = mod1s <| konst []
    let clr = flip stk PVec.empty
    let nip = mod2 <| fun x _ -> x

    // TODO: make sure index is length - i - 1
    // use ANY fns + custom minus''
    let nix env =
        arg1 env
        <| fun i ->
            ANY.modStack'' (RVec.remove (ANY.toI i) >> ARR)
            >> stk env

    let swap = mod2s <| fun x y -> [ y; x ]
    let tuck = mod2s <| fun x y -> [ y; x; y ]
    let rot = mod3s <| fun x y z -> [ y; z; x ]
    let rot_ = mod3s <| fun x y z -> [ z; x; y ]
    let roll = TODO
    let roll_ = TODO

    let dip env =
        arg2 env
        <| fun x y env1 -> evalr env1 y |> stk env1 |> push x

    let neg = mod1 ANY.neg
    let Lplus = mod2 ANY.Lplus
    let Lplus' = mod2 ANY.Lplus'
    let Lplus'' = mod2 ANY.Lplus''
    let Lsub = mod2 ANY.Lsub
    let Lmul = mod2 ANY.Lmul
    let Ldiv = mod2 ANY.Ldiv
    let Lmod = mod2 ANY.Lmod
    let Ldivmod = mod2s <| fun x y -> [ ANY.Ldiv x y; ANY.Lmod x y ]
    let Lpow = mod2 ANY.Lpow

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

    let startARR env =
        { stk env PVec.empty with arr = env.stack :: env.arr }

    let endARR env =
        match env.arr with
        | [] -> env
        | s :: ss ->
            SEQ env.stack
            |> ANY.toARR
            |> push' { stk env s with arr = ss }

    let endMAP env = endARR env |> LMap

    let emptyFN env = UN() |> ANY.toFN env |> push' env
    let emptyARR = UN() |> ANY.toARR |> push

    let wrap = mod1 <| fun x -> RVec.ofSeq [ x ] |> ARR
    let wrap' = mod2 <| fun x y -> RVec.ofSeq [ x; y ] |> ARR

    let wrap'' env =
        stk env PVec.empty
        |> push (ANY.fromStack env.stack)

    let unwrap = mod1s (ANY.toStack >> toList)

    let unwrap' env =
        arg1 env <| fun x _ -> ANY.toStack x |> stk env

    let enum' = mod1 ANY.toInds

    let es env = arg1 env <| flip eval

    let quar env =
        arg1 env
        <| fun x env ->
            evalr env x
            |> PVec.tryLast
            |> Option.defaultValue (UN())
            |> push' env

    let typ = mod1 (ANY.typ >> option STR (NUM 0))

    let Lstr = mod1 ANY.toSTR
    let Lfn env = ANY.toFN env </ mod1 /> env
    let Lnum = mod1 ANY.toNUM
    let Lseq = mod1 ANY.toSEQ
    let Larr = mod1 ANY.toARR
    let LMap = mod1 ANY.toMAP
    let Lbool = mod1 ANY.toBOOL

    let Lun =
        mod1
        <| (NUM
            << function
                | UN _ -> 0
                | _ -> 1)

    let Lnot' = mod1 ANY.not'
    let Lnot = mod1 (ANY.vec1 ANY.not')

    let eln env =
        arg1 env
        <| fun x env ->
            let (_, l), _ = env.code
            eline env (l + ANY.toI x)

    let eprev = NUM -1 |> push >> eln
    let ehere = NUM 0 |> push >> eln
    let enext = NUM 1 |> push >> eln

    let dot env =
        if snd env.code |> length = 0 then
            enext env
        else
            failwith "TODO"

    let SL =
        dict [ "type", typ
               ">S", Lstr
               ">N", Lnum
               ">F", Lfn
               ">A", Larr
               ">M", LMap
               ">Q", Lseq
               ">!", Lbool
               ">?", Lun
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
               "tuck", tuck
               "rot", rot
               "rot_", rot_
               "roll", roll
               "roll_", roll_
               "dip", dip

               "_", neg
               "__", TODO
               "_`", TODO
               "+", Lplus
               "++", Lplus'
               "+`", Lplus''
               "-", Lsub
               "--", TODO
               "-`", TODO
               "*", Lmul
               "**", TODO
               "*`", TODO
               "/", Ldiv
               "//", TODO
               "/`", TODO
               "%", Lmod
               "%%", TODO
               "%`", TODO
               "/%", TODO
               "^", Lpow
               "^^", TODO
               "^`", TODO

               "~", TODO
               "!", Lnot
               "!`", Lnot'
               "&", TODO
               "&&", TODO
               "|", TODO
               "||", TODO
               "$", TODO

               "(", startFN
               ")", id // TODO:?
               "#", es
               "Q", quar
               "@", ehere
               ";", enext
               ";;", eprev
               "e@", eln

               "[", startARR
               "]", endARR
               ",", wrap
               ",,", wrap'
               ",`", wrap''
               ",_", unwrap
               ",,_", unwrap'
               "enum", enum'

               "{", startARR
               "}", endMAP

               "$U", UN() |> push
               "()", emptyFN
               "[]", emptyARR

               ".", dot ]

let exec env =
    match env.code with
    | (_, []) -> env
    | (p, c :: cs) -> lcode env (p, cs) |> execA c |> exec

let execA c env =
    if env.STEP then TODO 0

    if env.STEP || env.VERB then
        pTrace c env false

    match c with
    | NUM _
    | STR _ -> push c env
    | CMD x ->
        if x.StartsWith '\\' && x.Length > 1 then
            drop 1 x |> CMD |> ANY.toFN env |> push' env
        elif x.StartsWith '#' && x.Length > 1 then
            env
        elif env.scope.ContainsKey <| STR x then
            eval env env.scope[c]
        elif LIB.SL.ContainsKey x then
            LIB.SL[x](env)
        else
            ANY.mkE env ERR_UNK_FN x

let run (s, v, i) file lines =
    let env =
        { stack = PVec.empty
          code = ((file, 0), [])
          lines = lines
          scope = PMap.empty
          arr = []
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
