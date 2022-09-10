module rec LIN.ENV

#nowarn "3391"

open System.IO
open Spectre.Console
open FSharpx.Collections
open FSharpPlus
open Ficus.RRBVector
open MathNet.Numerics.Random

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

        let p () =
            PVec.map (ANY.toForm >> printfn "%s") env.stack
            |> ignore

        if d <> 1 then p ()

        pprint "[dim grey]———>[/]"

        if length fcs > 7 then
            let cs = take 7 fcs |> String.intercalate " "
            ppprint $"[bold yellow]{c}[/] [grey]{cs} …[/]"
        elif length fcs = 0 then
            ppprint (
                if d > 0 then
                    $"[dim green](END)[/]"
                else
                    $"[bold yellow]{c}[/]"
            )
        else
            let cs = fcs |> String.intercalate " "
            ppprint $"[bold yellow]{c}[/] [grey]{cs}[/]"

        ppprint $"[dim grey]———({f}:{l})[/]"

        if d > 0 then p ()

    let errStr e =
        match e with
        | ERR_PARSE x -> $"bad syntax \"{x}\""
        | ERR_ST_LEN ((f, l), x) -> $"stack length < {x} @ {f}:{l}"
        | ERR_UNK_FN ((f, l), x) -> $"""unknown fn "{x}" @ {f}:{l}"""
        | ERR_MATCH ((f, l), (x, y)) -> $"no match {ANY.toForm x} -> {ANY.toForm y} @ {f}:{l}"
        | ERR_CAST ((f, l), (x, y)) -> $"bad cast {ANY.toForm x} -> {y} @ {f}:{l}"
        | ERR_ZIP ((f, l), (x, y)) -> $"bad zip {ANY.toForm x} -> {ANY.toForm y} @ {f}:{l}"
        | _ -> $"(other) {e.Message}"

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

    let evale env = code env [] |> eval >> exec
    let evalr env s = (evale env s).stack

    let evalq env = evalr env >> PVec.tryLast >> ANY.odef

    let evalS env f = stk env >> flip evalq f
    let evals env f = PVec.ofSeq >> evalS env f
    let eval1 env f x = evals env f [ x ]
    let eval2 env f x y = evals env f [ x; y ]
    let eval3 env f x y z = evals env f [ x; y; z ]

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

    let wrapFN y (FN (p, cs)) =
        let w = y = CMD ""
        let a = if w then [] else [ CMD "(" ]
        let b = if w then [] else [ CMD ")"; y ]
        FN(p, a ++ cs ++ b)

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

    let inp env =
        let s = System.ReadLine.Read("")
        push (STR s) env

    let inh env =
        let s = System.ReadLine.ReadPassword("")
        push (STR s) env

    let show = form >> outn

    let dup = mod1s <| fun x -> [ x; x ]

    let dups env = ANY.fromStack env.stack |> push' env

    let over = mod2s <| fun x y -> [ x; y; x ]

    let pick env =
        arg1 env
        <| fun i ->
            ANY.modStack' (fun x -> ANY.get (ANY.bNOT i) x |> ANY.Lplus'' x)
            >> stk env

    let pop = mod1s <| konst []
    let clr = flip stk PVec.empty
    let nip = mod2 <| fun x _ -> x

    let nix env =
        arg1 env
        <| fun i ->
            ANY.modStack'' (ANY.bNOT i |> ANY.toI |> RVec.remove >> ARR)
            >> stk env

    let swap = mod2s <| fun x y -> [ y; x ]
    let tuck = mod2s <| fun x y -> [ y; x; y ]
    let rot = mod3s <| fun x y z -> [ y; z; x ]
    let rot_ = mod3s <| fun x y z -> [ z; x; y ]
    let roll = TODO
    let roll_ = TODO

    let dip env =
        arg2 env
        <| fun x y env -> evalr env y |> stk env |> push x

    let neg = mod1 ANY.neg
    let neg' = mod1 ANY.neg'
    let Lplus = mod2 ANY.Lplus
    let Lplus' = mod2 ANY.Lplus'
    let Lplus'' = mod2 ANY.Lplus''
    let Lsub = mod2 ANY.Lsub
    let Lsub' = mod2 ANY.Lsub'
    let Lsub'' = mod2 ANY.Lsub''
    let Lmul = mod2 ANY.Lmul
    let Lmul' = mod2 ANY.Lmul'
    let Lmul'' = mod2 ANY.Lmul''
    let Ldiv = mod2 ANY.Ldiv
    let Lmod = mod2 ANY.Lmod
    let Ldivmod = mod2s <| fun x y -> [ ANY.Ldiv x y; ANY.Lmod x y ]
    let Lpow = mod2 ANY.Lpow

    let trunc = mod1 ANY.trunc
    let floor = mod1 ANY.floor
    let round = mod1 ANY.round
    let ceil = mod1 ANY.ceil

    let startFN env =
        let rec fn env i res =
            match i, env.code with
            | 0, (l, _) ->
                let c =
                    tryLast res
                    |> Option.defaultValue (CMD ")")
                    |> fun (CMD n) -> n
                    |> String.split [ ")" ]
                    |> rev
                    |> head
                    |> CMD

                let cs = limit (length res - 1) res
                FN(l, cs) |> wrapFN c |> push' env
            | _, (l, []) -> FN(l, res) |> push' env
            | _, (_, c :: cs) ->
                res ++ [ c ]
                |> fn
                    (code env cs)
                    (match c with
                     | CMD x when x.Contains "(" -> i + 1
                     | CMD x when x.Contains ")" -> i - 1
                     | _ -> i)

        fn env 1 []

    let wrFN env =
        wrap' env |> mod1 (fun x -> ANY.toFN env x)

    let pairFN env =
        wrap env |> mod1 (fun x -> ANY.toFN env x)

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
    let endSEQ env = endARR env |> Lseq

    let endIf env =
        arg2 (endMAP env)
        <| fun x y env ->
            let a =
                ANY.unMAP y
                |> Seq.tryFind (fun (f, _) -> eval1 env f x |> ANY.toBOOL)

            match a with
            | Some (_, g) -> eval env g
            | _ -> ANY.mkE env ERR_MATCH (x, y)

    let emptyFN env = UN() |> ANY.toFN env |> push' env
    let emptyARR = UN() |> ANY.toARR |> push

    let wrap = mod2 <| fun x y -> ANY.lARR [ x; y ]
    let wrap' = mod1 <| fun x -> ANY.lARR [ x ]

    let wrap'' env =
        stk env PVec.empty
        |> push (ANY.fromStack env.stack)

    let unwrap = mod1s (ANY.toStack >> toList)

    let unwrap' env =
        arg1 env <| fun x _ -> ANY.toStack x |> stk env

    let enum' = mod1 ANY.toInds

    let keys = mod1 ANY.keys
    let vals = mod1 ANY.vals
    let rkeys = mod1 <| ANY.rKV false false
    let rvals = mod1 <| ANY.rKV true false
    let wkeys = mod1 <| ANY.rKV false true
    let wvals = mod1 <| ANY.rKV true true

    let len = mod1 (ANY.len >> ANY.fromI)
    let dep = mod1 (ANY.dep >> ANY.fromI)

    let es env = arg1 env <| flip eval

    let eand env =
        arg2 env
        <| fun x f env -> if ANY.toBOOL x then eval env f else env

    let eor env =
        arg2 env
        <| fun x f env -> if ANY.toBOOL x then env else eval env f

    let eif env =
        arg3 env
        <| fun x f g env -> eval env (if ANY.toBOOL x then f else g)

    let etimes env =
        arg2 env
        <| fun f x env ->
            let f = ANY.toFN env f
            let x = ANY.unNUM x

            if x > 0 then
                evale env f |> pushs [ f; NUM(x - 1L) ] |> etimes
            else
                env

    let quar env =
        arg1 env <| fun x env -> evalq env x |> push' env

    let typ = mod1 (ANY.typ >> option STR (NUM 0))

    let Lstr = mod1 ANY.toSTR
    let Lfn env = mod1 (ANY.toFN env) env

    let Lnum env =
        mod1
            (fun x ->
                try
                    ANY.toNUM x
                with
                | ERR_cast (a, b) -> ANY.mkE env ERR_CAST (a, b)
                | e -> raise e)
            env

    let Lseq = mod1 ANY.toSEQ
    let Larr = mod1 ANY.toARR

    let LMap env =
        mod1
            (fun x ->
                try
                    ANY.toMAP x
                with
                | ERR_cast (a, b) -> ANY.mkE env ERR_CAST (a, b)
                | e -> raise e)
            env

    let Lbool = mod1 ANY.tru

    let Lun =
        mod1
        <| (NUM
            << function
                | UN _ -> 0
                | _ -> 1)

    let nform =
        mod2
        <| fun x y -> (ANY.unNUM x).ToString(ANY.unSTR y) |> STR

    let nstd = push (STR "L15") >> nform

    let Lnot = mod1 <| ANY.vec1 ANY.not'
    let Lnot' = mod1 ANY.not'

    let eq =
        mod2
        <| ANY.vec2 (fun x y -> ANY.eq x y |> ANY.fromBOOL)

    let eq' = mod2 <| fun x y -> ANY.eq x y |> ANY.fromBOOL

    let neq = eq >> Lnot
    let neq' = eq' >> Lnot'

    let gln env =
        mod1
            (ANY.vec1
             <| fun x ->
                 let (f, _), _ = env.code
                 env.lines[f, ANY.toI x])
            env

    let eln env =
        arg1 env <| fun x env -> ANY.toI x |> eline env

    let gcurl env =
        let (_, l), _ = env.code
        BR l |> NUM |> push' env

    let gcurf env =
        let (f, _), _ = env.code
        STR f |> push' env

    let grel = gcurl >> Lplus >> gln
    let erel = gcurl >> Lplus >> eln
    let gprev = NUM -1 |> push >> grel
    let eprev = NUM -1 |> push >> erel
    let ghere = NUM 0 |> push >> grel
    let ehere = NUM 0 |> push >> erel
    let gnext = NUM 1 |> push >> grel
    let enext = NUM 1 |> push >> erel

    let eStArr env =
        arg1 env
        <| fun x -> wrap'' >> push x >> quar >> unwrap'

    let eArrSt env =
        arg1 env
        <| fun x -> unwrap' >> flip evale x >> wrap''

    let rng env =
        env.rng.NextFullRangeInt64()
        |> ANY.fromN
        |> push' env

    let getS env =
        mod1
            (ANY.vec1
             <| fun x ->
                 let x = ANY.toVar x

                 if env.scope.ContainsKey x then
                     env.scope[x]
                 else
                     UN())
            env

    let setS env =
        arg2 env
        <| fun x y env ->
            ANY.vef1
                (fun e a ->
                    let a = ANY.toVar a
                    { e with scope = e.scope.Add(a, x) })
                env
                y

    let modS env =
        arg2 env
        <| fun x y env ->
            ANY.vef1
                (fun e a ->
                    let a = ANY.toVar a
                    let res = push a e |> getS |> flip evalq x
                    { e with scope = e.scope.Add(a, res) })
                env
                y

    let dot env =
        match snd env.code with
        | [] -> enext env
        | c :: cs ->
            let env = code env cs

            match c with
            | _ -> push c env |> getS

    let Lmap env =
        mod2 (fun x f -> ANY.vec1 (fun f -> ANY.map (eval1 env f) x) f) env

    let Lzip env =
        mod3
            (fun x y f ->
                ANY.vec1
                    (fun f ->
                        try ANY.sZip (eval2 env f) x y
                        with
                        | ERR_zip (a, b) -> ANY.mkE env ERR_ZIP (a, b)
                        | e -> raise e) f)
            env

    let tbl env =
        mod3 (fun x y f -> ANY.vec1 (fun f -> ANY.table (eval2 env f) x y) f) env

    let Lfold env =
        mod3 (fun x a f -> ANY.vec1 (fun f -> ANY.fold (eval2 env f) a x) f) env

    let fltr env =
        mod2 (fun x f -> ANY.vec1 (fun f -> ANY.filter (eval1 env f) x) f) env

    let rmap env =
        mod2 (fun x f -> ANY.vec1 (fun f -> ANY.rmap (eval2 env f) x) f) env

    let dmap env =
        mod3 (fun x f d -> ANY.vec2 (fun f d -> ANY.dmap (eval2 env f) (ANY.toI d) x) f d) env

    let walk env =
        mod2 (fun x f -> ANY.vec1 (fun f -> ANY.walk (eval2 env f) x) f) env

    let SL =
        dict [ "type", typ
               ">S", Lstr
               ">N", Lnum
               "N>f", nform
               "N>s", nstd
               ">F", Lfn
               ">A", Larr
               ">M", LMap
               ">Q", Lseq
               ">!", Lbool
               ">?", Lun
               "form", form

               ":", getS
               "=:", setS
               ">:", modS

               "I>", inp
               "I>_", inh
               ">O", out
               "n>O", outn
               "f>O", show

               "dup", dup
               "dups", dups
               "over", over
               "pick", pick
               "pop", pop
               "clr", clr
               "nip", nip
               "nix", nix
               "swap", swap
               "tuck", tuck
               "rot", rot
               "rot_", rot_
               "roll", roll
               "roll_", roll_
               "dip", dip

               "_", neg
               "__", neg'
               "_`", TODO
               "+", Lplus
               "++", Lplus'
               "+`", Lplus''
               "-", Lsub
               "--", Lsub'
               "-`", Lsub''
               "*", Lmul
               "**", Lmul'
               "*`", Lmul''
               "/", Ldiv
               "//", TODO
               "/`", TODO
               "%", Lmod
               "%%", TODO
               "%`", TODO
               "/%", Ldivmod
               "^", Lpow
               "^^", TODO
               "^`", TODO

               "I", trunc
               "|_", floor
               "|-", round
               "|^", ceil

               "!", Lnot
               "!`", Lnot'
               "&", TODO
               "&&", TODO
               "&`", TODO
               "|", TODO
               "||", TODO
               "|`", TODO
               "=", eq
               "=`", eq'
               "!=", neq
               "!=`", neq'
               "<", TODO
               ">", TODO
               "<=", TODO
               ">=", TODO
               "<=>", TODO

               "(", startFN
               ")", id // TODO:?
               "\\", wrFN
               "#", es
               "&#", eand
               "|#", eor
               "?#", eif
               "}?", endIf
               "*#", etimes
               "Q", quar
               "@", ehere
               ";", enext
               ";;", eprev
               "@@", eln
               "@~", erel
               "g@", ghere
               "g;", gnext
               "g;;", gprev
               "g@@", gln
               "g@~", grel
               "$", eStArr
               "'", eArrSt

               "[", startARR
               "]", endARR
               ",", wrap
               ",,", wrap'
               ",`", wrap''
               ",_", unwrap
               ",,_", unwrap'
               ">kv", enum'
               ">k", keys
               ">v", vals
               "r>k", rkeys
               "r>v", rvals
               "w>k", wkeys
               "w>v", wvals
               "len", len
               "dep", dep
               "tk", TODO
               "dp", TODO

               "map", Lmap
               "zip", Lzip
               "tbl", tbl
               "fold", Lfold
               "foldr", TODO
               "scan", TODO
               "scanr", TODO
               "fltr", fltr

               "rmap", rmap
               "dmap", dmap
               "walk", walk

               "{", startARR
               "}", endMAP

               "]`", endSEQ

               "UN", UN() |> push
               "OO", NUM infinity |> push
               "$L", gcurl
               "$F", gcurf
               "$R", rng
               "()", emptyFN
               "[]", emptyARR

               ".", dot ]

let exec env =
    match env.code with
    | (_, []) -> env
    | (p, c :: cs) -> lcode env (p, cs) |> execA c |> exec

let execA c env =
    if env.STEP || env.VERB then
        if env.STEP then AnsiConsole.Clear()
        pTrace c env 0

        if env.STEP then
            AnsiConsole.Markup "[dim]ENTER to continue...[/]"
            System.Console.ReadLine() |> ignore

    match c with
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
    | _ -> push c env

let run (s, v, i) file lines =
    let env =
        { stack = PVec.empty
          code = ((file, 0), [])
          lines = lines
          scope = PMap.empty
          arr = []
          rng = Random.shared
          STEP = s
          VERB = v
          IMPL = i }

    eline env 0
    |> exec
    |> tap (fun env ->
        if env.STEP || env.IMPL then
            pTrace (UN()) env 1
        elif env.VERB then
            pTrace (UN()) env 2)

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
