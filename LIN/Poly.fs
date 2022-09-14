module rec LIN.ANY

#nowarn "3391"

open FSharpx.Collections
open FSharpPlus
open FSharpPlus.Data
open Ficus.RRBVector

module PVec = PersistentVector
module PMap = PersistentHashMap
module RVec = RRBVector

let mkE env e s = e (fst env.code, s) |> raise

let inline mod' x y = (x % y + y) % y

let typ t =
    match t with
    | ARR _ -> Some "arr"
    | MAP _ -> Some "map"
    | SEQ _ -> Some "seq"
    | NUM _ -> Some "num"
    | STR _ -> Some "str"
    | CMD _ -> Some "cmd"
    | FN _ -> Some "fn"
    | _ -> None

let toForm a =
    match a with
    | ARR x ->
        RVec.map toForm x
        |> String.intercalate " "
        |> sprintf "[%s]"
    | MAP x ->
        Seq.map (fun (a, b) -> $"{toForm a}=>{toForm b}") x
        |> String.intercalate " "
        |> sprintf "{%s}"
    | SEQ _ -> "[...]`"
    | NUM x -> string x
    | STR x ->
        [ "\\", "\\\\"; "\"", "\\\"" ]
        |> Seq.fold (flip <| (<||) replace) x
        |> sprintf "\"%s\""
    | CMD x -> x
    | FN ((_, l), x) ->
        let n =
            string l
            |> String.map (fun d ->
                String.tryItem (int d - int '0') "⁰¹²³⁴⁵⁶⁷⁸⁹"
                |> Option.defaultValue d)

        List.map toForm x
        |> String.intercalate " "
        |> fun a -> $"({a}){n}"
    | UN _ -> "UN"

let toCode p t =
    match t with
    | FN (_, x) -> (p, x)
    | STR x -> (p, P.parse x)
    | SEQ x -> (p, toList x)
    | _ -> toSEQ t |> toCode p

let toStack t =
    match t with
    | SEQ x -> PVec.ofSeq x
    | _ -> toSEQ t |> toStack

let fromStack s = RVec.ofSeq s |> ARR
let modStack f = fromStack >> f >> toStack
let modStack' f { stack = st } = modStack f st
let modStack'' f = modStack' <| fun (ARR x) -> f x

let toFN env = fst env.code |> toCode >> FN
let iFN env i = toCode (fst env.code |> fst, i) >> FN

let toTups t =
    match t with
    | MAP x -> PMap.toSeq x
    | SEQ x -> Seq.mapi (fun i a -> (NUM i, a)) x
    | _ -> toSEQ t |> toTups

let toInds t =
    toTups t
    |> Seq.map (fun (i, v) -> lARR [ i; v ])
    |> SEQ

let toSEQ t =
    match t with
    | SEQ _ -> t
    | ARR x -> RVec.toSeq x |> SEQ
    | MAP _ -> toInds t
    | NUM _
    | CMD _ -> SEQ [ t ]
    | STR x -> Seq.map (string >> STR) x |> SEQ
    | FN (_, x) ->
        Seq.map
            (function
            | CMD x -> STR x
            | x -> x)
            x
        |> SEQ
    | UN _ -> SEQ Seq.empty

let toARR t =
    match t with
    | ARR _ -> t
    | SEQ x -> RVec.ofSeq x |> ARR
    | _ -> toSEQ t |> toARR

let toMAP t =
    match t with
    | MAP _ -> t
    | SEQ x ->
        Seq.filter
            (function
            | Itr a when len a > 0 -> true
            | _ -> false)
            x
        |> Seq.map (fun a -> (get (NUM 0) a, get (NUM 1) a))
        |> PMap.ofSeq
        |> MAP
    | ARR _ -> toSEQ t |> toMAP
    | UN _ -> MAP PMap.empty
    | _ -> ERR_cast(t, "map") |> raise

let toNUM t =
    match t with
    | NUM _ -> t
    | STR x -> BR.Parse x |> NUM
    | FN _ -> toSTR t |> toNUM
    | UN _ -> NUM 0
    | _ -> ERR_cast(t, "num") |> raise

let toSTR = string >> STR

let unNUM n =
    let (NUM n) = toNUM n
    n

let unSTR n =
    let (STR n) = toSTR n
    n

let unSEQ n =
    let (SEQ n) = toSEQ n
    n

let unARR n =
    let (ARR n) = toARR n
    n

let unMAP n =
    let (MAP n) = toMAP n
    n

let fromBOOL b = NUM(if b then 1 else 0)

let toBOOL n =
    match n with
    | NUM x -> x <> 0
    | STR x -> x <> ""
    | ARR x -> x <> RVec.empty
    | MAP x -> x <> PMap.empty
    | SEQ x -> x <> Seq.empty
    | FN (_, x) -> x <> []
    | CMD _ -> true
    | UN _ -> false

let tru = toBOOL >> fromBOOL

let toI = unNUM >> int
let fromI (n: int) = BR n |> NUM

let toF = unNUM >> float
let fromF (n: float) = BR n |> NUM

let fromN n = string n |> BR.Parse |> NUM

let fromNaN n =
    match n with
    | NUM x when x = nan -> UN()
    | _ -> n

let not' t =
    if toBOOL t |> not then NUM 1 else NUM 0

let toVar t =
    match t with
    | NUM _ -> t
    | _ -> toSTR t

let lSEQ = toSeq >> SEQ
let lARR = RVec.ofSeq >> ARR

let len t =
    match t with
    | SEQ x -> Seq.length x
    | ARR x -> RVec.length x
    | MAP x -> PMap.length x
    | STR x -> String.length x
    | FN (_, x) -> Seq.length x
    | CMD _ -> toSTR t |> len
    | _ -> 0

let (|It|_|) t =
    match t with
    | ARR _
    | SEQ _ -> Some t
    | _ -> None

let (|Itr|_|) t =
    match t with
    | MAP _
    | It _ -> Some t
    | _ -> None

let (|Equiv|_|) (t, s) =
    if len t = len s
       && (match t, s with
           | ARR _, ARR _
           | SEQ _, SEQ _
           | ARR _, SEQ _
           | SEQ _, ARR _ -> true
           | MAP x, MAP y when
               len t = len t
               && Seq.forall (fun (i, _) -> y.ContainsKey i) x
               ->
               true
           | _ -> false) then
        Some(t, s)
    else
        None

let get i t =
    let g i t =
        match t with
        | SEQ x -> Seq.tryItem (toI i) x |> odef
        | ARR x -> RVec.tryItem (toI i) x |> odef
        | MAP x -> if x.ContainsKey i then x[i] else UN()
        | STR x ->
            String.tryItem (toI i) x
            |> option (string >> STR) (UN())
        | FN _ -> toSEQ t |> get i
        | CMD _ -> toSTR t |> get i
        | _ -> UN()

    match t with
    | ARR _
    | SEQ _
    | STR _ when unNUM i < 0 -> g (fromI (toI i + len t)) t
    | _ -> g i t

let mget i t =
    if PMap.containsKey i t then
        t[i]
    else
        UN()

let mapi f t =
    let f' = fromI >> f

    match t with
    | SEQ x -> Seq.mapi f' x |> SEQ
    | ARR x -> RVec.mapi f' x |> ARR
    | MAP x ->
        PMap.toSeq x
        |> Seq.map (fun (i, a) -> (i, f i a))
        |> PMap.ofSeq
        |> MAP
    | _ -> f (UN()) t

let map f t =
    match t with
    | Itr _ -> mapi (konst f) t
    | _ -> f t

let flat t =
    match t with
    | SEQ x -> Seq.collect unSEQ x |> SEQ
    | ARR x -> RVec.collect unARR x |> ARR
    | MAP x -> PMap.toSeq x |> Seq.map snd |> SEQ |> flat
    | _ -> t

let fold f a t =
    match t with
    | SEQ x -> Seq.fold f a x
    | ARR x -> RVec.fold f a x
    | MAP x -> PMap.toSeq x |> Seq.fold (fun p (_, q) -> f p q) a
    | _ -> f a t

let scan f a t =
    let sc f a = Seq.scan f a >> Seq.tail

    match t with
    | SEQ x -> sc f a x |> SEQ
    | ARR x -> RVec.scan f a x |> RVec.tail |> ARR
    | MAP x ->
        PMap.toSeq x
        |> sc (fun (_, p) (k, q) -> (k, f p q)) (UN(), a)
        |> PMap.ofSeq
        |> MAP
    | _ -> f a t

let unfold f = Seq.unfold f >> SEQ

let dep t =
    match t with
    | Itr _ -> 1 + fold (fun a b -> max a <| dep b) 0 t
    | _ -> 0

let keys = toTups >> Seq.map fst >> SEQ
let vals = toTups >> Seq.map snd >> SEQ

let rKV kv walk t =
    let rec m i t =
        seq {
            for k, v in toTups t do
                let i = Lplus'' i k
                let x = if kv then v else i

                if walk then yield x

                yield!
                    match v with
                    | Itr _ -> m i v
                    | _ -> if walk then seq [] else seq [ x ]
        }

    m (lARR []) t |> SEQ

let rmap f t =
    let rec m f i t =
        match t with
        | Itr _ -> mapi (fun j x -> m f (Lplus'' i j) x) t
        | _ -> f i t

    m f (lARR []) t

let dmap f d t =
    let rec m f i t d =
        if d <= 0 then
            f i t
        else
            match t with
            | Itr _ -> mapi (fun j x -> m f (Lplus'' i j) x (d - 1)) t
            | _ -> f i t

    if d < 0 then
        m f (lARR []) t (d + dep t)
    elif d = 0 then
        rmap f t
    else
        m f (lARR []) t d

let walk f t =
    let rec m f i =
        mapi (fun j x ->
            let i = (Lplus'' i j)

            match f i x with
            | Itr _ -> m f i x
            | a -> a)

    m f (lARR []) t

let filter f t =
    let f = f >> toBOOL

    match t with
    | SEQ x -> Seq.filter f x |> SEQ
    | ARR x -> RVec.filter f x |> ARR
    | MAP x ->
        PMap.toSeq x
        |> Seq.filter (fun (_, a) -> f a)
        |> PMap.ofSeq
        |> MAP
    | _ -> f t |> fromBOOL

let zip f t s =
    match t, s with
    | Itr _, Itr _ -> mapi (fun i x -> get i s |> f x) t
    | _ -> table f t s

let sZip f t s =
    match t, s with
    | MAP x, Itr _ ->
        PMap.toSeq x
        |> Seq.fold
            (fun a (k, v) ->
                match get k s with
                | UN _ -> a
                | w -> PMap.add k (f v w) a)
            PMap.empty
        |> MAP
    | Itr _, MAP _ -> sZip (flip f) s t
    | _, MAP _ -> sZip f (toSEQ t) s
    | MAP _, _ -> sZip f t (toSEQ s)
    | Itr _, Itr _ ->
        Seq.zip (unSEQ t) (unSEQ s)
        |> Seq.map ((<||) f)
        |> SEQ
    | _ -> f t s

let table f t s =
    match t, s with
    | Itr _, Itr _ -> map (fun x -> table f x s) t
    | Itr _, _ -> map (flip f s) t
    | _, Itr _ -> map (f t) s
    | _ -> f t s

let isUN t =
    match t with
    | UN _ -> true
    | _ -> false

let isEmpty t =
    match t with
    | SEQ x -> Seq.isEmpty x
    | ARR x -> RVec.isEmpty x
    | MAP x -> x = PMap.empty
    | STR x -> x = ""
    | FN (_, x) -> Seq.isEmpty x
    | UN _ -> true
    | _ -> false

let tk_ n t =
    match t with
    | SEQ x -> Seq.truncate (toI n) x |> SEQ
    | ARR x -> RVec.truncate (toI n) x |> ARR
    | _ -> toSEQ t |> tk_ n

let dp_ n t =
    match t with
    | SEQ x ->
        let rec d n xs =
            if Seq.isEmpty xs || n <= 0 then
                xs
            else
                d (n - 1) (Seq.tail xs)

        d (toI n) x |> SEQ
    | ARR x ->
        let rec d n xs =
            if RVec.isEmpty xs || n <= 0 then
                xs
            else
                d (n - 1) (RVec.tail xs)

        d (toI n) x |> ARR
    | _ -> toSEQ t |> dp_ n

let tk n t =
    n
    |> vec1 (fun x ->
        let y = toI x

        if y < 0 then
            dp_ (fromI (y + len t)) t
        else
            tk_ x t)

let dp n t =
    n
    |> vec1 (fun x ->
        let y = toI x

        if y < 0 then
            tk_ (fromI (y + len t)) t
        else
            dp_ x t)

let vec1 f t =
    match t with
    | Itr _ -> map (vec1 f) t
    | _ -> f t

let vec2 f t s =
    match t, s with
    | Equiv _ -> zip f t s
    | _ -> table f t s

let vef1 f a t =
    match t with
    | Itr _ -> fold (fun x y -> vef1 f x y) a t
    | _ -> f a t

let num1 f = vec1 (unNUM >> f >> NUM)
let str1 f = vec1 (unSTR >> f >> STR)

let num2 f =
    vec2 <| fun t s -> f (unNUM t) (unNUM s) |> NUM

let str2 f =
    vec2 <| fun t s -> f (unSTR t) (unSTR s) |> STR

let strnum2 f =
    vec2 <| fun t s -> f (unSTR t) (unNUM s) |> STR

let has t s =
    match s with
    | SEQ x -> Seq.contains t x
    | ARR x -> RVec.contains t x
    | MAP x -> PMap.toSeq x |> Seq.exists (snd >> (=) t)
    | STR x -> String.isSubString (string t) x
    | _ -> has (toSTR t) s

let eq t s =
    match t, s with
    | FN (_, x), FN (_, y) -> x = y
    | _ -> t = s

let neg = num1 (~-)
let neg' = str1 String.rev

let neg'' t =
    match t with
    | SEQ x -> Seq.rev x |> SEQ
    | ARR x -> RVec.rev x |> ARR
    | MAP x -> PMap.toSeq x |> Seq.rev |> PMap.ofSeq |> MAP
    | FN (p, x) -> FN(p, List.rev x)
    | _ -> neg' t

let Lplus = num2 (+)
let Lplus' = str2 (++)

let Lplus'' t s =
    match t, s with
    | SEQ x, SEQ y -> x ++ y |> SEQ
    | ARR x, ARR y -> RVec.append x y |> ARR
    | MAP x, MAP y ->
        PMap.toSeq y
        |> Seq.fold (fun c (a, b) -> PMap.add a b c) x
        |> MAP
    | FN (p, x), FN (_, y) -> FN(p, x ++ y)
    | SEQ _, _ -> Lplus'' t (lSEQ [ s ])
    | _, SEQ _ -> Lplus'' (lSEQ [ t ]) s
    | ARR _, _ -> Lplus'' t (lARR [ s ])
    | _, ARR _ -> Lplus'' (lARR [ t ]) s
    | FN (p, x), _ -> FN(p, x ++ [ s ])
    | _, FN (p, x) -> FN(p, [ s ] ++ x)
    | _ -> Lplus' t s

let Lsub = num2 (-)
let Lsub' = str2 <| fun x y -> String.replace y "" x

let Lsub'' t s =
    match t, s with
    | Itr _, Itr _ -> filter (flip has s >> not >> fromBOOL) t
    | Itr _, _ -> filter (eq s >> not >> fromBOOL) t
    | _ -> Lsub' t s

let Lmul = num2 (*)
let Lmul' = strnum2 <| fun x y -> String.replicate (int y) x

let Lmul'' t s =
    let rep = toI >> Seq.replicate
    let arep = toI >> RVec.replicate

    match t, s with
    | Itr _, Itr _ ->
        s
        |> mapi (fun i y ->
            SEQ(
                match get i t, y with
                | UN _, _ -> Seq.empty
                | Itr x, Itr _ -> Lmul'' x y |> Seq.singleton
                | x, _ -> rep y x
            ))
        |> flat
    | ARR _, _ -> arep s t |> ARR |> flat
    | _ -> rep s t |> SEQ |> flat

let Ldiv t = num2 (/) t >> fromNaN

let Lmod = num2 mod'

let Lpow = num2 <| fun x y -> BR.Pow(x, y, BR.MaxDigits)

let bNOT = num1 <| fun x -> -1L - x

let trunc = num1 BR.Truncate
let floor = num1 BR.Floor
let round = num1 BR.Round
let ceil = num1 BR.Ceiling

let range x y =
    (x, y)
    ||> vec2 (fun x y ->
        let x = unNUM x
        let y = unNUM y

        x
        |> Seq.unfold (fun s ->
            if s <> y then
                Some(s, (if x > y then (-) else (+)) s (BR 1))
            else
                None)
        |> Seq.map NUM
        |> SEQ)

let odef = Option.defaultValue <| UN()
