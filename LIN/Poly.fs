module rec LIN.ANY

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
        |> fold (flip <| (<||) replace) x
        |> sprintf "\"%s\""
    | CMD x -> x
    | FN (_, x) ->
        List.map toForm x
        |> String.intercalate " "
        |> fun a -> $"( {a} )"
    | UN _ -> "$U"

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

let toSEQ t =
    match t with
    | SEQ _ -> t
    | ARR x -> RVec.toSeq x |> SEQ
    | MAP x ->
        Seq.map (fun (a, b) -> RVec.ofSeq [ a; b ] |> ARR) x
        |> SEQ
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

let toNUM t =
    match t with
    | NUM _ -> t
    | STR x -> BR.Parse x |> NUM
    | FN _ -> toSTR t |> toNUM
    | UN _ -> NUM 0

let unNUM n =
    let (NUM n) = toNUM n
    n

let toBOOL n =
    let toN b = NUM(if b then 1 else 0)

    toN (
        match n with
        | NUM x -> x <> 0
        | STR x -> x <> ""
        | ARR x -> x <> RVec.empty
        | MAP x -> x <> PMap.empty
        | SEQ x -> x <> Seq.empty
        | FN (_, x) -> x <> []
        | CMD _ -> true
        | UN _ -> false
    )

let toI = unNUM >> int
let fromI (n: int) = BR n |> NUM

let toF = unNUM >> float
let fromF (n: float) = BR n |> NUM

let fromNaN n =
    match n with
    | NUM x when BR.IsNaN x -> UN()
    | _ -> n

let toSTR = string >> STR

let not' t =
    if toBOOL t = NUM 0 then
        NUM 1
    else
        NUM 0

let toInds t =
    match t with
    | MAP _ -> toARR t
    | FN _
    | STR _ -> toARR t |> toInds
    | _ -> mapi (fun i a -> RVec.ofSeq [ i; a ] |> ARR) t

let len t =
    match t with
    | SEQ x -> Seq.length x
    | ARR x -> RVec.length x
    | MAP x -> PMap.length x
    | STR x -> String.length x
    | FN (_, x) -> Seq.length x
    | CMD _ -> toSTR t |> len
    | _ -> 0

let (|Itr|_|) t =
    match t with
    | ARR _
    | MAP _
    | SEQ _ -> Some t
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
    let isIn i = unNUM i < len t

    match t with
    | ARR _
    | SEQ _
    | STR _ when unNUM i < 0 -> get (NUM(unNUM i </ mod' /> BR(len t))) t
    | ARR x when isIn i -> x[toI i]
    | SEQ x when isIn i -> Seq.item (toI i) x
    | STR x when isIn i -> x[toI i] |> string |> STR
    | MAP x when x.ContainsKey i -> x[i]
    | FN _ -> toSEQ t |> get i
    | CMD _ -> toSTR t |> get i
    | _ -> UN()

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

let filter f t =
    match t with
    | SEQ x -> Seq.filter f x |> SEQ
    | ARR x -> RVec.filter f x |> ARR
    | MAP x ->
        PMap.toSeq x
        |> Seq.filter (fun (_, a) -> f a)
        |> PMap.ofSeq
        |> MAP

let zip f t s =
    match t, s with
    | Itr _, Itr _ -> mapi (fun i x -> get i s |> f x) t
    | _ -> table f t s

let table f t s =
    match t, s with
    | Itr _, Itr _ -> map (fun x -> table f x s) t
    | Itr _, _ -> map (flip f s) t
    | _, Itr _ -> map (f t) s
    | _ -> f t s

let vec1 f t =
    match t with
    | Itr _ -> map (vec1 f) t
    | _ -> f t

let vec2 f t s =
    match t, s with
    | Equiv _ -> zip f t s
    | _ -> table f t s

let num1 f =
    vec1 (fun t ->
        match t with
        | NUM x -> f x |> NUM
        | _ -> toNUM t |> num1 f)

let num2 f =
    vec2 (fun t s ->
        match t, s with
        | NUM x, NUM y -> f x y |> NUM
        | _ -> num2 f (toNUM t) (toNUM s))

let neg = num1 <| (~-)

let Lplus = num2 (+)

let Lplus' t s =
    vec2
        (fun t s ->
            match t, s with
            | FN (p, x), FN (_, y) -> FN(p, x ++ y)
            | FN (p, x), y -> FN(p, x ++ [ y ])
            | x, FN (p, y) -> FN(p, [ x ] ++ y)
            | STR x, STR y -> x ++ y |> STR
            | _ -> toSTR t </ Lplus' /> toSTR s)
        t
        s

let Lplus'' t s =
    match t, s with
    | SEQ x, SEQ y -> x ++ y |> SEQ
    | ARR x, ARR y -> RVec.append x y |> ARR
    | MAP x, MAP y ->
        PMap.toSeq y
        |> Seq.fold (fun c (a, b) -> PMap.add a b c) x
        |> MAP
    | SEQ _, x -> t </ Lplus'' /> (toSeq [ x ] |> SEQ)
    | ARR _, x -> t </ Lplus'' /> (RVec.ofSeq [ x ] |> ARR)
    | x, SEQ _ -> toSeq [ x ] |> SEQ </ Lplus'' /> s
    | x, ARR _ -> RVec.ofSeq [ x ] |> ARR </ Lplus'' /> s

let Lsub = num2 (-)

let Lmul = num2 (*)

let Ldiv t = num2 (/) t >> fromNaN

let Lmod = num2 mod'

let Lpow = num2 (fun x y -> BR.Pow(x, y, BR.MaxDigits))
