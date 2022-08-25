module rec LIN.ANY

open FSharpx.Collections
open FSharpPlus
open FSharpPlus.Data
open Ficus.RRBVector
open MathNet.Numerics

module PVec = PersistentVector
module PMap = PersistentHashMap
module RVec = RRBVector

let mkE env e s = e (fst env.code, s) |> raise
let mod' a b = (a % b + b) % b

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
        |> Seq.map (fun a -> (NUM 0N </ get /> a, NUM 1N </ get /> a))
        |> PMap.ofSeq
        |> MAP
    | ARR _ -> toSEQ t |> toMAP
    | UN _ -> MAP PMap.empty

let toNUM t =
    match t with
    | NUM _ -> t
    | STR x ->
        let [ n1; n2 ] = split [ "." ] x
        let d = 10N ** length n2 |> string
        BigRational.Parse $"{n1}{n2}/{d}" |> NUM
    | FN _ -> toSTR t |> toNUM
    | UN _ -> NUM 0N

let unNUM n =
    let (NUM n) = toNUM n
    n

let toBOOL n =
    let toN b = NUM(if b then 1N else 0N)

    toN (
        match n with
        | NUM x -> x <> 0N
        | STR x -> x <> ""
        | ARR x -> x <> RVec.empty
        | MAP x -> x <> PMap.empty
        | SEQ x -> x <> Seq.empty
        | FN (_, x) -> x <> []
        | CMD _ -> true
        | UN _ -> false
    )

let toInt = unNUM >> BigRational.ToInt32
let fromInt = BigRational.FromInt >> NUM

let toSTR = string >> STR

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
    let isIn i = toInt i < len t

    match t with
    | ARR _
    | SEQ _
    | STR _ when toInt i < 0 -> fromInt (toInt i </ mod' /> len t) </ get /> t
    | ARR x when isIn i -> x[toInt i]
    | SEQ x when isIn i -> toInt i </ Seq.item /> x
    | STR x when isIn i -> x[toInt i] |> string |> STR
    | MAP x when x.ContainsKey i -> x[i]
    | FN _ -> toSEQ t |> get i
    | CMD _ -> toSTR t |> get i
    | _ -> UN()

let mapi f t =
    let f' = fromInt >> f

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

let neg t =
    match t with
    | NUM x -> NUM -x
    | _ -> toNUM t |> neg

let plus t s =
    vec2
        (fun t s ->
            match t, s with
            | NUM x, NUM y -> x + y |> NUM
            | _ -> toNUM t </ plus /> toNUM s)
        t
        s

let plus' t s =
    vec2
        (fun t s ->
            match t, s with
            | FN (p, x), FN (_, y) -> FN(p, x ++ y)
            | FN (p, x), y -> FN(p, x ++ [ y ])
            | x, FN (p, y) -> FN(p, [ x ] ++ y)
            | STR x, STR y -> x + y |> STR
            | _ -> toSTR t </ plus' /> toSTR s)
        t
        s

let plus'' t s =
    match t, s with
    | SEQ x, SEQ y -> x ++ y |> SEQ
    | ARR x, ARR y -> RVec.append x y |> ARR
    | MAP x, MAP y ->
        PMap.toSeq y
        |> Seq.fold (fun c (a, b) -> PMap.add a b c) x
        |> MAP
    | SEQ _, x -> t </ plus'' /> (toSeq [ x ] |> SEQ)
    | ARR _, x -> t </ plus'' /> (RVec.ofSeq [ x ] |> ARR)
    | x, SEQ _ -> toSeq [ x ] |> SEQ </ plus'' /> s
    | x, ARR _ -> RVec.ofSeq [ x ] |> ARR </ plus'' /> s

let not' t =
    match toBOOL t with
    | NUM z when z = 0N -> NUM 1N
    | _ -> NUM 0N

let toInds t =
    match t with
    | MAP _ -> toARR t
    | FN _
    | STR _ -> toARR t |> toInds
    | _ -> mapi (fun i a -> RVec.ofSeq [ i; a ] |> ARR) t
