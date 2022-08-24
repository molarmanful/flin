module rec LIN.ANY

open FSharpx.Collections
open FSharpPlus
open FSharpPlus.Data
open Ficus.RRBVector
open MathNet.Numerics

module PVec = PersistentVector
module PMap = PersistentHashMap
module RVec = RRBVector

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
    | CMD _ -> (p, [ t ])
    | _ -> string t |> STR |> toCode p

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
    | FN (_, x) -> SEQ x
    | UN _ -> SEQ Seq.empty

let toARR t =
    match t with
    | ARR _ -> t
    | SEQ x -> RVec.ofSeq x |> ARR
    | _ -> toSEQ t |> toARR

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
        | CMD _
        | FN _ -> true
        | UN _ -> false
    )

let toInt = unNUM >> BigRational.ToInt32

let toSTR (x: ANY) = string x |> STR

let map f t =
    match t with
    | ARR x -> RVec.map f x |> ARR
    | MAP x -> PMap.map f x |> MAP
    | SEQ x -> Seq.map f x |> SEQ
    | _ -> f t

let neg t =
    match t with
    | NUM x -> NUM -x

let plus t s =
    match t, s with
    | NUM x, NUM y -> x + y |> NUM
    | _, STR _
    | STR _, _
    | _, FN _
    | FN _, _ -> toNUM t </ plus /> toNUM s

let plus' t s =
    match t, s with
    | FN (p, x), FN (_, y) -> FN(p, x ++ y)
    | FN (p, x), y -> FN(p, x ++ [ y ])
    | x, FN (p, y) -> FN(p, [ x ] ++ y)
    | STR x, STR y -> x + y |> STR
    | STR _, _
    | _, STR _
    | NUM _, NUM _ -> toSTR t </ plus' /> toSTR s

let plus'' t s =
    match t, s with
    | SEQ x, SEQ y -> x ++ y |> SEQ
    | ARR x, ARR y -> RVec.append x y |> ARR
    | ARR _, x -> t </ plus'' /> (RVec.ofSeq [ x ] |> ARR)
    | x, ARR _ -> RVec.ofSeq [ x ] |> ARR </ plus'' /> s
    | MAP x, MAP y ->
        PMap.toSeq y
        |> Seq.fold (fun c (a, b) -> PMap.add a b c) x
        |> MAP
