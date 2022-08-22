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

let toFN env = fst env.code |> toCode >> FN
let iFN env i = toCode (fst env.code |> fst, i) >> FN

let toARR t =
    match t with
    | ARR _ -> t
    | MAP x ->
        RVec.ofSeq x
        |> RVec.map (fun (a, b) -> RVec.ofSeq [ a; b ] |> ARR)
        |> ARR
    | SEQ x -> RVec.ofSeq x |> ARR
    | STR x -> RVec.ofSeq x |> RVec.map (string >> STR) |> ARR
    | NUM x ->
        RVec.ofSeq [ x.Numerator
                     x.Denominator ]
        |> RVec.map (BigRational.FromBigInt >> NUM)
        |> ARR
    | FN (_, x) -> RVec.ofSeq x |> ARR
    | UN _ -> ARR RVec.empty

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
    | FN (p, x), FN (_, y) -> FN(p, x ++ y)
    | FN (p, x), y -> FN(p, x ++ [ y ])
    | x, FN (p, y) -> FN(p, [ x ] ++ y)
    | STR x, STR y -> x + y |> STR
    | x, STR y -> string x + y |> STR
    | STR x, y -> x + string y |> STR
    | NUM x, NUM y -> x + y |> NUM

let plus' t s =
    match t, s with
    | ARR x, ARR y -> RVec.append x y |> ARR
    | MAP x, MAP y ->
        PMap.toSeq y
        |> Seq.fold (fun c (a, b) -> PMap.add a b c) x
        |> MAP
    | SEQ x, SEQ y -> x ++ y |> SEQ
