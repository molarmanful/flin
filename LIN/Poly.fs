module rec LIN.ANY

open FSharpx.Collections
open FSharpPlus
open FSharpPlus.Data
open Ficus.RRBVector

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

let toCode p s =
    match s with
    | FN (_, x) -> (p, x)
    | STR x -> (p, P.parse x)
    | CMD _ -> (p, [ s ])
    | _ -> string s |> STR |> toCode p

let toFN env = fst env.code |> toCode >> FN
let iFN env i = toCode (fst env.code |> fst, i) >> FN

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
