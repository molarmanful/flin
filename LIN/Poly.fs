module rec LIN.ANY

open FSharpx.Collections
open FSharpPlus
open FSharpPlus.Data
open MathNet.Numerics

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

let toCode f s =
    match s with
    | FN (x, _) -> (x, f)
    | STR x -> (P.parse x, f)
    | CMD _ -> ([ s ], f)
    | _ -> string s |> STR |> toCode f

let map f t =
    match t with
    | ARR x -> Array.map f x |> ARR
    | MAP x -> PersistentHashMap.map f x |> MAP
    | SEQ x -> Seq.map f x |> SEQ
    | _ -> f t

let imap f t =
    match t with
    | ARR _
    | MAP _
    | SEQ _ -> map (imap f) t
    | _ -> f t

let plus t s =
    match t, s with
    | NUM x, NUM y -> x + y |> NUM
    | STR x, STR y -> x + y |> STR
    | NUM x, STR y -> string x + y |> STR
    | STR x, NUM y -> x + string y |> STR
    | FN (x, p), FN (y, _) -> (x ++ y, p) |> FN
    | FN (x, p), y -> (x ++ [ y ], p) |> FN
    | x, FN (y, p) -> ([ x ] ++ y, p) |> FN
