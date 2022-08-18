module LIN.H

#if INTERACTIVE
#r "nuget: FSharpPlus"
#r "nuget: MathNet.Numerics.FSharp"
#endif

open FSharpPlus
open FSharpPlus.Data

let join s xs = map string xs |> String.intercalate s

let rec ftag (s, f) =
    match s with
    | FN (x, _) -> FN(x, f)
    | STR x -> FN(P.parse x, f)
    | CMD _ -> FN([ s ], f)
    | _ -> ftag (string s |> STR, f)

let push x env =
    { env with stack = DList.add x env.stack }

let pop env =
    { env with stack = take (length env.stack - 1) env.stack }
