module rec LIN.H

let ftag (s, f) =
    match s with
    | FN (x, _) -> FN(x, f)
    | STR x -> FN(P.parse x, f)
    | CMD _ -> FN([ s ], f)
    | _ -> ftag (string s |> STR, f)
