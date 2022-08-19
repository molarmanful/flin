namespace LIN

[<AutoOpen>]
module TYPES =
    open FSharpx.Collections
    open FSharpPlus
    open FSharpPlus.Data
    open MathNet.Numerics

    type PVec<'T> = 'T PersistentVector
    type PMap<[<EqualityConditionalOn>] 'T, 'S when 'T: equality and 'S: equality> = PersistentHashMap<'T, 'S>

    type PATH = string option * int

    type ANY =
        | ARR of ANY []
        | MAP of PMap<ANY, ANY>
        | NUM of BigRational
        | STR of string
        | CMD of string
        | FN of ANY list * PATH
        | UN of unit

        override t.ToString() =
            let join s = map string >> String.intercalate s

            match t with
            | ARR xs -> join " " xs
            | MAP xs ->
                PersistentHashMap.toSeq xs
                |> map (fun (a, b) -> $"{a} {b}")
                |> String.intercalate "\n"
            | NUM x -> BigRational.ToDouble x |> string
            | STR x
            | CMD x -> x
            | FN (xs, _) -> List.toArray xs |> join " "
            | _ -> string t

    type ENV =
        { stack: ANY PVec
          code: ANY
          lines: ANY list
          scope: PMap<ANY, ANY>
          STEP: bool
          VERB: bool
          IMPL: bool }

    exception ERR_PARSE of string
    exception ERR_ST_LEN of int
    exception ERR_UNK_FN of string
