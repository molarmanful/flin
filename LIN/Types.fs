namespace rec LIN

open FSharpx.Collections
open FSharpPlus
open FSharpPlus.Data
open MathNet.Numerics

type PVec<'T> = 'T PersistentVector
type PMap<[<EqualityConditionalOn>] 'T, 'S when 'T: equality and 'S: equality> = PersistentHashMap<'T, 'S>

type PATH = string option

type ANY =
    | ARR of ANY array
    | OBJ of Map<ANY, ANY>
    | NUM of BigRational
    | STR of string
    | CMD of string
    | FN of ANY list * (PATH * int)
    | UN of unit

    override s.ToString() =
        let join s = map string >> String.intercalate s

        match s with
        | ARR xs -> join " " xs
        | OBJ xs ->
            Map.toList xs
            |> map (sprintf "%A %A" |> (<||))
            |> String.intercalate "\n"
        | NUM x -> BigRational.ToDouble x |> string
        | STR x
        | CMD x -> x
        | FN (xs, _) -> List.toArray xs |> join " "
        | _ -> string s

type ENV =
    { stack: ANY PVec
      file: PATH
      lines: ANY list
      ln: int
      scope: PMap<ANY, ANY> }

exception ERR_PARSE of string
exception ERR_ST_LEN of int
