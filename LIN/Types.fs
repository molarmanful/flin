[<AutoOpen>]
module rec LIN.TYPES

open FSharpx.Collections
open FSharpPlus
open MathNet.Numerics

type PVec<'T> = 'T PersistentVector
type PMap<[<EqualityConditionalOn>] 'T, 'S when 'T: equality and 'S: equality> = PersistentHashMap<'T, 'S>

type PATH = string option * int

type ANY =
    | ARR of ANY []
    | MAP of PMap<ANY, ANY>
    | SEQ of ANY seq
    | NUM of BigRational
    | STR of string
    | CMD of string
    | FN of FN
    | UN of unit

    override t.ToString() =
        let join s = map string >> String.intercalate s

        match t with
        | ARR x -> join " " x
        | MAP x ->
            PersistentHashMap.toSeq x
            |> map (fun (a, b) -> $"{a} {b}")
            |> String.intercalate "\n"
        | SEQ x -> toArray x |> ARR |> string
        | NUM x -> BigRational.ToDouble x |> string
        | STR x
        | CMD x -> x
        | FN (x, _) -> toArray x |> ARR |> string
        | UN _ -> ""

type FN = ANY list * PATH

type PT =
    | UN = 0
    | STR = 1
    | NUM = 2
    | CMD = 3
    | ESC = 4
    | DEC = 5

type PST =
    { xs: ANY list
      x: string
      t: PT }


type ENV =
    { stack: ANY PVec
      code: FN
      lines: ANY list
      scope: PMap<ANY, ANY>
      STEP: bool
      VERB: bool
      IMPL: bool }

exception ERR_PARSE of string
exception ERR_ST_LEN of int * PATH
exception ERR_UNK_FN of string * PATH
