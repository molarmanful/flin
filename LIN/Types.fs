[<AutoOpen>]
module rec LIN.TYPES

open FSharpx.Collections
open FSharpPlus
open Ficus.RRBVector
open System.Numerics

module RVec = RRBVector
module PVec = PersistentVector
module PMap = PersistentHashMap

type RVec<'T> = RRBVector<'T>
type PVec<'T> = PersistentVector<'T>
type PMap<[<EqualityConditionalOn>] 'T, 'S when 'T: equality and 'S: equality> = PersistentHashMap<'T, 'S>
type BR = BigRational

type PATH = string * int

type ANY =
    | ARR of RVec<ANY>
    | MAP of PMap<ANY, ANY>
    | SEQ of ANY seq
    | NUM of BR
    | STR of string
    | CMD of string
    | FN of FN
    | UN of unit

    override t.ToString() =
        match t with
        | ARR x -> RVec.map string x |> String.intercalate " "
        | MAP x ->
            Seq.map (fun (a, b) -> $"{a} {b}") x
            |> String.intercalate "\n"
        | SEQ x -> map string x |> String.intercalate ""
        | NUM x -> x.ToString "R"
        | STR x
        | CMD x -> x
        | FN (_, x) -> map string x |> String.intercalate " "
        | UN _ -> ""

type FN = PATH * ANY list

type PT =
    | UN = 0
    | STR = 1
    | NUM = 2
    | CMD = 3
    | ESC = 4
    | DEC = 5

type PST = { xs: ANY list; x: string; t: PT }

type ENV =
    { stack: PVec<ANY>
      code: FN
      lines: PMap<PATH, ANY>
      scope: PMap<ANY, ANY>
      arr: PVec<ANY> list
      rng: System.Random
      STEP: bool
      VERB: bool
      IMPL: bool }

exception ERR_PARSE of string
exception ERR_ST_LEN of PATH * int
exception ERR_UNK_FN of PATH * string
exception ERR_CAST of PATH * string
