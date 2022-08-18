namespace LIN

#if INTERACTIVE
#r "nuget: FSharpPlus"
#r "nuget: MathNet.Numerics.FSharp"
#endif

open FSharpPlus
open FSharpPlus.Data
open MathNet.Numerics

type PATH = Option<string>

type ANY =
    | ARR of ANY array
    | OBJ of Map<ANY, ANY>
    | NUM of BigRational
    | STR of string
    | CMD of string
    | FN of ANY list * (PATH * int)
    | UN of unit

    override s.ToString() =
        let join s xs = map string xs |> String.intercalate s

        match s with
        | ARR xs -> join " " xs
        | OBJ xs ->
            Map.toList xs
            |> map (fun (x, y) -> map string [ x, y ] |> String.intercalate " ")
            |> String.intercalate "\n"
        | NUM x -> BigRational.ToDouble x |> string
        | STR x
        | CMD x -> x
        | FN (xs, _) -> List.toArray xs |> join " "
        | _ -> string s

type ENV =
    { stack: ANY DList
      file: PATH
      lns: ANY list
      lnn: int list }
