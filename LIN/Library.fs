module rec LIN.ENV

open System.Collections.Generic
open FSharpx.Collections
open FSharpPlus

module PVec = PersistentVector
module PMap = PersistentHashMap

module PVec =
    let lconj ys xs = fold (flip PVec.conj) xs ys

let (|C|Nil|) = PVec.(|Conj|Nil|)

let stak env st = { env with stack = st }

let arg1 env f =
    match env.stack with
    | C (xs, x) -> stak env <| f x xs
    | _ -> ERR_ST_LEN 1 |> raise

let arg2 env f =
    match env.stack with
    | C (C (xs, y), x) -> stak env <| f y x xs
    | _ -> ERR_ST_LEN 2 |> raise

let arg3 env f =
    match env.stack with
    | C (C (C (xs, z), y), x) -> stak env <| f z y x xs
    | _ -> ERR_ST_LEN 3 |> raise

let push env x = env.stack.Conj x |> stak env

let pop env = (fun _ xs -> xs) |> arg1 env

let dup env =
    (fun x -> PVec.lconj [ x; x ]) |> arg1 env

let swap env =
    (fun y x -> PVec.lconj [ x; y ]) |> arg2 env

let exec env =
    match env.lines with
    | [] -> env
    | c :: cs -> execA { env with lines = cs } c

let execA env c =
    match c with
    | NUM _
    | STR _ -> push env c |> exec
    | CMD x ->
        match x with
        | x when x.StartsWith '\\' -> CMD x |> push env |> exec

let run file lines =
    exec
        { stack = PVec.empty
          file = file
          lines = lines
          ln = 0
          scope = PMap.empty }

let SL = new Dictionary<string, ENV -> ENV>()
SL.Add("pop", pop)
SL.Add("dup", dup)
SL.Add("swap", swap)
