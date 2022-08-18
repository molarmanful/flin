module rec LIN.ENV

open System.Collections.Generic
open FSharpx.Collections
open FSharpPlus

module PVec = PersistentVector
module PMap = PersistentHashMap

module PVec =
    let lconj xs ys = fold (flip PVec.conj) xs ys

let stak env st = { env with stack = st }

let arg1 env =
    if env.stack.Length < 1 then
        ERR_ST_LEN 1 |> raise
    else
        let xs, x = env.stack.Unconj
        stak env xs, x

let arg2 env =
    if env.stack.Length < 2 then
        ERR_ST_LEN 2 |> raise
    else
        let xs, x = env.stack.Unconj
        let xs, y = xs.Unconj
        stak env xs, y, x

let arg3 env =
    if env.stack.Length < 3 then
        ERR_ST_LEN 3 |> raise
    else
        let xs, x = env.stack.Unconj
        let xs, y = xs.Unconj
        let xs, z = xs.Unconj
        xs, y, x, z

let push env x = env.stack.Conj x |> stak env

let pop env = arg1 env |> fst

let swap env =
    let { stack = st }, y, x = arg2 env
    PVec.lconj st [ x; y ] |> stak env

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
SL.Add("swap", swap)
