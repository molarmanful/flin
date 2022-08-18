module LIN.I

open FSharpPlus
open FSharpPlus.Data


let rec exec env =
    match env.lns with
    | [] -> env
    | c :: cs -> execA { env with lns = cs }

and execA env c =
    match c with
    | NUM _
    | STR _ -> H.push c env |> exec

let run (lns, file) =
    exec
        { stack = DList.empty
          file = file
          lns = lns
          lnn = [ 0 ] }
