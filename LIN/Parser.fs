module LIN.P

open FSharpPlus
open MathNet.Numerics

let isPar = String.forall <| flip String.contains "([{}])"

let clean S =
    match S.t with
    | PT.UN -> S
    | _ ->
        { x = ""
          t = PT.UN
          xs =
            match S.t with
            | PT.CMD when isPar S.x ->
                toList S.x
                |> List.map (string >> CMD)
                |> (++) S.xs
            | _ ->
                S.xs
                ++ [ match S.t with
                     | _ when S.x = "." -> CMD "."
                     | PT.STR -> STR S.x
                     | PT.CMD -> CMD S.x
                     | PT.NUM -> NUM <| BigRational.Parse S.x
                     | PT.DEC ->
                         let dec = split [ "." ] S.x |> toList
                         let n1 = head dec
                         let n2 = head <| drop 1 dec
                         let d = 10N ** length n2 |> string
                         NUM <| BigRational.Parse $"{n1}{n2}/{d}" ] }

let addc c S : PST = { S with x = $"{S.x}{c}" }
let addt t S : PST = { S with t = t }
let newc c S : PST = { clean S with x = string c }

let pstr S c =
    match S.t with
    | PT.ESC ->
        addc
            (match c with
             | '"' -> "\""
             | _ -> $"{c}")
            S
        |> addt PT.STR
    | PT.STR ->
        match c with
        | '\\' -> addt PT.ESC S
        | '"' -> clean S
        | _ -> addc c S

let pnum S c =
    match S.t with
    | PT.DEC
    | PT.NUM -> addc c S
    | _ -> newc c S |> addt PT.NUM

let pdot S =
    match S.t with
    | PT.NUM -> addc "." S |> addt PT.DEC
    | _ -> newc "." S |> addt PT.DEC

let pcmd S c =
    match S.t with
    | PT.CMD -> addc c S
    | _ -> newc c S |> addt PT.CMD

open System

let choice S c =
    match c with
    | ' '
    | '\t'
    | '\r'
    | '\n' -> clean S
    | _ when S.t = PT.STR || S.t = PT.ESC -> pstr S c
    | '"' -> clean S |> addt PT.STR
    | '.' -> pdot S
    | _ when Char.IsDigit c -> pnum S c
    | _ -> pcmd S c

let pline =
    fold choice { xs = []; x = ""; t = PT.UN }
    >> clean
    >> fun S -> S.xs

let parse s =
    String.split [ "\n"; "\r\n" ] s
    |> head
    |> toList
    |> pline
