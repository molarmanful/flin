module LIN.P

open FSharpPlus
open FParsec
open MathNet.Numerics

let ws =
    skipManySatisfy (function
        | ' '
        | '\t' -> true
        | _ -> false)

let onum =
    NumberLiteralOptions.AllowFraction
    ||| NumberLiteralOptions.AllowFractionWOIntegerPart

let pnum =
    numberLiteral onum "num"
    |>> fun n ->
            let s = n.String

            if n.HasFraction then
                let dec = split [ "." ] s |> toList
                let n1 = head dec
                let n2 = List.last dec
                let d = 10N ** length n2 |> string
                $"{n1}{n2}/{d}"
            else
                s
            |> BigRational.Parse
            |> NUM

let str = pstring

let pstr =
    let norm = manySatisfy (fun c -> c <> '"' && c <> '\\')
    let escq = stringReturn "\\\"" "\""

    between (str "\"") (manyMinMaxSatisfy 0 1 <| (=) '\"') (stringsSepBy norm escq)
    |>> STR

let pdot = str "." |>> CMD

let pcmd =
    manySatisfy (function
        | '.'
        | '"'
        | ' '
        | '\t'
        | '\n' -> false
        | c when isDigit c -> false
        | _ -> true)
    |>> CMD

let pline =
    manyTill
        (ws
         >>. choice [ attempt pnum
                      pdot
                      pstr
                      pcmd ])
        eof

let parse s =
    match String.split [ "\n"; "\r\n" ] s
          |> head
          |> run pline
        with
    | Success (p, _, _) -> p
    | Failure (e, _, _) -> ERR_PARSE e |> raise
