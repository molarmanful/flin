module LIN.P

#if INTERACTIVE
#r "nuget: FParsec"
#r "nuget: MathNet.Numerics.FSharp"
#endif

open System.Text
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
                let [ n1; n2 ] = split [ "." ] s |> toList
                let d = 10N ** length n2 |> string
                // n1 + n2 + "/" + d
                let sb = new StringBuilder()

                sb.Append(n1).Append(n2).Append('/').Append(d)
                |> string
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

exception ParseError of string

let parse s =
    match String.split [ "\n"; "\r\n" ] s
          |> head
          |> run pline
        with
    | Success (p, _, _) -> p
    | Failure (e, _, _) -> ParseError e |> raise

#if INTERACTIVE
parse "1 2.3asdf \"as\\\"df\"(6 8~ .59 a.+ )\"oof\n9 10"
#endif
