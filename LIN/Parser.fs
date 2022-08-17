module LIN.Parser

#if INTERACTIVE
#r "nuget: FParsec"
#r "nuget: MathNet.Numerics.FSharp"
#endif

open System
open FParsec
open MathNet.Numerics

type Parser<'t> = Parser<'t, unit>

let ws: Parser<_> =
    skipManySatisfy (function
        | ' '
        | '\t' -> true
        | _ -> false)

let onum =
    NumberLiteralOptions.AllowFraction
    ||| NumberLiteralOptions.AllowFractionWOIntegerPart

let pnum =
    numberLiteral onum "num"
    |>> fun n -> Decimal.Parse n.String
    |>> BigRational.FromDecimal
    |>> NUM

let str s = pstring s

let pstr =
    let norm = manySatisfy (fun c -> c <> '"' && c <> '\\')
    let escq = stringReturn "\\\"" "\""

    between (str "\"") (str "\"") (stringsSepBy norm escq)
    |>> STR

let pdot = str "." |>> CMD

let pcmd =
    manySatisfy (function
        | '.'
        | ' '
        | '\t'
        | '\n' -> false
        | c when isDigit c -> false
        | _ -> true)
    |>> CMD

let xs = choice [ pdot; pnum; pstr; pcmd ]

let pline = manyTill (ws >>. xs .>> ws) (newline)

#if INTERACTIVE
run pline "1 2.3 asdf (6 8~ a.+ )\"asdf\"\n9 10"
#endif
