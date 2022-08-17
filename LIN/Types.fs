namespace LIN

#if INTERACTIVE
#r "nuget: MathNet.Numerics.FSharp"
#endif

open MathNet.Numerics

type LIN =
    | MOL
    | ATOM

type MOL =
    | ARR of List<LIN>
    | OBJ of Map<LIN, LIN>

type ATOM =
    | NUM of BigRational
    | STR of string
    | CMD of string
