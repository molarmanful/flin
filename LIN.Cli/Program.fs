open System
open Argu

type CmdArgs =
    | [<MainCommand; Unique>] File of path: string
    | [<AltCommandLine("-e")>] Eval of str: string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | File _ -> "execute file."
            | Eval _ -> "execute string."

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CmdArgs>(programName = "flin")

    try
        let res = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        res.GetAllResults() |> printfn "%A"
        0
    with
    | e ->
        printfn "%s" e.Message
        1
