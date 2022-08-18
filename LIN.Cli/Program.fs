open System.IO
open Argu

type CmdArgs =
    | [<MainCommand; Unique>] File of path: string
    | [<AltCommandLine("-e"); Unique; EqualsAssignmentOrSpaced>] Eval of str: string
    | [<AltCommandLine("-v")>] Verbose

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | File _ -> "execute file."
            | Eval _ -> "execute string."
            | Verbose _ -> "toggle verbose mode."

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CmdArgs>(programName = "flin")

    try
        match parser.ParseCommandLine argv with
        | p when p.Contains(File) ->
            let f = p.GetResult(File)
            File.ReadLines f |> LIN.ENV.run f
            0
        | _ -> 0
    with
    | e ->
        printfn "%s" e.Message
        1
