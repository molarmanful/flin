open Argu

type CmdArgs =
    | [<MainCommand; Unique>] File of path: string
    | [<AltCommandLine("-e"); Unique; EqualsAssignmentOrSpaced>] Eval of str: string
    | [<AltCommandLine("-s")>] Step
    | [<AltCommandLine("-v")>] Verb
    | [<AltCommandLine("-i")>] Impl

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | File _ -> "execute file."
            | Eval _ -> "execute string."
            | Step _ -> "toggle step mode."
            | Verb _ -> "toggle verbose mode."
            | Impl _ -> "toggle implicit mode."

open Spectre.Console
open LIN

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CmdArgs>(programName = "flin")

    try
        let res = parser.ParseCommandLine argv
        let o = (res.Contains Step, res.Contains Verb, res.Contains Impl)

        if res.Contains(File) then
            res.GetResult File |> ENV.runf o |> ignore
        elif res.Contains(Eval) then
            res.GetResult Eval |> ENV.runs o |> ignore

        0
    with
    | e ->
        AnsiConsole.MarkupLineInterpolated $"[red]ERR: {ENV.HELP.errStr e}[/]"
        1
