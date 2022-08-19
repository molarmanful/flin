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

        let runner =
            let o = (res.Contains Step, res.Contains Verb, res.Contains Impl)

            match res with
            | p when p.Contains(File) -> p.GetResult File |> ENV.runf o |> Some
            | p when p.Contains(Eval) -> p.GetResult Eval |> ENV.runs o |> Some
            | _ -> None

        0
    with
    | e ->
        let err m =
            AnsiConsole.MarkupLineInterpolated $"[red]ERR: {m}[/]"

        err (
            match e with
            | ERR_PARSE x -> $"bad syntax \"{x}\""
            | ERR_ST_LEN (x, (f, l)) -> $"""stack length < {x} @ {defaultArg f "?"}:{l}"""
            | ERR_UNK_FN (x, (f, l)) -> $"""unknown fn "{x}" @ {defaultArg f "?"}:{l}"""
            | _ -> string e
        )

        1
