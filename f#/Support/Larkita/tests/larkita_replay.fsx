/// Replays the calls recorded by ocaml/tools/larkita_record.py with the F# port of Larkita.
///
///   dotnet fsi larkita_replay.fsx CASES.json RESULTS.json OUT_DIR
///
/// The results are written in the same form as those of ocaml/tools/larkita_replay.ml, so that
/// ocaml/tools/diff_larkita.py --impl fsharp can compare them with the recorded ones.
#load "../../Nested.fsx"
#load "../Larkita.fsx"

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Larkita

let exceptionJson (e: exn) : JsonNode =
    let simple (t: string) (m: string) =
        let o = JsonObject()
        o.["type"] <- JsonValue.Create t
        o.["message"] <- JsonValue.Create m
        o
    match e with
    | GrammarError m -> simple "GrammarError" m :> JsonNode
    | LexError m -> simple "LexError" m :> JsonNode
    | ConflictWarning m -> simple "ConflictWarning" m :> JsonNode
    | UnexpectedInput u ->
        let t =
            match u.Kind with
            | LarkitaAst.UnexpectedCharacters _ -> "UnexpectedCharacters"
            | LarkitaAst.UnexpectedToken _ -> "UnexpectedToken"
        let o = simple t u.Message
        o.["line"] <- JsonValue.Create u.Line
        o.["column"] <- JsonValue.Create u.Column
        o :> JsonNode
    | :? FileNotFoundException as f -> simple "FileNotFoundError" f.Message :> JsonNode
    | e -> simple "Exception" (e.ToString()) :> JsonNode

let args = fsi.CommandLineArgs |> Array.skip 1
let casesFile, resultsFile, outDir = args.[0], args.[1], args.[2]
let home = Directory.GetCurrentDirectory()
let cases = JsonNode.Parse(File.ReadAllText casesFile).AsArray()
let results = JsonArray()

let str (n: JsonNode) = n.GetValue<string>()

cases
|> Seq.iteri (fun i case ->
    Directory.SetCurrentDirectory home
    let result = JsonObject()
    if case.["skip"] <> null then result.["skipped"] <- JsonValue.Create true
    else
        (try Directory.SetCurrentDirectory(str case.["cwd"]) with _ -> ())
        let options = case.["options"].AsObject()
        let opt name = match options.TryGetPropertyValue name with | true, v when v <> null -> Some v | _ -> None
        let outFile = Path.Combine(outDir, sprintf "case-%d.out" i)
        conflictWarnings <-
            (match case.["outcome"] with
             | null -> LarkitaAst.Ignore
             | o when (o.["type"] |> str) = "ConflictWarning" -> LarkitaAst.Raise
             | _ -> LarkitaAst.Ignore)
        let construction =
            try
                Ok(Larkita.Create(
                    str case.["grammar"],
                    ?start = (opt "start" |> Option.map str),
                    ?lexer = (opt "lexer" |> Option.map (fun v -> if str v = "basic" then Basic else Contextual)),
                    ?importPaths = (opt "import_paths" |> Option.map (fun v -> v.AsArray() |> Seq.map str |> List.ofSeq)),
                    ?sourcePath = (opt "source_path" |> Option.map str),
                    outFile = outFile,
                    ?gRegexFlags = (opt "g_regex_flags" |> Option.map str),
                    ?mergeStates = (opt "merge_states" |> Option.map (fun v -> v.GetValue<bool>()))))
            with e -> Error e
        result.["outcome"] <- (match construction with Ok _ -> null | Error e -> exceptionJson e)
        result.["report"] <- (if File.Exists outFile then JsonValue.Create(File.ReadAllText outFile) :> JsonNode else null)
        result.["out_file"] <- JsonValue.Create outFile
        let calls = JsonArray()
        for call in case.["calls"].AsArray() do
            let text = str call.["text"]
            let r = JsonObject()
            match construction with
            | Error _ ->
                let o = JsonObject()
                o.["type"] <- JsonValue.Create "not constructed"
                r.["error"] <- o
            | Ok parser ->
                try
                    if str call.["kind"] = "parse" then r.["result"] <- JsonValue.Create(toTestString (parse parser text))
                    else
                        let tokens = JsonArray()
                        for t in lex parser text do
                            let a = JsonArray()
                            a.Add(JsonValue.Create t.Type)
                            a.Add(JsonValue.Create t.Value)
                            a.Add(JsonValue.Create t.Line)
                            a.Add(JsonValue.Create t.Column)
                            tokens.Add a
                        r.["result"] <- tokens
                with e -> r.["error"] <- exceptionJson e
            calls.Add r
        result.["calls"] <- calls
    results.Add result)

Directory.SetCurrentDirectory home
File.WriteAllText(resultsFile, results.ToJsonString())
