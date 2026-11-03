/// Regression test of the F# port of Larkita, without Python.
///
///   dotnet fsi f#/Support/Larkita/tests/test_larkita.fsx
///
/// larkita_expected.json (shared with ocaml/tests) contains the grammars and inputs of the unit tests
/// of the Python package Larkita together with the results that Python's Larkita produced for them.
/// ocaml/tools/diff_larkita.py --impl fsharp performs the full comparison, including the output files.
#load "../Larkita.fsx"

open System.IO
open System.Text.Json.Nodes
open Larkita

conflictWarnings <- LarkitaBase.Ignore

let errorOf (e: exn) =
    match e with
    | GrammarError m -> "GrammarError", m
    | LexError m -> "LexError", m
    | UnexpectedInput u ->
        (match u.Kind with
         | LarkitaBase.UnexpectedCharacters _ -> "UnexpectedCharacters"
         | LarkitaBase.UnexpectedToken _ -> "UnexpectedToken"), u.Message
    | e -> "Exception", e.Message

let fixture = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..", "..", "ocaml", "tests", "larkita_expected.json")
let cases = JsonNode.Parse(File.ReadAllText fixture).AsArray()
let outFile = Path.GetTempFileName()
let mutable failures = 0
let mutable checks = 0
let fail (i: int) what (expected: string) (got: string) =
    failures <- failures + 1
    printfn "case %d, %s:\n  expected %s\n  got      %s" i what expected got

let str (n: JsonNode) = n.GetValue<string>()

cases |> Seq.iteri (fun i case ->
    let options = case.["options"].AsObject()
    let opt name = match options.TryGetPropertyValue name with | true, v when v <> null -> Some v | _ -> None
    let expected = case.["outcome"]
    checks <- checks + 1
    let construction =
        try
            Ok(Larkita.Create(
                str case.["grammar"], ?start = (opt "start" |> Option.map str),
                ?lexer = (opt "lexer" |> Option.map (fun v -> if str v = "basic" then Basic else Contextual)),
                ?mergeStates = (opt "merge_states" |> Option.map (fun v -> v.GetValue<bool>())),
                ?gRegexFlags = (opt "g_regex_flags" |> Option.map str), outFile = outFile))
        with e -> Error e
    match construction with
    | Error e ->
        let t, m = errorOf e
        if expected = null then fail i "construction" "success" (t + ": " + m)
        else
            let et = str expected.["type"]
            let em = expected.["message"]
            if et <> t || (em <> null && str em <> m) then
                fail i "construction" (et + ": " + (if em = null then "" else str em)) (t + ": " + m)
    | Ok parser ->
        if expected <> null then fail i "construction" (expected.ToJsonString()) "success"
        for call in case.["calls"].AsArray() do
            checks <- checks + 1
            let text = str call.["text"]
            let kind = str call.["kind"]
            let got =
                try
                    if kind = "parse" then "result " + toString (parse parser text)
                    else
                        "result " + System.String.Join("|", lex parser text |> List.map (fun t -> sprintf "%s,%s,%d,%d" t.Type t.Value t.Line t.Column))
                with e ->
                    let t, m = errorOf e
                    "error " + t + ": " + m
            let want =
                match call.["error"] with
                | null ->
                    if kind = "parse" then "result " + str call.["result"]
                    else
                        "result " + System.String.Join("|", call.["result"].AsArray() |> Seq.map (fun t ->
                            let a = t.AsArray()
                            sprintf "%s,%s,%d,%d" (str a.[0]) (str a.[1]) (a.[2].GetValue<int>()) (a.[3].GetValue<int>())))
                | e -> "error " + str e.["type"] + ": " + str e.["message"]
            if got <> want then fail i (kind + " " + text) want got)

File.Delete outFile
printfn "%d checks, %d failures" checks failures
exit (if failures = 0 then 0 else 1)
