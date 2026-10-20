module Visualization

open System
open System.Diagnostics
open System.Net
open System.Text

type Tree = Leaf of string | Node of string * Tree list

let htmlEncode (text: string) = WebUtility.HtmlEncode text
let private quote (text: string) =
    "\"" + text.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n").Replace("\r", "") + "\""

/// Preorder addresses distinguish repeated labels without relying on object identity.
let assignNumbers tree =
    let rec visit address next tree numbers =
        let numbers = Map.add address next numbers
        match tree with
        | Leaf _ -> next + 1, numbers
        | Node (_, children) ->
            children
            |> List.indexed
            |> List.fold (fun (counter, known) (index, child) ->
                visit (address @ [index]) counter child known) (next + 1, numbers)
    visit [] 0 tree Map.empty |> snd

let treeToDot tree =
    let numbers = assignNumbers tree
    let lines = StringBuilder("digraph AST {\n  node [shape=box];\n")
    let rec visit address tree =
        let id = numbers[address]
        let label, children =
            match tree with
            | Leaf label -> label, []
            | Node (label, children) -> label, children
        lines.AppendLine(sprintf "  n%d [label=%s];" id (quote label)) |> ignore
        for index, child in List.indexed children do
            let childAddress = address @ [index]
            lines.AppendLine(sprintf "  n%d -> n%d;" id numbers[childAddress]) |> ignore
            visit childAddress child
    visit [] tree
    lines.AppendLine("}").ToString()

/// Graphviz is a native CLI boundary; arguments and graph source never pass through a shell.
let renderDot (dot: string) =
    let settings = ProcessStartInfo("dot")
    settings.ArgumentList.Add "-Tsvg"
    settings.UseShellExecute <- false
    settings.RedirectStandardInput <- true
    settings.RedirectStandardOutput <- true
    settings.RedirectStandardError <- true
    use graphviz = new Process(StartInfo = settings)
    try graphviz.Start() |> ignore
    with error -> failwithf "Install Graphviz and put dot on PATH: %s" error.Message
    let output = graphviz.StandardOutput.ReadToEndAsync()
    let errors = graphviz.StandardError.ReadToEndAsync()
    graphviz.StandardInput.Write dot
    graphviz.StandardInput.Close()
    graphviz.WaitForExit()
    let svg = output.GetAwaiter().GetResult()
    if graphviz.ExitCode <> 0 then failwithf "Graphviz failed: %s" (errors.GetAwaiter().GetResult())
    svg
