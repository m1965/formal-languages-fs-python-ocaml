/// Visualization uses the module Nested of Nested.fsx, which has to be loaded first, once per session.
module Visualization

open System
open System.Diagnostics
open System.Net
open System.Text
open Nested

/// The children of a tree: a leaf has no children, and the children of a tuple are all its components except
/// the first one, which is the label.
let children (t: nested) =
    match t with
    | Leaf _ -> []
    | Node [] -> []
    | Node (_ :: rest) -> rest

/// The label of a tree: the value of a leaf, or the first component of a tuple.  If the first component is
/// a tuple itself, as in ((":=", ";"), x, e), this tuple is the label.
let label (t: nested) =
    match t with
    | Leaf (Str s) -> s
    | Leaf v -> valueToString v
    | Node [] -> "()"
    | Node (Leaf (Str s) :: _) -> s
    | Node (Leaf v :: _) -> valueToString v
    | Node (head :: _) -> toString head

let htmlEncode (text: string) = WebUtility.HtmlEncode text
let private quote (text: string) =
    "\"" + text.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n").Replace("\r", "") + "\""

/// Preorder addresses distinguish repeated labels without relying on object identity.
let assignNumbers (tree: nested) =
    let rec visit address next tree numbers =
        let numbers = Map.add address next numbers
        children tree
        |> List.indexed
        |> List.fold (fun (counter, known) (index, child) ->
            visit (address @ [index]) counter child known) (next + 1, numbers)
    visit [] 0 tree Map.empty |> snd

/// treeToDot t: the tree t, i.e. a nested tuple, as a graph in the dot format of Graphviz
let treeToDot (tree: nested) =
    let numbers = assignNumbers tree
    let lines = StringBuilder("digraph AST {\n  node [shape=box];\n")
    let rec visit address tree =
        let id = numbers[address]
        lines.AppendLine(sprintf "  n%d [label=%s];" id (quote (label tree))) |> ignore
        for index, child in List.indexed (children tree) do
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
