/// Larkita for F#, part 5: the output file that documents the LR(1) automaton (report.py).
/// The format is the same as that of the Python version.
module LarkitaReport

#load "LarkitaBase.fsx" "LarkitaLr.fsx"

open System
open System.Collections.Generic
open LarkitaBase
open LarkitaLr

type Formatter = { A: Analyzer; Names: Dictionary<string, string> }

let symbol f (name: string) = match f.Names.TryGetValue name with | true, s -> s | _ -> name

/// the rows of the action table are sorted by symbol, with $END last
let sortRows (rows: string list list) =
    rows |> List.sortWith (fun r1 r2 ->
        match compare (List.head r1 = endSym) (List.head r2 = endSym) with
        | 0 -> cmpStr (List.head r1) (List.head r2)
        | c -> c)

let lookaheads f (terminals: Set<string>) =
    let names = terminals |> Set.toList |> List.filter (fun t -> t <> endSym) |> List.map (symbol f) |> sortStrings
    let names = if terminals.Contains endSym then names @ [ endSym ] else names
    "{ " + String.Join(", ", names) + " }"

let ruleText f (r: int) =
    let rule = f.A.Rules.[r]
    let rhs = String.Join(" ", rule.Expansion |> List.map (fun s -> symbol f s.Name))
    sprintf "%s → %s" rule.Origin (if rhs = "" then "ε" else rhs)

let itemText f (r: int) (dot: int) =
    let rule = f.A.Rules.[r]
    let names = rule.Expansion |> List.map (fun s -> symbol f s.Name)
    sprintf "%s → %s" rule.Origin (String.Join(" ", take dot names @ [ "•" ] @ drop dot names))

let number (r: int) = sprintf "(%d)" r

let conflictText (withState: bool) f (c: Conflict) =
    let options =
        (match c.ShiftState with Some s -> [ sprintf "shift and go to state %d" s ] | None -> [])
        @ (c.CRules |> List.map (fun r -> sprintf "reduce using rule %s %s" (number r) (ruleText f r)))
    let status =
        if not c.Resolved then "NOT RESOLVED (this is an error)"
        else
            match c.Chosen with
            | Shift _ -> c.Reason
            | Reduce r -> sprintf "%s: reduce using rule %s" c.Reason (number r)
    let where = if withState then sprintf "State %d, " c.CState else ""
    String.Join("\n", sprintf "%s%s conflict on %s, %s" where (conflictKind c) (symbol f c.CTerminal) status
                      :: (options |> List.map (fun o -> "    - " + o)))

/// what a state does for the symbol name, e.g. "reduces using rule (3) a → b"
let actionText f (name: string) (action: Action option) (other: string option) =
    match action with
    | None -> sprintf "does not accept %s (collides with %s)" (symbol f name) (symbol f other.Value)
    | Some (Reduce r) -> sprintf "reduces using rule %s %s" (number r) (ruleText f r)
    | Some (Shift s) ->
        if f.A.RulesByOrigin.ContainsKey name then sprintf "goes to state %d" s
        else sprintf "shifts and goes to state %d" s

let mergedStates f =
    let a = f.A
    let out = ResizeArray<string>()
    out.Add "States with the same core have been merged, unless this would cause"
    out.Add(sprintf "a conflict: %d LR(1) states have been merged into %d states." a.Lr1StateCount a.States.Length)
    if List.isEmpty a.KeptApart then
        out.AddRange [ "All states with the same core have been merged, so this is the"; "LALR(1) automaton."; "" ]
    else
        let isCause (_, _, differences: Difference list) =
            differences |> List.exists (fun (_, s, t, _) -> match s, t with Some (Shift _), Some (Shift _) -> false | _ -> true)
        let causes = a.KeptApart |> List.filter isCause
        let consequences = a.KeptApart |> List.filter (isCause >> not)
        if not (List.isEmpty causes) then
            out.AddRange
                [ ""; "The following states have the same core, but they have been kept"
                  "apart.  Either merging them would cause a conflict, or one of them"
                  "accepts a token that collides with a token of the other one, i.e."
                  "the scanner could read the same text as either of these tokens." ]
        for s, t, differences in causes do
            out.AddRange [ ""; sprintf "States %d and %d" s t ]
            for name, actionS, actionT, other in differences do
                out.Add(sprintf "    on %s:" (symbol f name))
                out.Add(sprintf "        state %d %s" s (actionText f name actionS other))
                out.Add(sprintf "        state %d %s" t (actionText f name actionT other))
        if not (List.isEmpty consequences) then
            out.AddRange
                [ ""; "The following states have the same core, but they have been kept"
                  "apart, because their successors have been kept apart:"; "" ]
            let lines = ResizeArray [ "   " ]
            for s, t, _ in consequences do
                let pair = sprintf "%d and %d" s t
                if lines.[lines.Count - 1].Length + pair.Length > 66 then lines.Add "   "
                lines.[lines.Count - 1] <- lines.[lines.Count - 1] + " " + pair + ","
            let last = lines.[lines.Count - 1]
            lines.[lines.Count - 1] <- last.TrimEnd ','
            out.AddRange lines
        out.Add ""
    List.ofSeq out

/// formats the rows so that their columns are aligned
let table (rows: string list list) (indent: string) =
    match rows with
    | [] -> []
    | first :: _ ->
        let k = List.length first - 1
        let widths = List.init k (fun i -> rows |> List.map (fun row -> (List.item i row).Length) |> List.max)
        rows |> List.map (fun row ->
            let cells = (widths |> List.mapi (fun i w -> ljust (List.item i row) w)) @ [ List.item k row ]
            (indent + String.Join("  ", cells)).TrimEnd())

let formatReport (a: Analyzer) (terminalNames: Dictionary<string, string>) (source: string) =
    let f = { A = a; Names = terminalNames }
    let userRules = a.Rules |> Array.toList |> List.tail
    let aux = auxNames userRules
    let errors = a.Conflicts |> List.filter (fun c -> not c.Resolved)
    let title, states =
        if a.MergeStates then
            "LR(1) automaton with merged states generated by Larkita",
            sprintf "%d (merged from %d LR(1) states)" a.States.Length a.Lr1StateCount
        else "Canonical LR(1) automaton generated by Larkita", string a.States.Length
    let out = ResizeArray<string>()
    out.AddRange
        [ title; ""; "Grammar:      " + source; "Start symbol: " + a.Start; "States:       " + states
          sprintf "Conflicts:    %d (%d unresolved)" (List.length a.Conflicts) (List.length errors); ""; ""
          "Rules"; "====="; ""
          "Every rule is followed by the abstract syntax tree that a reduction produces."
          "\"*name\" stands for the list of children collected by the auxiliary rule \"name\"."
          "\"text\" stands for the text of a named token."; "" ]
    let rows = userRules |> List.mapi (fun i rule -> [ number (i + 1); ruleText f (i + 1); "⇒ " + describeAst rule aux ])
    out.AddRange(table ([ number 0; ruleText f 0; "" ] :: rows) "  ")
    out.AddRange [ ""; ""; "Conflicts"; "========="; "" ]
    if List.isEmpty a.Conflicts then out.AddRange [ "There are no conflicts."; "" ]
    else
        for c in a.Conflicts do
            out.Add(conflictText true f c)
            out.Add ""
    if a.MergeStates then out.AddRange([ ""; "Merged states"; "============="; "" ] @ mergedStates f)
    out.AddRange
        [ ""; "States"; "======"; ""
          "Every state lists its items with their lookahead tokens; closure items are marked with \"+\"."
          "Rules are referred to by their numbers in the list of rules." ]
    for state in a.States do
        let header = sprintf "State %d" state.Index
        let header =
            if List.length state.MergedFrom > 1 then
                header + "   (merged from the LR(1) states " + String.Join(", ", state.MergedFrom |> List.map string) + ")"
            else header
        out.AddRange [ ""; ""; header; "" ]
        let rows =
            state.Items |> List.map (fun ((r, d), la) ->
                [ (if List.exists (fun (k, _) -> k = (r, d)) state.Kernel then "" else "+"); number r; itemText f r d; lookaheads f la ])
        out.AddRange(table rows "  ")
        out.Add ""
        let shifts = ResizeArray()
        let reduces = ResizeArray()
        let gotos = ResizeArray()
        for name, action in state.Actions do
            match action with
            | Shift s ->
                if a.RulesByOrigin.ContainsKey name then gotos.Add [ name; sprintf "go to state %d" s ]
                else shifts.Add [ symbol f name; sprintf "shift and go to state %d" s ]
            | Reduce r -> reduces.Add [ symbol f name; sprintf "reduce using rule %s %s" (number r) (ruleText f r) ]
        let reduces = List.ofSeq reduces
        let reduces = if state.Index = a.EndState then reduces @ [ [ endSym; "accept" ] ] else reduces
        let rows = sortRows (List.ofSeq shifts) @ sortRows reduces
        let rows =
            if gotos.Count > 0 then
                rows @ [ [ ""; "" ] ] @ (List.ofSeq gotos |> List.sortWith (fun x y -> cmpStr (String.Join("\u0000", x)) (String.Join("\u0000", y))))
            else rows
        out.AddRange(table rows "    ")
        let conflicts = a.Conflicts |> List.filter (fun c -> c.CState = state.Index)
        if not (List.isEmpty conflicts) then
            out.Add ""
            for c in conflicts do
                for line in (conflictText false f c).Split '\n' do out.Add("    " + line)
    String.Join("\n", out) + "\n"

let writeReport (path: string) (a: Analyzer) (terminalNames: Dictionary<string, string>) (source: string) =
    IO.File.WriteAllText(path, formatReport a terminalNames source, Text.UTF8Encoding(false))
