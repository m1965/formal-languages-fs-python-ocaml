// Run after exporting notebook libraries, from any current directory.
#load "../Chapter-04-05/AutomataTypes.fsx"
#load "../Chapter-04-05/RegExpParser.fsx"
#load "../Chapter-04-05/NfaToDfa.fsx"
#load "../Chapter-04-05/RegExpToNfa.fsx"
#load "../Chapter-04-05/DfaToRegExp.fsx"
#load "../Chapter-04-05/Rewrite.fsx"
#load "../Chapter-04-05/Minimize.fsx"
#load "../Chapter-04-05/MinimizeFP.fsx"
#load "../Chapter-04-05/Equivalence.fsx"
#load "../Chapter-04-05/FsmToDot.fsx"
open AutomataTypes
open RegExpParser
open NfaToDfa
open RegExpToNfa
open DfaToRegExp
open Rewrite
open Minimize
open MinimizeFP
open Equivalence
open FsmToDot

let check description condition = if not condition then failwith description
let rejects action = try action (); false with :? System.ArgumentException -> true
let alphabet = set ['a';'b';'c']
let rec words length =
    if length = 0 then [""]
    else [for prefix in words (length-1) do for c in alphabet do yield prefix + string c]
let sampleWords = [0..5] |> List.collect words

// The independent oracle uses the .NET regex engine, rather than another automaton.
let rec dotNetPattern expression =
    match expression with
    | Empty -> "(?!)"
    | Epsilon -> "(?:)"
    | Literal c -> System.Text.RegularExpressions.Regex.Escape(string c)
    | Alt (left, right) -> "(?:" + dotNetPattern left + "|" + dotNetPattern right + ")"
    | Concat (left, right) -> dotNetPattern left + dotNetPattern right
    | Star r -> "(?:" + dotNetPattern r + ")*"
let examples =
    [ "∅"; "𝜀"; "a"; "a*"; "∅*"; "𝜀*"; "a**"; "a*ba*"
      "(a+b)*"; "(ab+ba)*"; "(𝜀+a)b*"; "a(b+c)*"; "(a+b)(a+b)*"
      "(a+b+c)*(ac*b+bc*a)(a+b+c)*"; "(a+∅)(b+𝜀)"; "((a+b)*c)*" ]
for source in examples do
    let expression = parse source
    check "Pretty-printer must preserve the syntax tree." (parse (regexp2string expression) = expression)
    let nfa = regexp2nfa alphabet expression
    let dfa = nfa2dfa nfa
    let minimal = minimize dfa
    check "Fixed-point and explicit minimizers must agree." (minimizeFP dfa = minimal)
    let oracle = System.Text.RegularExpressions.Regex("\\A(?:" + dotNetPattern expression + ")\\z")
    for word in sampleWords do
        let expected = oracle.IsMatch word
        check (sprintf "NFA changed %s on %A" source word) (acceptsNfa nfa word = expected)
        check (sprintf "DFA changed %s on %A" source word) (acceptsDfa dfa word = expected)
        check (sprintf "Minimization changed %s on %A" source word) (acceptsDfa minimal word = expected)

// Every original rewrite identity is checked using exact automaton equivalence.
let bindings =
    [ Map.ofList ["R", parse "a*"; "S", parse "b"; "T", parse "c*"]
      Map.ofList ["R", parse "a+b"; "S", parse "∅"; "T", parse "𝜀"] ]
for left, right in getRules () do
    for substitution in bindings do
        let first, second = apply left substitution, apply right substitution
        check "A rewrite identity changed the language." (regExpEquiv first second alphabet)
        check "Simplification changed the language." (regExpEquiv first (simplify first) alphabet)
check "Uppercase input symbols must remain literal." (simplify (parse "R+R") = Literal 'R')
check "Repeated pattern variable must match the same subtree."
    (matchPattern (patternOf (parse "R+R")) (parse "a+b") Map.empty = None)

// Exact state-elimination round trips for partial and complete automata.
let partial : Dfa<int> =
    { States = set [0;1;2]; Alphabet = set ['a';'b']
      Transitions = Map.ofList [(0,'a'),1; (1,'b'),2; (2,'a'),1]
      Start = 0; Accepting = set [2] }
let reconstructed = dfa2regexp partial |> simplify |> regexp2DFA <| partial.Alphabet
for word in sampleWords do
    check "State elimination changed partial-DFA acceptance." (acceptsDfa partial word = acceptsDfa reconstructed word)
check "Partial minimization should explain its completeness precondition." (rejects (fun () -> minimize partial |> ignore))
let oneState accepting : Dfa<int> =
    { States = set [0]; Alphabet = alphabet
      Transitions = alphabet |> Seq.map (fun c -> (0,c),0) |> Map.ofSeq
      Start = 0; Accepting = if accepting then set [0] else Set.empty }
check "All-rejecting DFA accepts no words." (isEmpty (oneState false))
check "All-accepting DFA has a nonempty language." (not (isEmpty (oneState true)))
check "Degenerate minimization should retain one state." ((minimize (oneState false)).States.Count = 1)
check "Difference against itself must be empty." (isEmpty (fsmComplement reconstructed reconstructed))
check "Difference must reject mismatched alphabets."
    (rejects (fun () -> fsmComplement (oneState false) reconstructed |> ignore))
for invalid in [""; "a+"; "(a"; "a)"; "*a"; "a#"; "a||b"] do
    check ("Parser silently accepted " + invalid) (rejects (fun () -> parse invalid |> ignore))
check "An out-of-alphabet literal should be rejected."
    (rejects (fun () -> regexp2nfa (set ['a']) (parse "b") |> ignore))
check "Mathematical and ordinary epsilon must be equivalent." (parse "𝜀" = parse "ε")
check "DOT labels should escape quotes." (quote "a\"b\\c\nd" = "\"a\\\"b\\\\c\\nd\"")
let dot, names = dfa2dot partial
check "State naming must cover each state." (names.Count = partial.States.Count)
check "DOT must use a distinct initial arrow node." (dot.Contains "__start")
printfn "Automata passed: %d expressions × %d words, 52 rewrite instances, round trips and edge cases." examples.Length sampleWords.Length

let epsilonCycle : Nfa<int> =
    { States = set [0;1]; Alphabet = set ['a']
      Transitions = Map.ofList [(0,Eps),set [1]; (1,Eps),set [0]]
      Start = 0; Accepting = set [1] }
check "An epsilon-only cycle should terminate and accept the empty word." (acceptsNfa epsilonCycle "")
check "Subset construction must retain the epsilon closure." ((nfa2dfa epsilonCycle).Start = set [0;1])
check "Absent input transitions should reach the rejecting empty subset." (not (acceptsDfa (nfa2dfa epsilonCycle) "a"))
let svg = renderSvg dot
let rendered = System.Xml.Linq.XDocument.Parse svg
check "Graphviz rendering must produce an SVG element." (rendered.Root.Name.LocalName = "svg")
let nfaSvg = renderSvg (nfa2dot epsilonCycle)
check "NFA rendering must include its distinct start node." (nfaSvg.Contains "__start")
printfn "Native Graphviz SVG rendering and epsilon-only cycles passed."
