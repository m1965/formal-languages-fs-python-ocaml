/// Larkita for F#, part 4: grammar rules, the canonical LR(1) automaton, the parser, and the
/// construction of the abstract syntax trees (grammar.py, lr_analysis.py, lr_parser.py, ast_builder.py).
///
/// Shift/reduce conflicts are resolved in favour of shift.  Reduce/reduce conflicts are resolved in
/// favour of the rule with the highest priority; if there is no such rule, the conflict is an error.
/// Optionally, states with the same core are merged, as long as this does not introduce new conflicts.
module LarkitaLr

#load "LarkitaBase.fsx"

open System
open System.Collections.Generic
open LarkitaBase

let cmpStr (a: string) (b: string) = String.CompareOrdinal(a, b)
let sortStrings (l: string list) = List.sortWith cmpStr l

// ---------- Symbols and rules (grammar.py) ----------

/// A symbol of a rule.  For a terminal, FilterOut is true if it was written as a string literal (it
/// becomes part of the head), and Text is the literal if it is a child.  Two symbols are equal if
/// they are both terminals or both nonterminals and have the same name.
type Symbol = { Name: string; IsTerm: bool; FilterOut: bool; Text: string option }

let terminal filterOut text name = { Name = name; IsTerm = true; FilterOut = filterOut; Text = text }
let nonterminal name = { Name = name; IsTerm = false; FilterOut = false; Text = None }
let symbolEqual (a: Symbol) (b: Symbol) = a.IsTerm = b.IsTerm && a.Name = b.Name

/// Terminal(name, filter_out, text).fullrepr
let symbolFullrepr (s: Symbol) =
    if s.IsTerm then
        sprintf "Terminal(%s, %s, %s)" (pyRepr s.Name) (if s.FilterOut then "True" else "False")
            (match s.Text with Some t -> pyRepr t | None -> "None")
    else sprintf "NonTerminal(%s)" (pyRepr s.Name)

/// How the abstract syntax tree of an alternative is built: None passes the abstract syntax tree of
/// the only child through, Some head builds (head, child_1, ..., child_n).  Two AstSpecs are only
/// equal if they are the same object (SpecId).
type AstSpec = { SpecId: int; Head: string list option }

let mutable private specCounter = 0

let astSpec (head: string list option) =
    specCounter <- specCounter + 1
    { SpecId = specCounter; Head = head }

/// A rule origin -> expansion.  Ast is None for the auxiliary rules created for EBNF repetitions.
type Rule =
    { Origin: string
      Expansion: Symbol list
      Order: int
      Priority: int option
      Ast: AstSpec option }

let ruleEqual (a: Rule) (b: Rule) =
    a.Origin = b.Origin && List.length a.Expansion = List.length b.Expansion && List.forall2 symbolEqual a.Expansion b.Expansion

let ruleToString (r: Rule) = sprintf "<%s : %s>" r.Origin (String.Join(" ", r.Expansion |> List.map (fun s -> s.Name)))

// ---------- The parse table ----------

type Action =
    | Shift of int
    | Reduce of int     // Reduce r: reduce using the rule with index r

let endSym = "$END"

type Item = int * int   // the index of the rule, the position of the dot

type Lookaheads = Set<string>

type LR1State =
    { Index: int
      mutable Kernel: (Item * Lookaheads) list
      mutable Items: (Item * Lookaheads) list          // the kernel items first, then the closure items
      mutable Transitions: (string * int) list
      mutable Actions: (string * Action) list
      mutable MergedFrom: int list }

type Conflict =
    { CState: int
      CTerminal: string
      CRules: int list
      ShiftState: int option
      Chosen: Action
      Resolved: bool
      Reason: string }

let conflictKind (c: Conflict) =
    if List.length c.CRules > 1 then (if c.ShiftState.IsSome then "shift/reduce/reduce" else "reduce/reduce")
    else "shift/reduce"

type Difference = string * Action option * Action option * string option

type Analyzer =
    { Start: string
      Rules: Rule array
      RulesByOrigin: Dictionary<string, int list>
      States: LR1State array
      Conflicts: Conflict list
      EndState: int
      Lr1StateCount: int
      MergeStates: bool
      KeptApart: (int * int * Difference list) list }

type ParseTable = { Table: Dictionary<string, Action> array; StartState: int; EndState: int }

let coreKey (kernel: (Item * Lookaheads) list) = kernel |> List.map fst |> List.sort

/// the lists of a list of (key, value) pairs in the order of the first occurrence of their keys
let private appendTo (l: ResizeArray<'k * ResizeArray<'v>>) (key: 'k) (value: 'v) =
    match Seq.tryFind (fun (k, _) -> k = key) l with
    | Some (_, vs) -> vs.Add value
    | None -> l.Add(key, ResizeArray [ value ])

// ---------- FIRST sets ----------

let firstSets (rules: Rule array) =
    let first = Dictionary<string, Lookaheads>()
    let nullable = HashSet<string>()
    let get n = match first.TryGetValue n with | true, s -> s | _ -> Set.empty
    let mutable changed = true
    while changed do
        changed <- false
        for rule in rules do
            let origin = rule.Origin
            let rec go syms =
                match syms with
                | [] ->
                    if nullable.Add origin then changed <- true
                | sym :: rest ->
                    let target = get origin
                    if sym.IsTerm then
                        if not (target.Contains sym.Name) then
                            first.[origin] <- target.Add sym.Name
                            changed <- true
                    else
                        let f = get sym.Name
                        if not (Set.isSubset f target) then
                            first.[origin] <- Set.union target f
                            changed <- true
                        if nullable.Contains sym.Name then go rest
            go rule.Expansion
    first, nullable

// ---------- The automaton ----------

let build (mergeStates: bool) (collision: (string -> string list -> string option) option) (userRules: Rule list) (start: string) =
    let rootRule = { Origin = "$root_" + start; Expansion = [ nonterminal start ]; Order = 0; Priority = None; Ast = None }
    let rules = Array.ofList (rootRule :: userRules)
    let rulesByOrigin = Dictionary<string, int list>()
    rules |> Array.iteri (fun i r ->
        rulesByOrigin.[r.Origin] <- (match rulesByOrigin.TryGetValue r.Origin with | true, l -> l | _ -> []) @ [ i ])
    let ruleList = List.ofArray rules
    let duplicates = ruleList |> List.indexed |> List.filter (fun (i, r) -> List.exists (ruleEqual r) (take i ruleList)) |> List.map snd
    if not (List.isEmpty duplicates) then
        raise (GrammarError("Rules defined twice: " + String.Join(", ", List.map ruleToString duplicates)))
    if not (rulesByOrigin.ContainsKey start) then
        raise (GrammarError(sprintf "The start symbol %s is not defined" (pyRepr start)))
    for r in rules do
        for s in r.Expansion do
            if not s.IsTerm && not (rulesByOrigin.ContainsKey s.Name) then
                raise (GrammarError("Using an undefined rule: " + s.Name))
    let first, nullable = firstSets rules
    let firstOf n = match first.TryGetValue n with | true, s -> s | _ -> Set.empty
    let firstOfSequence (symbols: Symbol list) =
        let rec go acc syms =
            match syms with
            | [] -> acc, true
            | sym :: rest ->
                if sym.IsTerm then Set.add sym.Name acc, false
                else
                    let acc = Set.union acc (firstOf sym.Name)
                    if nullable.Contains sym.Name then go acc rest else acc, false
        go Set.empty symbols
    let afterDot = Dictionary<Item, string * Lookaheads * bool>()
    rules |> Array.iteri (fun r rule ->
        rule.Expansion |> List.iteri (fun d sym ->
            if not sym.IsTerm then
                let f, nl = firstOfSequence (drop (d + 1) rule.Expansion)
                afterDot.[(r, d)] <- (sym.Name, f, nl)))
    let expansionArr = rules |> Array.map (fun r -> Array.ofList r.Expansion)
    let closure (kernel: (Item * Lookaheads) list) =
        let items = Dictionary<Item, Lookaheads>()
        for it, la in kernel do items.[it] <- la
        let todo = Stack<Item>()
        for it, _ in kernel do todo.Push it
        while todo.Count > 0 do
            let item = todo.Pop()
            match afterDot.TryGetValue item with
            | false, _ -> ()
            | true, (name, f, nl) ->
                let lookaheads = if nl then Set.union f items.[item] else f
                for r in rulesByOrigin.[name] do
                    let newItem = (r, 0)
                    match items.TryGetValue newItem with
                    | false, _ ->
                        items.[newItem] <- lookaheads
                        todo.Push newItem
                    | true, current ->
                        if not (Set.isSubset lookaheads current) then
                            items.[newItem] <- Set.union current lookaheads
                            todo.Push newItem
        items
    // building the states
    let states = ResizeArray<LR1State>()
    let index = Dictionary<string, int>()
    let keyOf (kernel: (Item * Lookaheads) list) =
        kernel
        |> List.map (fun ((r, d), la) -> sprintf "%d.%d:%s" r d (String.Join("\u0001", Set.toList la)))
        |> List.sort
        |> fun l -> String.Join("\u0002", l)
    let getState (kernel: (Item * Lookaheads) list) =
        let key = keyOf kernel
        match index.TryGetValue key with
        | true, i -> i
        | _ ->
            let i = states.Count
            index.[key] <- i
            states.Add { Index = i; Kernel = kernel; Items = []; Transitions = []; Actions = []; MergedFrom = [ i ] }
            i
    getState [ ((0, 0), Set.singleton endSym) ] |> ignore
    let mutable i = 0
    while i < states.Count do
        let state = states.[i]
        i <- i + 1
        let items = closure state.Kernel
        let kernelItems = state.Kernel |> List.map fst |> List.sort
        let closureItems = items.Keys |> Seq.filter (fun it -> not (List.exists (fun (k, _) -> k = it) state.Kernel)) |> List.ofSeq |> List.sort
        state.Items <- (kernelItems @ closureItems) |> List.map (fun it -> it, items.[it])
        let successors = ResizeArray<string * ResizeArray<Item * Lookaheads>>()
        for (r, d), la in state.Items do
            let exp = expansionArr.[r]
            if d < exp.Length then appendTo successors exp.[d].Name ((r, d + 1), la)
        state.Transitions <- successors |> Seq.map (fun (name, k) -> name, getState (List.ofSeq k)) |> List.ofSeq
    let states = states.ToArray()
    let endState = states.[0].Transitions |> List.find (fun (n, _) -> n = start) |> snd
    // the actions
    let conflicts = ResizeArray<Conflict>()
    let priority r = defaultArg rules.[r].Priority 0
    for state in states do
        let actions = ResizeArray(state.Transitions |> List.map (fun (name, target) -> name, Shift target))
        let reductions = ResizeArray<string * ResizeArray<int>>()
        for (r, d), la in state.Items do
            if d = expansionArr.[r].Length && r <> 0 then
                for t in la do appendTo reductions t r
        for terminal in reductions |> Seq.map fst |> List.ofSeq |> sortStrings do
            let rs = reductions |> Seq.find (fun (t, _) -> t = terminal) |> snd |> List.ofSeq
            let mutable rule = List.head rs
            let mutable resolved = true
            let mutable reason = ""
            if List.length rs > 1 then
                let top = rs |> List.map priority |> List.max
                let best = rs |> List.filter (fun r -> priority r = top)
                rule <- List.head best
                if List.length best = 1 then reason <- "resolved by priority"
                else
                    resolved <- false
                    reason <- "not resolved"
            match Seq.tryFind (fun (n, _) -> n = terminal) actions with
            | Some (_, (Shift target as chosen)) ->
                if resolved then reason <- "resolved as shift"
                conflicts.Add
                    { CState = state.Index; CTerminal = terminal; CRules = rs; ShiftState = Some target; Chosen = chosen
                      Resolved = resolved; Reason = reason }
            | Some (_, Reduce _) -> failwith "impossible"
            | None ->
                actions.Add(terminal, Reduce rule)
                if List.length rs > 1 then
                    conflicts.Add
                        { CState = state.Index; CTerminal = terminal; CRules = rs; ShiftState = None; Chosen = Reduce rule
                          Resolved = resolved; Reason = reason }
        state.Actions <- List.ofSeq actions
    let conflicts = List.ofSeq conflicts
    let lr1StateCount = states.Length
    if not mergeStates then
        { Start = start; Rules = rules; RulesByOrigin = rulesByOrigin; States = states; Conflicts = conflicts
          EndState = endState; Lr1StateCount = lr1StateCount; MergeStates = false; KeptApart = [] }
    else
        // ---------- merging states with the same core ----------
        let isNonterminal name = rulesByOrigin.ContainsKey name
        let terminalsOf (st: LR1State) = st.Actions |> List.choose (fun (n, _) -> if isNonterminal n then None else Some n) |> Set.ofList
        let collisions (nw: Set<string>) (accepted: Set<string>) =
            match collision with
            | None -> []
            | Some f ->
                nw |> Set.toList |> sortStrings |> List.choose (fun name ->
                    match f name (Set.toList accepted) with
                    | Some o -> Some(name, o)
                    | None -> None)
        let terminals = states |> Array.map terminalsOf
        let mergeable members1 terminals1 members2 terminals2 =
            List.forall (fun i -> List.isEmpty (collisions (Set.difference terminals2 terminals.[i]) terminals.[i])) members1
            && List.forall (fun i -> List.isEmpty (collisions (Set.difference terminals1 terminals.[i]) terminals.[i])) members2
        let signature (st: LR1State) (block: int array) =
            st.Actions |> List.map (fun (name, a) -> match a with Shift t -> name, Shift block.[t] | Reduce r -> name, Reduce r)
        let compatible (sg: (string * Action) list) (merged: Dictionary<string, Action>) =
            sg |> List.forall (fun (name, a) -> match merged.TryGetValue name with | true, b -> a = b | _ -> true)
        let cores = Dictionary<Item list, int>(HashIdentity.Structural)
        let mutable block =
            states |> Array.map (fun st ->
                let key = coreKey st.Kernel
                match cores.TryGetValue key with
                | true, b -> b
                | _ ->
                    let b = cores.Count
                    cores.[key] <- b
                    b)
        let n = states.Length
        let mutable finished = false
        while not finished do
            let members = SortedDictionary<int, ResizeArray<LR1State>>()
            for st in states do
                let b = block.[st.Index]
                if not (members.ContainsKey b) then members.[b] <- ResizeArray()
                members.[b].Add st
            let newBlock = Array.zeroCreate n
            let mutable cnt = 0
            for KeyValue (_, ms) in members do
                let parts = ResizeArray<Dictionary<string, Action> * ResizeArray<int> * Set<string> ref>()
                for st in ms do
                    let i = st.Index
                    let sg = signature st block
                    let placed =
                        parts |> Seq.tryFind (fun (merged, indices, mterms) ->
                            compatible sg merged && mergeable (List.ofSeq indices) mterms.Value [ i ] terminals.[i])
                    match placed with
                    | Some (merged, indices, mterms) ->
                        for k, a in sg do merged.[k] <- a
                        indices.Add i
                        mterms.Value <- Set.union mterms.Value terminals.[i]
                    | None ->
                        let merged = Dictionary<string, Action>()
                        for k, a in sg do merged.[k] <- a
                        parts.Add(merged, ResizeArray [ i ], ref terminals.[i])
                for _, indices, _ in parts do
                    for i in indices do newBlock.[i] <- cnt
                    cnt <- cnt + 1
            block <- newBlock
            if cnt = members.Count then finished <- true
        let mutable joined = true
        while joined do
            joined <- false
            let mergedActions = Dictionary<int, Dictionary<string, Action>>()
            let blockMembers = Dictionary<int, ResizeArray<int>>()
            let blocksOfCore = ResizeArray<Item list * ResizeArray<int>>()
            for st in states do
                let b = block.[st.Index]
                if not (mergedActions.ContainsKey b) then
                    mergedActions.[b] <- Dictionary()
                    appendTo blocksOfCore (coreKey st.Kernel) b
                for k, a in signature st block do mergedActions.[b].[k] <- a
                if not (blockMembers.ContainsKey b) then blockMembers.[b] <- ResizeArray()
                blockMembers.[b].Add st.Index
            let blockTerminals b = blockMembers.[b] |> Seq.fold (fun acc i -> Set.union acc terminals.[i]) Set.empty
            let asList (h: Dictionary<string, Action>) = h |> Seq.map (fun (KeyValue (k, a)) -> k, a) |> List.ofSeq
            let rec pairs l = match l with [] -> [] | b1 :: rest -> List.map (fun b2 -> b1, b2) rest @ pairs rest
            let candidate =
                blocksOfCore
                |> Seq.collect (fun (_, blocks) -> pairs (List.ofSeq blocks))
                |> Seq.tryFind (fun (b1, b2) ->
                    compatible (asList mergedActions.[b1]) mergedActions.[b2]
                    && mergeable (List.ofSeq blockMembers.[b1]) (blockTerminals b1) (List.ofSeq blockMembers.[b2]) (blockTerminals b2))
            match candidate with
            | Some (b1, b2) ->
                block <- block |> Array.map (fun b -> if b = b2 then b1 else b)
                joined <- true
            | None -> ()
        // the merged states are numbered in the order of their first canonical state
        let number = Dictionary<int, int>()
        let newIndex =
            block |> Array.map (fun b ->
                match number.TryGetValue b with
                | true, k -> k
                | _ ->
                    let k = number.Count
                    number.[b] <- k
                    k)
        let merged = Array.init number.Count (fun k -> { Index = k; Kernel = []; Items = []; Transitions = []; Actions = []; MergedFrom = [] })
        let unionInto (l: (Item * Lookaheads) list) (key, value) =
            if List.exists (fun (k, _) -> k = key) l then l |> List.map (fun (k, v) -> if k = key then k, Set.union v value else k, v)
            else l @ [ key, value ]
        let replace (l: ('k * 'v) list) (key: 'k, value: 'v) =
            if List.exists (fun (k, _) -> k = key) l then l |> List.map (fun (k, v) -> if k = key then k, value else k, v)
            else l @ [ key, value ]
        for st in states do
            let nw = merged.[newIndex.[st.Index]]
            nw.MergedFrom <- nw.MergedFrom @ [ st.Index ]
            nw.Kernel <- List.fold unionInto nw.Kernel st.Kernel
            nw.Items <- List.fold unionInto nw.Items st.Items
            nw.Transitions <- List.fold (fun l (name, t) -> replace l (name, newIndex.[t])) nw.Transitions st.Transitions
            nw.Actions <-
                List.fold (fun l (name, a) -> replace l (name, (match a with Shift t -> Shift newIndex.[t] | r -> r))) nw.Actions st.Actions
        let seen = HashSet<int * string * int list * int option>(HashIdentity.Structural)
        let mergedConflicts =
            conflicts
            |> List.choose (fun c ->
                let shiftState = c.ShiftState |> Option.map (fun s -> newIndex.[s])
                let chosen = match c.Chosen with Shift t -> Shift newIndex.[t] | r -> r
                let key = newIndex.[c.CState], c.CTerminal, c.CRules, shiftState
                if seen.Add key then Some { c with CState = newIndex.[c.CState]; ShiftState = shiftState; Chosen = chosen }
                else None)
            |> List.sortBy (fun c -> c.CState)
        // why states with the same core have been kept apart
        let statesOfCore = ResizeArray<Item list * ResizeArray<LR1State>>()
        for st in merged do appendTo statesOfCore (coreKey st.Kernel) st
        let keptApart = ResizeArray<int * int * Difference list>()
        for _, group in statesOfCore do
            let group = List.ofSeq group
            let rec go l =
                match l with
                | [] -> ()
                | s :: rest ->
                    for t in rest do
                        let find name (st: LR1State) = st.Actions |> List.find (fun (n, _) -> n = name) |> snd
                        let names =
                            s.Actions |> List.choose (fun (n, _) -> if List.exists (fun (m, _) -> m = n) t.Actions then Some n else None) |> sortStrings
                        let differences =
                            names |> List.choose (fun name ->
                                let a, b = find name s, find name t
                                if a <> b then Some(name, Some a, Some b, None) else None)
                        let ts, tt = terminalsOf s, terminalsOf t
                        let d1 = collisions (Set.difference tt ts) ts |> List.map (fun (name, other) -> name, None, Some(find name t), Some other)
                        let d2 = collisions (Set.difference ts tt) tt |> List.map (fun (name, other) -> name, Some(find name s), None, Some other)
                        keptApart.Add(s.Index, t.Index, differences @ d1 @ d2)
                    go rest
            go group
        { Start = start; Rules = rules; RulesByOrigin = rulesByOrigin; States = merged; Conflicts = mergedConflicts
          EndState = newIndex.[endState]; Lr1StateCount = lr1StateCount; MergeStates = true; KeptApart = List.ofSeq keptApart }

let parseTable (a: Analyzer) =
    let table =
        a.States |> Array.map (fun st ->
            let h = Dictionary<string, Action>()
            for name, act in st.Actions do h.[name] <- act
            h)
    { Table = table; StartState = 0; EndState = a.EndState }

/// the names of the symbols of every state, i.e. the terminals that the contextual lexer accepts
let acceptedSymbols (a: Analyzer) = a.States |> Array.map (fun st -> st.Actions |> List.map fst)

// ---------- The parser (lr_parser.py) ----------

let parse (pt: ParseTable) (rules: Rule array) (callbacks: ('v list -> 'v) array) (ofToken: Token -> 'v)
          (nextToken: ParserState -> Token option) (formatExpected: string list -> string) : 'v =
    let stateStack = Stack<int>()
    let valueStack = ResizeArray<'v>()
    stateStack.Push pt.StartState
    let current () = { StackHeight = stateStack.Count; Position = stateStack.Peek() }
    let feedToken (token: Token) (isEnd: bool) : 'v option =
        let mutable result = None
        let mutable go = true
        while go do
            let state = stateStack.Peek()
            match pt.Table.[state].TryGetValue token.Type with
            | false, _ ->
                let expected = pt.Table.[state].Keys |> Seq.filter isUpper |> List.ofSeq
                let expected = if state = pt.EndState && not isEnd then endSym :: expected else expected
                let what = if token.Type = endSym then "Unexpected end of input" else "Unexpected token " + tokenRepr token
                let message = sprintf "%s at line %d, column %d.\n%s" what token.Line token.Column (formatExpected (dedup expected))
                raise (UnexpectedInput
                           { Kind = UnexpectedToken(token, dedup expected); Line = token.Line; Column = token.Column
                             PosInStream = token.StartPos; State = Some(current ()); Message = message })
            | true, Shift target ->
                stateStack.Push target
                valueStack.Add(ofToken token)
                go <- false
            | true, Reduce r ->
                let rule = rules.[r]
                let size = List.length rule.Expansion
                let children = List.ofSeq (valueStack.GetRange(valueStack.Count - size, size))
                valueStack.RemoveRange(valueStack.Count - size, size)
                for _ in 1..size do stateStack.Pop() |> ignore
                let value = callbacks.[r] children
                match pt.Table.[stateStack.Peek()].TryGetValue rule.Origin with
                | true, Shift newState ->
                    stateStack.Push newState
                    valueStack.Add value
                    if isEnd && newState = pt.EndState then
                        result <- Some value
                        go <- false
                | _ -> failwith "Larkita: missing goto"
        result
    let mutable last = None
    let mutable go = true
    while go do
        match nextToken (current ()) with
        | Some token ->
            last <- Some token
            feedToken token false |> ignore
        | None -> go <- false
    // the end state is only reached by a goto, so after the last shift we always have to reduce
    let endToken =
        match last with
        | Some t -> { t with Type = endSym; Value = "" }
        | None -> { Type = endSym; Value = ""; StartPos = 0; Line = 1; Column = 1; EndLine = 0; EndColumn = 0; EndPos = 0 }
    match feedToken endToken true with
    | Some v -> v
    | None -> failwith "Larkita: the parser did not accept"

// ---------- Abstract syntax trees (ast_builder.py) ----------

type Kind =
    | KNode
    | KToken
    | KLiteral
    | KSplice

/// the positions of the children in the rule's expansion, together with their kind
let plan (rule: Rule) (auxNames: Set<string>) =
    rule.Expansion
    |> List.indexed
    |> List.choose (fun (i, sym) ->
        if sym.IsTerm then
            if sym.Text.IsSome then Some(i, KLiteral) elif not sym.FilterOut then Some(i, KToken) else None
        elif auxNames.Contains sym.Name then Some(i, KSplice)
        else Some(i, KNode))

let auxNames (rules: Rule list) = rules |> List.choose (fun r -> if r.Ast.IsNone then Some r.Origin else None) |> Set.ofList

/// the values on the stack of the parser
type Value =
    | AstValue of Ast
    | Kids of Ast list
    | TokenValue of Token

let makeCallback (rule: Rule) (aux: Set<string>) : Value list -> Value =
    let pl = plan rule aux
    let expansion = Array.ofList rule.Expansion
    let collect (children: Value array) =
        pl |> List.collect (fun (i, kind) ->
            match kind, children.[i] with
            | KNode, AstValue a -> [ a ]
            | KToken, TokenValue t -> [ Tok(t.Type, t.Value) ]
            | KLiteral, _ -> [ Lit expansion.[i].Text.Value ]
            | KSplice, Kids l -> l
            | _ -> failwith ("Larkita: unexpected value in rule " + ruleToString rule))
    match rule.Ast with
    | None -> fun children -> Kids(collect (Array.ofList children))
    | Some { Head = None } ->
        match pl with
        | [ (i, KToken) ] ->
            fun children ->
                match List.item i children with
                | TokenValue t -> AstValue(Tok(t.Type, t.Value))
                | v -> v
        | [ (i, KNode) ] -> fun children -> List.item i children
        | _ -> failwith ("Larkita: a pass-through rule needs exactly one child: " + ruleToString rule)
    | Some { Head = Some head } -> fun children -> AstValue(Node(head, collect (Array.ofList children)))

let formatHead (head: string list) =
    match head with
    | [ h ] -> jsonQuote h
    | hs -> "(" + String.Join(", ", List.map jsonQuote hs) + ")"

/// describes the abstract syntax tree that a reduction by rule produces, e.g. ("+", ast(expr), ast(product))
let describeAst (rule: Rule) (aux: Set<string>) =
    let parts =
        plan rule aux |> List.map (fun (i, kind) ->
            let sym = List.item i rule.Expansion
            match kind with
            | KNode -> sprintf "ast(%s)" sym.Name
            | KToken -> sprintf "(%s, text)" (jsonQuote sym.Name)
            | KLiteral -> jsonQuote sym.Text.Value
            | KSplice -> "*" + sym.Name)
    match rule.Ast with
    | None -> "[" + String.Join(", ", parts) + "]"
    | Some { Head = None } -> List.head parts
    | Some { Head = Some head } ->
        if List.isEmpty parts then "(" + formatHead head + ",)"
        else "(" + String.Join(", ", formatHead head :: parts) + ")"
