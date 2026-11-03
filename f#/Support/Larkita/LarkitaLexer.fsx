/// Larkita for F#, part 3: terminals, tokens, and the lexers (lexer.py).
///
/// The regular expressions of the terminals are written in Python's syntax and matched with .NET's
/// Regex, whose syntax is almost the same; translateRegexp bridges the differences.  Like Python's
/// re module, the lexer tries the terminals in a fixed order and takes the first one that matches
/// (not the longest one).  A string literal like "while" that is also matched by a regular expression
/// like CNAME is found via the regular expression and then renamed (the "unless" callback of Lark).
module LarkitaLexer

#load "LarkitaBase.fsx" "LarkitaRegex.fsx"

open System
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open LarkitaBase

// ---------- Patterns ----------

type Pattern =
    { IsStr: bool               // PatternStr (a string literal) or PatternRE (a regular expression)
      Value: string
      Flags: string             // the flags as a sorted string without duplicates, e.g. "i"
      Raw: string option }      // the literal as written in the grammar, e.g. "\"+\""

let normalizeFlags (flags: string) = String(flags |> Seq.distinct |> Seq.sort |> Array.ofSeq)

let patternStr raw flags value = { IsStr = true; Value = value; Flags = normalizeFlags flags; Raw = raw }
let patternRe raw flags value = { IsStr = false; Value = value; Flags = normalizeFlags flags; Raw = raw }

/// Patterns are equal if they have the same type, value, and flags (Raw is not compared).
let patternEqual (p: Pattern) (q: Pattern) = p.IsStr = q.IsStr && p.Value = q.Value && p.Flags = q.Flags

/// re.escape of Python
let reEscape (s: string) =
    let b = StringBuilder()
    for c in s do
        if "()[]{}?*+-|^$\\.&~# \t\n\r\011\012".Contains c then b.Append '\\' |> ignore
        b.Append c |> ignore
    b.ToString()

let toRegexp (p: Pattern) =
    let value = if p.IsStr then reEscape p.Value else p.Value
    p.Flags |> Seq.fold (fun v f -> sprintf "(?%c:%s)" f v) value

let private widthCache = Dictionary<string, int64 * int64>()

let width (p: Pattern) =
    if p.IsStr then int64 p.Value.Length, int64 p.Value.Length
    else
        let r = toRegexp p
        match widthCache.TryGetValue r with
        | true, w -> w
        | _ ->
            let w = LarkitaRegex.width r
            widthCache.[r] <- w
            w

let minWidth p = fst (width p)
let maxWidth p = snd (width p)

/// repr(pattern) of Python: the repr of the regular expression
let patternRepr (p: Pattern) = pyRepr (toRegexp p)

// ---------- Translation to .NET ----------

/// Translates a regular expression from Python's syntax into .NET's syntax:
/// (?P<name> -> (?<name>,  (?P=name) -> \k<name>,  \Z -> \z,  and the flags u, L, a are dropped.
let translateRegexp (r: string) =
    let n = r.Length
    let b = StringBuilder()
    let mutable i = 0
    while i < n do
        let c = r.[i]
        if c = '\\' && i + 1 < n then
            let d = r.[i + 1]
            if d = 'Z' then b.Append @"\z" |> ignore
            else b.Append(c).Append(d) |> ignore
            i <- i + 2
        elif c = '(' && i + 2 < n && r.[i + 1] = '?' && r.[i + 2] = 'P' then
            if i + 3 < n && r.[i + 3] = '<' then
                b.Append "(?<" |> ignore
                i <- i + 4
            elif i + 3 < n && r.[i + 3] = '=' then
                let close = r.IndexOf(')', i)
                b.Append(@"\k<").Append(r.Substring(i + 4, close - i - 4)).Append(">") |> ignore
                i <- close + 1
            else
                b.Append c |> ignore
                i <- i + 1
        elif c = '(' && i + 1 < n && r.[i + 1] = '?' then
            let mutable j = i + 2
            while j < n && "aiLmsux-".Contains r.[j] do j <- j + 1
            if j > i + 2 && j < n && (r.[j] = ':' || r.[j] = ')') then
                let flags = r.Substring(i + 2, j - i - 2) |> String.filter (fun f -> not ("uLa".Contains f))
                let flags = if flags.EndsWith "-" then flags.Substring(0, flags.Length - 1) else flags
                if r.[j] = ':' then b.Append("(?").Append(flags).Append(":") |> ignore
                elif flags <> "" then b.Append("(?").Append(flags).Append(")") |> ignore
                i <- j + 1
            else
                b.Append "(?" |> ignore
                i <- i + 2
        else
            b.Append c |> ignore
            i <- i + 1
    b.ToString()

let private regexCache = Dictionary<string, Regex>()

/// compiles a regular expression in Python's syntax (anchored at the start position); raises
/// ArgumentException if .NET rejects it
let compile (r: string) =
    match regexCache.TryGetValue r with
    | true, rex -> rex
    | _ ->
        let rex = Regex(@"\G(?:" + translateRegexp r + ")", RegexOptions.CultureInvariant)
        regexCache.[r] <- rex
        rex

let private fullCache = Dictionary<string, Regex>()

/// the regular expression r anchored at both ends (re.fullmatch)
let compileFull (r: string) =
    match fullCache.TryGetValue r with
    | true, rex -> rex
    | _ ->
        let rex = Regex(@"\A(?:" + translateRegexp r + @")\z", RegexOptions.CultureInvariant)
        fullCache.[r] <- rex
        rex

/// _get_match: the text that the regular expression r matches at the start of s
let getMatch (gFlags: string) (r: string) (s: string) : string option =
    let r = if gFlags = "" then r else sprintf "(?%s)%s" gFlags r
    let m = (compile r).Match(s, 0)
    if m.Success then Some m.Value else None

// ---------- Terminals ----------

type TerminalDef =
    { Name: string
      Pattern: Pattern
      Priority: int
      mutable FromLiteral: bool }    // created for a literal (string or regular expression) used in a rule

let terminalDef priority name pattern = { Name = name; Pattern = pattern; Priority = priority; FromLiteral = false }

/// how a terminal is shown to the user: a literal as it is written in the grammar, otherwise its name
let userRepr (t: TerminalDef) =
    if t.FromLiteral then (match t.Pattern.Raw with Some r when r <> "" -> r | _ -> t.Name) else t.Name

type LexerConf =
    { Terminals: TerminalDef list
      TerminalsByName: Dictionary<string, TerminalDef>
      Ignore: string list
      GRegexFlags: string
      SkipValidation: bool }

let lexerConf gRegexFlags (terminals: TerminalDef list) ignore =
    let byName = Dictionary<string, TerminalDef>()
    for t in terminals do byName.[t.Name] <- t
    { Terminals = terminals; TerminalsByName = byName; Ignore = ignore; GRegexFlags = gRegexFlags; SkipValidation = false }

let formatExpected (byName: Dictionary<string, TerminalDef>) (expected: string list) =
    let expected =
        expected
        |> List.sortWith (fun a b -> String.CompareOrdinal(a, b))
        |> List.map (fun n -> match byName.TryGetValue n with | true, t -> userRepr t | _ -> n)
    "Expected one of: \n\t* " + String.Join("\n\t* ", expected) + "\n"

// ---------- Scanner ----------

/// A scanner matches the alternative of the regular expressions of several terminals.  Every
/// terminal is wrapped in a named group __t<i>, so that the scanner knows which terminal has matched.
type Scanner =
    { ScTerminals: TerminalDef array
      Rex: Regex
      Full: Lazy<Regex>
      AllowedTypes: string list }

let makeScanner (terminals: TerminalDef list) (gFlags: string) =
    let terminals = Array.ofList terminals
    let alternatives =
        terminals
        |> Array.mapi (fun i t -> sprintf "(?P<__t%d>%s)" i (toRegexp t.Pattern))
        |> fun a -> String.Join("|", a)
    let prefix = if gFlags = "" then "" else sprintf "(?%s)" gFlags
    { ScTerminals = terminals
      Rex = (if terminals.Length = 0 then compile "(?!)" else compile (prefix + alternatives))
      Full = lazy (compileFull (prefix + alternatives))
      AllowedTypes = terminals |> Array.map (fun t -> t.Name) |> List.ofArray }

let private matchedTerminal (sc: Scanner) (m: Match) =
    let mutable result = ""
    let mutable k = 0
    while result = "" && k < sc.ScTerminals.Length do
        if m.Groups.["__t" + string k].Success then result <- sc.ScTerminals.[k].Name
        k <- k + 1
    result

/// the text and the type of the token at position pos
let scannerMatch (sc: Scanner) (text: string) (pos: int) : (string * string) option =
    if sc.ScTerminals.Length = 0 then None
    else
        let m = sc.Rex.Match(text, pos)
        if m.Success then Some(m.Value, matchedTerminal sc m) else None

let scannerFullmatch (sc: Scanner) (text: string) : string option =
    let m = sc.Full.Value.Match text
    if m.Success then Some(matchedTerminal sc m) else None

/// Expressions that may indicate newlines in a regexp
let regexpHasNewline (r: string) =
    r.Contains '\n' || r.Contains @"\n" || r.Contains @"\s" || r.Contains "[^" || (r.Contains "(?s" && r.Contains '.')

// ---------- The basic lexer ----------

/// the state of the lexer while it scans a text
type LexerState =
    { Text: string
      mutable Pos: int
      mutable Line: int
      mutable LineStartPos: int
      mutable LastToken: Token option }

let lexerState (text: string) = { Text = text; Pos = 0; Line = 1; LineStartPos = 0; LastToken = None }

let column (st: LexerState) = st.Pos - st.LineStartPos + 1

/// LineCounter.feed
let feed (st: LexerState) (value: string) (testNewline: bool) =
    if testNewline then
        let last = value.LastIndexOf '\n'
        if last >= 0 then
            for c in value do
                if c = '\n' then st.Line <- st.Line + 1
            st.LineStartPos <- st.Pos + last + 1
    st.Pos <- st.Pos + value.Length

type BasicLexer =
    { Conf: LexerConf
      NewlineTypes: string list
      IgnoreTypes: string list
      Scanner: Lazy<Scanner>
      Callback: Lazy<(string * Scanner) list> }

/// _create_unless: string literals that are also matched by a regular expression
let createUnless (terminals: TerminalDef list) (gFlags: string) =
    let res = terminals |> List.filter (fun t -> not t.Pattern.IsStr)
    let strs = terminals |> List.filter (fun t -> t.Pattern.IsStr)
    let embedded = HashSet<string>()
    let callback =
        res
        |> List.choose (fun retok ->
            let unless =
                strs
                |> List.filter (fun strtok ->
                    strtok.Priority = retok.Priority
                    && (let s = strtok.Pattern.Value
                        if getMatch gFlags (toRegexp retok.Pattern) s = Some s then
                            if strtok.Pattern.Flags |> Seq.forall (fun f -> retok.Pattern.Flags.Contains f) then
                                embedded.Add strtok.Name |> ignore
                            true
                        else false))
            if List.isEmpty unless then None else Some(retok.Name, makeScanner unless gFlags))
    terminals |> List.filter (fun t -> not (embedded.Contains t.Name)), callback

let sortKey (t: TerminalDef) = -(int64 t.Priority), -(maxWidth t.Pattern), -(t.Pattern.Value.Length), t.Name

let compareKey (a: TerminalDef) (b: TerminalDef) =
    let (p1, w1, l1, n1), (p2, w2, l2, n2) = sortKey a, sortKey b
    match compare p1 p2 with
    | 0 ->
        match compare w1 w2 with
        | 0 ->
            match compare l1 l2 with
            | 0 -> String.CompareOrdinal(n1, n2)
            | c -> c
        | c -> c
    | c -> c

let basicLexer (conf: LexerConf) =
    let terminals = conf.Terminals
    if not conf.SkipValidation then
        for t in terminals do
            (try compile (toRegexp t.Pattern) |> ignore
             with :? ArgumentException -> raise (LexError(sprintf "Cannot compile token %s: %s" t.Name (patternRepr t.Pattern))))
            if minWidth t.Pattern = 0L then
                raise (LexError(sprintf "Lexer does not allow zero-width terminals. (%s: %s)" t.Name (patternRepr t.Pattern)))
        let names = terminals |> List.map (fun t -> t.Name)
        let missing = conf.Ignore |> List.filter (fun n -> not (List.contains n names))
        if not (List.isEmpty missing) then
            raise (LexError("Ignore terminals are not defined: {" + String.Join(", ", List.map pyRepr (dedup missing)) + "}"))
    let sorted = List.sortWith compareKey terminals
    let unless = lazy (createUnless sorted conf.GRegexFlags)
    { Conf = conf
      NewlineTypes = terminals |> List.filter (fun t -> regexpHasNewline (toRegexp t.Pattern)) |> List.map (fun t -> t.Name)
      IgnoreTypes = conf.Ignore
      Scanner = lazy (makeScanner (fst unless.Value) conf.GRegexFlags)
      Callback = lazy (snd unless.Value) }

let unexpectedCharacters (st: LexerState) (allowed: string list) (state: ParserState option) byName =
    let char = string st.Text.[st.Pos]
    let e0 =
        { Kind = UnexpectedCharacters(char, allowed); Line = st.Line; Column = column st
          PosInStream = st.Pos; State = state; Message = "" }
    let message =
        sprintf "No terminal matches '%s' in the current parser context, at line %d col %d" char st.Line (column st)
        + "\n\n" + getContext 40 e0 st.Text
        + (if List.isEmpty allowed then "" else formatExpected byName allowed)
        + (match st.LastToken with
           | Some t -> "\nPrevious tokens: " + tokenRepr t + "\n"
           | None -> "")
    UnexpectedInput { e0 with Message = message }

/// nextToken: the next token that is not ignored, or None at the end of the text
let nextToken (lx: BasicLexer) (st: LexerState) (state: ParserState option) : Token option =
    let scanner = lx.Scanner.Value
    let callback = lx.Callback.Value
    let rec loop () =
        if st.Pos >= st.Text.Length then None
        else
            match scannerMatch scanner st.Text st.Pos with
            | None ->
                let allowed = scanner.AllowedTypes |> List.filter (fun n -> not (List.contains n lx.IgnoreTypes))
                let allowed = if List.isEmpty allowed then [ "<END-OF-FILE>" ] else allowed
                raise (unexpectedCharacters st (dedup allowed) state lx.Conf.TerminalsByName)
            | Some (value, type_) ->
                let ignored = List.contains type_ lx.IgnoreTypes
                let startPos, line, col = st.Pos, st.Line, column st
                feed st value (List.contains type_ lx.NewlineTypes)
                if ignored then loop ()
                else
                    let type_ =
                        match List.tryFind (fun (n, _) -> n = type_) callback with
                        | Some (_, sc) -> defaultArg (scannerFullmatch sc value) type_
                        | None -> type_
                    let t =
                        { Type = type_; Value = value; StartPos = startPos; Line = line; Column = col
                          EndLine = st.Line; EndColumn = column st; EndPos = st.Pos }
                    st.LastToken <- Some t
                    Some t
    loop ()

/// all tokens of the text (Larkita.lex)
let lexAll (lx: BasicLexer) (text: string) : Token list =
    let st = lexerState text
    let result = ResizeArray<Token>()
    let mutable go = true
    while go do
        match nextToken lx st None with
        | Some t -> result.Add t
        | None -> go <- false
    List.ofSeq result

// ---------- The contextual lexer ----------

type ContextualLexer = { Lexers: BasicLexer array; RootLexer: BasicLexer }

/// states.[i] are the symbols that state i accepts
let contextualLexer (conf: LexerConf) (states: string list array) =
    let byTokens = Dictionary<string, BasicLexer>()
    let lexers =
        states
        |> Array.map (fun accepts ->
            let key = String.Join("\u0000", accepts |> List.distinct |> List.sortWith (fun a b -> String.CompareOrdinal(a, b)))
            match byTokens.TryGetValue key with
            | true, lx -> lx
            | _ ->
                let accepts = (accepts @ conf.Ignore) |> List.distinct |> List.sortWith (fun a b -> String.CompareOrdinal(a, b))
                let terminals = accepts |> List.choose (fun n -> match conf.TerminalsByName.TryGetValue n with | true, t -> Some t | _ -> None)
                let lx = basicLexer { conf with Terminals = terminals }
                byTokens.[key] <- lx
                lx)
    { Lexers = lexers; RootLexer = basicLexer { conf with SkipValidation = true } }

/// the next token in the parser state state
let contextualNextToken (cl: ContextualLexer) (st: LexerState) (state: ParserState) : Token option =
    try nextToken cl.Lexers.[state.Position] st (Some state)
    with UnexpectedInput ({ Kind = UnexpectedCharacters (_, allowed) } as e) ->
        let lastToken = st.LastToken
        let token =
            try nextToken cl.RootLexer st (Some state)
            with UnexpectedInput { Kind = UnexpectedCharacters _ } -> raise (UnexpectedInput e)
        match token with
        | None -> raise (UnexpectedInput e)
        | Some token ->
            let message =
                sprintf "Unexpected token %s at line %d, column %d.\n%s" (tokenRepr token) token.Line token.Column
                    (formatExpected cl.RootLexer.Conf.TerminalsByName allowed)
                + sprintf "Previous tokens: [%s]\n" (match lastToken with Some t -> tokenRepr t | None -> "None")
            raise (UnexpectedInput
                       { Kind = UnexpectedToken(token, allowed); Line = token.Line; Column = token.Column
                         PosInStream = token.StartPos; State = Some state; Message = message })

// ---------- Collisions (for merging states with the contextual lexer) ----------

/// tokenCollisions conf returns a function collision nw accepted that tells whether the contextual
/// lexer might split some input differently if the terminal nw is added to the terminals accepted
/// by a parser state.  It returns the colliding terminal, or None.
let tokenCollisions (conf: LexerConf) : string -> string list -> string option =
    let firstChars = Dictionary<string, Set<string> option>()
    let collides = Dictionary<string * string, bool>()
    let first (t: TerminalDef) =
        match firstChars.TryGetValue t.Name with
        | true, f -> f
        | _ ->
            let f = LarkitaRegex.firstChars (toRegexp t.Pattern)
            firstChars.[t.Name] <- f
            f
    let collide (nw: TerminalDef) (old: TerminalDef) =
        match first nw, first old with
        | Some a, Some b when Set.isEmpty (Set.intersect a b) -> false
        | _ ->
            if (not nw.Pattern.IsStr) && old.Pattern.IsStr then true
            elif nw.Pattern.IsStr && (not old.Pattern.IsStr) && nw.Priority = old.Priority
                 && getMatch conf.GRegexFlags (toRegexp old.Pattern) nw.Pattern.Value = Some nw.Pattern.Value then true
            else compareKey nw old < 0
    fun nw accepted ->
        match conf.TerminalsByName.TryGetValue nw with
        | false, _ -> None
        | true, nwDef ->
            let names = (accepted @ conf.Ignore) |> List.distinct |> List.sortWith (fun a b -> String.CompareOrdinal(a, b))
            names
            |> List.tryFind (fun name ->
                match conf.TerminalsByName.TryGetValue name with
                | true, old when name <> nw ->
                    let key = nw, name
                    match collides.TryGetValue key with
                    | true, c -> c
                    | _ ->
                        let c = collide nwDef old
                        collides.[key] <- c
                        c
                | _ -> false)
