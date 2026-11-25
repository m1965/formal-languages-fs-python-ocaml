/// Larkita for F#: a parser generator for canonical LR(1) parsers that return abstract syntax trees.
///
/// This is a port of the Python package Larkita by Karl Stroetmann
/// (https://github.com/karlstroetmann/Larkita).  A grammar is written in Lark's EBNF notation and
/// contains nothing but the grammar: no semantic actions, no token declarations, and no separate
/// scanner specification.  The abstract syntax tree follows from the grammar:
///
///   expr: expr "+" product              ("+", ast(expr), ast(product))
///   stmnt: IDENTIFIER ":=" expr ";"     ((":=", ";"), ("IDENTIFIER", name), ast(expr))
///   factor: "(" expr ")" %ignore "(", ")"   ast(expr)
///   atom: NUMBER                        ("NUMBER", text)
///   program: statement*                 ("program", ast(s1), ..., ast(sn))
///
/// parse returns these trees as values of the type nested of ../Nested.fsx, e.g. ("+", l, r) is
/// Node [Leaf (Str "+"); l; r].  Load it with
///
///   #load "../Support/Larkita/Larkita.fsx"
///   open Nested
///   open Larkita
///   let parser = Larkita.Open "calc.lark"          // writes calc.out
///   let ast = parse parser "x := 1 + 2;"
/// Larkita uses the module Nested of ../Nested.fsx, which has to be loaded first, once per session:
///   #load "../Support/Nested.fsx" "../Support/Larkita/Larkita.fsx"
module Larkita

#load "LarkitaAst.fsx" "LarkitaBase.fsx" "LarkitaRegex.fsx" "LarkitaLexer.fsx" "LarkitaLr.fsx" "LarkitaReport.fsx" "LarkitaGrammar.fsx"

open System
open System.Collections.Generic
open System.Text

LarkitaGrammar.grammarsDir <- IO.Path.Combine(__SOURCE_DIRECTORY__, "grammars")

type Ast = LarkitaAst.Ast
type Token = LarkitaAst.Token
type UnexpectedInputInfo = LarkitaAst.UnexpectedInputInfo
type WarningMode = LarkitaAst.WarningMode
exception UnexpectedInput = LarkitaAst.UnexpectedInput
exception GrammarError = LarkitaAst.GrammarError
exception ConflictWarning = LarkitaAst.ConflictWarning
exception LexError = LarkitaAst.LexError

type LexerKind =
    | Contextual
    | Basic

type Parser =
    { SourcePath: string option
      OutFile: string
      Start: string
      Terminals: LarkitaLexer.TerminalDef list
      Rules: LarkitaLr.Rule list
      IgnoreTokens: string list
      Conflicts: LarkitaLr.Conflict list
      Analyzer: LarkitaLr.Analyzer
      Conf: LarkitaLexer.LexerConf
      Run: string -> Ast
      BasicLexer: Lazy<LarkitaLexer.BasicLexer> }
    /// A parser is shown with its grammar file and its start symbol, e.g. <parser "calc.lark", start "stmnt">
    override this.ToString() =
        sprintf "<parser %s, start %s>" (Nested.quote (defaultArg this.SourcePath "<string>")) (Nested.quote this.Start)

let private makeParser (a: LarkitaLr.Analyzer) (conf: LarkitaLexer.LexerConf) (lexer: LexerKind) : string -> Ast =
    let table = LarkitaLr.parseTable a
    let aux = LarkitaLr.auxNames (List.tail (List.ofArray a.Rules))
    let callbacks = a.Rules |> Array.mapi (fun i r -> if i = 0 then (fun _ -> failwith "root") else LarkitaLr.makeCallback r aux)
    let next =
        match lexer with
        | Basic ->
            let lx = LarkitaLexer.basicLexer conf
            fun st state -> LarkitaLexer.nextToken lx st (Some state)
        | Contextual ->
            let cl = LarkitaLexer.contextualLexer conf (LarkitaLr.acceptedSymbols a)
            fun st state -> LarkitaLexer.contextualNextToken cl st state
    fun text ->
        let st = LarkitaLexer.lexerState text
        match LarkitaLr.parse table a.Rules callbacks LarkitaLr.TokenValue (next st) (LarkitaLexer.formatExpected conf.TerminalsByName) with
        | LarkitaLr.AstValue t -> t
        | _ -> failwith "Larkita: the start rule did not return an abstract syntax tree"

/// Treatment of shift/reduce conflict warnings, see withConflictWarnings
let mutable conflictWarnings = LarkitaAst.Print

/// create: generates a parser for the grammar (a string).
/// - start: the start symbol, by default the first rule of the grammar.
/// - lexer: Contextual (the default) only matches the terminals that the parser can accept in its
///   current state; Basic always matches all terminals.
/// - importPaths: additional directories for %import.
/// - sourcePath: the file name of the grammar, used for relative imports and to name the output file.
/// - outFile: the output file; by default sourcePath with the extension .out, or grammar.out.
/// - mergeStates: merge states with the same core, as long as this is safe.
type Larkita =
    static member Create(grammar: string, ?start: string, ?lexer: LexerKind, ?importPaths: string list, ?sourcePath: string,
                         ?outFile: string, ?gRegexFlags: string, ?mergeStates: bool) : Parser =
        let lexer = defaultArg lexer Contextual
        let source = defaultArg sourcePath "<string>"
        let g = LarkitaGrammar.loadGrammar grammar source (defaultArg importPaths [])
        let start =
            match start, g.FirstRule with
            | Some s, _ | None, Some s -> s
            | None, None -> raise (GrammarError "The grammar does not contain any rules")
        let terminals, rules, ignoreTokens = LarkitaGrammar.compileGrammar g [ start ]
        let outFile =
            match outFile, sourcePath with
            | Some f, _ -> f
            | None, Some p -> IO.Path.ChangeExtension(p, ".out")
            | None, None -> "grammar.out"
        let conf = LarkitaLexer.lexerConf (defaultArg gRegexFlags "") terminals ignoreTokens
        let collision = if lexer = Contextual then Some(LarkitaLexer.tokenCollisions conf) else None
        let a = LarkitaLr.build (defaultArg mergeStates false) collision rules start
        let terminalNames = Dictionary<string, string>()
        for t in terminals do terminalNames.[t.Name] <- LarkitaLexer.userRepr t
        LarkitaReport.writeReport outFile a terminalNames source
        let unresolved = a.Conflicts |> List.filter (fun c -> not c.Resolved)
        if not (List.isEmpty unresolved) then
            raise (GrammarError(
                sprintf "The grammar has %d unresolved reduce/reduce conflict(s), see %s:\n%s" (List.length unresolved) outFile
                    (String.Join("\n", unresolved |> List.map (fun c ->
                        sprintf "  * state %d on %s" c.CState (match terminalNames.TryGetValue c.CTerminal with | true, n -> n | _ -> c.CTerminal))))))
        let shiftReduce = a.Conflicts |> List.filter (fun c -> c.ShiftState.IsSome)
        if not (List.isEmpty shiftReduce) then
            LarkitaBase.conflictWarnings <- conflictWarnings
            LarkitaBase.warnConflict (sprintf "%d shift/reduce conflict(s) were resolved as shift, see %s" (List.length shiftReduce) outFile)
        { SourcePath = sourcePath; OutFile = outFile; Start = start; Terminals = terminals; Rules = rules; IgnoreTokens = ignoreTokens
          Conflicts = a.Conflicts; Analyzer = a; Conf = conf; Run = makeParser a conf lexer
          BasicLexer = lazy (LarkitaLexer.basicLexer conf) }

    /// reads the grammar from a file (Larkita.open of Python)
    static member Open(fileName: string, ?start: string, ?lexer: LexerKind, ?importPaths: string list, ?outFile: string,
                       ?gRegexFlags: string, ?mergeStates: bool) : Parser =
        let text = IO.File.ReadAllText fileName
        Larkita.Create(text, ?start = start, ?lexer = lexer, ?importPaths = importPaths, sourcePath = fileName, ?outFile = outFile,
                       ?gRegexFlags = gRegexFlags, ?mergeStates = mergeStates)

/// toNested t converts the internal tree t into a nested tuple:
///   Node ([h], cs)   ->  (h, c1, ..., cn)
///   Node (hs, cs)    ->  ((h1, ..., hk), c1, ..., cn)
///   Tok (n, text)    ->  (n, text)
///   Lit s            ->  s
let rec toNested (t: Ast) : Nested.nested =
    let str (s: string) = Nested.Leaf(Nested.Str s)
    match t with
    | LarkitaAst.Lit s -> str s
    | LarkitaAst.Tok (name, text) -> Nested.Node [ str name; str text ]
    | LarkitaAst.Node ([ h ], children) -> Nested.Node(str h :: List.map toNested children)
    | LarkitaAst.Node (hs, children) -> Nested.Node(Nested.Node(List.map str hs) :: List.map toNested children)

/// parse parser text: the abstract syntax tree of text.  Raises UnexpectedInput on a syntax error.
let parse (p: Parser) (text: string) : Nested.nested = toNested (p.Run text)

/// lex parser text: the tokens of the text, produced by a basic lexer; ignored tokens are skipped
let lex (p: Parser) (text: string) : Token list = LarkitaLexer.lexAll p.BasicLexer.Value text

/// toTestString t: the tree t with strings in single quotes, e.g. ((':=', ';'), ('IDENTIFIER', 'x'), ...),
/// the format of the test data in ../../../ocaml/tests/larkita_expected.json
let rec toTestString (t: Nested.nested) =
    match t with
    | Nested.Leaf (Nested.Str s) -> LarkitaBase.pyRepr s
    | Nested.Leaf v -> Nested.valueToString v
    | Nested.Node [ x ] -> "(" + toTestString x + ",)"
    | Nested.Node xs -> "(" + String.Join(", ", List.map toTestString xs) + ")"

let getContext (e: UnexpectedInputInfo) (text: string) = LarkitaBase.getContext 40 e text

/// the number of states of the parse table (len(parser.parse_table.states) in Python)
let numberOfStates (p: Parser) = p.Analyzer.States.Length

/// the text of the output file
let report (p: Parser) = IO.File.ReadAllText p.OutFile

let pyRepr (s: string) = LarkitaBase.pyRepr s

/// withConflictWarnings mode f calls f with the given treatment of conflict warnings, like
/// "with warnings.catch_warnings(): warnings.simplefilter('error', ConflictWarning)" for mode = Raise
let withConflictWarnings (mode: WarningMode) (f: unit -> 'a) : 'a =
    let saved = conflictWarnings
    conflictWarnings <- mode
    try f () finally conflictWarnings <- saved

// ---------- pprint ----------

/// the abstract syntax tree in tuple notation, split into several lines if it is too long
let pformat (width: int) (t: Nested.nested) = Nested.pformat width t

/// prints the abstract syntax tree in tuple notation
let pprint (t: Nested.nested) = Nested.pprint t
