/// Larkita for F#: a parser generator for canonical LR(1) parsers that return abstract syntax trees.
///
/// This is a port of the Python package Larkita by Karl Stroetmann
/// (https://github.com/karlstroetmann/Larkita).  A grammar is written in Lark's EBNF notation and
/// contains nothing but the grammar: no semantic actions, no token declarations, and no separate
/// scanner specification.  The abstract syntax tree follows from the grammar:
///
///   expr: expr "+" product              Node (["+"], [ast(expr); ast(product)])
///   stmnt: IDENTIFIER ":=" expr ";"     Node ([":="; ";"], [Tok ("IDENTIFIER", name); ast(expr)])
///   factor: "(" expr ")" %ignore "(", ")"   ast(expr)
///   atom: NUMBER                        Tok ("NUMBER", text)
///   program: statement*                 Node (["program"], [ast(s1); ...; ast(sn)])
///
/// Load it with
///
///   #load "../Support/Larkita/Larkita.fsx"
///   open Larkita
///   let parser = Larkita.openFile "calc.lark"          // writes calc.out
///   let ast = Larkita.parse parser "x := 1 + 2;"
module Larkita

#load "LarkitaBase.fsx" "LarkitaRegex.fsx" "LarkitaLexer.fsx" "LarkitaLr.fsx" "LarkitaReport.fsx" "LarkitaGrammar.fsx"

open System
open System.Collections.Generic
open System.Text

LarkitaGrammar.grammarsDir <- IO.Path.Combine(__SOURCE_DIRECTORY__, "grammars")

type Ast = LarkitaBase.Ast
type Token = LarkitaBase.Token
type UnexpectedInputInfo = LarkitaBase.UnexpectedInputInfo
type WarningMode = LarkitaBase.WarningMode
exception UnexpectedInput = LarkitaBase.UnexpectedInput
exception GrammarError = LarkitaBase.GrammarError
exception ConflictWarning = LarkitaBase.ConflictWarning
exception LexError = LarkitaBase.LexError

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
    /// A parser is shown like Python shows it: Larkita(open('calc.lark'), start='stmnt')
    override this.ToString() =
        sprintf "Larkita(open(%s), start=%s)" (LarkitaBase.pyRepr (defaultArg this.SourcePath "<string>")) (LarkitaBase.pyRepr this.Start)

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
let mutable conflictWarnings = LarkitaBase.Print

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

/// parse parser text: the abstract syntax tree of text.  Raises UnexpectedInput on a syntax error.
let parse (p: Parser) (text: string) : Ast = p.Run text

/// lex parser text: the tokens of the text, produced by a basic lexer; ignored tokens are skipped
let lex (p: Parser) (text: string) : Token list = LarkitaLexer.lexAll p.BasicLexer.Value text

/// the abstract syntax tree in Python's notation, e.g. ((':=', ';'), ('IDENTIFIER', 'x'), ...)
let toString (t: Ast) = LarkitaBase.astToString t

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

type private PyObj =
    | PStr of string
    | PTuple of PyObj list

let rec private pyobjOfAst (t: Ast) =
    match t with
    | LarkitaBase.Lit s -> PStr s
    | LarkitaBase.Tok (name, text) -> PTuple [ PStr name; PStr text ]
    | LarkitaBase.Node (head, children) ->
        let head = match head with [ h ] -> PStr h | hs -> PTuple(List.map PStr hs)
        PTuple(head :: List.map pyobjOfAst children)

let rec private pyobjRepr (o: PyObj) =
    match o with
    | PStr s -> LarkitaBase.pyRepr s
    | PTuple [ x ] -> "(" + pyobjRepr x + ",)"
    | PTuple xs -> "(" + String.Join(", ", List.map pyobjRepr xs) + ")"

/// the abstract syntax tree formatted like Python's pprint.pformat formats the nested tuple
let pformat (width: int) (t: Ast) =
    let b = StringBuilder()
    let rec format (o: PyObj) (indent: int) (allowance: int) =
        let rep = pyobjRepr o
        match o with
        | PTuple items when rep.Length > width - indent - allowance ->
            b.Append '(' |> ignore
            let endchar = if List.length items = 1 then ",)" else ")"
            formatItems items indent (allowance + endchar.Length)
            b.Append endchar |> ignore
        | _ -> b.Append rep |> ignore
    and formatItems (items: PyObj list) (indent: int) (allowance: int) =
        let indent = indent + 1
        let n = List.length items
        items |> List.iteri (fun i item ->
            if i > 0 then b.Append(",\n").Append(String(' ', indent)) |> ignore
            format item indent (if i = n - 1 then allowance else 1))
    format (pyobjOfAst t) 0 0
    b.ToString()

/// prints the abstract syntax tree like Python's pprint prints the nested tuple
let pprint (t: Ast) = printfn "%s" (pformat 80 t)
