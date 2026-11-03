/// Larkita for F#, part 1: the types and helpers shared by the other parts.
///
/// This is a port of the Python package Larkita by Karl Stroetmann
/// (https://github.com/karlstroetmann/Larkita), translated from the OCaml port in ocaml/lib/larkita*.ml.
/// The files are
///
///   LarkitaBase.fsx     types, exceptions, abstract syntax trees, Python-style quoting
///   LarkitaRegex.fsx    the widths and first characters of regular expressions (re._parser)
///   LarkitaLexer.fsx    terminals, tokens, the basic and the contextual lexer (lexer.py)
///   LarkitaLr.fsx       the canonical LR(1) automaton and the parser runtime (lr_analysis.py, lr_parser.py)
///   LarkitaReport.fsx   the output file (report.py)
///   LarkitaGrammar.fsx  the grammar loader (load_grammar.py)
///   Larkita.fsx         the public interface (larkita.py)
///
/// Positions count UTF-16 code units, which agrees with Python for all characters of the Basic
/// Multilingual Plane.
module LarkitaBase

open System
open System.Globalization
open System.Text

/// The abstract syntax tree that a parser returns.  Python represents it as a nested tuple
/// (head, child_1, ..., child_n); F# needs one type for all of them:
///
///   Node (["+"], [l; r])             ("+", l, r)
///   Node ([":="; ";"], [x; e])       ((":=", ";"), x, e)        a head of several literals
///   Node (["program"], children)     ("program", ...)            the rule name or an alias as head
///   Tok ("NUMBER", "42")             ("NUMBER", "42")            a named token
///   Lit "-"                          "-"                         a literal inside a repetition
type Ast =
    | Node of string list * Ast list
    | Tok of string * string
    | Lit of string

// ---------- Python's string functions ----------

let private isCased (c: char) =
    match CharUnicodeInfo.GetUnicodeCategory c with
    | UnicodeCategory.UppercaseLetter | UnicodeCategory.LowercaseLetter | UnicodeCategory.TitlecaseLetter -> true
    | _ -> false

/// str.isupper(): at least one cased character, and no lowercase character
let isUpper (s: string) =
    Seq.exists isCased s && not (Seq.exists Char.IsLower s)

let toUpper (s: string) = s.ToUpperInvariant()
let toLower (s: string) = s.ToLowerInvariant()

/// str.isprintable() for one character
let isPrintable (c: char) =
    c = ' '
    || match CharUnicodeInfo.GetUnicodeCategory c with
       | UnicodeCategory.Control | UnicodeCategory.Format | UnicodeCategory.Surrogate
       | UnicodeCategory.PrivateUse | UnicodeCategory.OtherNotAssigned | UnicodeCategory.LineSeparator
       | UnicodeCategory.ParagraphSeparator | UnicodeCategory.SpaceSeparator -> false
       | _ -> true

/// repr(s) of Python
let pyRepr (s: string) =
    let quote = if s.Contains '\'' && not (s.Contains '"') then '"' else '\''
    let b = StringBuilder()
    b.Append quote |> ignore
    for c in s do
        let n = int c
        if c = quote || c = '\\' then b.Append('\\').Append(c) |> ignore
        elif c = '\t' then b.Append "\\t" |> ignore
        elif c = '\n' then b.Append "\\n" |> ignore
        elif c = '\r' then b.Append "\\r" |> ignore
        elif n < 0x20 || n = 0x7F then b.AppendFormat("\\x{0:x2}", n) |> ignore
        elif n < 0x7F || isPrintable c then b.Append c |> ignore
        elif n <= 0xFF then b.AppendFormat("\\x{0:x2}", n) |> ignore
        else b.AppendFormat("\\u{0:x4}", n) |> ignore
    b.Append quote |> ignore
    b.ToString()

/// json.dumps(s, ensure_ascii=False)
let jsonQuote (s: string) =
    let b = StringBuilder()
    b.Append '"' |> ignore
    for c in s do
        match c with
        | '"' -> b.Append "\\\"" |> ignore
        | '\\' -> b.Append "\\\\" |> ignore
        | '\n' -> b.Append "\\n" |> ignore
        | '\r' -> b.Append "\\r" |> ignore
        | '\t' -> b.Append "\\t" |> ignore
        | '\b' -> b.Append "\\b" |> ignore
        | '\012' -> b.Append "\\f" |> ignore
        | c when int c < 0x20 -> b.AppendFormat("\\u{0:x4}", int c) |> ignore
        | c -> b.Append c |> ignore
    b.Append '"' |> ignore
    b.ToString()

/// The abstract syntax tree in Python's notation, e.g. ((':=', ';'), ('IDENTIFIER', 'x'), ...)
let rec astToString (t: Ast) =
    match t with
    | Lit s -> pyRepr s
    | Tok (name, text) -> sprintf "(%s, %s)" (pyRepr name) (pyRepr text)
    | Node (head, children) ->
        let head =
            match head with
            | [ h ] -> pyRepr h
            | hs -> "(" + String.Join(", ", List.map pyRepr hs) + ")"
        if List.isEmpty children then "(" + head + ",)"
        else "(" + String.Join(", ", head :: List.map astToString children) + ")"

/// ljust for the columns of the output file
let ljust (s: string) (n: int) = s + String(' ', max 0 (n - s.Length))

// ---------- Exceptions ----------

exception GrammarError of string
exception LexError of string
exception ConfigurationError of string

/// A token of the scanner.  Positions count characters from 0, lines and columns from 1.
type Token =
    { Type: string
      Value: string
      StartPos: int
      Line: int
      Column: int
      EndLine: int
      EndColumn: int
      EndPos: int }

let tokenRepr (t: Token) = sprintf "Token(%s, %s)" (pyRepr t.Type) (pyRepr t.Value)

/// The state of the parser when an error was detected: the height of its stack and its current state.
type ParserState = { StackHeight: int; Position: int }

type UnexpectedKind =
    | UnexpectedCharacters of char: string * allowed: string list
    | UnexpectedToken of token: Token * expected: string list

/// raised by parse if the text does not match the grammar; the subclasses UnexpectedCharacters and
/// UnexpectedToken of Python are the two kinds
type UnexpectedInputInfo =
    { Kind: UnexpectedKind
      Line: int
      Column: int
      PosInStream: int
      State: ParserState option
      Message: string }
    override this.ToString() = this.Message

exception UnexpectedInput of UnexpectedInputInfo
    with
        override this.Message = this.Data0.Message

/// getContext e text: the line of text containing the error, and a line with a marker below the error
let getContext (span: int) (e: UnexpectedInputInfo) (text: string) =
    let n = text.Length
    let pos = min e.PosInStream n
    let start = max (pos - span) 0
    let stop = min (pos + span) n
    let before = text.Substring(start, pos - start)
    let before =
        match before.LastIndexOf '\n' with
        | -1 -> before
        | i -> before.Substring(i + 1)
    let after = text.Substring(pos, stop - pos)
    let after =
        match after.IndexOf '\n' with
        | -1 -> after
        | i -> after.Substring(0, i)
    // len(before.expandtabs())
    let mutable width = 0
    for c in before do
        if c = '\t' then width <- (width / 8 + 1) * 8 else width <- width + 1
    before + after + "\n" + String(' ', width) + "^\n"

/// Shift/reduce conflicts are reported by a warning.  conflictWarnings decides what happens:
/// Print writes it to standard error (like Python's default), Raise raises ConflictWarning
/// (like warnings.simplefilter('error')), and Ignore drops it.
type WarningMode =
    | Print
    | Raise
    | Ignore

exception ConflictWarning of string

let mutable conflictWarnings = Print

let warnConflict (msg: string) =
    match conflictWarnings with
    | Print -> eprintfn "ConflictWarning: %s" msg
    | Raise -> raise (ConflictWarning msg)
    | Ignore -> ()

// ---------- Lists ----------

/// removes duplicates, keeping the first occurrence (dict.fromkeys)
let dedupWith (eq: 'a -> 'a -> bool) (l: 'a list) =
    let result = ResizeArray<'a>()
    for x in l do
        if not (Seq.exists (eq x) result) then result.Add x
    List.ofSeq result

let dedup (l: 'a list) = dedupWith (=) l

let take n (l: 'a list) = List.truncate (max 0 n) l
let drop n (l: 'a list) = if n <= 0 then l elif n >= List.length l then [] else List.skip n l
