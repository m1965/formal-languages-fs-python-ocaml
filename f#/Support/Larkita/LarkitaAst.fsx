/// Larkita for F#: the types that the users of Larkita see (abstract syntax trees, tokens, exceptions).
/// Open this module to use the constructors Node, Tok, and Lit.
module LarkitaAst

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

/// Shift/reduce conflicts are reported by a warning: Print writes it to standard error, Raise raises
/// ConflictWarning, and Ignore drops it.
type WarningMode =
    | Print
    | Raise
    | Ignore

exception ConflictWarning of string
