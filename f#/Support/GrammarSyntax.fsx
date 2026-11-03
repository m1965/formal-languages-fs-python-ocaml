/// Grammars for the parsing algorithms of chapters 8 and 9 (Earley, SLR, LR).
///
/// A grammar file is written in the notation of Larkita: the alternatives of a rule are separated by
/// "|", literals are enclosed in double quotes, and a rule is not terminated by a semicolon.  The
/// grammar of grammars below is the one of the Python notebooks; the abstract syntax tree that
/// Larkita returns is converted into the types Rule and Grammar.
module GrammarSyntax

#load "Larkita/Larkita.fsx"

type Symbol = Nonterminal of string | Terminal of string
type Rule = { Head: string; Body: Symbol list }
type Grammar = { Start: string; Rules: Rule list }

// Grammar productions and parse-table data cross notebook/module boundaries.
type MarkedRule = { Rule: Rule; Dot: int }
type Action = Shift of int | Reduce of Rule | Accept
type ParseTable<'Item when 'Item: comparison> =
    { States: Set<'Item> array
      Actions: Map<int * string, Set<Action>>
      Gotos: Map<int * string, int> }

/// The grammar of grammars.  RULE_NAME recognizes the variable that is defined by a rule: as an LR(1)
/// parser sees only one token ahead, the scanner tells a variable that is followed by a colon from
/// the next item of the previous rule.
let grammarOfGrammars = """
grammar : rule+
rule    : RULE_NAME ":" body ("|" body)*        %ignore ":", "|"
body    : item*
item    : VARIABLE | TOKEN | LITERAL

RULE_NAME.2 : /[a-z][a-z0-9_]*(?=[ \t]*:)/
VARIABLE    : /[a-z][a-z0-9_]*/
TOKEN       : /[A-Z][A-Z0-9_]*/
LITERAL     : /".*?"/
COMMENT     : /\/\/[^\n]*/

%import common.WS
%ignore WS, COMMENT
"""

module Parse =
    let private parser =
        Larkita.Larkita.Create(grammarOfGrammars, lexer = Larkita.Basic,
                               outFile = System.IO.Path.Combine(System.IO.Path.GetTempPath(), "grammar-of-grammars.out"))

    let private symbol (item: Larkita.Ast) =
        match item with
        | LarkitaBase.Tok ("VARIABLE", name) -> Nonterminal name
        | LarkitaBase.Tok ("TOKEN", name) -> Terminal name
        | LarkitaBase.Tok ("LITERAL", text) -> Terminal(text.Substring(1, text.Length - 2))   // strip the double quotes
        | t -> failwithf "Unexpected item %s" (Larkita.toString t)

    /// ('grammar', ('rule', ('RULE_NAME', v), ('body', item, ...), ...), ...) -> the list of rules
    let private rules (ast: Larkita.Ast) =
        match ast with
        | LarkitaBase.Node (_, rules) ->
            rules |> List.collect (function
                | LarkitaBase.Node (_, LarkitaBase.Tok (_, head) :: bodies) ->
                    bodies |> List.map (function
                        | LarkitaBase.Node (_, items) -> { Head = head; Body = List.map symbol items }
                        | t -> failwithf "Unexpected body %s" (Larkita.toString t))
                | t -> failwithf "Unexpected rule %s" (Larkita.toString t))
        | t -> failwithf "Unexpected grammar %s" (Larkita.toString t)

    let text source =
        let ast =
            try Larkita.parse parser source
            with Larkita.UnexpectedInput e -> failwithf "Grammar error at line %d, column %d: %s" e.Line e.Column e.Message
        let rules = rules ast
        if List.isEmpty rules then invalidArg "source" "A grammar needs at least one rule."
        let variables = rules |> List.map (fun rule -> rule.Head) |> Set.ofList
        for rule in rules do
            for symbol in rule.Body do
                match symbol with
                | Nonterminal name when not (Set.contains name variables) -> invalidArg "source" ("Undefined nonterminal: " + name)
                | _ -> ()
        { Start = rules.Head.Head; Rules = rules }

    let file path = text (System.IO.File.ReadAllText path)
