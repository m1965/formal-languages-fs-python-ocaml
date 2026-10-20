namespace GrammarSyntax

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
