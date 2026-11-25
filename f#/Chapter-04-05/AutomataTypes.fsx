/// Shared mathematical representations. Algorithms are defined in the notebooks.
/// The module Nested of ../Support/Nested.fsx has to be loaded first, once per session.
open Nested

/// Regular expressions are nested tuples of the type nested (see ../Support/Nested.fsx):
///   ∅          the number 0               Leaf (Int 0)
///   𝜀          the string "𝜀"             Leaf (Str "𝜀")
///   c          the string "c"             Leaf (Str "c")
///   r*         the pair (r, "*")          Node [ r; Leaf (Str "*") ]
///   r1 ⋅ r2    the triple (r1, "⋅", r2)   Node [ r1; Leaf (Str "⋅"); r2 ]
///   r1 + r2    the triple (r1, "+", r2)   Node [ r1; Leaf (Str "+"); r2 ]
type RegExp = nested

let empty : RegExp = Leaf (Int 0)
let epsilon : RegExp = Leaf (Str "𝜀")
let literal (c: char) : RegExp = Leaf (Str (string c))
let star (r: RegExp) : RegExp = Node [ r; Leaf (Str "*") ]
let concat (r1: RegExp) (r2: RegExp) : RegExp = Node [ r1; Leaf (Str "⋅"); r2 ]
let alt (r1: RegExp) (r2: RegExp) : RegExp = Node [ r1; Leaf (Str "+"); r2 ]

/// Epsilon is not an input character: it cannot accidentally enter the alphabet.
type Symbol = Eps | Input of char

type Nfa<'state when 'state : comparison> = {
    States: Set<'state>
    Alphabet: Set<char>
    Transitions: Map<'state * Symbol, Set<'state>>
    Start: 'state
    Accepting: Set<'state>
}

type Dfa<'state when 'state : comparison> = {
    States: Set<'state>
    Alphabet: Set<char>
    Transitions: Map<'state * char, 'state>
    Start: 'state
    Accepting: Set<'state>
}
