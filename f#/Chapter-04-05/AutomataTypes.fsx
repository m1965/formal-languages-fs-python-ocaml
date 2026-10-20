/// Shared mathematical representations. Algorithms are defined in the notebooks.
type RegExp =
    | Empty
    | Epsilon
    | Literal of char
    | Concat of RegExp * RegExp
    | Alt of RegExp * RegExp
    | Star of RegExp

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
