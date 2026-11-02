let singleFinal (nfa: Nfa<int>) =
    if Set.count nfa.Accepting <> 1 then invalidArg "nfa" "A Thompson fragment must have exactly one final state."
    Set.minElement nfa.Accepting

let kleene q0 q3 (fragment: Nfa<int>) : Nfa<int> =
    let transitions =
        fragment.Transitions
        |> Map.add (q0, Eps) (set [fragment.Start; q3])
        |> Map.add (singleFinal fragment, Eps) (set [fragment.Start; q3])
    { fragment with States = Set.union (set [q0; q3]) fragment.States
                    Transitions = transitions; Start = q0; Accepting = set [q3] }
