let regexp2nfa (alphabet: Set<char>) (expression: RegExp) : Nfa<int> =
    // State allocation is local to one conversion. No previous run affects its result.
    let mutable stateCount = 0
    let getNewState () =
        stateCount <- stateCount + 1
        stateCount
    let rec toNFA expression =
        match expression with
        | Empty -> genEmptyNFA alphabet (getNewState ()) (getNewState ())
        | Epsilon -> genEpsilonNFA alphabet (getNewState ()) (getNewState ())
        | Literal c -> genCharNFA alphabet c (getNewState ()) (getNewState ())
        | Concat (left, right) ->
            let first = toNFA left
            let second = toNFA right
            catenate first second
        | Alt (left, right) ->
            let first = toNFA left
            let second = toNFA right
            disjunction (getNewState ()) (getNewState ()) first second
        | Star expression ->
            let fragment = toNFA expression
            kleene (getNewState ()) (getNewState ()) fragment
    toNFA expression
