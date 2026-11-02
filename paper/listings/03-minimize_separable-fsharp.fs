let arb states =
    if Set.isEmpty states then invalidArg "states" "Cannot choose an element of an empty set."
    Set.minElement states

let separate pairs states alphabet (delta: Map<'state * char, 'state>) =
    seq { for q1 in states do
              for q2 in states do
                  if alphabet |> Seq.exists (fun c -> Set.contains (delta[q1, c], delta[q2, c]) pairs) then
                      yield q1, q2 }
    |> Set.ofSeq

let allSeparable states accepting alphabet delta =
    let accepting = Set.intersect states accepting
    let nonAccepting = Set.difference states accepting
    let initial = Set.union (cartesianProduct nonAccepting accepting) (cartesianProduct accepting nonAccepting)
    let rec expand pairs =
        let newPairs = separate pairs states alphabet delta
        if Set.isSubset newPairs pairs then pairs else expand (Set.union pairs newPairs)
    expand initial
