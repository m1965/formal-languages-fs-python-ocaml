    let classOf q = findEquivalenceClass q classes
    let transitions =
        seq { for source in states do
                  for c in dfa.Alphabet do
                      yield (classOf source, c), classOf dfa.Transitions[source, c] }
        |> Map.ofSeq
