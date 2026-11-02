let findEquivalenceClass state partition =
    partition |> Seq.find (Set.contains state)
