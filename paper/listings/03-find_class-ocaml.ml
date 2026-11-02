let find_equivalence_class (p : 'q) (partition : 'q FSet.t FSet.t) : 'q FSet.t =
  arb (FSet.filter (fun c -> FSet.mem p c) partition)
