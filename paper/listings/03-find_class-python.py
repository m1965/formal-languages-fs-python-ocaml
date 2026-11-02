def find_equivalence_class(p: State, Partition: set[frozenset[State]]) -> frozenset[State]:
    return arb({ C for C in Partition if p in C })
