def arb[S](M: set[S] | frozenset[S]) -> S:
    x, *_ = M
    return x

def separate(Pairs: set[Pair], States: set[State], Σ: set[Char], 𝛿: TransRel) -> set[Pair]:
    Result = { (q1, q2) for q1 in States
                        for q2 in States
                        for c  in Σ
                        if (𝛿[q1, c], 𝛿[q2, c]) in Pairs
             }
    return Result

def all_separable(Q: set[State], A: set[State], Σ: set[Char], 𝛿: TransRel) -> set[Pair]:
        Separable = cart_prod(Q - A, A) | cart_prod(A, Q - A)
        while True:
            NewPairs = separate(Separable, Q, Σ, 𝛿)
            if NewPairs <= Separable:
                return Separable
            Separable |= NewPairs
