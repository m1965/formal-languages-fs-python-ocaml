    newDelta: TransRelSet = {}
    for q in Q:
        classOfQ = find_equivalence_class(q, EquivClasses)
        for c in Σ:
            newDelta[(classOfQ, c)] = find_equivalence_class(𝛿[q, c], EquivClasses)
    return EquivClasses, Σ, newDelta, newQ0, newAccept
