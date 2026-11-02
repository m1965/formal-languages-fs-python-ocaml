def toNFA(self: RegExp2NFA, r: RegExp) -> NFA:
    match r:
        case 0:
            return self.genEmptyNFA()
        case '𝜀':
            return self.genEpsilonNFA()
        case r if isinstance(r, str) and len(r) == 1:
            return self.genCharNFA(r)
        case (r1, '⋅', r2):
            return self.catenate(self.toNFA(r1), self.toNFA(r2))
        case (r1, '+', r2):
            return self.disjunction(self.toNFA(r1), self.toNFA(r2))
        case (r1, '*'):
            return self.kleene(self.toNFA(r1))
        case _:
            raise ValueError(f'{r} is not a proper regular expression.')

RegExp2NFA.toNFA = toNFA # type: ignore
del toNFA
