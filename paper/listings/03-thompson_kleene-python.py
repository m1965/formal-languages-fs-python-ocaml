def kleene(self: RegExp2NFA, f: NFA) -> NFA:
    Q, Sigma, delta0, q1, A = f
    q2, = A
    q0 = self.getNewState()
    q3 = self.getNewState()
    delta = delta0 | {}    # copy delta0, so that f is not changed
    delta[q0, '𝜀'] = { q1, q3 }
    delta[q2, '𝜀'] = { q1, q3 }
    return { q0, q3 } | Q, Sigma, delta, q0, { q3 }
