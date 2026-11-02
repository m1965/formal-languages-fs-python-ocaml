type Flat = (str | tuple[Literal['*'], Flat] | tuple[Literal['+'], frozenset[Flat]]
                 | tuple[Literal['⋅'], tuple[Flat, ...]])

Empty:   Flat = ('+', frozenset())
Epsilon: Flat = ('⋅', ())

@cache
def nullable(r: Flat) -> bool:
    match r:
        case ('*', _):
            return True
        case ('+', S):
            return any(nullable(s) for s in S)
        case ('⋅', F):
            return all(nullable(f) for f in F)
        case _:
            return False
