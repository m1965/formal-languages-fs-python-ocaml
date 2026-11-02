def assign_numbers(address: tuple[int, ...], 
                   t: AST, 
                   Nodes2Numbers: NodeMap, 
                   n: int =0) -> int:
    Nodes2Numbers[address] = str(n)
    if isinstance(t, str) or isinstance(t, int) or isinstance(t, float):
        return n + 1
    n += 1
    j  = 1
    for t in t[1:]:
        n = assign_numbers(address + (j,), t, Nodes2Numbers, n)
        j += 1
    return n
