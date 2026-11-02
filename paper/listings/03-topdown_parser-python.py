def parseExprRest(Sum: float, TL: list[str]) -> tuple[float, list[str]]:
    match TL:
        case []:
            return Sum, []
        case '+', *RL:
            product, Rest = parseProduct(RL)
            return parseExprRest(Sum + product, Rest)
        case '-', *RL:
            product, Rest = parseProduct(RL)
            return parseExprRest(Sum - product, Rest)
        case _:
            return Sum, TL

def parseFactor(TL: list[str]) -> tuple[float, list[str]]:
    match TL:
        case '(', *RL:
            expr, Rest = parseExpr(RL)
            assert Rest[0] == ')', 'Parse Error: expected ")"'
            return expr, Rest[1:]
        case _:
            return float(TL[0]), TL[1:]
