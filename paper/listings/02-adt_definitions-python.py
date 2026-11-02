type BinaryOp  = Literal['⋅', '+']
type UnaryOp   = Literal['*']
type NullaryOP = Literal[0]

Char = str
type RegExp = NullaryOP | Char | tuple[RegExp, UnaryOp] | tuple[RegExp, BinaryOp, RegExp]
