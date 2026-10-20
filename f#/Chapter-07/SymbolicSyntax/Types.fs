namespace SymbolicSyntax

type BinaryOp = Add | Subtract | Multiply | Divide | Power
type UnaryOp = Exp | Ln
type Expr = Number of int | Variable | Binary of BinaryOp * Expr * Expr | Unary of UnaryOp * Expr
