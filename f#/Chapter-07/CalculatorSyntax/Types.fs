namespace CalculatorSyntax

type BinaryOp = Add | Subtract | Multiply | Divide
type Expr = Number of float | Variable of string | Binary of BinaryOp * Expr * Expr
type Statement = Assign of string * Expr | Evaluate of Expr
