namespace LectureCalculator
type Expr = Integer of bigint | Identifier of string | Binary of string * Expr * Expr
type Statement = Assignment of string * Expr | Print of Expr
