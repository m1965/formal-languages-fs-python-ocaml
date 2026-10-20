namespace IntegerC

/// Static types make malformed tuple-shaped ASTs unrepresentable.
type Expr =
    | Number of int
    | Variable of string
    | Binary of string * Expr * Expr
    | Not of Expr
    | Call of string * Expr list
    | Conditional of Expr * Expr * Expr

type Statement =
    | Assign of string * Expr
    | If of Expr * Statement * Statement option
    | While of Expr * Statement
    | Return of Expr
    | Block of Statement list
    | Expression of Expr

type Function = { Name: string; Parameters: string list; Variables: string list; Body: Statement list }
