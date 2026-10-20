namespace LanguageSyntax

open System.Numerics

type Value = Integer of BigInteger | Real of float | Text of string | Boolean of bool
type BinaryOp = Add | Subtract | Multiply | Divide | Modulo | Equal | NotEqual | Less | Greater | LessEqual | GreaterEqual | And | Or
type Expr =
    | Literal of Value
    | Variable of string
    | Binary of BinaryOp * Expr * Expr
    | Not of Expr
    | Negative of Expr
    | Call of string * Expr list

type Statement =
    | Block of Statement list
    | Assign of string * Expr
    | Expression of Expr
    | If of Expr * Statement
    | While of Expr * Statement
    | For of Statement * Expr * Statement * Statement
    | Define of string * string list * Statement
    | Return of Expr
