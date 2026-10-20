namespace FormalLanguages.Lalr

/// A closed syntax tree makes every interpreter case visible to the compiler.
type Expression =
    | Number of float
    | Name of string
    | Unary of string * Expression
    | Binary of string * Expression * Expression

type Statement =
    | Assign of string * Expression
    | Evaluate of Expression

type Lexeme =
    | Numeric of float
    | Identifier of string
    | Operator of string
    | Character of char
    | End
