type ast =
  | Node of string list * ast list   (* (head, child_1, ..., child_n) *)
  | Tok  of string * string          (* ('NUMBER', '42') *)
  | Lit  of string                   (* a literal like '-' as a child *)
