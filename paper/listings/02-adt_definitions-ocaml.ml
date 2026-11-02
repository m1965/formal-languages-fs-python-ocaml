type regexp =
  | Empty                         (* ∅       *)
  | Epsilon                       (* 𝜀       *)
  | Char    of char               (* c       *)
  | Star    of regexp             (* r*      *)
  | Product of regexp * regexp    (* r1 ⋅ r2 *)
  | Sum     of regexp * regexp    (* r1 + r2 *)
