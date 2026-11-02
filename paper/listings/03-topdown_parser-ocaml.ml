let rec parseExprRest (sum : float) (tl : string list) : float * string list =
  match tl with
  | [] -> sum, []
  | "+" :: rl ->
      let product, rest = !parseProduct_ref rl in
      parseExprRest (sum +. product) rest
  | "-" :: rl ->
      let product, rest = !parseProduct_ref rl in
      parseExprRest (sum -. product) rest
  | _ -> sum, tl
;;
parseExprRest_ref := parseExprRest

let parseFactor (tl : string list) : float * string list =
  match tl with
  | "(" :: rl -> (
      match parseExpr rl with
      | expr, ")" :: rest -> expr, rest
      | _ -> failwith "Parse Error: expected \")\"")
  | t :: rest -> float_of_string t, rest
  | [] -> failwith "Parse Error: unexpected end of input"
