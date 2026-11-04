let rec evaluate_expr (adt : ast) : float =
  match adt with
  | Tok ("NUMBER", value) -> float_of_string value
  | Tok ("IDENTIFIER", var_name) -> (
      match Dict.find_opt var_name !names2Values with
      | Some value -> value
      | None ->
          Printf.printf "Error: variable '%s' is undefined.\n%!" var_name;
          0.0)
  | Node ([ ("+" | "-" | "*" | "/") as op ], [ left; right ]) -> (
      let val_left  = evaluate_expr left  in
      let val_right = evaluate_expr right in
      match op with
      | "+" -> val_left +. val_right
      | "-" -> val_left -. val_right
      | "*" -> val_left *. val_right
      | _   -> val_left /. val_right)
  | _ -> invalid_arg ("Unknown ADT structure: " ^ Larkita.to_string adt)

let evaluate_adt (adt : ast) : unit =
  match adt with
  | Node ([ ":="; ";" ], [ Tok ("IDENTIFIER", var_name); expr ]) ->
      names2Values := Dict.add var_name (evaluate_expr expr) !names2Values
  | Node ([ ";" ], [ expr ]) -> print_endline (float_repr (evaluate_expr expr))
  | _ -> invalid_arg ("Unknown ADT structure: " ^ Larkita.to_string adt)
