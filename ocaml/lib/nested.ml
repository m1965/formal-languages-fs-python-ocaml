(* Abstract syntax trees as nested tuples.

   Every parser of the notebooks returns its abstract syntax tree as a value of the type nested:
   a tree is either a leaf, which holds an integer, a floating point number, a string, or a Boolean
   value, or a node, which is a tuple of trees.  By convention, the first component of a node is the
   operator, e.g.

     ("+", "a", ("*", "b"))          Node [Leaf (Str "+"); Leaf (Str "a"); Node [Leaf (Str "*"); Leaf (Str "b")]]
     ("NUMBER", "42")                Node [Leaf (Str "NUMBER"); Leaf (Str "42")]

   The toplevel prints trees in the tuple notation shown on the left.  nb.ml loads this file; the
   test scripts of Larkita, which do not use nb.ml, load it with  #use "../lib/nested.ml". *)

type value =
  | Int of int
  | Float of float
  | Str of string
  | Bool of bool

type nested =
  | Leaf of value
  | Node of nested list

module Nested = struct
  (* a string in double quotes, with the escape sequences of OCaml *)
  let quote (s : string) : string =
    let b = Buffer.create (String.length s + 2) in
    Buffer.add_char b '"';
    String.iter
      (fun c ->
        match c with
        | '"' -> Buffer.add_string b "\\\""
        | '\\' -> Buffer.add_string b "\\\\"
        | '\n' -> Buffer.add_string b "\\n"
        | '\t' -> Buffer.add_string b "\\t"
        | '\r' -> Buffer.add_string b "\\r"
        | c when Char.code c < 32 || Char.code c = 127 -> Buffer.add_string b (Printf.sprintf "\\%03d" (Char.code c))
        | c -> Buffer.add_char b c)
      s;
    Buffer.add_char b '"';
    Buffer.contents b

  (* the shortest decimal representation of x that is read back as x, e.g. 0.5, 1.0, or 1e+100 *)
  let float_to_string (x : float) : string =
    if Float.is_integer x && Float.abs x < 1e16 then Printf.sprintf "%.1f" x
    else if Float.is_nan x then "nan"
    else if Float.is_finite x then
      let rec try_precision p =
        let s = Printf.sprintf "%.*g" p x in
        if p >= 17 || float_of_string s = x then s else try_precision (p + 1)
      in
      try_precision 1
    else if x > 0.0 then "inf"
    else "-inf"

  let value_to_string ?(quote = quote) (v : value) : string =
    match v with
    | Int n -> string_of_int n
    | Float x -> float_to_string x
    | Str s -> quote s
    | Bool b -> string_of_bool b

  (* to_string t: the tree t in tuple notation.  A tuple with one component is written (x,), so that it
     differs from x.  The optional argument quote determines how strings are written. *)
  let rec to_string ?(quote = quote) (t : nested) : string =
    match t with
    | Leaf v -> value_to_string ~quote v
    | Node [ x ] -> "(" ^ to_string ~quote x ^ ",)"
    | Node xs -> "(" ^ String.concat ", " (List.map (to_string ~quote) xs) ^ ")"

  (* the number of characters (not bytes) of a UTF-8 string *)
  let utf8_length (s : string) : int =
    String.fold_left (fun n c -> if Char.code c land 0xC0 = 0x80 then n else n + 1) 0 s

  (* pformat t: the tree t in tuple notation.  A tuple that does not fit into the line is split: its
     components are written one below the other. *)
  let pformat ?(width = 80) ?(quote = quote) (t : nested) : string =
    let b = Buffer.create 256 in
    let rec format (t : nested) (indent : int) (allowance : int) : unit =
      let rep = to_string ~quote t in
      match t with
      | Node items when utf8_length rep > width - indent - allowance ->
          Buffer.add_char b '(';
          let endchar = if List.length items = 1 then ",)" else ")" in
          format_items items indent (allowance + String.length endchar);
          Buffer.add_string b endchar
      | _ -> Buffer.add_string b rep
    and format_items (items : nested list) (indent : int) (allowance : int) : unit =
      let indent = indent + 1 in
      let n = List.length items in
      List.iteri
        (fun i item ->
          if i > 0 then Buffer.add_string b (",\n" ^ String.make indent ' ');
          format item indent (if i = n - 1 then allowance else 1))
        items
    in
    format t 0 0;
    Buffer.contents b

  let pprint ?width (t : nested) : unit = print_endline (pformat ?width t)
end

let pp_nested ppf (t : nested) = Format.pp_print_string ppf (Nested.to_string t);;

#install_printer pp_nested;;
