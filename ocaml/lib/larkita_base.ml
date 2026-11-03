(* Larkita for OCaml, part 1: the types and helpers shared by the other parts.

   This is a port of the Python package Larkita by Karl Stroetmann
   (https://github.com/karlstroetmann/Larkita), a light-weight version of Lark that generates
   canonical LR(1) parsers whose abstract syntax trees follow from the grammar.  The files are

     larkita_base.ml     types, exceptions, abstract syntax trees, Python-style quoting
     larkita_regex.ml    the widths and first characters of regular expressions (re._parser)
     larkita_lexer.ml    terminals, tokens, the basic and the contextual lexer (lexer.py)
     larkita_lr.ml       the canonical LR(1) automaton and the parser runtime (lr_analysis.py, lr_parser.py)
     larkita_report.ml   the output file (report.py, ast_builder.py)
     larkita_grammar.ml  the grammar loader (load_grammar.py)
     larkita.ml          the public interface (larkita.py)

   Load the whole package with  load_lib "larkita.ml". *)

#require "pcre2";;
#require "uucp";;

module Larkita_base = struct
  (* The abstract syntax tree that a parser returns.  Python represents it as a nested tuple
     (head, child_1, ..., child_n); OCaml needs one type for all of them:

       Node (["+"], [l; r])             ("+", l, r)
       Node ([":="; ";"], [x; e])       ((":=", ";"), x, e)        a head of several literals
       Node (["program"], children)     ("program", ...)            the rule name or an alias as head
       Tok ("NUMBER", "42")             ("NUMBER", "42")            a named token
       Lit "-"                          "-"                         a literal inside a repetition

     A head with exactly one element corresponds to a string in Python, a longer head to a tuple. *)
  type ast =
    | Node of string list * ast list
    | Tok of string * string
    | Lit of string

  (* ---------- UTF-8 ---------- *)

  let utf8_decode (s : string) : Uchar.t list =
    let rec go i acc =
      if i >= String.length s then List.rev acc
      else
        let d = String.get_utf_8_uchar s i in
        go (i + Uchar.utf_decode_length d) (Uchar.utf_decode_uchar d :: acc)
    in
    go 0 []

  let utf8_encode (us : Uchar.t list) : string =
    let b = Buffer.create 16 in
    List.iter (Buffer.add_utf_8_uchar b) us;
    Buffer.contents b

  (* the number of characters (code points) of a UTF-8 string *)
  let utf8_length (s : string) : int =
    let n = ref 0 in
    String.iter (fun c -> if Char.code c land 0xC0 <> 0x80 then incr n) s;
    !n

  (* the number of characters in s between the byte positions i and j *)
  let utf8_count (s : string) (i : int) (j : int) : int =
    let n = ref 0 in
    for k = i to j - 1 do
      if Char.code s.[k] land 0xC0 <> 0x80 then incr n
    done;
    !n

  let ljust (s : string) (n : int) : string = s ^ String.make (max 0 (n - utf8_length s)) ' '

  (* ---------- Python's string functions ---------- *)

  let is_cased (u : Uchar.t) : bool =
    match Uucp.Gc.general_category u with `Lu | `Ll | `Lt -> true | _ -> false

  (* str.isupper(): at least one cased character, and no lowercase character *)
  let is_upper (s : string) : bool =
    let us = utf8_decode s in
    List.exists is_cased us && not (List.exists (fun u -> Uucp.Case.is_lower u) us)

  let to_upper (s : string) : string =
    utf8_encode
      (List.concat_map
         (fun u -> match Uucp.Case.Map.to_upper u with `Self -> [ u ] | `Uchars us -> us)
         (utf8_decode s))

  let to_lower (s : string) : string =
    utf8_encode
      (List.concat_map
         (fun u -> match Uucp.Case.Map.to_lower u with `Self -> [ u ] | `Uchars us -> us)
         (utf8_decode s))

  (* str.isprintable() for one character *)
  let is_printable (u : Uchar.t) : bool =
    Uchar.to_int u = 0x20
    ||
    match Uucp.Gc.general_category u with
    | `Cc | `Cf | `Cs | `Co | `Cn | `Zl | `Zp | `Zs -> false
    | _ -> true

  (* repr(s) of Python *)
  let py_repr (s : string) : string =
    let quote = if String.contains s '\'' && not (String.contains s '"') then '"' else '\'' in
    let b = Buffer.create (String.length s + 2) in
    Buffer.add_char b quote;
    List.iter
      (fun u ->
        let c = Uchar.to_int u in
        if c = Char.code quote || c = Char.code '\\' then (Buffer.add_char b '\\'; Buffer.add_char b (Char.chr c))
        else if c = 0x09 then Buffer.add_string b "\\t"
        else if c = 0x0A then Buffer.add_string b "\\n"
        else if c = 0x0D then Buffer.add_string b "\\r"
        else if c < 0x20 || c = 0x7F then Buffer.add_string b (Printf.sprintf "\\x%02x" c)
        else if c < 0x7F || is_printable u then Buffer.add_utf_8_uchar b u
        else if c <= 0xFF then Buffer.add_string b (Printf.sprintf "\\x%02x" c)
        else if c <= 0xFFFF then Buffer.add_string b (Printf.sprintf "\\u%04x" c)
        else Buffer.add_string b (Printf.sprintf "\\U%08x" c))
      (utf8_decode s);
    Buffer.add_char b quote;
    Buffer.contents b

  (* json.dumps(s, ensure_ascii=False) *)
  let json_quote (s : string) : string =
    let b = Buffer.create (String.length s + 2) in
    Buffer.add_char b '"';
    String.iter
      (fun c ->
        match c with
        | '"' -> Buffer.add_string b "\\\""
        | '\\' -> Buffer.add_string b "\\\\"
        | '\n' -> Buffer.add_string b "\\n"
        | '\r' -> Buffer.add_string b "\\r"
        | '\t' -> Buffer.add_string b "\\t"
        | '\b' -> Buffer.add_string b "\\b"
        | '\012' -> Buffer.add_string b "\\f"
        | c when Char.code c < 0x20 -> Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
        | c -> Buffer.add_char b c)
      s;
    Buffer.add_char b '"';
    Buffer.contents b

  (* The abstract syntax tree in Python's notation, e.g. ((':=', ';'), ('IDENTIFIER', 'x'), ...) *)
  let rec ast_to_string (t : ast) : string =
    match t with
    | Lit s -> py_repr s
    | Tok (name, text) -> Printf.sprintf "(%s, %s)" (py_repr name) (py_repr text)
    | Node (head, children) ->
        let head =
          match head with
          | [ h ] -> py_repr h
          | hs -> "(" ^ String.concat ", " (List.map py_repr hs) ^ ")"
        in
        if children = [] then "(" ^ head ^ ",)"
        else "(" ^ String.concat ", " (head :: List.map ast_to_string children) ^ ")"

  (* ---------- Exceptions ---------- *)

  exception Grammar_error of string
  exception Lex_error of string
  exception Configuration_error of string

  (* A token of the scanner.  The positions start_pos and end_pos count characters, like in
     Python; byte_pos is the position of the token in the UTF-8 string. *)
  type token = {
    type_ : string;
    value : string;
    start_pos : int;
    line : int;
    column : int;
    end_line : int;
    end_column : int;
    end_pos : int;
    byte_pos : int;
  }

  let token_repr (t : token) : string = Printf.sprintf "Token(%s, %s)" (py_repr t.type_) (py_repr t.value)

  (* The state of the parser when an error was detected: the height of its stack and its current
     state.  Two errors with the same parser state are considered equal (see match_examples). *)
  type parser_state = { stack_height : int; position : int }

  type unexpected_kind =
    | Unexpected_characters of { char : string; allowed : string list }
    | Unexpected_token of { token : token; expected : string list }

  (* raised by parse if the text does not match the grammar; the subclasses UnexpectedCharacters and
     UnexpectedToken of Python are the two kinds *)
  type unexpected_input = {
    kind : unexpected_kind;
    line : int;
    column : int;
    pos_in_stream : int;              (* the position of the error, counted in characters *)
    byte_pos : int;                   (* the same position, counted in bytes *)
    state : parser_state option;
    message : string;                 (* the message of Python's __str__ *)
  }

  exception Unexpected_input of unexpected_input

  (* get_context text: the line of text containing the error, and a line with a marker below the error *)
  let get_context ?(span = 40) (e : unexpected_input) (text : string) : string =
    let chars = Array.of_list (utf8_decode text) in
    let n = Array.length chars in
    let pos = min e.pos_in_stream n in
    let start = max (pos - span) 0 in
    let stop = min (pos + span) n in
    let sub i j = utf8_encode (Array.to_list (Array.sub chars i (j - i))) in
    let before = sub start pos in
    let before =
      match String.rindex_opt before '\n' with
      | Some i -> String.sub before (i + 1) (String.length before - i - 1)
      | None -> before
    in
    let after = sub pos stop in
    let after = match String.index_opt after '\n' with Some i -> String.sub after 0 i | None -> after in
    (* len(before.expandtabs()) *)
    let width = ref 0 in
    List.iter
      (fun u -> if Uchar.to_int u = 9 then width := (!width / 8 + 1) * 8 else incr width)
      (utf8_decode before);
    before ^ after ^ "\n" ^ String.make !width ' ' ^ "^\n"

  (* Shift/reduce conflicts are reported by a warning.  conflict_warnings decides what happens:
     `Print writes it to standard error (like Python's default), `Raise raises Conflict_warning
     (like warnings.simplefilter('error')), and `Ignore drops it. *)
  exception Conflict_warning of string

  let conflict_warnings : [ `Print | `Raise | `Ignore ] ref = ref `Print

  let warn_conflict (msg : string) : unit =
    match !conflict_warnings with
    | `Print -> prerr_endline ("ConflictWarning: " ^ msg); flush stderr
    | `Raise -> raise (Conflict_warning msg)
    | `Ignore -> ()

  (* ---------- Lists ---------- *)

  (* removes duplicates, keeping the first occurrence (dict.fromkeys) *)
  let dedup ?(eq = ( = )) (l : 'a list) : 'a list =
    List.rev (List.fold_left (fun acc x -> if List.exists (eq x) acc then acc else x :: acc) [] l)

  let rec take n l = if n <= 0 then [] else match l with [] -> [] | x :: r -> x :: take (n - 1) r
  let rec drop n l = if n <= 0 then l else match l with [] -> [] | _ :: r -> drop (n - 1) r
end
