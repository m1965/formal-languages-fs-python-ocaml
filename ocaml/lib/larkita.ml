(* Larkita for OCaml: a parser generator for canonical LR(1) parsers that return abstract syntax trees.

   This is a port of the Python package Larkita by Karl Stroetmann
   (https://github.com/karlstroetmann/Larkita).  A grammar is written in Lark's EBNF notation and
   contains nothing but the grammar: no semantic actions, no token declarations, and no separate
   scanner specification.  The abstract syntax tree follows from the grammar:

     expr: expr "+" product              Node (["+"], [ast(expr); ast(product)])
     stmnt: IDENTIFIER ":=" expr ";"     Node ([":="; ";"], [Tok ("IDENTIFIER", name); ast(expr)])
     factor: "(" expr ")" %ignore "(", ")"   ast(expr)
     atom: NUMBER                        Tok ("NUMBER", text)
     program: statement*                 Node (["program"], [ast(s1); ...; ast(sn)])

   Load it with  load_lib "larkita.ml"  (or #use it after #use "topfind").  Then

     let parser = Larkita.open_ "calc.lark"        (* writes calc.out *)
     let ast = Larkita.parse parser "x := 1 + 2;"

   Every time a parser is generated, the file <grammar>.out describes the LR(1) automaton: the rules
   with the abstract syntax trees they build, the conflicts, and all states with their lookaheads. *)

#directory "+compiler-libs";;

let larkita_dir =
  let d = Filename.dirname __FILE__ in
  if Filename.is_relative d then Filename.concat (Sys.getcwd ()) d else d

let () =
  List.iter
    (fun file ->
      let path = Filename.concat larkita_dir file in
      if not (Toploop.use_silently Format.err_formatter (Toploop.File path)) then failwith ("could not load " ^ path))
    [ "larkita_base.ml"; "larkita_regex.ml"; "larkita_lexer.ml"; "larkita_lr.ml"; "larkita_report.ml"; "larkita_grammar.ml" ]
;;

let () = Larkita_grammar.grammars_dir := Filename.concat larkita_dir "grammars"

(* the abstract syntax trees, see larkita_base.ml *)
type ast = Larkita_base.ast =
  | Node of string list * ast list
  | Tok of string * string
  | Lit of string

type token = Larkita_base.token = {
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

module Larkita = struct
  include Larkita_base
  open Larkita_lexer
  open Larkita_lr

  type lexer_kind = [ `Contextual | `Basic ]

  type t = {
    source_path : string option;
    out_file : string;
    start : string;
    terminals : terminal_def list;
    rules : rule list;
    ignore_tokens : string list;
    conflicts : conflict list;
    analyzer : analyzer;
    conf : lexer_conf;
    lexer : lexer_kind;
    run : string -> ast;
    basic : basic_lexer Lazy.t;
  }

  let make_parser (a : analyzer) (conf : lexer_conf) (lexer : lexer_kind) : string -> ast =
    let table = parse_table a in
    let aux = aux_names (List.tl (Array.to_list a.rules)) in
    let callbacks = Array.mapi (fun i r -> if i = 0 then (fun _ -> failwith "root") else make_callback r aux) a.rules in
    let next_token =
      match lexer with
      | `Basic ->
          let lx = basic_lexer conf in
          fun st state -> next_token lx st (Some state)
      | `Contextual ->
          let cl = contextual_lexer conf (accepted_symbols a) in
          fun st state -> contextual_next_token cl st state
    in
    fun text ->
      let st = lexer_state text in
      match
        Larkita_lr.parse table a.rules callbacks (fun t -> Token t) (next_token st) (format_expected conf.terminals_by_name)
      with
      | Ast t -> t
      | Kids _ | Token _ -> failwith "Larkita: the start rule did not return an abstract syntax tree"

  (* create grammar: generates a parser for the grammar (a string).
     - start: the start symbol, by default the first rule of the grammar.
     - lexer: `Contextual (the default) only matches the terminals that the parser can accept in
       its current state; `Basic always matches all terminals.
     - import_paths: additional directories for %import.
     - source_path: the file name of the grammar, used for relative imports and to name the output file.
     - out_file: the output file; by default source_path with the extension .out, or grammar.out.
     - g_regex_flags: flags applied to all terminals, e.g. "i".
     - merge_states: merge states with the same core, as long as this is safe.
     Raises Grammar_error if the grammar is wrong or has unresolved reduce/reduce conflicts (the
     output file is written anyway).  Shift/reduce conflicts are resolved in favour of shift and
     reported with a warning (see conflict_warnings). *)
  let create ?start ?(lexer : lexer_kind = `Contextual) ?(import_paths = []) ?source_path ?out_file ?(g_regex_flags = "")
      ?(merge_states = false) (grammar : string) : t =
    let source = Option.value source_path ~default:"<string>" in
    let g = Larkita_grammar.load_grammar grammar source import_paths in
    let start =
      match (start, g.first_rule) with
      | Some s, _ | None, Some s -> s
      | None, None -> raise (Grammar_error "The grammar does not contain any rules")
    in
    let terminals, rules, ignore_tokens = Larkita_grammar.compile_grammar g [ start ] in
    let out_file =
      match (out_file, source_path) with
      | Some f, _ -> f
      | None, Some p -> Filename.remove_extension p ^ ".out"
      | None, None -> "grammar.out"
    in
    let conf = lexer_conf ~g_regex_flags terminals ignore_tokens in
    let collision = if lexer = `Contextual then Some (token_collisions conf) else None in
    let a = Larkita_lr.build ~merge_states ?collision rules start in
    let terminal_names = Hashtbl.create 32 in
    List.iter (fun (t : terminal_def) -> Hashtbl.replace terminal_names t.name (user_repr t)) terminals;
    Larkita_report.write_report out_file a terminal_names source;
    let unresolved = List.filter (fun c -> not c.resolved) a.conflicts in
    if unresolved <> [] then
      raise
        (Grammar_error
           (Printf.sprintf "The grammar has %d unresolved reduce/reduce conflict(s), see %s:\n%s" (List.length unresolved) out_file
              (String.concat "\n"
                 (List.map
                    (fun c ->
                      Printf.sprintf "  * state %d on %s" c.c_state
                        (Option.value (Hashtbl.find_opt terminal_names c.c_terminal) ~default:c.c_terminal))
                    unresolved))));
    let shift_reduce = List.filter (fun c -> c.shift_state <> None) a.conflicts in
    if shift_reduce <> [] then
      warn_conflict
        (Printf.sprintf "%d shift/reduce conflict(s) were resolved as shift, see %s" (List.length shift_reduce) out_file);
    {
      source_path; out_file; start; terminals; rules; ignore_tokens; conflicts = a.conflicts; analyzer = a; conf; lexer;
      run = make_parser a conf lexer; basic = lazy (basic_lexer conf);
    }

  (* open_ file_name: reads the grammar from a file (Larkita.open of Python) *)
  let open_ ?rel_to ?start ?lexer ?import_paths ?out_file ?g_regex_flags ?merge_states (file_name : string) : t =
    let file_name = match rel_to with Some r -> Filename.concat (Filename.dirname r) file_name | None -> file_name in
    let text = In_channel.with_open_bin file_name In_channel.input_all in
    create ?start ?lexer ?import_paths ~source_path:file_name ?out_file ?g_regex_flags ?merge_states text

  (* parse parser text: the abstract syntax tree of text.  Raises Unexpected_input on a syntax error. *)
  let parse (p : t) (text : string) : ast = p.run text

  (* lex parser text: the tokens of the text, produced by a basic lexer; ignored tokens are skipped *)
  let lex (p : t) (text : string) : token list = List.of_seq (lex_all (Lazy.force p.basic) text)

  let to_string (t : ast) : string = ast_to_string t
  let out_file (p : t) = p.out_file
  let start (p : t) = p.start

  (* the text of the output file *)
  let report (p : t) : string = In_channel.with_open_bin p.out_file In_channel.input_all

  (* pformat t: the abstract syntax tree formatted like Python's pprint.pformat formats the nested tuple *)
  type pyobj = PStr of string | PTuple of pyobj list

  let rec pyobj_of_ast (t : ast) : pyobj =
    match t with
    | Lit s -> PStr s
    | Tok (name, text) -> PTuple [ PStr name; PStr text ]
    | Node (head, children) ->
        let head = match head with [ h ] -> PStr h | hs -> PTuple (List.map (fun h -> PStr h) hs) in
        PTuple (head :: List.map pyobj_of_ast children)

  let rec pyobj_repr (o : pyobj) : string =
    match o with
    | PStr s -> py_repr s
    | PTuple [ x ] -> "(" ^ pyobj_repr x ^ ",)"
    | PTuple xs -> "(" ^ String.concat ", " (List.map pyobj_repr xs) ^ ")"

  let pformat ?(width = 80) (t : ast) : string =
    let b = Buffer.create 256 in
    (* PrettyPrinter._format and _pprint_tuple of Python's module pprint *)
    let rec format (o : pyobj) (indent : int) (allowance : int) : unit =
      let rep = pyobj_repr o in
      match o with
      | PTuple items when utf8_length rep > width - indent - allowance ->
          Buffer.add_char b '(';
          let endchar = if List.length items = 1 then ",)" else ")" in
          format_items items indent (allowance + String.length endchar);
          Buffer.add_string b endchar
      | _ -> Buffer.add_string b rep
    and format_items (items : pyobj list) (indent : int) (allowance : int) : unit =
      let indent = indent + 1 in
      let n = List.length items in
      List.iteri
        (fun i item ->
          if i > 0 then Buffer.add_string b (",\n" ^ String.make indent ' ');
          format item indent (if i = n - 1 then allowance else 1))
        items
    in
    format (pyobj_of_ast t) 0 0;
    Buffer.contents b

  (* pprint t prints the abstract syntax tree like Python's pprint prints the nested tuple *)
  let pprint ?width (t : ast) : unit = print_endline (pformat ?width t)

  (* the message of an exception like Python's str(e) *)
  let message (e : unexpected_input) : string = e.message
end

let () =
  Printexc.register_printer (function
    | Larkita.Unexpected_input e -> Some ("Unexpected_input: " ^ e.message)
    | Larkita.Grammar_error m -> Some ("Grammar_error: " ^ m)
    | Larkita.Conflict_warning m -> Some ("Conflict_warning: " ^ m)
    | Larkita.Lex_error m -> Some ("Lex_error: " ^ m)
    | _ -> None)

(* The toplevel shows abstract syntax trees like Python shows nested tuples. *)
let pp_larkita_ast ppf (t : ast) = Format.pp_print_string ppf (Larkita.to_string t);;

#install_printer pp_larkita_ast;;

(* A parser is shown like Python shows it: Larkita(open('calc.lark'), start='stmnt') *)
let pp_larkita_parser ppf (p : Larkita.t) =
  Format.fprintf ppf "Larkita(open(%s), start=%s)"
    (Larkita.py_repr (Option.value p.source_path ~default:"<string>")) (Larkita.py_repr p.start);;

#install_printer pp_larkita_parser;;

(* An uncaught syntax error shows the message of Python's exception, e.g.
   Exception: Larkita_base.Unexpected_input
   Unexpected token Token('ID', 'f') at line 1, column 10. ... *)
let pp_larkita_unexpected ppf (e : Larkita.unexpected_input) = Format.fprintf ppf "@\n%s" e.message;;

#install_printer pp_larkita_unexpected;;
