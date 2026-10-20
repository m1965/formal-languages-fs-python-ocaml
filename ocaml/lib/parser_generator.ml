(* Support for the notebooks that use the scanner generator ocamllex and the parser generator Menhir
   instead of the Python library Lark.  Load it with

     load_lib "parser_generator.ml"

   - Parse_tree.tree is a parse tree in the style of Lark: a node has a name and a list of children,
     and a leaf is a token of type Parse_tree.tok that knows its type, its value, its line, and its
     column.  (The type is not called token, since Menhir defines a type token for the tokens of
     every grammar.)  The semantic
     actions of the Menhir grammars build these trees, so the notebooks can convert them into abstract
     syntax trees in the same way as the Python notebooks convert the trees produced by Lark.
   - Parse_tree.pretty renders a tree as an indented string, like the method pretty of Lark.
   - generate_parser ~name ~lexer ~grammar writes the scanner specification lexer to the file
     _build/<name>_lexer.mll and the grammar to the file _build/<name>_parser.mly, runs ocamllex and
     menhir --lalr, and loads the generated modules <Name>_lexer and <Name>_parser.  Menhir's warnings
     about conflicts are printed.  If Menhir rejects the grammar, the exception Grammar_error is raised
     with Menhir's messages.  generate_parser_with ~options ... passes the options instead of "--lalr", e.g.
     "--lalr --strict" (conflicts are errors), "--canonical" (canonical LR(1)), or "--GLR" (a GLR parser,
     which needs the library menhirGLR).  Menhir also writes the LR states into the files
     _build/<name>_parser.automaton (before conflicts are resolved) and .automaton.resolved (after),
     and explains the conflicts in _build/<name>_parser.conflicts.
   - generate_lexer ~name ~lexer and generate_grammar ~options ~name ~grammar do the two halves of
     generate_parser_with separately.  This is needed if the scanner is tested before the grammar is written;
     then the tokens are defined by an OCaml module and the grammar uses them via the option --external-tokens.
   - Parse_tree.parse_string parser lexer s parses the string s.  Syntax errors are reported by the
     exception Unexpected_input, which carries the line and the column of the error, like the exception
     UnexpectedInput of Lark.  Parse_tree.get_context shows the offending line with a marker. *)

module Parse_tree = struct
  (* pos is the position of the token in the input, i.e. the number of characters preceding it *)
  type tok = { kind : string; value : string; line : int; column : int; pos : int }

  type tree =
    | Tree of string * tree list    (* a node of the parse tree: its name and its children *)
    | Token of tok                  (* a leaf of the parse tree: a token *)

  (* token kind lexbuf creates a token of the given kind from the string that has just been scanned *)
  let token (kind : string) (lexbuf : Lexing.lexbuf) : tok =
    let p = Lexing.lexeme_start_p lexbuf in
    { kind; value = Lexing.lexeme lexbuf; line = p.pos_lnum; column = p.pos_cnum - p.pos_bol + 1; pos = p.pos_cnum }

  (* the scanner has to call new_lines on every string that contains newlines *)
  let new_lines (lexbuf : Lexing.lexbuf) : unit =
    String.iter (fun c -> if c = '\n' then Lexing.new_line lexbuf) (Lexing.lexeme lexbuf)

  let pretty (t : tree) : string =
    let b = Buffer.create 256 in
    let rec pp level t =
      let indent = String.make (2 * level) ' ' in
      match t with
      | Token tok -> Buffer.add_string b (indent ^ tok.value ^ "\n")
      | Tree (name, [ Token tok ]) -> Buffer.add_string b (indent ^ name ^ "\t" ^ tok.value ^ "\n")
      | Tree (name, children) ->
          Buffer.add_string b (indent ^ name ^ "\n");
          List.iter (pp (level + 1)) children
    in
    pp 0 t;
    Buffer.contents b

  (* The toplevel shows parse trees like Lark shows them, e.g. Tree('add', [Token('NUMBER', '1'), ...]). *)
  let rec to_string (t : tree) : string =
    let quote s = "'" ^ s ^ "'" in
    match t with
    | Token tok -> Printf.sprintf "Token(%s, %s)" (quote tok.kind) (quote tok.value)
    | Tree (name, children) -> Printf.sprintf "Tree(%s, [%s])" (quote name) (String.concat ", " (List.map to_string children))

  let pp ppf (t : tree) = Format.pp_print_string ppf (to_string t)

  (* raised by the scanners on an illegal character and by parse_string on a syntax error *)
  exception Unexpected_input of { line : int; column : int; position : int; message : string }

  let unexpected (lexbuf : Lexing.lexbuf) (message : string) : exn =
    let p = Lexing.lexeme_start_p lexbuf in
    Unexpected_input { line = p.pos_lnum; column = p.pos_cnum - p.pos_bol + 1; position = p.pos_cnum; message }

  let parse_string (parser : (Lexing.lexbuf -> 'tok) -> Lexing.lexbuf -> 'a) (lexer : Lexing.lexbuf -> 'tok)
      (text : string) : 'a =
    let lexbuf = Lexing.from_string text in
    try parser lexer lexbuf with
    | e when String.ends_with ~suffix:".Error" (Printexc.exn_slot_name e) ->
        raise (unexpected lexbuf (Printf.sprintf "Unexpected token %s" (repr (Lexing.lexeme lexbuf))))

  (* the line of text containing the error, followed by a line with a marker below the error *)
  let get_context ?(span = 40) (text : string) (position : int) : string =
    let before = String.sub text (max 0 (position - span)) (position - max 0 (position - span)) in
    let before = match String.rindex_opt before '\n' with Some i -> String.sub before (i + 1) (String.length before - i - 1) | None -> before in
    let after = String.sub text position (min span (String.length text - position)) in
    let after = match String.index_opt after '\n' with Some i -> String.sub after 0 i | None -> after in
    before ^ after ^ "\n" ^ String.make (utf8_length before) ' ' ^ "^\n"
end

(* Run a command and return its output (stdout and stderr). *)
let run_command (cmd : string) : int * string =
  let out = Filename.temp_file "cmd" ".txt" in
  let status = Sys.command (Printf.sprintf "%s > %s 2>&1" cmd (Filename.quote out)) in
  let output = read_file out in
  Sys.remove out;
  (status, output)

(* raised by generate_parser if Menhir rejects the grammar; the argument contains Menhir's messages *)
exception Grammar_error of string

(* load a generated file as the module <File> without printing its signature *)
let load_module (file : string) : unit =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let ok = Toploop.mod_use_input ppf (Toploop.File file) in
  Format.pp_print_flush ppf ();
  if not ok then failwith ("could not load " ^ file ^ ":\n" ^ Buffer.contents buf)

let build_dir () : string =
  if not (Sys.file_exists "_build") then Sys.mkdir "_build" 0o755;
  "_build"

(* generate_lexer ~name ~lexer runs ocamllex on the scanner specification lexer and loads the module <Name>_lexer *)
let generate_lexer ~(name : string) ~(lexer : string) : unit =
  let base = Filename.concat (build_dir ()) name in
  write_file (base ^ "_lexer.mll") lexer;
  let status, output = run_command (Printf.sprintf "ocamllex -q %s" (Filename.quote (base ^ "_lexer.mll"))) in
  if output <> "" then (print_string output; flush stdout);
  if status <> 0 then failwith ("ocamllex failed:\n" ^ output);
  load_module (base ^ "_lexer.ml");
  flush stdout

(* generate_grammar ~options ~name ~grammar runs menhir with the given options on the grammar and loads the
   module <Name>_parser *)
let generate_grammar ~(options : string) ~(name : string) ~(grammar : string) : unit =
  let base = Filename.concat (build_dir ()) name in
  write_file (base ^ "_parser.mly") grammar;
  let status, output =
    run_command (Printf.sprintf "menhir %s --dump --dump-resolved --explain %s" options (Filename.quote (base ^ "_parser.mly")))
  in
  if status <> 0 then raise (Grammar_error output);
  if output <> "" then (print_string output; flush stdout);
  if List.mem "--GLR" (String.split_on_char ' ' options) then Topfind.load_deeply [ "menhirGLR" ];
  load_module (base ^ "_parser.ml");
  flush stdout

let generate_parser_with ~(options : string) ~(name : string) ~(lexer : string) ~(grammar : string) : unit =
  generate_grammar ~options ~name ~grammar;
  generate_lexer ~name ~lexer

let generate_parser ~(name : string) ~(lexer : string) ~(grammar : string) : unit =
  generate_parser_with ~options:"--lalr" ~name ~lexer ~grammar
;;

#install_printer Parse_tree.pp;;
