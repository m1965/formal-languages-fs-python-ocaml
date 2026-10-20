(* Notebook helpers shared by all OCaml notebooks, loaded by nb.ml.  They replace what the Python notebooks get from IPython:
   - load_css ()          instead of  HTML(open('../style.css').read())
   - display_html s       instead of  display(HTML(s))
   - display_text s       shows text in the same order as display_html (see below)
   - display_svg s        shows an SVG image
   - display_vegalite j   shows a Vega-Lite plot given as a JSON value (instead of matplotlib)
   - dot_to_svg s         renders a graphviz/dot text with the dot program
   - run_notebook f       instead of  %run f
   - repr s               writes the string s in OCaml syntax, like Python's repr
   - cat file             prints the content of a file, like the shell command !cat file
   - shell cmd            runs a shell command and prints its output, like !cmd
   - time f               computes f () and prints the time needed, like %%time
   - python_repr, python_set  show strings and sets of strings like Python
   - input prompt         returns the next of the inputs given to simulate_input (the kernel has no keyboard)
   - float_repr x         writes x like Python's repr, e.g. 0.1, 1.0, 1e-05
   - ljust s n, rjust s n pad s with blanks to n characters, like Python's f'{s:n}' and f'{s:>n}'
*)


(* The kernel forwards displayed objects asynchronously.  Without the short pause, the kernel may
   report that a cell has finished before its last objects have been forwarded, and a tool that
   executes notebooks without a browser, e.g. jupyter nbconvert, then loses them. *)
let display mime s =
  ignore (Jupyter_notebook.display mime s);
  Unix.sleepf 0.05

let display_html s = display "text/html" s

(* The kernel sends printed text and displayed objects through different channels, so text that is
   printed before an object is displayed may appear after it.  display_text shows text through the
   same channel as display_html, so that the order is preserved. *)
let display_text s = display "text/plain" s
let display_svg s = display "image/svg+xml" s

(* A Vega-Lite plot is a JSON object.  Jupyter_notebook.display can only send strings, so the
   message is built by hand: the JSON object is sent as it is. *)
let display_vegalite (spec : Yojson.Safe.t) : unit =
  Jupyter_notebook__Unsafe.send_iopub
    (Jupyter.Iopub.IOPUB_DISPLAY_DATA
       { display_data = `Assoc [ ("application/vnd.vegalite.v5+json", spec);
                                 ("text/plain", `String "<Vega-Lite plot>") ];
         display_metadata = `Assoc [];
         display_transient = Some { display_id = Jupyter.Message.next_uuid () } });
  Unix.sleepf 0.05

let read_file name = In_channel.with_open_bin name In_channel.input_all

let write_file name s = Out_channel.with_open_bin name (fun oc -> output_string oc s)

let load_css () =
  let rec find dir n =
    let f = Filename.concat dir "style.css" in
    if Sys.file_exists f then f
    else if n = 0 then failwith "style.css not found"
    else find (Filename.concat dir Filename.parent_dir_name) (n - 1)
  in
  display_html (read_file (find Filename.parent_dir_name 2))

(* Run the program dot on the given text and return the SVG it produces.  The optional argument
   engine selects the layout engine, e.g. "neato". *)
let dot_to_svg ?(engine = "dot") (dot : string) : string =
  let input = Filename.temp_file "graph" ".dot" in
  let output = Filename.temp_file "graph" ".svg" in
  write_file input dot;
  let cmd = Printf.sprintf "dot -K%s -Tsvg %s -o %s" (Filename.quote engine) (Filename.quote input) (Filename.quote output) in
  if Sys.command cmd <> 0 then failwith "dot failed";
  let svg = read_file output in
  Sys.remove input;
  Sys.remove output;
  svg

(* run_notebook "X.ipynb" executes the code cells of X.ipynb, just like %run X.ipynb.
   Every notebook is loaded at most once: loading a notebook a second time would define
   its types again, and the new types would be incompatible with the values that have
   already been created using the old ones.  Cells tagged "raises-exception" (they demonstrate
   errors) are skipped, and so are the lines that load ../lib/nb.ml, display the style sheet,
   or show a type with #show_type. *)
let loaded_notebooks : (string, unit) Hashtbl.t = Hashtbl.create 16

let run_notebook (file : string) : unit =
  let key = Filename.basename file in
  if not (Hashtbl.mem loaded_notebooks key) then begin
    (* If loading fails, the toplevel forgets the definitions made while loading, so we
       have to forget which notebooks and library files have been loaded, too. *)
    let notebooks_before = Hashtbl.copy loaded_notebooks in
    let libs_before = Hashtbl.copy loaded_libs in
    Hashtbl.add loaded_notebooks key ();
    let open Yojson.Safe.Util in
    let cells = Yojson.Safe.from_file file |> member "cells" |> to_list in
    let source c =
      match member "source" c with
      | `List l -> String.concat "" (List.map to_string l)
      | `String s -> s
      | _ -> ""
    in
    let tags c =
      match member "metadata" c |> member "tags" with
      | `List l -> List.map to_string l
      | _ -> []
    in
    let wanted c =
      member "cell_type" c = `String "code" && not (List.mem "raises-exception" (tags c))
    in
    (* drop the lines that load this file or display the style sheet *)
    let keep line =
      let l = String.trim line in
      not (List.exists (fun prefix -> String.starts_with ~prefix l) [ "#use \"../lib/"; "#use \"lib/"; "load_css ()"; "#show_type" ])
    in
    let strip src = String.concat "\n" (List.filter keep (String.split_on_char '\n' src)) in
    let code =
      cells |> List.filter wanted
      |> List.map (fun c -> strip (source c) ^ "\n;;\n")
      |> String.concat ""
    in
    let tmp = Filename.temp_file (Filename.remove_extension key) ".ml" in
    write_file tmp code;
    let buf = Buffer.create 256 in
    let ppf = Format.formatter_of_buffer buf in
    let ok = Toploop.use_silently ppf (Toploop.File tmp) in
    Format.pp_print_flush ppf ();
    Sys.remove tmp;
    if not ok then begin
      Hashtbl.reset loaded_notebooks;
      Hashtbl.iter (Hashtbl.replace loaded_notebooks) notebooks_before;
      Hashtbl.reset loaded_libs;
      Hashtbl.iter (Hashtbl.replace loaded_libs) libs_before;
      failwith (Printf.sprintf "run_notebook %s failed:\n%s" file (Buffer.contents buf))
    end
  end

(* The toplevel and the format %S escape every byte that is not ASCII, so "ε" would be shown as
   "\206\181".  repr only escapes quotes, backslashes, and control characters.  It is also
   installed as the printer for strings. *)
let repr (s : string) : string =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\t' -> Buffer.add_string b "\\t"
      | '\r' -> Buffer.add_string b "\\r"
      | c when Char.code c < 32 || Char.code c = 127 ->
          Buffer.add_string b (Printf.sprintf "\\%03d" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

(* Long strings, e.g. the content of a file, are shortened when the toplevel shows them. *)
let pp_string ppf s =
  let limit = 500 in
  if String.length s <= limit then Format.pp_print_string ppf (repr s)
  else begin
    (* do not cut a UTF-8 character into pieces *)
    let n = ref limit in
    while !n > 0 && Char.code s.[!n] land 0xC0 = 0x80 do decr n done;
    let shown = repr (String.sub s 0 !n) in
    Format.fprintf ppf "%s...  (%d bytes)" (String.sub shown 0 (String.length shown - 1)) (String.length s)
  end;;

#install_printer pp_string;;

(* The formats %-24s of Printf count bytes.  ljust counts characters, since it does not count the
   continuation bytes of UTF-8, which have the form 0b10xxxxxx. *)
let utf8_length (s : string) : int =
  String.fold_left (fun n c -> if Char.code c land 0xC0 = 0x80 then n else n + 1) 0 s

let ljust (s : string) (n : int) : string = s ^ String.make (max 0 (n - utf8_length s)) ' '

let rjust (s : string) (n : int) : string = String.make (max 0 (n - utf8_length s)) ' ' ^ s

(* The OCaml kernel can not read from the keyboard: read_line raises End_of_file.  Therefore, the
   notebooks that need input from a user call simulate_input with the list of the lines that the user
   is assumed to type.  input prompt prints the prompt together with the next of these lines and
   returns it.  If there are no more lines, it raises End_of_file, like read_line at the end of a file. *)
let simulated_input : string list ref = ref []

let simulate_input (lines : string list) : unit = simulated_input := lines

let input (prompt : string) : string =
  match !simulated_input with
  | line :: rest ->
      simulated_input := rest;
      print_endline (prompt ^ line);
      line
  | [] -> raise End_of_file

(* float_repr x is the shortest string that is read back as x, formatted like Python's repr:
   exponential notation is used if the exponent is less than -4 or at least 16. *)
let float_repr (x : float) : string =
  if Float.is_nan x then "nan"
  else if Float.is_integer x && Float.abs x < 1e16 then Printf.sprintf "%.1f" x
  else if x = Float.infinity then "inf"
  else if x = Float.neg_infinity then "-inf"
  else begin
    let rec shortest p =
      let s = Printf.sprintf "%.*e" (p - 1) x in
      if p >= 17 || float_of_string s = x then s else shortest (p + 1)
    in
    let s = shortest 1 in
    let sign, s = if s.[0] = '-' then ("-", String.sub s 1 (String.length s - 1)) else ("", s) in
    let i = String.index s 'e' in
    let mantissa = String.sub s 0 i and exp = int_of_string (String.sub s (i + 1) (String.length s - i - 1)) in
    let digits = String.concat "" (String.split_on_char '.' mantissa) in
    let n = String.length digits in
    if exp >= -4 && exp < 16 then
      if exp >= n - 1 then sign ^ digits ^ String.make (exp - n + 1) '0' ^ ".0"
      else if exp >= 0 then sign ^ String.sub digits 0 (exp + 1) ^ "." ^ String.sub digits (exp + 1) (n - exp - 1)
      else sign ^ "0." ^ String.make (-exp - 1) '0' ^ digits
    else
      let m = if n = 1 then digits else String.sub digits 0 1 ^ "." ^ String.sub digits 1 (n - 1) in
      Printf.sprintf "%s%se%s%02d" sign m (if exp < 0 then "-" else "+") (abs exp)
  end

(* The toplevel shows floating point numbers with 17 significant digits, e.g. 0.1 as 0.1 but 1/3 as
   0.333333333333333315.  Like Python, we show the shortest representation instead. *)
let pp_float ppf (x : float) = Format.pp_print_string ppf (float_repr x);;

#install_printer pp_float;;

let cat (file : string) : unit =
  print_string (read_file file);
  flush stdout

(* The output of a command started with Sys.command does not reach the notebook, so shell collects it in a file. *)
let shell (cmd : string) : unit =
  let out = Filename.temp_file "shell" ".txt" in
  ignore (Sys.command (Printf.sprintf "(%s) > %s 2>&1" cmd (Filename.quote out)));
  print_string (read_file out);
  flush stdout;
  Sys.remove out

(* python_repr s writes the string s like Python's repr, i.e. enclosed in single quotes, unless s
   contains a single quote but no double quote.  python_set shows a set of strings like Python. *)
let python_repr (s : string) : string =
  if String.contains s '\'' && not (String.contains s '"') then "\"" ^ s ^ "\"" else "'" ^ s ^ "'"

let python_set (show : 'a -> string) (l : 'a list) : string =
  if l = [] then "set()" else "{" ^ String.concat ", " (List.map show l) ^ "}"

(* time f computes f () and prints the time needed, like the magic command %%time *)
let time (f : unit -> 'a) : 'a =
  let fmt t = if t < 1.0 then Printf.sprintf "%.2f ms" (t *. 1000.) else Printf.sprintf "%.2f s" t in
  let cpu = Sys.time () and wall = Unix.gettimeofday () in
  let result = f () in
  display_text (Printf.sprintf "CPU time: %s\nWall time: %s" (fmt (Sys.time () -. cpu)) (fmt (Unix.gettimeofday () -. wall)));
  result
