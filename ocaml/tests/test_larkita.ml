(* Regression test of the OCaml port of Larkita, without Python.

     cd ocaml && ocaml tests/test_larkita.ml

   larkita_expected.json contains the grammars and inputs of the unit tests of the Python package
   Larkita together with the results that Python's Larkita produced for them (abstract syntax trees,
   tokens, and error messages).  It was generated with tools/larkita_record.py.  Every grammar is
   compiled with the OCaml port, and every result is compared.  tools/diff_larkita.py performs the
   full comparison (including the output files) if Python's Larkita is installed. *)

#use "topfind";;
#require "yojson";;
#directory "+compiler-libs";;

let () =
  let lib = Filename.concat (Filename.dirname (Filename.dirname Sys.argv.(0))) "lib/nested.ml" in
  if not (Toploop.use_silently Format.err_formatter (Toploop.File lib)) then exit 2
;;

let () =
  let lib = Filename.concat (Filename.dirname (Filename.dirname Sys.argv.(0))) "lib/larkita.ml" in
  if not (Toploop.use_silently Format.err_formatter (Toploop.File lib)) then exit 2
;;

open Yojson.Safe.Util

let () = Larkita.conflict_warnings := `Ignore

let error_of (e : exn) : string * string =
  match e with
  | Larkita.Grammar_error m -> ("GrammarError", m)
  | Larkita.Lex_error m -> ("LexError", m)
  | Larkita.Unexpected_input u ->
      ((match u.kind with Unexpected_characters _ -> "UnexpectedCharacters" | Unexpected_token _ -> "UnexpectedToken"), u.message)
  | e -> ("Exception", Printexc.to_string e)

let () =
  let fixture = Filename.concat (Filename.dirname Sys.argv.(0)) "larkita_expected.json" in
  let cases = to_list (Yojson.Safe.from_file fixture) in
  let out_file = Filename.temp_file "larkita" ".out" in
  let failures = ref 0 and checks = ref 0 in
  let fail i what expected got =
    incr failures;
    Printf.printf "case %d, %s:\n  expected %s\n  got      %s\n" i what expected got
  in
  List.iteri
    (fun i case ->
      let options = case |> member "options" in
      let opt name f = match options |> member name with `Null -> None | v -> Some (f v) in
      let lexer = opt "lexer" (fun v -> if to_string v = "basic" then `Basic else `Contextual) in
      let expected = case |> member "outcome" in
      incr checks;
      match
        Larkita.create ?start:(opt "start" to_string) ?lexer ?merge_states:(opt "merge_states" to_bool)
          ?g_regex_flags:(opt "g_regex_flags" to_string) ~out_file (case |> member "grammar" |> to_string)
      with
      | exception e -> (
          let t, m = error_of e in
          match expected with
          | `Null -> fail i "construction" "success" (t ^ ": " ^ m)
          | _ ->
              let et = expected |> member "type" |> to_string in
              let em = expected |> member "message" in
              if et <> t || (em <> `Null && to_string em <> m) then
                fail i "construction" (et ^ ": " ^ Yojson.Safe.to_string em) (t ^ ": " ^ m))
      | parser ->
          if expected <> `Null then fail i "construction" (Yojson.Safe.to_string expected) "success";
          List.iter
            (fun call ->
              incr checks;
              let text = call |> member "text" |> to_string in
              let kind = call |> member "kind" |> to_string in
              let got =
                try
                  if kind = "parse" then `Assoc [ ("result", `String (Larkita.to_string (Larkita.parse parser text))) ]
                  else
                    `Assoc
                      [ ( "result",
                          `List
                            (List.map
                               (fun (t : token) -> `List [ `String t.type_; `String t.value; `Int t.line; `Int t.column ])
                               (Larkita.lex parser text)) ) ]
                with e ->
                  let t, m = error_of e in
                  `Assoc [ ("error", `Assoc [ ("type", `String t); ("message", `String m) ]) ]
              in
              let strip = function
                | `Assoc l -> `Assoc (List.filter (fun (k, _) -> k <> "line" && k <> "column") l)
                | v -> v
              in
              let want =
                match call |> member "error" with
                | `Null -> `Assoc [ ("result", call |> member "result") ]
                | e -> `Assoc [ ("error", strip e) ]
              in
              if got <> want then fail i (kind ^ " " ^ String.escaped text) (Yojson.Safe.to_string want) (Yojson.Safe.to_string got))
            (case |> member "calls" |> to_list))
    cases;
  Sys.remove out_file;
  Printf.printf "%d checks, %d failures\n" !checks !failures;
  exit (if !failures = 0 then 0 else 1)
