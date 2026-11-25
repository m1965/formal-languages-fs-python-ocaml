(* Replays the calls recorded by larkita_record.py with the OCaml port of Larkita.

     ocaml tools/larkita_replay.ml CASES.json RESULTS.json OUT_DIR

   For every recorded grammar, the parser is generated (the output file is written into OUT_DIR),
   and every recorded call of parse and lex is repeated.  The results are written in the same form
   as the recorded ones, so that diff_larkita.py can compare them. *)

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

let () = Larkita.conflict_warnings := `Ignore

let exception_json (e : exn) : Yojson.Safe.t =
  let simple t m = `Assoc [ ("type", `String t); ("message", `String m) ] in
  match e with
  | Larkita.Grammar_error m -> simple "GrammarError" m
  | Larkita.Lex_error m -> simple "LexError" m
  | Larkita.Conflict_warning m -> simple "ConflictWarning" m
  | Larkita.Configuration_error m -> simple "ConfigurationError" m
  | Larkita.Unexpected_input u ->
      let t = match u.kind with Unexpected_characters _ -> "UnexpectedCharacters" | Unexpected_token _ -> "UnexpectedToken" in
      `Assoc [ ("type", `String t); ("message", `String u.message); ("line", `Int u.line); ("column", `Int u.column) ]
  | Sys_error m -> simple "FileNotFoundError" m
  | e -> simple "Exception" (Printexc.to_string e)

let () =
  let cases_file = Sys.argv.(1) and results_file = Sys.argv.(2) and out_dir = Sys.argv.(3) in
  let home = Sys.getcwd () in
  let cases = match Yojson.Safe.from_file cases_file with `List l -> l | _ -> failwith "list expected" in
  let results =
    List.mapi
      (fun i case ->
        let open Yojson.Safe.Util in
        Sys.chdir home;
        if case |> member "skip" <> `Null then `Assoc [ ("skipped", `Bool true) ]
        else begin
          (try Sys.chdir (case |> member "cwd" |> to_string) with Sys_error _ -> ());
          let options = case |> member "options" in
          let opt name f = match options |> member name with `Null -> None | v -> Some (f v) in
          (Larkita.conflict_warnings :=
             match case |> member "outcome" with `Assoc l when List.assoc_opt "type" l = Some (`String "ConflictWarning") -> `Raise | _ -> `Ignore);
          let out_file = Filename.concat out_dir (Printf.sprintf "case-%d.out" i) in
          let lexer = opt "lexer" (fun v -> if to_string v = "basic" then `Basic else `Contextual) in
          let construction =
            try
              Ok
                (Larkita.create ?start:(opt "start" to_string) ?lexer
                   ?import_paths:(opt "import_paths" (fun v -> List.map to_string (to_list v)))
                   ?source_path:(opt "source_path" to_string) ~out_file
                   ?g_regex_flags:(opt "g_regex_flags" to_string)
                   ?merge_states:(opt "merge_states" to_bool)
                   (case |> member "grammar" |> to_string))
            with e -> Error e
          in
          let report = try `String (In_channel.with_open_bin out_file In_channel.input_all) with _ -> `Null in
          let calls =
            List.map
              (fun call ->
                let text = call |> member "text" |> to_string in
                match construction with
                | Error _ -> `Assoc [ ("error", `Assoc [ ("type", `String "not constructed") ]) ]
                | Ok parser -> (
                    if call |> member "kind" |> to_string = "parse" then
                      match Larkita.parse parser text with
                      | t -> `Assoc [ ("result", `String (Larkita.to_string t)) ]
                      | exception e -> `Assoc [ ("error", exception_json e) ]
                    else
                      match Larkita.lex parser text with
                      | tokens ->
                          `Assoc
                            [ ( "result",
                                `List
                                  (List.map
                                     (fun (t : token) -> `List [ `String t.type_; `String t.value; `Int t.line; `Int t.column ])
                                     tokens) ) ]
                      | exception e -> `Assoc [ ("error", exception_json e) ]))
              (case |> member "calls" |> to_list)
          in
          `Assoc
            [ ("outcome", (match construction with Ok _ -> `Null | Error e -> exception_json e));
              ("report", report); ("out_file", `String out_file); ("calls", `List calls) ]
        end)
      cases
  in
  Sys.chdir home;
  Yojson.Safe.to_file results_file (`List results)
