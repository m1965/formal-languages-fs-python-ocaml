let display mime s =
  ignore (Jupyter_notebook.display mime s);
  Unix.sleepf 0.05
(* ... *)
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
