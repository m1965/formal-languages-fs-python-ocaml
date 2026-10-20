(* Every OCaml notebook starts with

     #use "../lib/nb.ml";;

   This loads the packages the notebooks need and then, without printing their signatures,
   - notebook.ml: display functions and run_notebook (the replacement of %run),
   - fset.ml:     the polymorphic modules FSet and Dict (Python's set and dict).
   Further files of this directory are loaded with load_lib, e.g.  load_lib "regex.ml". *)

#use "topfind";;
Topfind.log := ignore;;
#require "jupyter.notebook";;
#require "yojson";;
#require "unix";;
#directory "+compiler-libs";;

let lib_dir = Filename.dirname __FILE__

(* Every file is loaded at most once, since loading it again would create new, incompatible
   versions of the types it defines. *)
let loaded_libs : (string, unit) Hashtbl.t = Hashtbl.create 8

let load_lib (file : string) : unit =
  if not (Hashtbl.mem loaded_libs file) then begin
    if not (Toploop.use_silently Format.std_formatter (Toploop.File (Filename.concat lib_dir file))) then
      failwith ("could not load " ^ file);
    Hashtbl.add loaded_libs file ()
  end

let () = List.iter load_lib [ "notebook.ml"; "fset.ml" ]
