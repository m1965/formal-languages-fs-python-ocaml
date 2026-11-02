let loaded_libs : (string, unit) Hashtbl.t = Hashtbl.create 8

let load_lib (file : string) : unit =
  if not (Hashtbl.mem loaded_libs file) then begin
    if not (Toploop.use_silently Format.std_formatter
              (Toploop.File (Filename.concat lib_dir file))) then
      failwith ("could not load " ^ file);
    Hashtbl.add loaded_libs file ()
  end
