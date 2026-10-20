(* A minimal replacement of the Python package graphviz: a directed graph is built with the
   functions node and edge and rendered with the program dot.  Load it with

     load_lib "graphviz.ml"

   Python                                     OCaml
   dot = gv.Digraph('name')                   let dot = Graphviz.digraph "name"
   dot = gv.Digraph(engine='neato')           let dot = Graphviz.digraph ~engine:"neato" ""
   dot.graph_attr['rankdir'] = 'LR'           Graphviz.graph_attr dot "rankdir" "LR"
   dot.edge_attr['len'] = '1.8'               Graphviz.edge_attr dot "len" "1.8"
   dot.node('q', peripheries='2')             Graphviz.node dot ~attrs:[("peripheries", "2")] "q"
   dot.edge('p', 'q', label='a')              Graphviz.edge dot ~attrs:[("label", "a")] "p" "q"
   dot.source                                 Graphviz.source dot
   dot        (as the last line of a cell)    Graphviz.display dot
*)

module Graphviz : sig
  type t

  val digraph : ?engine:string -> string -> t
  val graph_attr : t -> string -> string -> unit
  val edge_attr : t -> string -> string -> unit
  val node : t -> ?attrs:(string * string) list -> string -> unit
  val edge : t -> ?attrs:(string * string) list -> string -> string -> unit
  val source : t -> string
  val display : t -> unit
end = struct
  type t = {
    name : string;
    engine : string;
    mutable graph_attrs : (string * string) list;
    mutable edge_attrs : (string * string) list;
    mutable lines : string list;
  }

  let digraph ?(engine = "dot") name = { name; engine; graph_attrs = []; edge_attrs = []; lines = [] }

  let set attrs key value = List.filter (fun (k, _) -> k <> key) attrs @ [ (key, value) ]
  let graph_attr g key value = g.graph_attrs <- set g.graph_attrs key value
  let edge_attr g key value = g.edge_attrs <- set g.edge_attrs key value

  (* dot accepts any identifier in double quotes *)
  let quote s =
    let b = Buffer.create (String.length s + 2) in
    Buffer.add_char b '"';
    String.iter (function '"' -> Buffer.add_string b "\\\"" | '\\' -> Buffer.add_string b "\\\\" | c -> Buffer.add_char b c) s;
    Buffer.add_char b '"';
    Buffer.contents b

  let attributes attrs =
    if attrs = [] then ""
    else " [" ^ String.concat " " (List.map (fun (k, v) -> k ^ "=" ^ quote v) attrs) ^ "]"

  let node g ?(attrs = []) name = g.lines <- g.lines @ [ "\t" ^ quote name ^ attributes attrs ]

  let edge g ?(attrs = []) tail head =
    g.lines <- g.lines @ [ "\t" ^ quote tail ^ " -> " ^ quote head ^ attributes attrs ]

  let source g =
    let graph = if g.graph_attrs = [] then [] else [ "\tgraph" ^ attributes g.graph_attrs ] in
    let edge = if g.edge_attrs = [] then [] else [ "\tedge" ^ attributes g.edge_attrs ] in
    "digraph " ^ quote g.name ^ " {\n" ^ String.concat "\n" (graph @ edge @ g.lines) ^ "\n}\n"

  let display g = display_svg (dot_to_svg ~engine:g.engine (source g))
end
