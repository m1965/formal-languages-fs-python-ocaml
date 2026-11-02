let rec assign_numbers (address : int list) (t : nested) (nodes2Numbers : node_map) (n : int) : int =
  Hashtbl.replace nodes2Numbers address (string_of_int n);
  match t with
  | Str _ | Int _ | Float _ -> n + 1
  | Tuple (_, children) ->
      let n = ref (n + 1) in
      List.iteri (fun j child -> n := assign_numbers (address @ [ j + 1 ]) child nodes2Numbers !n) children;
      !n
