(* Larkita for OCaml, part 5: the output file that documents the LR(1) automaton (report.py).

   The file lists the rules (with the shape of the abstract syntax tree that every rule produces),
   the conflicts, and all states.  Every state shows its kernel and closure items in the form

       A → α • β    { lookahead tokens }

   followed by its actions and gotos.  If the states have been merged, the file also explains which
   states could not be merged.  The format is the same as that of the Python version. *)

module Larkita_report = struct
  open Larkita_base
  open Larkita_lr

  type formatter = { a : analyzer; names : (string, string) Hashtbl.t }

  let symbol f name = Option.value (Hashtbl.find_opt f.names name) ~default:name

  (* the rows of the action table are sorted by symbol, with $END last *)
  let sort_rows rows = List.stable_sort (fun r1 r2 -> compare (List.hd r1 = end_, List.hd r1) (List.hd r2 = end_, List.hd r2)) rows

  let lookaheads f (terminals : SSet.t) : string =
    let names = List.sort compare (List.filter_map (fun t -> if t = end_ then None else Some (symbol f t)) (SSet.elements terminals)) in
    let names = if SSet.mem end_ terminals then names @ [ end_ ] else names in
    "{ " ^ String.concat ", " names ^ " }"

  let rule_text f (r : int) : string =
    let rule = f.a.rules.(r) in
    let rhs = String.concat " " (List.map (fun s -> symbol f s.name) rule.expansion) in
    Printf.sprintf "%s → %s" rule.origin (if rhs = "" then "ε" else rhs)

  let item_text f (r : int) (dot : int) : string =
    let rule = f.a.rules.(r) in
    let names = List.map (fun s -> symbol f s.name) rule.expansion in
    Printf.sprintf "%s → %s" rule.origin (String.concat " " (take dot names @ [ "•" ] @ drop dot names))

  let number (r : int) : string = Printf.sprintf "(%d)" r

  let conflict_text ?(with_state = true) f (c : conflict) : string =
    let options =
      (match c.shift_state with Some s -> [ Printf.sprintf "shift and go to state %d" s ] | None -> [])
      @ List.map (fun r -> Printf.sprintf "reduce using rule %s %s" (number r) (rule_text f r)) c.c_rules
    in
    let status =
      if not c.resolved then "NOT RESOLVED (this is an error)"
      else match c.chosen with Shift _ -> c.reason | Reduce r -> Printf.sprintf "%s: reduce using rule %s" c.reason (number r)
    in
    let where = if with_state then Printf.sprintf "State %d, " c.c_state else "" in
    String.concat "\n"
      (Printf.sprintf "%s%s conflict on %s, %s" where (conflict_kind c) (symbol f c.c_terminal) status
       :: List.map (fun o -> "    - " ^ o) options)

  (* what a state does for the symbol name, e.g. "reduces using rule (3) a → b" *)
  let action_text f (name : string) (action : action option) (other : string option) : string =
    match action with
    | None -> Printf.sprintf "does not accept %s (collides with %s)" (symbol f name) (symbol f (Option.get other))
    | Some (Reduce r) -> Printf.sprintf "reduces using rule %s %s" (number r) (rule_text f r)
    | Some (Shift s) ->
        if Hashtbl.mem f.a.rules_by_origin name then Printf.sprintf "goes to state %d" s
        else Printf.sprintf "shifts and goes to state %d" s

  let merged_states f : string list =
    let a = f.a in
    let out =
      [ "States with the same core have been merged, unless this would cause";
        Printf.sprintf "a conflict: %d LR(1) states have been merged into %d states." a.lr1_state_count (Array.length a.states) ]
    in
    if a.kept_apart = [] then out @ [ "All states with the same core have been merged, so this is the"; "LALR(1) automaton."; "" ]
    else begin
      let is_cause (_, _, differences) =
        List.exists
          (fun (_, s, t, _) -> match (s, t) with Some (Shift _), Some (Shift _) -> false | _ -> true)
          differences
      in
      let causes = List.filter is_cause a.kept_apart in
      let consequences = List.filter (fun p -> not (is_cause p)) a.kept_apart in
      let out = ref out in
      if causes <> [] then
        out :=
          !out
          @ [ ""; "The following states have the same core, but they have been kept";
              "apart.  Either merging them would cause a conflict, or one of them";
              "accepts a token that collides with a token of the other one, i.e.";
              "the scanner could read the same text as either of these tokens." ];
      List.iter
        (fun (s, t, differences) ->
          out := !out @ [ ""; Printf.sprintf "States %d and %d" s t ];
          List.iter
            (fun (name, action_s, action_t, other) ->
              out :=
                !out
                @ [ Printf.sprintf "    on %s:" (symbol f name);
                    Printf.sprintf "        state %d %s" s (action_text f name action_s other);
                    Printf.sprintf "        state %d %s" t (action_text f name action_t other) ])
            differences)
        causes;
      if consequences <> [] then begin
        out :=
          !out
          @ [ ""; "The following states have the same core, but they have been kept";
              "apart, because their successors have been kept apart:"; "" ];
        let pairs = List.map (fun (s, t, _) -> Printf.sprintf "%d and %d" s t) consequences in
        let lines = ref [ "   " ] in
        List.iter
          (fun pair ->
            let last = List.hd !lines in
            if String.length last + String.length pair > 66 then lines := "   " :: !lines;
            lines := (List.hd !lines ^ " " ^ pair ^ ",") :: List.tl !lines)
          pairs;
        let lines = List.rev !lines in
        let last = List.nth lines (List.length lines - 1) in
        let last = if String.ends_with ~suffix:"," last then String.sub last 0 (String.length last - 1) else last in
        out := !out @ take (List.length lines - 1) lines @ [ last ]
      end;
      !out @ [ "" ]
    end

  (* formats the rows so that their columns are aligned *)
  let table (rows : string list list) (indent : string) : string list =
    match rows with
    | [] -> []
    | first :: _ ->
        let k = List.length first - 1 in
        let widths = List.init k (fun i -> List.fold_left (fun m row -> max m (utf8_length (List.nth row i))) 0 rows) in
        List.map
          (fun row ->
            let cells = List.mapi (fun i w -> ljust (List.nth row i) w) widths @ [ List.nth row k ] in
            let line = indent ^ String.concat "  " cells in
            (* rstrip *)
            let n = ref (String.length line) in
            while !n > 0 && (line.[!n - 1] = ' ' || line.[!n - 1] = '\t' || line.[!n - 1] = '\n') do decr n done;
            String.sub line 0 !n)
          rows

  let format_report (a : analyzer) (terminal_names : (string, string) Hashtbl.t) (source : string) : string =
    let f = { a; names = terminal_names } in
    let user_rules = List.tl (Array.to_list a.rules) in
    let aux = aux_names user_rules in
    let errors = List.filter (fun c -> not c.resolved) a.conflicts in
    let title, states =
      if a.merge_states then
        ( "LR(1) automaton with merged states generated by Larkita",
          Printf.sprintf "%d (merged from %d LR(1) states)" (Array.length a.states) a.lr1_state_count )
      else ("Canonical LR(1) automaton generated by Larkita", string_of_int (Array.length a.states))
    in
    let out = ref [] in
    let add lines = out := List.rev_append lines !out in
    add
      [ title; ""; "Grammar:      " ^ source; "Start symbol: " ^ a.start; "States:       " ^ states;
        Printf.sprintf "Conflicts:    %d (%d unresolved)" (List.length a.conflicts) (List.length errors); ""; "";
        "Rules"; "====="; "";
        "Every rule is followed by the abstract syntax tree that a reduction produces.";
        "\"*name\" stands for the list of children collected by the auxiliary rule \"name\".";
        "\"text\" stands for the text of a named token."; "" ];
    let rows =
      List.mapi (fun i rule -> [ number (i + 1); rule_text f (i + 1); "⇒ " ^ describe_ast rule aux ]) user_rules
    in
    add (table ([ number 0; rule_text f 0; "" ] :: rows) "  ");
    add [ ""; ""; "Conflicts"; "========="; "" ];
    if a.conflicts <> [] then List.iter (fun c -> add [ conflict_text f c; "" ]) a.conflicts
    else add [ "There are no conflicts."; "" ];
    if a.merge_states then add ([ ""; "Merged states"; "============="; "" ] @ merged_states f);
    add
      [ ""; "States"; "======"; "";
        "Every state lists its items with their lookahead tokens; closure items are marked with \"+\".";
        "Rules are referred to by their numbers in the list of rules." ];
    Array.iter
      (fun state ->
        let header = Printf.sprintf "State %d" state.index in
        let header =
          if List.length state.merged_from > 1 then
            header ^ "   (merged from the LR(1) states " ^ String.concat ", " (List.map string_of_int state.merged_from) ^ ")"
          else header
        in
        add [ ""; ""; header; "" ];
        let rows =
          List.map
            (fun ((r, d), la) ->
              [ (if List.mem_assoc (r, d) state.kernel then "" else "+"); number r; item_text f r d; lookaheads f la ])
            state.items
        in
        add (table rows "  ");
        add [ "" ];
        let shifts = ref [] and reduces = ref [] and gotos = ref [] in
        List.iter
          (fun (name, action) ->
            match action with
            | Shift s ->
                if Hashtbl.mem a.rules_by_origin name then gotos := [ name; Printf.sprintf "go to state %d" s ] :: !gotos
                else shifts := [ symbol f name; Printf.sprintf "shift and go to state %d" s ] :: !shifts
            | Reduce r -> reduces := [ symbol f name; Printf.sprintf "reduce using rule %s %s" (number r) (rule_text f r) ] :: !reduces)
          state.actions;
        let reduces = List.rev !reduces in
        let reduces = if state.index = a.end_state then reduces @ [ [ end_; "accept" ] ] else reduces in
        let rows = sort_rows (List.rev !shifts) @ sort_rows reduces in
        let rows = if !gotos <> [] then rows @ [ [ ""; "" ] ] @ List.sort compare (List.rev !gotos) else rows in
        add (table rows "    ");
        let conflicts = List.filter (fun c -> c.c_state = state.index) a.conflicts in
        if conflicts <> [] then begin
          add [ "" ];
          List.iter
            (fun c -> add (List.map (fun line -> "    " ^ line) (String.split_on_char '\n' (conflict_text ~with_state:false f c))))
            conflicts
        end)
      a.states;
    String.concat "\n" (List.rev !out) ^ "\n"

  let write_report (path : string) (a : analyzer) (terminal_names : (string, string) Hashtbl.t) (source : string) : unit =
    Out_channel.with_open_bin path (fun oc -> output_string oc (format_report a terminal_names source))
end
