(* Larkita for OCaml, part 4: grammar rules, the canonical LR(1) automaton, the parser, and the
   construction of the abstract syntax trees (grammar.py, lr_analysis.py, lr_parser.py, ast_builder.py).

   The automaton is built with Knuth's construction: a state is a set of LR(1) items, i.e. dotted
   rules annotated with a set of lookahead tokens.  Shift/reduce conflicts are resolved in favour of
   shift.  Reduce/reduce conflicts are resolved in favour of the rule with the highest priority; if
   there is no such rule, the conflict is an error.  Optionally, states with the same core are merged
   afterwards, as long as this does not introduce new conflicts. *)

module Larkita_lr = struct
  open Larkita_base
  module SSet = Set.Make (String)

  (* ---------- Symbols and rules (grammar.py) ---------- *)

  (* A symbol of a rule.  For a terminal, filter_out is true if it was written as a string literal,
     so that it becomes part of the head instead of a child of the abstract syntax tree; text is the
     literal if it is a child (a literal inside an inner alternative that could not be expanded).
     Two symbols are equal if they are both terminals or both nonterminals and have the same name. *)
  type symbol = { name : string; is_term : bool; filter_out : bool; text : string option }

  let terminal ?(filter_out = false) ?text name = { name; is_term = true; filter_out; text }
  let nonterminal name = { name; is_term = false; filter_out = false; text = None }
  let symbol_equal (a : symbol) (b : symbol) = a.is_term = b.is_term && a.name = b.name

  (* Terminal(name, filter_out, text).fullrepr *)
  let symbol_fullrepr (s : symbol) : string =
    if s.is_term then
      Printf.sprintf "Terminal(%s, %s, %s)" (py_repr s.name) (if s.filter_out then "True" else "False")
        (match s.text with Some t -> py_repr t | None -> "None")
    else Printf.sprintf "NonTerminal(%s)" (py_repr s.name)

  (* How the abstract syntax tree of an alternative is built: None passes the abstract syntax tree of
     the only child through, Some head builds (head, child_1, ..., child_n).  Every AstSpec is a new
     object: two of them are only equal if they are the same object (spec_id). *)
  type ast_spec = { spec_id : int; head : string list option }

  let spec_counter = ref 0
  let ast_spec (head : string list option) : ast_spec = incr spec_counter; { spec_id = !spec_counter; head }

  (* A rule origin -> expansion.  ast is None for the auxiliary rules created for EBNF repetitions:
     their children are spliced into the tuple of the parent. *)
  type rule = {
    origin : string;
    expansion : symbol list;
    order : int;
    priority : int option;
    ast : ast_spec option;
  }

  let rule_equal (a : rule) (b : rule) = a.origin = b.origin && List.equal symbol_equal a.expansion b.expansion
  let rule_to_string (r : rule) = Printf.sprintf "<%s : %s>" r.origin (String.concat " " (List.map (fun s -> s.name) r.expansion))

  (* ---------- The parse table ---------- *)

  type action = Shift of int | Reduce of int      (* Reduce r: reduce using the rule with index r *)

  let end_ = "$END"

  type item = int * int   (* the index of the rule, the position of the dot *)

  type lr1_state = {
    index : int;
    mutable kernel : (item * SSet.t) list;
    mutable items : (item * SSet.t) list;        (* the kernel items first, then the closure items *)
    mutable transitions : (string * int) list;   (* symbol -> successor *)
    mutable actions : (string * action) list;
    mutable merged_from : int list;              (* the canonical LR(1) states merged into this state *)
  }

  (* A conflict in state state for the lookahead terminal.  rules are the rules that could be reduced
     (as indices), shift_state is the state the parser could shift to, and chosen is the action that
     was put into the table. *)
  type conflict = {
    c_state : int;
    c_terminal : string;
    c_rules : int list;
    shift_state : int option;
    chosen : action;
    resolved : bool;
    reason : string;
  }

  let conflict_kind (c : conflict) : string =
    if List.length c.c_rules > 1 then (if c.shift_state <> None then "shift/reduce/reduce" else "reduce/reduce")
    else "shift/reduce"

  (* Why two states with the same core have been kept apart: a symbol, the actions of both states for
     this symbol, and the terminal that the lexer could confuse with it. *)
  type difference = string * action option * action option * string option

  type analyzer = {
    start : string;
    rules : rule array;                         (* rule 0 is $root_<start> -> <start> *)
    rules_by_origin : (string, int list) Hashtbl.t;
    mutable states : lr1_state array;
    mutable conflicts : conflict list;
    mutable end_state : int;
    lr1_state_count : int;
    merge_states : bool;
    kept_apart : (int * int * difference list) list;
  }

  type parse_table = { table : (string, action) Hashtbl.t array; start_state : int; end_state : int }

  let core_key (kernel : (item * SSet.t) list) : item list = List.sort compare (List.map fst kernel)

  (* ---------- FIRST sets ---------- *)

  let first_sets (rules : rule array) : (string, SSet.t) Hashtbl.t * (string, unit) Hashtbl.t =
    let first = Hashtbl.create 64 in
    let nullable = Hashtbl.create 64 in
    let get n = Option.value (Hashtbl.find_opt first n) ~default:SSet.empty in
    let changed = ref true in
    while !changed do
      changed := false;
      Array.iter
        (fun rule ->
          let origin = rule.origin in
          let rec go = function
            | [] -> if not (Hashtbl.mem nullable origin) then (Hashtbl.replace nullable origin (); changed := true)
            | sym :: rest ->
                let target = get origin in
                if sym.is_term then begin
                  if not (SSet.mem sym.name target) then (Hashtbl.replace first origin (SSet.add sym.name target); changed := true)
                end
                else begin
                  let f = get sym.name in
                  if not (SSet.subset f target) then (Hashtbl.replace first origin (SSet.union target f); changed := true);
                  if Hashtbl.mem nullable sym.name then go rest
                end
          in
          go rule.expansion)
        rules
    done;
    (first, nullable)

  (* ---------- The automaton ---------- *)

  let build ?(merge_states = false) ?(collision : (string -> string list -> string option) option)
      (user_rules : rule list) (start : string) : analyzer =
    let root_rule = { origin = "$root_" ^ start; expansion = [ nonterminal start ]; order = 0; priority = None; ast = None } in
    let rules = Array.of_list (root_rule :: user_rules) in
    let rules_by_origin = Hashtbl.create 64 in
    Array.iteri
      (fun i r ->
        Hashtbl.replace rules_by_origin r.origin (Option.value (Hashtbl.find_opt rules_by_origin r.origin) ~default:[] @ [ i ]))
      rules;
    let rule_list = Array.to_list rules in
    let duplicates =
      List.filteri (fun i r -> List.exists (rule_equal r) (take i rule_list)) rule_list
    in
    if duplicates <> [] then
      raise (Grammar_error ("Rules defined twice: " ^ String.concat ", " (List.map rule_to_string duplicates)));
    if not (Hashtbl.mem rules_by_origin start) then
      raise (Grammar_error (Printf.sprintf "The start symbol %s is not defined" (py_repr start)));
    Array.iter
      (fun r ->
        List.iter
          (fun s -> if (not s.is_term) && not (Hashtbl.mem rules_by_origin s.name) then
              raise (Grammar_error ("Using an undefined rule: " ^ s.name)))
          r.expansion)
      rules;
    let first, nullable = first_sets rules in
    let first_of n = Option.value (Hashtbl.find_opt first n) ~default:SSet.empty in
    let first_of_sequence (symbols : symbol list) : SSet.t * bool =
      let rec go acc = function
        | [] -> (acc, true)
        | sym :: rest ->
            if sym.is_term then (SSet.add sym.name acc, false)
            else
              let acc = SSet.union acc (first_of sym.name) in
              if Hashtbl.mem nullable sym.name then go acc rest else (acc, false)
      in
      go SSet.empty symbols
    in
    (* for every item [A : alpha . B beta]: B, FIRST(beta), and whether beta is nullable *)
    let after_dot = Hashtbl.create 256 in
    Array.iteri
      (fun r rule ->
        List.iteri
          (fun d sym ->
            if not sym.is_term then begin
              let f, nl = first_of_sequence (drop (d + 1) rule.expansion) in
              Hashtbl.replace after_dot (r, d) (sym.name, f, nl)
            end)
          rule.expansion)
      rules;
    let expansion_arr = Array.map (fun r -> Array.of_list r.expansion) rules in
    let closure (kernel : (item * SSet.t) list) : (item, SSet.t) Hashtbl.t =
      let items = Hashtbl.create 32 in
      List.iter (fun (it, la) -> Hashtbl.replace items it la) kernel;
      let todo = ref (List.rev (List.map fst kernel)) in
      while !todo <> [] do
        let item = List.hd !todo in
        todo := List.tl !todo;
        match Hashtbl.find_opt after_dot item with
        | None -> ()
        | Some (name, f, nl) ->
            let lookaheads = if nl then SSet.union f (Hashtbl.find items item) else f in
            List.iter
              (fun r ->
                let new_item = (r, 0) in
                match Hashtbl.find_opt items new_item with
                | None -> Hashtbl.replace items new_item lookaheads; todo := new_item :: !todo
                | Some current ->
                    if not (SSet.subset lookaheads current) then begin
                      Hashtbl.replace items new_item (SSet.union current lookaheads);
                      todo := new_item :: !todo
                    end)
              (Hashtbl.find rules_by_origin name)
      done;
      items
    in
    (* building the states *)
    let states = ref [||] in
    let count = ref 0 in
    let index = Hashtbl.create 256 in
    let get_state (kernel : (item * SSet.t) list) : int =
      let key = List.sort compare (List.map (fun (it, la) -> (it, SSet.elements la)) kernel) in
      match Hashtbl.find_opt index key with
      | Some i -> i
      | None ->
          let i = !count in
          Hashtbl.add index key i;
          let st = { index = i; kernel; items = []; transitions = []; actions = []; merged_from = [ i ] } in
          if i >= Array.length !states then begin
            let bigger = Array.make (max 16 (2 * i)) st in
            Array.blit !states 0 bigger 0 (Array.length !states);
            states := bigger
          end;
          !states.(i) <- st;
          incr count;
          i
    in
    ignore (get_state [ ((0, 0), SSet.singleton end_) ]);
    let i = ref 0 in
    while !i < !count do
      let state = !states.(!i) in
      incr i;
      let items = closure state.kernel in
      let kernel_items = List.sort compare (List.map fst state.kernel) in
      let closure_items =
        List.sort compare (Hashtbl.fold (fun it _ acc -> if List.mem_assoc it state.kernel then acc else it :: acc) items [])
      in
      state.items <- List.map (fun it -> (it, Hashtbl.find items it)) (kernel_items @ closure_items);
      (* the successors, in the order of the first occurrence of their symbols *)
      let successors = ref [] in
      List.iter
        (fun ((r, d), la) ->
          let exp = expansion_arr.(r) in
          if d < Array.length exp then begin
            let name = exp.(d).name in
            match List.assoc_opt name !successors with
            | Some k -> k := !k @ [ ((r, d + 1), la) ]
            | None -> successors := !successors @ [ (name, ref [ ((r, d + 1), la) ]) ]
          end)
        state.items;
      state.transitions <- List.map (fun (name, k) -> (name, get_state !k)) !successors
    done;
    let states = Array.sub !states 0 !count in
    let end_state = List.assoc start states.(0).transitions in
    (* the actions *)
    let conflicts = ref [] in
    let priority r = Option.value rules.(r).priority ~default:0 in
    Array.iter
      (fun state ->
        let actions = ref (List.map (fun (name, target) -> (name, Shift target)) state.transitions) in
        let reductions = Hashtbl.create 16 in
        List.iter
          (fun ((r, d), la) ->
            if d = Array.length expansion_arr.(r) && r <> 0 then
              SSet.iter
                (fun t -> Hashtbl.replace reductions t (Option.value (Hashtbl.find_opt reductions t) ~default:[] @ [ r ]))
                la)
          state.items;
        let terminals = List.sort compare (Hashtbl.fold (fun t _ acc -> t :: acc) reductions []) in
        List.iter
          (fun terminal ->
            let rs = Hashtbl.find reductions terminal in
            let rule = ref (List.hd rs) in
            let resolved = ref true and reason = ref "" in
            if List.length rs > 1 then begin
              let top = List.fold_left (fun m r -> max m (priority r)) min_int rs in
              let best = List.filter (fun r -> priority r = top) rs in
              rule := List.hd best;
              if List.length best = 1 then reason := "resolved by priority"
              else (resolved := false; reason := "not resolved")
            end;
            match List.assoc_opt terminal !actions with
            | Some (Shift target as chosen) ->
                if !resolved then reason := "resolved as shift";
                conflicts :=
                  { c_state = state.index; c_terminal = terminal; c_rules = rs; shift_state = Some target; chosen;
                    resolved = !resolved; reason = !reason } :: !conflicts
            | Some (Reduce _) -> assert false
            | None ->
                actions := !actions @ [ (terminal, Reduce !rule) ];
                if List.length rs > 1 then
                  conflicts :=
                    { c_state = state.index; c_terminal = terminal; c_rules = rs; shift_state = None;
                      chosen = Reduce !rule; resolved = !resolved; reason = !reason } :: !conflicts)
          terminals;
        state.actions <- !actions)
      states;
    let conflicts = List.rev !conflicts in
    let lr1_state_count = Array.length states in
    if not merge_states then
      { start; rules; rules_by_origin; states; conflicts; end_state; lr1_state_count; merge_states; kept_apart = [] }
    else begin
      (* ---------- merging states with the same core ---------- *)
      let is_nonterminal name = Hashtbl.mem rules_by_origin name in
      let terminals_of (st : lr1_state) =
        SSet.of_list (List.filter_map (fun (n, _) -> if is_nonterminal n then None else Some n) st.actions)
      in
      let collisions (nw : SSet.t) (accepted : SSet.t) : (string * string) list =
        match collision with
        | None -> []
        | Some f ->
            List.filter_map
              (fun name -> match f name (SSet.elements accepted) with Some o -> Some (name, o) | None -> None)
              (SSet.elements nw)
      in
      let terminals = Array.map terminals_of states in
      let mergeable members1 terminals1 members2 terminals2 =
        List.for_all (fun i -> collisions (SSet.diff terminals2 terminals.(i)) terminals.(i) = []) members1
        && List.for_all (fun i -> collisions (SSet.diff terminals1 terminals.(i)) terminals.(i) = []) members2
      in
      let signature (st : lr1_state) (block : int array) : (string * action) list =
        List.map (fun (name, a) -> match a with Shift t -> (name, Shift block.(t)) | Reduce r -> (name, Reduce r)) st.actions
      in
      let compatible (sg : (string * action) list) (merged : (string, action) Hashtbl.t) =
        List.for_all (fun (name, a) -> match Hashtbl.find_opt merged name with Some b -> a = b | None -> true) sg
      in
      let cores = Hashtbl.create 64 in
      let block =
        ref
          (Array.map
             (fun st ->
               let key = core_key st.kernel in
               match Hashtbl.find_opt cores key with
               | Some b -> b
               | None -> let b = Hashtbl.length cores in Hashtbl.add cores key b; b)
             states)
      in
      let n = Array.length states in
      let finished = ref false in
      while not !finished do
        let members = Hashtbl.create 64 in
        Array.iter
          (fun st ->
            let b = !block.(st.index) in
            Hashtbl.replace members b (Option.value (Hashtbl.find_opt members b) ~default:[] @ [ st ]))
          states;
        let new_block = Array.make n 0 in
        let cnt = ref 0 in
        let blocks = List.sort compare (Hashtbl.fold (fun b _ acc -> b :: acc) members []) in
        List.iter
          (fun b ->
            (* parts: merged signature, indices, merged terminals *)
            let parts = ref [] in
            List.iter
              (fun st ->
                let i = st.index in
                let sg = signature st !block in
                let rec place = function
                  | [] -> false
                  | ((merged, indices, mterms) as part) :: rest ->
                      if compatible sg merged && mergeable !indices !mterms [ i ] terminals.(i) then begin
                        List.iter (fun (k, a) -> Hashtbl.replace merged k a) sg;
                        indices := !indices @ [ i ];
                        mterms := SSet.union !mterms terminals.(i);
                        ignore part;
                        true
                      end
                      else place rest
                in
                if not (place !parts) then begin
                  let merged = Hashtbl.create 16 in
                  List.iter (fun (k, a) -> Hashtbl.replace merged k a) sg;
                  parts := !parts @ [ (merged, ref [ i ], ref terminals.(i)) ]
                end)
              (Hashtbl.find members b);
            List.iter
              (fun (_, indices, _) ->
                List.iter (fun i -> new_block.(i) <- !cnt) !indices;
                incr cnt)
              !parts)
          blocks;
        block := new_block;
        if !cnt = Hashtbl.length members then finished := true
      done;
      let joined = ref true in
      while !joined do
        joined := false;
        let merged_actions = Hashtbl.create 64 in
        let block_members = Hashtbl.create 64 in
        let blocks_of_core = ref [] in
        Array.iter
          (fun st ->
            let b = !block.(st.index) in
            if not (Hashtbl.mem merged_actions b) then begin
              Hashtbl.add merged_actions b (Hashtbl.create 16);
              let key = core_key st.kernel in
              match List.assoc_opt key !blocks_of_core with
              | Some l -> l := !l @ [ b ]
              | None -> blocks_of_core := !blocks_of_core @ [ (key, ref [ b ]) ]
            end;
            List.iter (fun (k, a) -> Hashtbl.replace (Hashtbl.find merged_actions b) k a) (signature st !block);
            Hashtbl.replace block_members b (Option.value (Hashtbl.find_opt block_members b) ~default:[] @ [ st.index ]))
          states;
        let block_terminals b =
          List.fold_left (fun acc i -> SSet.union acc terminals.(i)) SSet.empty (Hashtbl.find block_members b)
        in
        let as_list h = Hashtbl.fold (fun k a acc -> (k, a) :: acc) h [] in
        (try
           List.iter
             (fun (_, blocks) ->
               let rec pairs = function [] -> [] | b1 :: rest -> List.map (fun b2 -> (b1, b2)) rest @ pairs rest in
               List.iter
                 (fun (b1, b2) ->
                   if
                     compatible (as_list (Hashtbl.find merged_actions b1)) (Hashtbl.find merged_actions b2)
                     && mergeable (Hashtbl.find block_members b1) (block_terminals b1) (Hashtbl.find block_members b2)
                          (block_terminals b2)
                   then begin
                     block := Array.map (fun b -> if b = b2 then b1 else b) !block;
                     joined := true;
                     raise Exit
                   end)
                 (pairs !blocks))
             !blocks_of_core
         with Exit -> ())
      done;
      (* the merged states are numbered in the order of their first canonical state *)
      let number = Hashtbl.create 64 in
      let new_index =
        Array.map
          (fun b ->
            match Hashtbl.find_opt number b with
            | Some k -> k
            | None -> let k = Hashtbl.length number in Hashtbl.add number b k; k)
          !block
      in
      let merged = ref [] in
      let get k =
        match List.nth_opt !merged k with
        | Some s -> s
        | None ->
            let s = { index = k; kernel = []; items = []; transitions = []; actions = []; merged_from = [] } in
            merged := !merged @ [ s ];
            s
      in
      let union_into l (key, value) =
        match List.assoc_opt key l with
        | Some v -> List.map (fun (k, v') -> if k = key then (k, SSet.union v value) else (k, v')) l |> fun l -> ignore v; l
        | None -> l @ [ (key, value) ]
      in
      let replace l (key, value) =
        if List.mem_assoc key l then List.map (fun (k, v) -> if k = key then (k, value) else (k, v)) l else l @ [ (key, value) ]
      in
      let merged_arr = Array.make (Hashtbl.length number) (get 0) in
      Array.iter
        (fun st ->
          let k = new_index.(st.index) in
          let nw = get k in
          merged_arr.(k) <- nw;
          nw.merged_from <- nw.merged_from @ [ st.index ];
          nw.kernel <- List.fold_left union_into nw.kernel st.kernel;
          nw.items <- List.fold_left union_into nw.items st.items;
          nw.transitions <- List.fold_left (fun l (name, t) -> replace l (name, new_index.(t))) nw.transitions st.transitions;
          nw.actions <-
            List.fold_left
              (fun l (name, a) -> replace l (name, match a with Shift t -> Shift new_index.(t) | r -> r))
              nw.actions st.actions)
        states;
      let seen = Hashtbl.create 16 in
      let merged_conflicts =
        List.filter_map
          (fun c ->
            let shift_state = Option.map (fun s -> new_index.(s)) c.shift_state in
            let chosen = match c.chosen with Shift t -> Shift new_index.(t) | r -> r in
            let key = (new_index.(c.c_state), c.c_terminal, c.c_rules, shift_state) in
            if Hashtbl.mem seen key then None
            else begin
              Hashtbl.add seen key ();
              Some { c with c_state = new_index.(c.c_state); shift_state; chosen }
            end)
          conflicts
      in
      let merged_conflicts = List.stable_sort (fun a b -> compare a.c_state b.c_state) merged_conflicts in
      (* why states with the same core have been kept apart *)
      let states_of_core = ref [] in
      Array.iter
        (fun st ->
          let key = core_key st.kernel in
          match List.assoc_opt key !states_of_core with
          | Some l -> l := !l @ [ st ]
          | None -> states_of_core := !states_of_core @ [ (key, ref [ st ]) ])
        merged_arr;
      let kept_apart = ref [] in
      List.iter
        (fun (_, group) ->
          let rec go = function
            | [] -> ()
            | s :: rest ->
                List.iter
                  (fun t ->
                    let names =
                      List.sort compare
                        (List.filter_map (fun (n, _) -> if List.mem_assoc n t.actions then Some n else None) s.actions)
                    in
                    let differences =
                      List.filter_map
                        (fun name ->
                          let a = List.assoc name s.actions and b = List.assoc name t.actions in
                          if a <> b then Some (name, Some a, Some b, None) else None)
                        names
                    in
                    let ts = terminals_of s and tt = terminals_of t in
                    let d1 =
                      List.map (fun (name, other) -> (name, None, Some (List.assoc name t.actions), Some other)) (collisions (SSet.diff tt ts) ts)
                    in
                    let d2 =
                      List.map (fun (name, other) -> (name, Some (List.assoc name s.actions), None, Some other)) (collisions (SSet.diff ts tt) tt)
                    in
                    kept_apart := (s.index, t.index, differences @ d1 @ d2) :: !kept_apart)
                  rest;
                go rest
          in
          go !group)
        !states_of_core;
      { start; rules; rules_by_origin; states = merged_arr; conflicts = merged_conflicts; end_state = new_index.(end_state);
        lr1_state_count; merge_states; kept_apart = List.rev !kept_apart }
    end

  let parse_table (a : analyzer) : parse_table =
    let table =
      Array.map
        (fun st ->
          let h = Hashtbl.create 16 in
          List.iter (fun (name, act) -> Hashtbl.replace h name act) st.actions;
          h)
        a.states
    in
    { table; start_state = 0; end_state = a.end_state }

  (* the names of the symbols of the state, i.e. the terminals that the contextual lexer accepts *)
  let accepted_symbols (a : analyzer) : string list array = Array.map (fun st -> List.map fst st.actions) a.states

  (* ---------- The parser (lr_parser.py) ---------- *)

  (* parse table rules callbacks of_token next_token format_expected:  next_token state returns the
     next token for the parser state state, or None at the end of the input.  callbacks.(r) builds
     the value of a reduction by rule r from the values of the symbols of its right hand side. *)
  let parse (pt : parse_table) (rules : rule array) (callbacks : ('v list -> 'v) array) (of_token : token -> 'v)
      (next_token : parser_state -> token option) (format_expected : string list -> string) : 'v =
    let state_stack = ref [ pt.start_state ] in
    let value_stack = ref [] in
    let height = ref 1 in
    let current () = { stack_height = !height; position = List.hd !state_stack } in
    let feed_token (token : token) (is_end : bool) : 'v option =
      let result = ref None in
      let continue = ref true in
      while !continue do
        let state = List.hd !state_stack in
        match Hashtbl.find_opt pt.table.(state) token.type_ with
        | None ->
            let expected =
              Hashtbl.fold (fun s _ acc -> if is_upper s then s :: acc else acc) pt.table.(state) []
            in
            let expected = if state = pt.end_state && not is_end then end_ :: expected else expected in
            let expected = dedup expected in
            let what =
              if token.type_ = end_ then "Unexpected end of input" else "Unexpected token " ^ token_repr token
            in
            let message =
              Printf.sprintf "%s at line %d, column %d.\n%s" what token.line token.column (format_expected expected)
            in
            raise
              (Unexpected_input
                 { kind = Unexpected_token { token; expected }; line = token.line; column = token.column;
                   pos_in_stream = token.start_pos; byte_pos = token.byte_pos; state = Some (current ()); message })
        | Some (Shift target) ->
            assert (not is_end);
            state_stack := target :: !state_stack;
            value_stack := of_token token :: !value_stack;
            incr height;
            continue := false
        | Some (Reduce r) ->
            let rule = rules.(r) in
            let size = List.length rule.expansion in
            let children = List.rev (take size !value_stack) in
            value_stack := drop size !value_stack;
            state_stack := drop size !state_stack;
            height := !height - size;
            let value = callbacks.(r) children in
            (match Hashtbl.find_opt pt.table.(List.hd !state_stack) rule.origin with
             | Some (Shift new_state) ->
                 state_stack := new_state :: !state_stack;
                 value_stack := value :: !value_stack;
                 incr height;
                 if is_end && new_state = pt.end_state then (result := Some value; continue := false)
             | _ -> assert false)
      done;
      !result
    in
    let last = ref None in
    let rec loop () =
      match next_token (current ()) with
      | Some token -> last := Some token; ignore (feed_token token false); loop ()
      | None -> ()
    in
    loop ();
    (* the end state is only reached by a goto, so after the last shift we always have to reduce *)
    let end_token =
      match !last with
      | Some t -> { t with type_ = end_; value = "" }
      | None ->
          { type_ = end_; value = ""; start_pos = 0; line = 1; column = 1; end_line = 0; end_column = 0; end_pos = 0; byte_pos = 0 }
    in
    match feed_token end_token true with Some v -> v | None -> assert false

  (* ---------- Abstract syntax trees (ast_builder.py) ---------- *)

  type kind = K_node | K_token | K_literal | K_splice

  (* the positions of the children in the rule's expansion, together with their kind *)
  let plan (rule : rule) (aux_names : SSet.t) : (int * kind) list =
    List.concat
      (List.mapi
         (fun i sym ->
           if sym.is_term then
             if sym.text <> None then [ (i, K_literal) ] else if not sym.filter_out then [ (i, K_token) ] else []
           else if SSet.mem sym.name aux_names then [ (i, K_splice) ]
           else [ (i, K_node) ])
         rule.expansion)

  let aux_names (rules : rule list) : SSet.t =
    SSet.of_list (List.filter_map (fun r -> if r.ast = None then Some r.origin else None) rules)

  (* the values on the stack of the parser: an abstract syntax tree, the children collected by an
     auxiliary rule, or a token *)
  type value = Ast of ast | Kids of ast list | Token of token

  let make_callback (rule : rule) (aux : SSet.t) : value list -> value =
    let pl = plan rule aux in
    let expansion = Array.of_list rule.expansion in
    let collect (children : value array) : ast list =
      List.concat_map
        (fun (i, kind) ->
          match (kind, children.(i)) with
          | K_node, Ast a -> [ a ]
          | K_token, Token t -> [ Tok (t.type_, t.value) ]
          | K_literal, _ -> [ Lit (Option.get expansion.(i).text) ]
          | K_splice, Kids l -> l
          | _ -> failwith ("Larkita: unexpected value in rule " ^ rule_to_string rule))
        pl
    in
    match rule.ast with
    | None -> fun children -> Kids (collect (Array.of_list children))
    | Some { head = None; _ } -> (
        match pl with
        | [ (i, K_token) ] ->
            fun children -> (match List.nth children i with Token t -> Ast (Tok (t.type_, t.value)) | v -> v)
        | [ (i, K_node) ] -> fun children -> List.nth children i
        | _ -> failwith ("Larkita: a pass-through rule needs exactly one child: " ^ rule_to_string rule))
    | Some { head = Some head; _ } -> fun children -> Ast (Node (head, collect (Array.of_list children)))

  let format_head (head : string list) : string =
    match head with [ h ] -> json_quote h | hs -> "(" ^ String.concat ", " (List.map json_quote hs) ^ ")"

  (* describes the abstract syntax tree that a reduction by rule produces, e.g. ("+", ast(expr), ast(product)) *)
  let describe_ast (rule : rule) (aux : SSet.t) : string =
    let parts =
      List.map
        (fun (i, kind) ->
          let sym = List.nth rule.expansion i in
          match kind with
          | K_node -> Printf.sprintf "ast(%s)" sym.name
          | K_token -> Printf.sprintf "(%s, text)" (json_quote sym.name)
          | K_literal -> json_quote (Option.get sym.text)
          | K_splice -> "*" ^ sym.name)
        (plan rule aux)
    in
    match rule.ast with
    | None -> "[" ^ String.concat ", " parts ^ "]"
    | Some { head = None; _ } -> List.hd parts
    | Some { head = Some head; _ } ->
        if parts = [] then "(" ^ format_head head ^ ",)" else "(" ^ String.concat ", " (format_head head :: parts) ^ ")"
end
