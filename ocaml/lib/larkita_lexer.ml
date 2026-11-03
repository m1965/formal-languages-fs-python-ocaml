(* Larkita for OCaml, part 3: terminals, tokens, and the lexers (lexer.py).

   The regular expressions of the terminals are written in Python's syntax.  They are matched with
   PCRE2, whose syntax is almost the same; translate_regexp bridges the few differences.  Like
   Python's re module, the lexer tries the terminals in a fixed order and takes the first one that
   matches (not the longest one):  terminals with a higher priority first, then terminals whose
   matches can be longer, then longer strings, and finally in the order of their names.  A string
   literal like "while" that is also matched by a regular expression like CNAME is found via the
   regular expression and then renamed (the "unless" callback of Lark). *)

module Larkita_lexer = struct
  open Larkita_base

  (* ---------- Patterns ---------- *)

  type pattern = {
    is_str : bool;          (* PatternStr (a string literal) or PatternRE (a regular expression) *)
    value : string;
    flags : string;         (* the flags as a sorted string without duplicates, e.g. "i" *)
    raw : string option;    (* the literal as written in the grammar, e.g. "\"+\"" *)
  }

  let normalize_flags (flags : string) : string =
    String.of_seq (List.to_seq (List.sort_uniq compare (List.of_seq (String.to_seq flags))))

  let pattern_str ?raw ?(flags = "") value = { is_str = true; value; flags = normalize_flags flags; raw }
  let pattern_re ?raw ?(flags = "") value = { is_str = false; value; flags = normalize_flags flags; raw }

  (* Patterns are equal if they have the same type, value, and flags (raw is not compared). *)
  let pattern_equal (p : pattern) (q : pattern) : bool = p.is_str = q.is_str && p.value = q.value && p.flags = q.flags

  (* re.escape of Python *)
  let re_escape (s : string) : string =
    let b = Buffer.create (String.length s * 2) in
    String.iter
      (fun c ->
        if String.contains "()[]{}?*+-|^$\\.&~# \t\n\r\011\012" c then Buffer.add_char b '\\';
        Buffer.add_char b c)
      s;
    Buffer.contents b

  let to_regexp (p : pattern) : string =
    let value = if p.is_str then re_escape p.value else p.value in
    String.fold_left (fun v f -> Printf.sprintf "(?%c:%s)" f v) value p.flags

  let width_cache : (string, int * int) Hashtbl.t = Hashtbl.create 64

  let width (p : pattern) : int * int =
    if p.is_str then (let n = utf8_length p.value in (n, n))
    else
      let r = to_regexp p in
      match Hashtbl.find_opt width_cache r with
      | Some w -> w
      | None ->
          let w = Larkita_regex.width r in
          Hashtbl.add width_cache r w;
          w

  let min_width p = fst (width p)
  let max_width p = snd (width p)

  (* repr(pattern) of Python: the repr of the regular expression *)
  let pattern_repr (p : pattern) : string = py_repr (to_regexp p)

  (* ---------- Translation to PCRE2 ---------- *)

  (* Translates a regular expression from Python's syntax into PCRE2's syntax:
     \uXXXX, \UXXXXXXXX -> \x{...},  \Z -> \z,  and the flags u, L, a are dropped from inline flag groups. *)
  let translate_regexp (r : string) : string =
    let n = String.length r in
    let b = Buffer.create (n + 8) in
    let i = ref 0 in
    while !i < n do
      let c = r.[!i] in
      if c = '\\' && !i + 1 < n then begin
        let d = r.[!i + 1] in
        if (d = 'u' && !i + 6 <= n) || (d = 'U' && !i + 10 <= n) then begin
          let k = if d = 'u' then 4 else 8 in
          Buffer.add_string b ("\\x{" ^ String.sub r (!i + 2) k ^ "}");
          i := !i + 2 + k
        end
        else if d = 'Z' then (Buffer.add_string b "\\z"; i := !i + 2)
        else (Buffer.add_char b c; Buffer.add_char b d; i := !i + 2)
      end
      else if c = '(' && !i + 1 < n && r.[!i + 1] = '?' then begin
        (* (?flags) or (?flags:  or (?flags-flags: *)
        let j = ref (!i + 2) in
        while !j < n && String.contains "aiLmsux-" r.[!j] do incr j done;
        if !j > !i + 2 && !j < n && (r.[!j] = ':' || r.[!j] = ')') then begin
          let flags = String.sub r (!i + 2) (!j - !i - 2) in
          let flags = String.concat "" (List.map (String.make 1) (List.filter (fun f -> not (String.contains "uLa" f)) (List.of_seq (String.to_seq flags)))) in
          let flags = if flags = "-" || (String.length flags > 0 && flags.[String.length flags - 1] = '-') then String.sub flags 0 (max 0 (String.length flags - 1)) else flags in
          if r.[!j] = ':' then Buffer.add_string b ("(?" ^ flags ^ ":")
          else if flags <> "" then Buffer.add_string b ("(?" ^ flags ^ ")");
          i := !j + 1
        end
        else (Buffer.add_string b "(?"; i := !i + 2)
      end
      else (Buffer.add_char b c; incr i)
    done;
    Buffer.contents b

  let regex_cache : (string, Pcre2.regexp) Hashtbl.t = Hashtbl.create 64

  (* compiles a regular expression in Python's syntax; raises Failure if PCRE2 rejects it *)
  let compile (r : string) : Pcre2.regexp =
    match Hashtbl.find_opt regex_cache r with
    | Some rex -> rex
    | None ->
        let rex =
          try Pcre2.regexp ~flags:[ `UTF; `UCP ] ("(*LF)" ^ translate_regexp r)
          with Pcre2.Error e ->
            failwith (match e with Pcre2.BadPattern (msg, _) -> msg | _ -> "cannot compile the regular expression")
        in
        Hashtbl.add regex_cache r rex;
        rex

  (* the match of the regular expression at byte position pos, as in re.match(r, s, pos) *)
  let match_at (rex : Pcre2.regexp) (s : string) (pos : int) : Pcre2.substrings option =
    try Some (Pcre2.exec ~rex ~flags:[ `ANCHORED; `NO_UTF_CHECK ] ~pos s) with Not_found -> None

  (* _get_match: the text that the regular expression r matches at the start of s *)
  let get_match ?(g_flags = "") (r : string) (s : string) : string option =
    let r = if g_flags = "" then r else Printf.sprintf "(?%s)%s" g_flags r in
    match match_at (compile r) s 0 with Some m -> Some (Pcre2.get_substring m 0) | None -> None

  (* ---------- Terminals ---------- *)

  type terminal_def = {
    name : string;
    pattern : pattern;
    priority : int;
    mutable from_literal : bool;    (* created for a literal (string or regular expression) used in a rule *)
  }

  let terminal_def ?(priority = 0) name pattern = { name; pattern; priority; from_literal = false }

  (* how a terminal is shown to the user: a literal as it is written in the grammar, otherwise its name *)
  let user_repr (t : terminal_def) : string =
    if t.from_literal then (match t.pattern.raw with Some r when r <> "" -> r | _ -> t.name) else t.name

  type lexer_conf = {
    terminals : terminal_def list;
    terminals_by_name : (string, terminal_def) Hashtbl.t;
    ignore : string list;
    g_regex_flags : string;
    skip_validation : bool;
  }

  let lexer_conf ?(g_regex_flags = "") ?(skip_validation = false) terminals ignore =
    let by_name = Hashtbl.create 32 in
    List.iter (fun t -> Hashtbl.replace by_name t.name t) terminals;
    { terminals; terminals_by_name = by_name; ignore; g_regex_flags; skip_validation }

  let format_expected (by_name : (string, terminal_def) Hashtbl.t) (expected : string list) : string =
    let expected = List.sort compare expected in
    let expected = List.map (fun n -> match Hashtbl.find_opt by_name n with Some t -> user_repr t | None -> n) expected in
    "Expected one of: \n\t* " ^ String.concat "\n\t* " expected ^ "\n"

  (* ---------- Scanner ---------- *)

  (* A scanner matches the alternative of the regular expressions of several terminals.  Its groups
     are counted, so that it knows which terminal has matched, even if the regular expressions of
     the terminals contain groups themselves. *)
  type scanner = {
    sc_terminals : terminal_def array;
    group_of : int array;              (* the number of the group of every terminal *)
    rex : Pcre2.regexp;
    full : Pcre2.regexp Lazy.t;        (* for fullmatch *)
    allowed_types : string list;
  }

  let make_scanner (terminals : terminal_def list) (g_flags : string) : scanner =
    let terminals = Array.of_list terminals in
    let group = ref 1 in
    let group_of =
      Array.map
        (fun t ->
          let g = !group in
          group := !group + 1 + Pcre2.capturecount (compile (to_regexp t.pattern));
          g)
        terminals
    in
    let alternatives = String.concat "|" (Array.to_list (Array.map (fun t -> "(" ^ to_regexp t.pattern ^ ")") terminals)) in
    let prefix = if g_flags = "" then "" else Printf.sprintf "(?%s)" g_flags in
    {
      sc_terminals = terminals;
      group_of;
      rex = (if terminals = [||] then compile "(?!)" else compile (prefix ^ alternatives));
      full = lazy (compile (prefix ^ "(?:" ^ alternatives ^ ")\\Z"));
      allowed_types = List.map (fun t -> t.name) (Array.to_list terminals);
    }

  let matched_terminal (sc : scanner) (m : Pcre2.substrings) : string =
    let result = ref "" in
    (try
       Array.iteri
         (fun i g ->
           match Pcre2.get_substring_ofs m g with
           | _ -> result := sc.sc_terminals.(i).name; raise Exit
           | exception Not_found -> ())
         sc.group_of
     with Exit -> ());
    !result

  (* the text and the type of the token at byte position pos *)
  let scanner_match (sc : scanner) (text : string) (pos : int) : (string * string) option =
    if sc.sc_terminals = [||] then None
    else
      match match_at sc.rex text pos with
      | Some m -> Some (Pcre2.get_substring m 0, matched_terminal sc m)
      | None -> None

  let scanner_fullmatch (sc : scanner) (text : string) : string option =
    match match_at (Lazy.force sc.full) text 0 with Some m -> Some (matched_terminal sc m) | None -> None

  (* r"""Expressions that may indicate newlines in a regexp""" *)
  let regexp_has_newline (r : string) : bool =
    let contains sub =
      let n = String.length sub in
      let rec go i = i + n <= String.length r && (String.sub r i n = sub || go (i + 1)) in
      go 0
    in
    String.contains r '\n' || contains "\\n" || contains "\\s" || contains "[^" || (contains "(?s" && String.contains r '.')

  (* ---------- The basic lexer ---------- *)

  (* the state of the lexer while it scans a text *)
  type lexer_state = {
    text : string;
    mutable byte_pos : int;
    mutable char_pos : int;
    mutable line : int;
    mutable line_start_pos : int;
    mutable last_token : token option;
  }

  let lexer_state (text : string) : lexer_state =
    if not (String.is_valid_utf_8 text) then invalid_arg "Larkita: the text is not valid UTF-8";
    { text; byte_pos = 0; char_pos = 0; line = 1; line_start_pos = 0; last_token = None }

  let column (st : lexer_state) : int = st.char_pos - st.line_start_pos + 1

  (* LineCounter.feed *)
  let feed (st : lexer_state) (value : string) (test_newline : bool) : unit =
    let len = utf8_length value in
    if test_newline then begin
      match String.rindex_opt value '\n' with
      | Some last ->
          String.iter (fun c -> if c = '\n' then st.line <- st.line + 1) value;
          st.line_start_pos <- st.char_pos + utf8_count value 0 last + 1
      | None -> ()
    end;
    st.char_pos <- st.char_pos + len;
    st.byte_pos <- st.byte_pos + String.length value

  type basic_lexer = {
    conf : lexer_conf;
    sorted_terminals : terminal_def list;
    newline_types : string list;
    ignore_types : string list;
    scanner : scanner Lazy.t;
    callback : (string * scanner) list Lazy.t;
  }

  (* _create_unless: string literals that are also matched by a regular expression *)
  let create_unless (terminals : terminal_def list) (g_flags : string) : terminal_def list * (string * scanner) list =
    let res = List.filter (fun t -> not t.pattern.is_str) terminals in
    let strs = List.filter (fun t -> t.pattern.is_str) terminals in
    let embedded = ref [] in
    let callback =
      List.filter_map
        (fun retok ->
          let unless =
            List.filter
              (fun strtok ->
                strtok.priority = retok.priority
                && begin
                     let s = strtok.pattern.value in
                     if get_match ~g_flags (to_regexp retok.pattern) s = Some s then begin
                       let subset = String.for_all (fun f -> String.contains retok.pattern.flags f) strtok.pattern.flags in
                       if subset then embedded := strtok.name :: !embedded;
                       true
                     end
                     else false
                   end)
              strs
          in
          if unless = [] then None else Some (retok.name, make_scanner unless g_flags))
        res
    in
    (List.filter (fun t -> not (List.mem t.name !embedded)) terminals, callback)

  let sort_key (t : terminal_def) = (- t.priority, - max_width t.pattern, - utf8_length t.pattern.value, t.name)

  let basic_lexer (conf : lexer_conf) : basic_lexer =
    let terminals = conf.terminals in
    if not conf.skip_validation then begin
      List.iter
        (fun t ->
          (try ignore (compile (to_regexp t.pattern))
           with Failure _ -> raise (Lex_error (Printf.sprintf "Cannot compile token %s: %s" t.name (pattern_repr t.pattern))));
          if min_width t.pattern = 0 then
            raise (Lex_error (Printf.sprintf "Lexer does not allow zero-width terminals. (%s: %s)" t.name (pattern_repr t.pattern))))
        terminals;
      let names = List.map (fun t -> t.name) terminals in
      let missing = List.filter (fun n -> not (List.mem n names)) conf.ignore in
      if missing <> [] then
        raise (Lex_error ("Ignore terminals are not defined: {" ^ String.concat ", " (List.map py_repr (dedup missing)) ^ "}"))
    end;
    let sorted = List.stable_sort (fun a b -> compare (sort_key a) (sort_key b)) terminals in
    let unless = lazy (create_unless sorted conf.g_regex_flags) in
    {
      conf;
      sorted_terminals = sorted;
      newline_types = List.filter_map (fun t -> if regexp_has_newline (to_regexp t.pattern) then Some t.name else None) terminals;
      ignore_types = conf.ignore;
      scanner = lazy (make_scanner (fst (Lazy.force unless)) conf.g_regex_flags);
      callback = lazy (snd (Lazy.force unless));
    }

  let unexpected_characters (st : lexer_state) (allowed : string list) (state : parser_state option)
      (by_name : (string, terminal_def) Hashtbl.t) : exn =
    let d = String.get_utf_8_uchar st.text st.byte_pos in
    let char = String.sub st.text st.byte_pos (Uchar.utf_decode_length d) in
    let e0 =
      { kind = Unexpected_characters { char; allowed }; line = st.line; column = column st;
        pos_in_stream = st.char_pos; byte_pos = st.byte_pos; state; message = "" }
    in
    let message =
      Printf.sprintf "No terminal matches '%s' in the current parser context, at line %d col %d" char st.line (column st)
      ^ "\n\n" ^ get_context e0 st.text
      ^ (if allowed <> [] then format_expected by_name allowed else "")
      ^ (match st.last_token with
         | Some t -> "\nPrevious tokens: " ^ token_repr t ^ "\n"
         | None -> "")
    in
    Unexpected_input { e0 with message }

  (* next_token: the next token that is not ignored, or None at the end of the text *)
  let next_token (lx : basic_lexer) (st : lexer_state) (state : parser_state option) : token option =
    let scanner = Lazy.force lx.scanner in
    let callback = Lazy.force lx.callback in
    let rec loop () =
      if st.byte_pos >= String.length st.text then None
      else
        match scanner_match scanner st.text st.byte_pos with
        | None ->
            let allowed = List.filter (fun n -> not (List.mem n lx.ignore_types)) scanner.allowed_types in
            let allowed = if allowed = [] then [ "<END-OF-FILE>" ] else allowed in
            raise (unexpected_characters st (dedup allowed) state lx.conf.terminals_by_name)
        | Some (value, type_) ->
            let ignored = List.mem type_ lx.ignore_types in
            let start_pos = st.char_pos and line = st.line and col = column st and byte_pos = st.byte_pos in
            feed st value (List.mem type_ lx.newline_types);
            if ignored then loop ()
            else begin
              let type_ =
                match List.assoc_opt type_ callback with
                | Some sc -> (match scanner_fullmatch sc value with Some t -> t | None -> type_)
                | None -> type_
              in
              let t =
                { type_; value; start_pos; line; column = col; end_line = st.line; end_column = column st;
                  end_pos = st.char_pos; byte_pos }
              in
              st.last_token <- Some t;
              Some t
            end
    in
    loop ()

  (* all tokens of the text (Larkita.lex) *)
  let lex_all (lx : basic_lexer) (text : string) : token Seq.t =
    let st = lexer_state text in
    let rec seq () = match next_token lx st None with Some t -> Seq.Cons (t, seq) | None -> Seq.Nil in
    seq

  (* ---------- The contextual lexer ---------- *)

  type contextual_lexer = {
    lexers : basic_lexer array;     (* the lexer of every state of the parser *)
    root_lexer : basic_lexer;
  }

  (* states.(i) are the symbols that state i accepts *)
  let contextual_lexer (conf : lexer_conf) (states : string list array) : contextual_lexer =
    let by_tokens = Hashtbl.create 16 in
    let lexers =
      Array.map
        (fun accepts ->
          let key = List.sort_uniq compare accepts in
          match Hashtbl.find_opt by_tokens key with
          | Some lx -> lx
          | None ->
              let accepts = List.sort_uniq compare (accepts @ conf.ignore) in
              let terminals = List.filter_map (fun n -> Hashtbl.find_opt conf.terminals_by_name n) accepts in
              let lx = basic_lexer { conf with terminals } in
              Hashtbl.add by_tokens key lx;
              lx)
        states
    in
    { lexers; root_lexer = basic_lexer { conf with skip_validation = true } }

  (* the next token in the parser state state *)
  let contextual_next_token (cl : contextual_lexer) (st : lexer_state) (state : parser_state) : token option =
    try next_token cl.lexers.(state.position) st (Some state)
    with Unexpected_input ({ kind = Unexpected_characters { allowed; _ }; _ } as e) -> (
      let last_token = st.last_token in
      match next_token cl.root_lexer st (Some state) with
      | exception Unexpected_input { kind = Unexpected_characters _; _ } -> raise (Unexpected_input e)
      | None -> raise (Unexpected_input e)
      | Some token ->
          let message =
            Printf.sprintf "Unexpected token %s at line %d, column %d.\n%s" (token_repr token) token.line token.column
              (format_expected cl.root_lexer.conf.terminals_by_name allowed)
            ^ Printf.sprintf "Previous tokens: [%s]\n" (match last_token with Some t -> token_repr t | None -> "None")
          in
          raise
            (Unexpected_input
               { kind = Unexpected_token { token; expected = allowed }; line = token.line; column = token.column;
                 pos_in_stream = token.start_pos; byte_pos = token.byte_pos; state = Some state; message }))

  (* ---------- Collisions (for merging states with the contextual lexer) ---------- *)

  (* token_collisions conf returns a function collision new accepted that tells whether the
     contextual lexer might split some input differently if the terminal new is added to the
     terminals accepted by a parser state.  It returns the colliding terminal, or None. *)
  let token_collisions (conf : lexer_conf) : string -> string list -> string option =
    let first_chars = Hashtbl.create 32 in
    let collides = Hashtbl.create 64 in
    let first (t : terminal_def) =
      match Hashtbl.find_opt first_chars t.name with
      | Some f -> f
      | None ->
          let f = Larkita_regex.first_chars (to_regexp t.pattern) in
          Hashtbl.add first_chars t.name f;
          f
    in
    let collide (nw : terminal_def) (old : terminal_def) : bool =
      match (first nw, first old) with
      | Some a, Some b when Larkita_regex.SSet.disjoint a b -> false
      | _ ->
          if (not nw.pattern.is_str) && old.pattern.is_str then true
          else if
            nw.pattern.is_str && (not old.pattern.is_str) && nw.priority = old.priority
            && get_match ~g_flags:conf.g_regex_flags (to_regexp old.pattern) nw.pattern.value = Some nw.pattern.value
          then true
          else compare (sort_key nw) (sort_key old) < 0
    in
    fun nw accepted ->
      match Hashtbl.find_opt conf.terminals_by_name nw with
      | None -> None
      | Some nw_def ->
          let names = List.sort_uniq compare (accepted @ conf.ignore) in
          List.find_opt
            (fun name ->
              match Hashtbl.find_opt conf.terminals_by_name name with
              | Some old when name <> nw ->
                  let key = (nw, name) in
                  let c =
                    match Hashtbl.find_opt collides key with
                    | Some c -> c
                    | None -> let c = collide nw_def old in Hashtbl.add collides key c; c
                  in
                  c
              | _ -> false)
            names
end
