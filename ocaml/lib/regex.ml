(* The module Re mimics the part of Python's module re that the notebooks of chapter 3 use.
   It is a thin layer on top of the PCRE2 library (opam package pcre2), whose syntax for
   regular expressions is almost the same as Python's: named groups (?P<name>...), back
   references \1 and (?P=name), look-ahead (?=...) and (?!...), lazy quantifiers like *?,
   and {,n} are all supported.  Load it with

     load_lib "regex.ml"

   The functions follow Python's semantics, in particular
   - findall returns the whole matches if the pattern has no groups and the matches of the
     first group if it has exactly one group.  Python returns a list of tuples if there are
     several groups.  As a list of strings and a list of tuples have different types in OCaml,
     there is a second function findall_groups for this case.
   - Empty matches are found as in Python: after an empty match at position i, the next match
     must not be empty at i.
   - Patterns and subjects are UTF-8 strings.  \w, \d, \b, and IGNORECASE work with Unicode,
     as they do in Python.  Positions (start, end_) count bytes, not characters.
*)

#require "pcre2";;

module Re : sig
  type flag = IGNORECASE | MULTILINE | DOTALL | VERBOSE

  (* a match object *)
  type match_

  val group : match_ -> int -> string               (* mo.group(n); raises Not_found if group n did not match *)
  val named_group : match_ -> string -> string      (* mo.group('name') *)
  val lastgroup : match_ -> string option           (* mo.lastgroup *)
  val start : match_ -> int                         (* mo.start() *)
  val end_ : match_ -> int                          (* mo.end() *)

  val findall : ?flags:flag list -> string -> string -> string list
  val findall_groups : ?flags:flag list -> string -> string -> string list list
  val finditer : ?flags:flag list -> string -> string -> match_ Seq.t
  val search : ?flags:flag list -> string -> string -> match_ option
  val match_ : ?flags:flag list -> ?pos:int -> string -> string -> match_ option   (* re.match: anchored *)
  val sub : ?flags:flag list -> string -> string -> string -> string   (* the replacement is taken literally *)
  val sub_fn : ?flags:flag list -> string -> (match_ -> string) -> string -> string

  (* compiled patterns, i.e. the result of re.compile *)
  type pattern

  val compile : ?flags:flag list -> string -> pattern
  val pattern : pattern -> string                   (* p.pattern *)

  module Pattern : sig
    val findall : pattern -> string -> string list
    val search : pattern -> string -> match_ option
    val match_ : ?pos:int -> pattern -> string -> match_ option
  end
end = struct
  type flag = IGNORECASE | MULTILINE | DOTALL | VERBOSE

  type pattern = { source : string; rex : Pcre2.regexp; groups : int; names : string array }

  type match_ = { pat : pattern; subs : Pcre2.substrings }

  let cflag = function
    | IGNORECASE -> `CASELESS
    | MULTILINE -> `MULTILINE
    | DOTALL -> `DOTALL
    | VERBOSE -> `EXTENDED

  (* Like Python, we keep the compiled patterns in a cache. *)
  let cache : (string * flag list, pattern) Hashtbl.t = Hashtbl.create 64

  let compile ?(flags = []) (source : string) : pattern =
    match Hashtbl.find_opt cache (source, flags) with
    | Some p -> p
    | None ->
        let rex = Pcre2.regexp ~flags:(`UTF :: `UCP :: List.map cflag flags) source in
        let p = { source; rex; groups = Pcre2.capturecount rex; names = Pcre2.names rex } in
        Hashtbl.add cache (source, flags) p;
        p

  let pattern p = p.source

  let group m n = Pcre2.get_substring m.subs n
  let named_group m name = Pcre2.get_named_substring m.pat.rex name m.subs
  let start m = fst (Pcre2.get_substring_ofs m.subs 0)
  let end_ m = snd (Pcre2.get_substring_ofs m.subs 0)

  let lastgroup m =
    let matched name =
      match Pcre2.get_named_substring_ofs m.pat.rex name m.subs with
      | _ -> true
      | exception Not_found -> false
    in
    (* the matched named group with the largest number *)
    Array.to_list m.pat.names
    |> List.filter matched
    |> List.sort (fun a b -> compare (Pcre2.get_stringnumber m.pat.rex b) (Pcre2.get_stringnumber m.pat.rex a))
    |> function [] -> None | name :: _ -> Some name

  let exec ?(flags = []) p s pos =
    match Pcre2.exec ~flags ~rex:p.rex ~pos s with
    | subs -> Some { pat = p; subs }
    | exception Not_found -> None

  (* the position of the character after the one that starts at position i *)
  let next_char (s : string) (i : int) : int =
    if i >= String.length s then i + 1
    else i + Uchar.utf_decode_length (String.get_utf_8_uchar s i)

  (* all matches from left to right, with Python's treatment of empty matches *)
  let matches (p : pattern) (s : string) : match_ Seq.t =
    let rec from pos after_empty () =
      if pos > String.length s then Seq.Nil
      else
        let m =
          if after_empty then
            match exec ~flags:[ `NOTEMPTY_ATSTART; `ANCHORED ] p s pos with
            | Some m -> Some m
            | None -> if next_char s pos > String.length s then None else exec p s (next_char s pos)
          else exec p s pos
        in
        match m with
        | None -> Seq.Nil
        | Some m -> Seq.Cons (m, from (end_ m) (start m = end_ m))
    in
    from 0 false

  let group_or_empty m n = try group m n with Not_found -> ""

  let finditer ?flags source s = matches (compile ?flags source) s

  let findall_p (p : pattern) (s : string) : string list =
    match p.groups with
    | 0 -> List.of_seq (Seq.map (fun m -> group m 0) (matches p s))
    | 1 -> List.of_seq (Seq.map (fun m -> group_or_empty m 1) (matches p s))
    | _ -> invalid_arg "Re.findall: the pattern has several groups, use Re.findall_groups"

  let findall ?flags source s = findall_p (compile ?flags source) s

  let findall_groups ?flags source s =
    let p = compile ?flags source in
    List.of_seq (Seq.map (fun m -> List.init p.groups (fun i -> group_or_empty m (i + 1))) (matches p s))

  let search ?flags source s = exec (compile ?flags source) s 0

  let match_p ?(pos = 0) p s = exec ~flags:[ `ANCHORED ] p s pos
  let match_ ?flags ?pos source s = match_p ?pos (compile ?flags source) s

  let sub_fn ?flags source f s =
    let b = Buffer.create (String.length s) in
    let last =
      Seq.fold_left
        (fun last m ->
          Buffer.add_string b (String.sub s last (start m - last));
          Buffer.add_string b (f m);
          end_ m)
        0 (finditer ?flags source s)
    in
    Buffer.add_string b (String.sub s last (String.length s - last));
    Buffer.contents b

  let sub ?flags source repl s = sub_fn ?flags source (fun _ -> repl) s

  module Pattern = struct
    let findall = findall_p
    let search p s = exec p s 0
    let match_ = match_p
  end
end
