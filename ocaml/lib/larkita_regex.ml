(* Larkita for OCaml, part 2: the analysis of regular expressions in Python's syntax.

   Lark sorts the terminals by the maximal width of their regular expressions, and Larkita checks
   whether two terminals can start with the same character.  Python computes both with its parser
   for regular expressions (re._parser).  This module contains a small parser for the same syntax
   that builds just enough structure for these two questions:

     width re        = (min, max)   the minimal and maximal length of a match (getwidth)
     first_chars re  = Some chars   the characters a match can start with (lexer._first_chars) *)

module Larkita_regex = struct
  (* MAXWIDTH of Python is 2**64; every width at least as large is shown as maxwidth *)
  let maxwidth = max_int / 4
  let maxrepeat = 4294967295

  let add a b = if a >= maxwidth || b >= maxwidth then maxwidth else min (a + b) maxwidth
  let mul a b = if a = 0 || b = 0 then 0 else if a >= maxwidth / b then maxwidth else min (a * b) maxwidth

  type item = ILit of int | IRange of int * int | IOther   (* IOther: a category like \d, or NEGATE *)

  type node =
    | Literal of int
    | In of item list
    | Unit                              (* ANY, CATEGORY, NOT_LITERAL: one character *)
    | At                                (* ^, $, \b, \A, ...: no character *)
    | Assert                            (* look-ahead and look-behind *)
    | Branch of node list list
    | Sub of node list                  (* a group *)
    | Atomic of node list
    | Repeat of int * int * node list   (* min, max (maxrepeat: unbounded), body *)
    | Groupref

  exception Error

  let parse (pattern : string) : node list =
    let s = Array.of_list (List.map Uchar.to_int (Larkita_base.utf8_decode pattern)) in
    let n = Array.length s in
    let pos = ref 0 in
    let peek () = if !pos < n then Some s.(!pos) else None in
    let next () = if !pos < n then (let c = s.(!pos) in incr pos; c) else raise Error in
    let is_c c ch = c = Char.code ch in
    let digit c = c >= 48 && c <= 57 in
    let hex c = digit c || (c >= 97 && c <= 102) || (c >= 65 && c <= 70) in
    let hex_value k =
      let v = ref 0 in
      for _ = 1 to k do
        let c = next () in
        if not (hex c) then raise Error;
        v := (!v * 16) + int_of_string ("0x" ^ String.make 1 (Char.chr c))
      done;
      !v
    in
    let verbose = ref false in
    (* an escape sequence after the backslash; in_class: inside [...] *)
    let escape (in_class : bool) : node =
      let c = next () in
      let lit v = Literal v in
      match Char.unsafe_chr (if c < 128 then c else 0) with
      | 'd' | 'D' | 's' | 'S' | 'w' | 'W' -> if in_class then In [ IOther ] else In [ IOther ]
      | 'b' -> if in_class then lit 8 else At
      | 'B' | 'A' | 'Z' -> if in_class then raise Error else At
      | 'n' -> lit 10
      | 't' -> lit 9
      | 'r' -> lit 13
      | 'f' -> lit 12
      | 'v' -> lit 11
      | 'a' -> lit 7
      | 'x' -> lit (hex_value 2)
      | 'u' -> lit (hex_value 4)
      | 'U' -> lit (hex_value 8)
      | 'N' ->
          (* \N{name}: the character is unknown here, but it is one character *)
          while peek () <> None && not (is_c (next ()) '}') do () done;
          Unit
      | '0' ->
          let v = ref 0 in
          let k = ref 0 in
          while !k < 2 && (match peek () with Some d -> d >= 48 && d <= 55 | None -> false) do
            v := (!v * 8) + (next () - 48);
            incr k
          done;
          lit !v
      | '1' .. '9' ->
          if in_class then begin
            (* an octal escape *)
            let v = ref (c - 48) in
            let k = ref 0 in
            while !k < 2 && (match peek () with Some d -> d >= 48 && d <= 55 | None -> false) do
              v := (!v * 8) + (next () - 48);
              incr k
            done;
            lit !v
          end
          else begin
            while (match peek () with Some d -> digit d | None -> false) do ignore (next ()) done;
            Groupref
          end
      | _ ->
          if (c >= 65 && c <= 90) || (c >= 97 && c <= 122) then raise Error;
          lit c
    in
    let parse_class () : node =
      (* after '[' *)
      let items = ref [] in
      let negate = (match peek () with Some c when is_c c '^' -> ignore (next ()); true | _ -> false) in
      let first = ref true in
      let continue = ref true in
      while !continue do
        let c = next () in
        if is_c c ']' && not !first then continue := false
        else begin
          first := false;
          let lo =
            if is_c c '\\' then escape true
            else Literal c
          in
          (* a range? *)
          match (lo, peek ()) with
          | Literal a, Some d when is_c d '-' && !pos + 1 < n && not (is_c s.(!pos + 1) ']') ->
              ignore (next ());
              let c2 = next () in
              let hi = if is_c c2 '\\' then escape true else Literal c2 in
              (match hi with
               | Literal b -> if b < a then raise Error; items := IRange (a, b) :: !items
               | _ -> raise Error)
          | Literal a, _ -> items := ILit a :: !items
          | In _, _ | Unit, _ -> items := IOther :: !items
          | _ -> raise Error
        end
      done;
      let items = List.rev !items in
      if negate then In (IOther :: items)
      else match items with [ ILit a ] -> Literal a | _ -> In items
    in
    let read_int () =
      let start = !pos in
      while (match peek () with Some d -> digit d | None -> false) do ignore (next ()) done;
      if !pos = start then None
      else Some (int_of_string (String.concat "" (List.map (fun c -> String.make 1 (Char.chr c)) (Array.to_list (Array.sub s start (!pos - start))))))
    in
    let rec parse_sub () : node list =
      (* alternatives separated by '|' *)
      let alts = ref [ parse_seq () ] in
      while (match peek () with Some c -> is_c c '|' | None -> false) do
        ignore (next ());
        alts := parse_seq () :: !alts
      done;
      match !alts with [ seq ] -> seq | alts -> [ Branch (List.rev alts) ]
    and parse_seq () : node list =
      let items = ref [] in
      let stop = ref false in
      while not !stop do
        match peek () with
        | None -> stop := true
        | Some c when is_c c '|' || is_c c ')' -> stop := true
        | Some c ->
            ignore (next ());
            if !verbose && (c = 32 || c = 9 || c = 10 || c = 13 || c = 11 || c = 12) then ()
            else if !verbose && is_c c '#' then
              while (match peek () with Some d -> d <> 10 | None -> false) do ignore (next ()) done
            else if is_c c '*' || is_c c '+' || is_c c '?' || (is_c c '{' && is_repeat ()) then begin
              let mn, mx =
                if is_c c '*' then (0, maxrepeat)
                else if is_c c '+' then (1, maxrepeat)
                else if is_c c '?' then (0, 1)
                else begin
                  let lo = read_int () in
                  let hi =
                    if (match peek () with Some d -> is_c d ',' | None -> false) then (ignore (next ()); read_int ())
                    else match lo with Some v -> Some v | None -> raise Error
                  in
                  if not (is_c (next ()) '}') then raise Error;
                  let lo = Option.value lo ~default:0 in
                  let hi = Option.value hi ~default:maxrepeat in
                  if hi < lo then raise Error;
                  (lo, hi)
                end
              in
              (* lazy or possessive *)
              (match peek () with Some d when is_c d '?' || is_c d '+' -> ignore (next ()) | _ -> ());
              match !items with
              | [] -> raise Error
              | (At | Repeat _) :: _ -> raise Error
              | last :: rest -> items := Repeat (mn, mx, [ last ]) :: rest
            end
            else items := atom c :: !items
      done;
      List.rev !items
    and is_repeat () =
      (* '{' starts a repetition only if it has the form {m}, {m,}, {,n} or {m,n} *)
      let p = ref !pos in
      let digits () = let st = !p in while !p < n && digit s.(!p) do incr p done; !p > st in
      let d1 = digits () in
      let ok =
        if !p < n && is_c s.(!p) ',' then (incr p; ignore (digits ()); !p < n && is_c s.(!p) '}')
        else d1 && !p < n && is_c s.(!p) '}'
      in
      ok
    and atom (c : int) : node =
      if is_c c '.' then Unit
      else if is_c c '^' || is_c c '$' then At
      else if is_c c '\\' then escape false
      else if is_c c '[' then parse_class ()
      else if is_c c '(' then group ()
      else Literal c
    and group () : node =
      let close () = if not (is_c (next ()) ')') then raise Error in
      match peek () with
      | Some q when is_c q '?' -> (
          ignore (next ());
          let c = next () in
          match Char.unsafe_chr (if c < 128 then c else 0) with
          | ':' -> let body = parse_sub () in close (); Sub body
          | '>' -> let body = parse_sub () in close (); Atomic body
          | '=' | '!' -> ignore (parse_sub ()); close (); Assert
          | '<' -> (
              match peek () with
              | Some d when is_c d '=' || is_c d '!' -> ignore (next ()); ignore (parse_sub ()); close (); Assert
              | _ -> raise Error)
          | 'P' -> (
              match next () with
              | d when is_c d '<' ->
                  while not (is_c (next ()) '>') do () done;
                  let body = parse_sub () in close (); Sub body
              | d when is_c d '=' -> while not (is_c (next ()) ')') do () done; Groupref
              | _ -> raise Error)
          | '#' -> while not (is_c (next ()) ')') do () done; At
          | '(' ->
              (* a conditional group (?(id)yes|no) *)
              while not (is_c (next ()) ')') do () done;
              let body = parse_sub () in close ();
              (match body with
               | [ Branch [ yes; no ] ] -> Branch [ yes; no ]
               | yes -> Branch [ yes; [] ])
          | _ ->
              (* flags: (?aiLmsux) or (?flags-flags:...) *)
              pos := !pos - 1;
              let flags = Buffer.create 4 in
              let negative = ref false in
              let finished = ref false in
              let scoped = ref false in
              while not !finished do
                let d = next () in
                if is_c d ':' then (scoped := true; finished := true)
                else if is_c d ')' then finished := true
                else if is_c d '-' then negative := true
                else if not !negative then Buffer.add_char flags (Char.chr d)
              done;
              let set_x = String.contains (Buffer.contents flags) 'x' in
              if !scoped then begin
                let saved = !verbose in
                if set_x then verbose := true;
                let body = parse_sub () in
                verbose := saved;
                close ();
                Sub body
              end
              else begin
                if set_x then verbose := true;
                At
              end)
      | _ -> let body = parse_sub () in close (); Sub body
    in
    let result = parse_sub () in
    if !pos < n then raise Error;
    result

  let rec width_seq (seq : node list) : int * int =
    List.fold_left (fun (lo, hi) node -> let i, j = width_node node in (add lo i, add hi j)) (0, 0) seq

  and width_node (node : node) : int * int =
    match node with
    | Literal _ | In _ | Unit -> (1, 1)
    | At | Assert -> (0, 0)
    | Groupref -> (0, maxwidth)
    | Sub body | Atomic body -> width_seq body
    | Branch alts ->
        List.fold_left
          (fun (lo, hi) alt -> let l, h = width_seq alt in (min lo l, max hi h))
          (maxwidth, 0) alts
    | Repeat (mn, mx, body) ->
        let i, j = width_seq body in
        (mul i mn, if mx = maxrepeat && j > 0 then maxwidth else mul j mx)

  (* the minimal and maximal width of a match, like re._parser.parse(r).getwidth() *)
  let width (pattern : string) : int * int =
    match parse pattern with
    | nodes -> width_seq nodes
    | exception Error -> raise (Larkita_base.Grammar_error ("Cannot parse the regular expression " ^ pattern))

  module SSet = Set.Make (String)

  (* the characters that a match can start with (lexer._first_chars) *)
  let first_chars (pattern : string) : SSet.t option =
    let exception Unknown in
    let chr c = Larkita_base.utf8_encode [ Uchar.of_int c ] in
    let rec first (seq : node list) : SSet.t * bool =
      let rec go result = function
        | [] -> (result, true)
        | At :: rest -> go result rest
        | node :: rest ->
            let chars, nullable =
              match node with
              | Literal c -> (SSet.singleton (chr c), false)
              | In items ->
                  ( List.fold_left
                      (fun acc item ->
                        match item with
                        | ILit c -> SSet.add (chr c) acc
                        | IRange (a, b) when b - a < 1000 ->
                            let acc = ref acc in
                            for k = a to b do acc := SSet.add (chr k) !acc done;
                            !acc
                        | _ -> raise Unknown)
                      SSet.empty items,
                    false )
              | Branch alts ->
                  List.fold_left
                    (fun (acc, nl) alt -> let sub, sn = first alt in (SSet.union acc sub, nl || sn))
                    (SSet.empty, false) alts
              | Sub body -> first body
              | Repeat (mn, _, body) -> let sub, nl = first body in (sub, nl || mn = 0)
              | _ -> raise Unknown
            in
            let result = SSet.union result chars in
            if not nullable then (result, false) else go result rest
      in
      go SSet.empty seq
    in
    match parse pattern with
    | exception Error -> None
    | nodes -> (
        match first nodes with
        | exception Unknown -> None
        | _, true -> None
        | chars, false ->
            Some
              (SSet.fold
                 (fun ch acc -> SSet.add (Larkita_base.to_lower ch) (SSet.add (Larkita_base.to_upper ch) acc))
                 chars chars))
end
