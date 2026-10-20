(* Finite sets and dictionaries for the notebooks of chapters 4 and 5, loaded by nb.ml.

   Python's set, frozenset, and dict work for elements of any type.  The stdlib modules
   Set and Map need a functor application for every element type, which would force every
   algorithm into a single cell.  The two modules below are polymorphic instead:

   - 'a FSet.t is an immutable set.  It is represented as a strictly increasing list,
     so every set has exactly one representation.  Hence the polymorphic functions
     compare and (=) work on sets, and sets of sets, pairs of sets, etc. work, too.
   - ('k, 'v) Dict.t is an immutable dictionary: a balanced search tree ordered by compare.
     Dictionaries must not be compared with (=), use Dict.equal instead.

   Both are printed like Python sets and dictionaries:  {1, 2, 3}  and  {(0, 'a') ↦ 1}.
   At most max_shown elements are printed. *)

let max_shown = 60

module FSet : sig
  type 'a t = private 'a list

  val empty : 'a t
  val singleton : 'a -> 'a t
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val is_empty : 'a t -> bool
  val cardinal : 'a t -> int
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t
  val remove : 'a -> 'a t -> 'a t
  val union : 'a t -> 'a t -> 'a t            (* A | B  *)
  val inter : 'a t -> 'a t -> 'a t            (* A & B  *)
  val diff : 'a t -> 'a t -> 'a t             (* A - B  *)
  val subset : 'a t -> 'a t -> bool           (* A <= B *)
  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val exists : ('a -> bool) -> 'a t -> bool
  val for_all : ('a -> bool) -> 'a t -> bool
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : ('a -> unit) -> 'a t -> unit
  val choose : 'a t -> 'a                     (* an arbitrary element, here the smallest *)
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val to_string : ('a -> string) -> 'a t -> string   (* to_string string_of_int s = "{1, 2, 3}" *)

  (* Set comprehensions: { f(x, y) for x in A for y in B if p(x, y) } is written as
       let* x = A in let* y = B in if p x y then return (f x y) else empty         *)
  val return : 'a -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end = struct
  type 'a t = 'a list

  let empty = []
  let singleton x = [ x ]
  let of_list l = List.sort_uniq compare l
  let to_list s = s
  let is_empty s = s = []
  let cardinal = List.length
  let mem x s = List.exists (fun y -> compare x y = 0) s

  let rec union a b =
    match a, b with
    | [], s | s, [] -> s
    | x :: a', y :: b' ->
        let c = compare x y in
        if c < 0 then x :: union a' b
        else if c > 0 then y :: union a b'
        else x :: union a' b'

  let rec inter a b =
    match a, b with
    | [], _ | _, [] -> []
    | x :: a', y :: b' ->
        let c = compare x y in
        if c < 0 then inter a' b else if c > 0 then inter a b' else x :: inter a' b'

  let rec diff a b =
    match a, b with
    | [], _ -> []
    | s, [] -> s
    | x :: a', y :: b' ->
        let c = compare x y in
        if c < 0 then x :: diff a' b else if c > 0 then diff a b' else diff a' b'

  let rec subset a b =
    match a, b with
    | [], _ -> true
    | _, [] -> false
    | x :: a', y :: b' ->
        let c = compare x y in
        if c < 0 then false else if c > 0 then subset a b' else subset a' b'

  let add x s = union [ x ] s
  let remove x s = diff s [ x ]
  let map f s = of_list (List.map f s)
  let filter = List.filter
  let exists = List.exists
  let for_all = List.for_all
  let fold f s acc = List.fold_left (fun acc x -> f x acc) acc s
  let iter = List.iter

  let choose = function
    | x :: _ -> x
    | [] -> invalid_arg "FSet.choose: empty set"

  let return = singleton
  let ( let* ) s f = of_list (List.concat_map f s)

  let to_string show s = "{" ^ String.concat ", " (List.map show s) ^ "}"

  let pp pp_elt ppf s =
    let shown = List.filteri (fun i _ -> i < max_shown) s in
    Format.fprintf ppf "@[<hov 1>{%a%s}@]"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ") pp_elt)
      shown
      (if List.length s > max_shown then Printf.sprintf ", … (%d elements)" (List.length s) else "")
end

module Dict : sig
  type ('k, 'v) t

  val empty : ('k, 'v) t
  val of_list : ('k * 'v) list -> ('k, 'v) t  (* later bindings win, as in Python *)
  val to_list : ('k, 'v) t -> ('k * 'v) list  (* sorted by key *)
  val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  val find : 'k -> ('k, 'v) t -> 'v           (* d[k], raises Not_found *)
  val find_opt : 'k -> ('k, 'v) t -> 'v option
  val get : 'k -> default:'v -> ('k, 'v) t -> 'v  (* d.get(k, default) *)
  val mem : 'k -> ('k, 'v) t -> bool
  val union : ('k, 'v) t -> ('k, 'v) t -> ('k, 'v) t  (* d1 | d2: bindings of d2 win *)
  val keys : ('k, 'v) t -> 'k list
  val cardinal : ('k, 'v) t -> int
  val fold : ('k -> 'v -> 'a -> 'a) -> ('k, 'v) t -> 'a -> 'a
  val iter : ('k -> 'v -> unit) -> ('k, 'v) t -> unit
  val equal : ('k, 'v) t -> ('k, 'v) t -> bool

  val pp :
    (Format.formatter -> 'k -> unit) ->
    (Format.formatter -> 'v -> unit) ->
    Format.formatter -> ('k, 'v) t -> unit
end = struct
  (* AVL trees, as in the stdlib module Map. *)
  type ('k, 'v) t = Empty | Node of ('k, 'v) t * 'k * 'v * ('k, 'v) t * int

  let height = function Empty -> 0 | Node (_, _, _, _, h) -> h
  let node l k v r = Node (l, k, v, r, 1 + max (height l) (height r))

  let balance l k v r =
    let hl = height l and hr = height r in
    if hl > hr + 1 then
      match l with
      | Node (ll, lk, lv, lr, _) when height ll >= height lr -> node ll lk lv (node lr k v r)
      | Node (ll, lk, lv, Node (lrl, lrk, lrv, lrr, _), _) ->
          node (node ll lk lv lrl) lrk lrv (node lrr k v r)
      | _ -> assert false
    else if hr > hl + 1 then
      match r with
      | Node (rl, rk, rv, rr, _) when height rr >= height rl -> node (node l k v rl) rk rv rr
      | Node (Node (rll, rlk, rlv, rlr, _), rk, rv, rr, _) ->
          node (node l k v rll) rlk rlv (node rlr rk rv rr)
      | _ -> assert false
    else node l k v r

  let empty = Empty

  let rec add k v = function
    | Empty -> node Empty k v Empty
    | Node (l, k', v', r, h) ->
        let c = compare k k' in
        if c = 0 then Node (l, k, v, r, h)
        else if c < 0 then balance (add k v l) k' v' r
        else balance l k' v' (add k v r)

  let rec find_opt k = function
    | Empty -> None
    | Node (l, k', v, r, _) ->
        let c = compare k k' in
        if c = 0 then Some v else if c < 0 then find_opt k l else find_opt k r

  let find k d = match find_opt k d with Some v -> v | None -> raise Not_found
  let get k ~default d = match find_opt k d with Some v -> v | None -> default
  let mem k d = find_opt k d <> None

  let rec fold f d acc =
    match d with
    | Empty -> acc
    | Node (l, k, v, r, _) -> fold f r (f k v (fold f l acc))

  let to_list d = List.rev (fold (fun k v acc -> (k, v) :: acc) d [])
  let of_list l = List.fold_left (fun d (k, v) -> add k v d) Empty l
  let union d1 d2 = fold add d2 d1
  let keys d = List.map fst (to_list d)
  let cardinal d = fold (fun _ _ n -> n + 1) d 0
  let iter f d = fold (fun k v () -> f k v) d ()
  let equal d1 d2 = to_list d1 = to_list d2

  let pp pp_key pp_val ppf d =
    let pp_binding ppf (k, v) = Format.fprintf ppf "@[%a ↦@ %a@]" pp_key k pp_val v in
    let l = to_list d in
    Format.fprintf ppf "@[<hov 1>{%a%s}@]"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ") pp_binding)
      (List.filteri (fun i _ -> i < max_shown) l)
      (if List.length l > max_shown then Printf.sprintf ", … (%d entries)" (List.length l) else "")
end

#install_printer FSet.pp;;
#install_printer Dict.pp;;
