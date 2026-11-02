let cache (f : ('a -> 'b) -> 'a -> 'b) : 'a -> 'b =
  let table = Hashtbl.create 1024 in
  let rec self x =
    let key = (Hashtbl.hash_param 100 1000 x, x) in
    match Hashtbl.find_opt table key with
    | Some y -> y
    | None ->
        let y = f self x in
        Hashtbl.replace table key y;
        y
  in
  self

type flat =
  | FChar of char           (* a character c                                  *)
  | FStar of flat           (* r*                                             *)
  | FSum  of flat FSet.t    (* r1 + ... + rn, none of the summands is a sum   *)
  | FProd of flat list      (* r1 ⋅ ... ⋅ rn, none of the factors is a product *)

let empty   : flat = FSum FSet.empty    (* ∅ is the empty sum     *)
let epsilon : flat = FProd []           (* 𝜀 is the empty product *)

let nullable : flat -> bool =
  cache (fun nullable r ->
    match r with
    | FStar _ -> true
    | FSum s  -> FSet.exists nullable s
    | FProd f -> List.for_all nullable f
    | FChar _ -> false)
