let arb (m : 's FSet.t) : 's =
  FSet.choose m

let separate (pairs : 'q pair FSet.t) (states : 'q FSet.t) (sigma : char FSet.t) (delta : 'q trans_rel_det)
    : 'q pair FSet.t =
  let open FSet in
  let* q1 = states in
  let* q2 = states in
  let* c  = sigma in
  if mem (Dict.find (q1, c) delta, Dict.find (q2, c) delta) pairs then return (q1, q2) else empty

let all_separable (q : 'q FSet.t) (a : 'q FSet.t) (sigma : char FSet.t) (delta : 'q trans_rel_det) : 'q pair FSet.t =
  let rec loop (separable : 'q pair FSet.t) : 'q pair FSet.t =
    let newPairs = separate separable q sigma delta in
    if FSet.subset newPairs separable then separable
    else loop (FSet.union separable newPairs)
  in
  loop (FSet.union (cart_prod (FSet.diff q a) a) (cart_prod a (FSet.diff q a)))
