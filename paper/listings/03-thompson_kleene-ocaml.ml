let kleene (self : regExp2NFA) (f : state nfa) : state nfa =
  let ({ states = q; sigma; delta = delta0; start = q1; accepting = a } : state nfa) = f in
  let q2 = the_element a in
  let q0 = !getNewState_ref self in
  let q3 = !getNewState_ref self in
  let delta =
    delta0
    |> Dict.add (q0, Eps) (FSet.of_list [ q1; q3 ])
    |> Dict.add (q2, Eps) (FSet.of_list [ q1; q3 ])
  in
  { states = FSet.union (FSet.of_list [ q0; q3 ]) q; sigma; delta; start = q0; accepting = FSet.singleton q3 }
