type regExp2NFA = { alphabet : char FSet.t; mutable stateCount : int }
let genEmptyNFA_ref   : (regExp2NFA -> state nfa) ref                             = ref not_yet_defined
(* ... *)
let rec toNFA (self : regExp2NFA) (r : regexp) : state nfa =
  match r with
  | Empty -> !genEmptyNFA_ref self
  | Epsilon -> !genEpsilonNFA_ref self
  | Char c -> !genCharNFA_ref self c
  | Product (r1, r2) ->
      let f1 = toNFA self r1 in
      let f2 = toNFA self r2 in
      !catenate_ref self f1 f2
  | Sum (r1, r2) ->
      let f1 = toNFA self r1 in
      let f2 = toNFA self r2 in
      !disjunction_ref self f1 f2
  | Star r1 -> !kleene_ref self (toNFA self r1)
toNFA_ref := toNFA
