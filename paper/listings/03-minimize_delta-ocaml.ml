  let newDelta : 'q trans_rel_set ref = ref Dict.empty in
  FSet.iter
    (fun q' ->
      let classOfQ = find_equivalence_class q' equivClasses in
      FSet.iter
        (fun c -> newDelta := Dict.add (classOfQ, c) (find_equivalence_class (Dict.find (q', c) delta) equivClasses) !newDelta)
        sigma)
    q;
  { states = equivClasses; sigma; delta = !newDelta; start = newQ0; accepting = newAccept }
