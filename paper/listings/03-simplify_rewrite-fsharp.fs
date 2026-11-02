let rec matchPattern pattern term (substitution: Substitution) : Substitution option =
    match pattern, term with
    | PatternVariable name, _ ->
        match Map.tryFind name substitution with
        | Some previous when previous = term -> Some substitution
        | Some _ -> None
        | None -> Some (Map.add name term substitution)
    | PatternEmpty, Empty | PatternEpsilon, Epsilon -> Some substitution
    | PatternLiteral c, Literal d when c = d -> Some substitution
    | PatternConcat (p1, p2), Concat (t1, t2)
    | PatternAlt (p1, p2), Alt (t1, t2) ->
        matchPattern p1 t1 substitution |> Option.bind (fun next -> matchPattern p2 t2 next)
    | PatternStar p, Star t -> matchPattern p t substitution
    | _ -> None

let getRules () : Rule list =
    [ "R+∅", "R"
      "∅+R", "R"
      "R+R", "R"
      "𝜀+R*", "R*"
      // ...
      "(RS*)(𝜀+S)", "RS*" ]
    |> List.map (fun (left, right) -> patternOf (parse left), patternOf (parse right))
