let rec evaluate names (adt: Ast) : float =
    match adt with
    | Tok("NUMBER", value) -> System.Double.Parse(value, System.Globalization.CultureInfo.InvariantCulture)
    | Tok("IDENTIFIER", name) ->
        match Map.tryFind name names with
        | Some value -> value
        | None -> failwithf "Undefined variable: %s" name
    | Node([ ("+" | "-" | "*" | "/") as op ], [ left; right ]) ->
        let l, r = evaluate names left, evaluate names right
        match op with
        | "+" -> l + r
        | "-" -> l - r
        | "*" -> l * r
        | _ -> divide l r
    | _ -> failwithf "Unknown ADT structure: %s" (toString adt)

let execute names (adt: Ast) =
    match adt with
    | Node([ ":="; ";" ], [ Tok("IDENTIFIER", name); expr ]) -> Map.add name (evaluate names expr) names, None
    | Node([ ";" ], [ expr ]) -> names, Some(evaluate names expr)
    | _ -> failwithf "Unknown ADT structure: %s" (toString adt)

let calculate names source = execute names (parse parser source)
