and parseExprRest sum tokens =
    match tokens with
    | "+" :: tail ->
        let product, rest = parseProduct tail
        parseExprRest (sum + product) rest
    | "-" :: tail ->
        let product, rest = parseProduct tail
        parseExprRest (sum - product) rest
    | _ -> sum, tokens
// ...
and parseFactor tokens =
    match tokens with
    | "(" :: tail ->
        let expr, rest = parseExpr tail
        match rest with
        | ")" :: remaining -> expr, remaining
        | _ -> failwith "Parse error: expected ')'"
    | number :: tail ->
        match Double.TryParse(number, CultureInfo.InvariantCulture) with
        | true, value -> value, tail
        | _ -> failwithf "Parse error: expected a number, got %s" number
    | [] -> failwith "Parse error: expected a factor"
