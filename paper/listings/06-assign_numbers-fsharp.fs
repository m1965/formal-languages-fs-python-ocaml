/// Preorder addresses distinguish repeated labels without relying on object identity.
let assignNumbers tree =
    let rec visit address next tree numbers =
        let numbers = Map.add address next numbers
        match tree with
        | Leaf _ -> next + 1, numbers
        | Node (_, children) ->
            children
            |> List.indexed
            |> List.fold (fun (counter, known) (index, child) ->
                visit (address @ [index]) counter child known) (next + 1, numbers)
    visit [] 0 tree Map.empty |> snd
