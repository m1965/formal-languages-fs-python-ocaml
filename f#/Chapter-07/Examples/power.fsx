let rec power (set: Set<int>) : Set<Set<int>> =
    if Set.isEmpty set then Set.singleton Set.empty
    else
        let element = Set.minElement set
        let subsets = power (Set.remove element set)
        Set.union subsets (subsets |> Set.map (Set.add element))
let result = power (Set.ofList [1..4])
if result.Count <> 16 then failwith "Expected 16 subsets"
printfn "%A" result
