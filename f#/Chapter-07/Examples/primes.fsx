let primes n =
    let all = Set.ofList [2..n]
    let composite = Set.ofList [for p in 2..n/2 do for q in p..n/p do yield p*q]
    Set.difference all composite
if primes 10 <> Set.ofList [2;3;5;7] then failwith "Prime enumeration failed"
printfn "%A" (primes 100)
printfn "%A" (primes 10_000)
