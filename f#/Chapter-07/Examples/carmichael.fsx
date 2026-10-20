let rec gcd a b = if b = 0I then a else gcd b (a % b)
let rec power a n m =
    if n = 0I then 1I
    else
        let p = power a (n / 2I) m
        if n % 2I = 0I then p * p % m else p * p * a % m
let isPrime n = n >= 2 && not (seq {2..int (sqrt (float n))} |> Seq.exists (fun d -> n % d = 0))
let isCarmichael n =
    n > 1 && not (isPrime n) &&
    (seq {2..n-1} |> Seq.forall (fun a -> gcd (bigint a) (bigint n) <> 1I || power (bigint a) (bigint (n-1)) (bigint n) = 1I))
let numbers = [2..10_000] |> List.filter isCarmichael
if numbers <> [561;1105;1729;2465;2821;6601;8911] then failwith "Carmichael enumeration failed"
printfn "%A" numbers
let factors n = Set.ofList [for d in 2..int (sqrt (float n)) do if n % d = 0 then yield d]
printfn "%A" (List.map factors numbers)
