/// The integer summation example from the JVM/Jasmin lecture, expressed in F#.
let sum n =
    if n < 0 then invalidArg "n" "The lecture example sums natural numbers"
    let rec accumulate remaining total =
        if remaining = 0 then total
        else accumulate (remaining - 1) (total + remaining)
    accumulate n 0

let result = sum (6 * 6)
if result <> 666 || sum 0 <> 0 || sum 1 <> 1 then
    failwith "The summation example produced an incorrect result"
printfn "%d" result
