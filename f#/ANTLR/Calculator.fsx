#load "../Chapter-07/InterpreterFrame.fsx"

open System
open LanguageSyntax
open InterpreterFrame

// The lecture calculator's FLOAT tokens denote real numbers, even for "3".
// Normalize the reused parser's integer literals before evaluation, so 3/2 is 1.5.
let rec calculatorExpression = function
    | Literal (Integer value) -> Literal (Real (float value))
    | Literal (Real value) -> Literal (Real value)
    | Variable name -> Variable name
    | Binary ((Add | Subtract | Multiply | Divide) as operation, left, right) ->
        Binary (operation, calculatorExpression left, calculatorExpression right)
    | Call ("sqrt", [argument]) -> Call ("sqrt", [calculatorExpression argument])
    | expression -> invalidArg "source" (sprintf "Outside the lecture calculator grammar: %A" expression)

/// Reuse FsLexYacc parsing and the canonical evaluator; only statement presentation differs.
let calculate source =
    let statements =
        match LanguageSyntax.Parse.program source with
        | Block (_ :: _ as statements) -> statements
        | _ -> invalidArg "source" "The calculator program needs at least one statement"
    // Calculator expressions are arithmetic and sqrt calls; they never read or print via IO.
    let io : InterpreterFrame.IO =
        { Read = (fun () -> failwith "read is not part of the lecture calculator")
          Write = (fun _ -> failwith "print is not part of the lecture calculator") }
    statements |> List.fold (fun (state, output) statement ->
        match statement with
        | Assign (name, expression) ->
            let expression = calculatorExpression expression
            InterpreterFrame.execute io state (Assign (name, expression)), output
        | Expression expression ->
            let value = InterpreterFrame.evaluate io state (calculatorExpression expression)
            state, value :: output
        | other -> invalidArg "source" (sprintf "Outside the lecture calculator statement grammar: %A" other)) (emptyState, [])
    |> fun (state, reversedOutput) -> state, List.rev reversedOutput

let check message condition = if not condition then failwith message
let closeTo expected actual = abs (actual - expected) < 1e-12
let number state name = state.Values |> Map.find name |> asNumber

// Worked program from Lecture-Notes/antlr.tex, with the original bare expression output.
let program = "x := 2.3 * 3.4; y := sqrt(2 * x); z := x * x + y * y; sqrt(z);"
printfn "%s" program
let state, output = calculate program
check "x differs from the original calculator result" (closeTo 7.82 (number state "x"))
check "y differs from the original calculator result" (closeTo 3.954743986657038 (number state "y"))
check "z differs from the original calculator result" (closeTo 76.7924 (number state "z"))
match output with
| [value] -> check "sqrt(z) differs from the original calculator result" (closeTo 8.763127295663345 (asNumber value))
| _ -> failwith "Exactly one expression should be printed by the worked program"
output |> List.iter (asNumber >> printfn "%.14g")

// A new run has a fresh environment, and integer-looking literals retain FLOAT division.
let _, fractions = calculate "a := 3; a / 2; sqrt(16);"
check "Calculator division or sqrt changed semantics" (List.map asNumber fractions = [1.5; 4.0])
let expectFailure source =
    let mutable failed = false
    try calculate source |> ignore with _ -> failed <- true
    check ("Expected the calculator to reject: " + source) failed
expectFailure "missing;"
expectFailure "sqrt(0 - 1);"
expectFailure "1 / 0;"
expectFailure "x := ;"
expectFailure "while (1) { x := 1; }"
printfn "Lecture calculator checks passed."
