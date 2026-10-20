#r "nuget: FsLexYacc.Runtime, 11.3.0"
#r "../Chapter-03/Scanning/bin/Debug/net9.0/Scanning.dll"
#r "../Chapter-12/IntegerC/bin/Debug/net9.0/IntegerC.dll"
#r "../Legacy-ANTLR/04-Calc/Calculator/bin/Debug/net9.0/Calculator.dll"
#r "../Exercises/Grammar2HTML-Antlr/GrammarParser/bin/Debug/net9.0/GrammarParser.dll"
#load "../Chapter-03/ExamEvaluation.fsx"

open System
open LectureScanning
open ExamEvaluation

// F# assert can be disabled; these checks run in every FSI/Jupyter configuration.
let check message condition =
    if not condition then failwith message

let expectFailure action =
    let mutable failed = false
    try action () with _ -> failed <- true
    check "Expected the operation to fail" failed

// Leading zeroes retain the lecture scanner's separate-token behavior.
let tokens = Scanning.arithmetic "007\n@ 42"
check "Leading zeroes must be scanned separately" (tokens |> List.map (fun t -> t.Value) = [Number 0; Number 0; Number 7; Illegal '@'; Number 42])
check "Illegal character location must retain the newline offset" (tokens.[3].Line = 2 && tokens.[3].Offset = 4)

// Last student is flushed even without a trailing newline; each scan starts fresh.
let exam = "Class: Test\nMaxPoints = 60\nStudent One: 15 15\n1234567: 20 20 20"
let grades = Scanning.exam exam |> evaluate
check "Half the maximum points must receive grade 4.0" (grades.Length = 2 && grades.[0].Points = 30 && grades.[0].Grade = 4.0)
check "The last student must be flushed at EOF" (grades.[1].Name = "1234567" && grades.[1].Grade = 1.0)
check "A fresh scan must produce identical grades" (Scanning.exam exam |> evaluate = grades)
expectFailure (fun () -> mark 0 0 |> ignore)
expectFailure (fun () -> Scanning.exam "MaxPoints = 60\nStudent: @" |> ignore)

check "HTML scanning must omit hidden text and decode entities" (Scanning.html "<head>hidden</head><p>Gr&uuml;ße &#128034;</p><script>hidden()</script>" = "Grüße 🐢")
check "HTML entities may omit a semicolon" (Scanning.html "&auml &#128034" = "ä 🐢")
check "Unknown HTML entities must survive unchanged" (Scanning.html "<p>Unknown &notARealEntity;</p>" = "Unknown &notARealEntity;")
expectFailure (fun () -> Scanning.html "<head>never closed" |> ignore)
check "LaTeX scanner must distinguish delimiters and exponent tokens" (Scanning.latex "$x^{2}$" = [MathDelimiter "$";Text "x";Hat;Brace '{';Text "2";Brace '}';MathDelimiter "$"])

let parsed = IntegerC.Frontend.parse "/* comment\n */ int main() { if (1) if (0) println(1); else println(2); }"
match parsed.Head.Body with
| [IntegerC.If(_,IntegerC.If(_,_,Some _),None)] -> ()
| _ -> failwith "Dangling else associated incorrectly"
let ternary = IntegerC.Frontend.parse "int main() { println(0 ? 1 : 2 + 3 * 4); }"
match ternary.Head.Body with
| [IntegerC.Expression(IntegerC.Call("println",[IntegerC.Conditional(_,_,IntegerC.Binary("+",_,IntegerC.Binary("*",_,_)))]))] -> ()
| _ -> failwith "Ternary/arithmetic precedence incorrect"

// The lecture grammar binds comparison operators more tightly than Boolean !.
let printedExpression source =
    let functions = IntegerC.Frontend.parse ("int main() { println(" + source + "); }")
    match functions.Head.Body with
    | [IntegerC.Expression(IntegerC.Call("println",[expression]))] -> expression
    | _ -> failwith "Expected one printed expression"
let lessThan = IntegerC.Binary("<",IntegerC.Variable "x",IntegerC.Variable "y")
check "!x < y must negate the complete comparison" (printedExpression "!x < y" = IntegerC.Not lessThan)
check "Explicit comparison grouping must retain the same AST" (printedExpression "!(x < y)" = IntegerC.Not lessThan)
check "Double negation must also wrap the complete comparison" (printedExpression "!!x < y" = IntegerC.Not(IntegerC.Not lessThan))
check "Boolean ! must bind more tightly than &&"
    (printedExpression "!x < y && x == 1" = IntegerC.Binary("&&",IntegerC.Not lessThan,IntegerC.Binary("==",IntegerC.Variable "x",IntegerC.Number 1)))
check "Boolean && must bind more tightly than ||"
    (printedExpression "!x < y || x == 1 && y == 2" =
        IntegerC.Binary("||",IntegerC.Not lessThan,IntegerC.Binary("&&",IntegerC.Binary("==",IntegerC.Variable "x",IntegerC.Number 1),IntegerC.Binary("==",IntegerC.Variable "y",IntegerC.Number 2))))
check "Arithmetic must bind more tightly than comparison and Boolean !"
    (printedExpression "!x + 1 < y * 2" = IntegerC.Not(IntegerC.Binary("<",IntegerC.Binary("+",IntegerC.Variable "x",IntegerC.Number 1),IntegerC.Binary("*",IntegerC.Variable "y",IntegerC.Number 2))))
expectFailure (fun () -> IntegerC.Frontend.parse "int main() { 1 < 2 < 3; }" |> ignore)
expectFailure (fun () -> IntegerC.Frontend.parse "int main() { /* unclosed" |> ignore)

let large = LectureCalculator.Calculator.parse "x = 100000000000000000000000000000; print(x);"
match large with
| [LectureCalculator.Assignment("x",LectureCalculator.Integer number);LectureCalculator.Print(LectureCalculator.Identifier "x")] -> check "Calculator numbers must retain arbitrary precision" (number > bigint Int64.MaxValue)
| _ -> failwith "Calculator lost arbitrary-precision semantics"
let grammar = LectureGrammar.Grammar.parse "start: | '<' start;"
check "Grammar parser must retain the epsilon alternative" (grammar.Head.Alternatives.Head.IsEmpty)
check "Grammar parser must retain literals and variables" (grammar.Head.Alternatives.[1] = [LectureGrammar.Literal "'<'";LectureGrammar.Variable "start"])
expectFailure (fun () -> LectureGrammar.Grammar.parse "start: 'unterminated;" |> ignore)
printfn "Scanning/compiler checks passed."
