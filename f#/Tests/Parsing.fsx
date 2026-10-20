#load "../Chapter-06/TopDown.fsx"
#load "../Chapter-06/Ebnf.fsx"
#load "../Chapter-06/EbnfAst.fsx"
#load "../Chapter-07/Calculator.fsx"
#load "../Chapter-07/Differentiator.fsx"
#load "../Chapter-07/Interpreter.fsx"
#load "../Chapter-07/InterpreterFrame.fsx"
#load "../Chapter-07/Examples/power.fsx"
#load "../Chapter-07/Examples/primes.fsx"
#load "../Chapter-07/Examples/carmichael.fsx"
#load "../Chapter-08/Earley.fsx"
#load "../Chapter-09/LR.fsx"
#load "../Chapter-09/ShiftReduce.fsx"

open GrammarSyntax
open System.IO
let check message condition = if not condition then failwith message
let chapter chapter name = Path.Combine(__SOURCE_DIRECTORY__, "../Chapter-" + chapter, name)
let rejects action = try action (); false with _ -> true

// Every supplied grammar fixture must parse and resolve all referenced nonterminals.
let edition = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
let grammarFiles = Directory.GetFiles(edition, "*.g", SearchOption.AllDirectories)
for file in grammarFiles do
    Parse.file file |> ignore
check "No grammar fixtures were found" (grammarFiles.Length > 0)

// Compare three independent recognizers on every token word of length at most five.
let grammar = Parse.file (chapter "09" "Examples/arith.g")
let slr = SLR.computeTables grammar
let lr = LR.computeTables grammar
check "Arithmetic grammar has an SLR conflict" ((SLR.conflicts slr).IsEmpty)
check "Arithmetic grammar has an LR conflict" ((SLR.conflicts lr).IsEmpty)
let alphabet = ["NUMBER"; "+"; "*"; "("; ")"]
let rec words length =
    if length = 0 then [[]]
    else [for head in alphabet do for tail in words (length - 1) do yield head :: tail]
let mutable checkedWords = 0
for length in 0..5 do
    for word in words length do
        let earleyTokens : EarleyPure.Token list = word |> List.map (fun kind -> { Kind = kind; Lexeme = if kind = "NUMBER" then "2" else kind })
        let shiftTokens : ShiftReducePure.InputToken list = word |> List.map (fun kind -> { Kind = kind; Lexeme = if kind = "NUMBER" then "2" else kind })
        let expected = EarleyPure.recognizes grammar earleyTokens
        check (sprintf "SLR/Earley disagree on %A" word) (ShiftReducePure.parse slr shiftTokens = expected)
        check (sprintf "LR/Earley disagree on %A" word) (ShiftReducePure.parse lr shiftTokens = expected)
        check (sprintf "Static table/Earley disagree on %A" word) (ShiftReducePure.parse ParseTable.table shiftTokens = expected)
        if expected then check "Accepted Earley word has no tree" (not (Earley.parseTrees grammar earleyTokens).IsEmpty)
        checkedWords <- checkedWords + 1

let ambiguous = Parse.file (chapter "08" "ambiguous.g")
check "Ambiguous arithmetic should produce two trees" ((Earley.parseTrees ambiguous (EarleyPure.tokenize "1 + 2 * 3")).Length = 2)
let nullable = Parse.text "s: a a; a: NUMBER; a: ;"
for text in [""; "1"; "1 2"] do check "Nullable Earley recognition failed" (EarleyPure.recognizes nullable (EarleyPure.tokenize text))
check "Nullable grammar accepts too many tokens" (not (EarleyPure.recognizes nullable (EarleyPure.tokenize "1 2 3")))
check "Undefined nonterminal should fail" (rejects (fun () -> Parse.text "s: missing;" |> ignore))

// Establish the textbook LR(1)-but-not-SLR distinction independently of the C fixture.
let assignment = Parse.text "s: l '=' r | r; l: '*' r | IDENTIFIER; r: l;"
check "Assignment grammar should have an SLR conflict" (not (SLR.conflicts (SLR.computeTables assignment)).IsEmpty)
check "Assignment grammar should be LR(1)" ((SLR.conflicts (LR.computeTables assignment)).IsEmpty)
let conflicted = SLR.computeTables (Parse.text "e: e '+' e | NUMBER;")
let ambiguityResult = ShiftReducePure.parseBy conflicted (fun _ -> ()) (fun _ _ -> ()) (ShiftReducePure.tokenize "1+2+3")
check "Parser should report the encountered table conflict" (Result.isError ambiguityResult)

for text, expected in ["2-3-4", -5.0; "3+4*2/(1-5)", 1.0; "(1+2)*3", 9.0] do
    check "Top-down arithmetic result differs" (TopDown.parse text = expected)
    check "EBNF arithmetic result differs" (Ebnf.parse text = expected)
    check "Typed AST arithmetic result differs" (EbnfAst.evaluate Map.empty (EbnfAst.parse text) = expected)
for invalid in ["1+"; "1 2"; "(1+2"; "1+@2"; "007"; "1/0"] do
    check "Invalid arithmetic input accepted" (rejects (fun () -> TopDown.parse invalid |> ignore))

let names, _ = Calculator.calculate Map.empty "x := 6;"
let _, result = Calculator.calculate names "x * 7;"
check "Calculator variable state was lost" (result = Some 42.0)
check "Calculator undefined variable accepted" (rejects (fun () -> Calculator.calculate names "missing;" |> ignore))
for text in ["x*x"; "x/x"; "exp(x)"; "ln(x)"; "x**x"] do
    let expr = Differentiator.parse text
    let derivative = Differentiator.diff expr
    let x, h = 1.3, 1e-6
    let finiteDifference = (Differentiator.evaluate (x+h) expr - Differentiator.evaluate (x-h) expr) / (2.0*h)
    check "Symbolic derivative disagrees with finite differences" (abs(Differentiator.evaluate x derivative - finiteDifference) < 1e-4)
for text in ["x/(x*x)"; "x-(x+x)"; "(x**x)**x"; "x**(x**x)"; "x+(x-x)"; "x*(x/x)"] do
    let ast = Differentiator.parse text
    check "Expression printing changes grouping" (Differentiator.parse (Differentiator.toString ast) = ast)

let output = ResizeArray<LanguageSyntax.Value>()
let simpleIO : Interpreter.IO = { Read = (fun () -> LanguageSyntax.Integer 10I); Write = output.Add }
Interpreter.run simpleIO (File.ReadAllText(chapter "07" "sum.sl")) |> ignore
check "Simple interpreter sum differs" (List.ofSeq output = [LanguageSyntax.Integer 5050I])
let exactIO : Interpreter.IO = { Read = (fun () -> LanguageSyntax.Integer 9007199254740993I); Write = output.Add }
output.Clear()
let integers = Interpreter.run exactIO "x := read(); y := x + 1; z := x * x; print(y);"
check "Read rounded an exact integer" (integers["x"] = LanguageSyntax.Integer 9007199254740993I)
check "Integer multiplication lost precision" (integers["z"] = LanguageSyntax.Integer 81129638414606699710187514626049I)
check "Integer output lost precision" (List.ofSeq output = [LanguageSyntax.Integer 9007199254740994I])
check "Integer formatting rounded the output" (Interpreter.valueText (List.head (List.ofSeq output)) = "9007199254740994")
let numeric = Interpreter.run simpleIO "q := 3 / 2; r := -7 % 3; s := 7 % -3; t := -7 % -3; u := 1 + 0.5; rounded := 9007199254740993 / 3;"
check "Simple division should be true division" (numeric["q"] = LanguageSyntax.Real 1.5)
check "Negative integer modulo differs from Python" (numeric["r"] = LanguageSyntax.Integer 2I && numeric["s"] = LanguageSyntax.Integer -2I && numeric["t"] = LanguageSyntax.Integer -1I)
check "Mixed arithmetic should yield a real" (numeric["u"] = LanguageSyntax.Real 1.5)
check "Integer division rounded operands before their ratio" (numeric["rounded"] = LanguageSyntax.Real 3002399751580331.0)
let huge = System.Numerics.BigInteger.Pow(10I, 400)
check "A finite ratio of enormous integers should remain finite" (Interpreter.divideIntegers huge huge = 1.0)
check "Integer division should round to even" (Interpreter.divideIntegers 9007199254740993I 1I = 9007199254740992.0)
check "Subnormal integer division differs" (Interpreter.divideIntegers 1I (System.Numerics.BigInteger.One <<< 1074) = System.Double.Epsilon)
check "Simple division by zero should fail" (rejects (fun () -> Interpreter.run simpleIO "1 / 0;" |> ignore))
check "Simple modulo by zero should fail" (rejects (fun () -> Interpreter.run simpleIO "1 % 0;" |> ignore))
let compared = Interpreter.run simpleIO "x := 0; if (9007199254740993 > 9007199254740992.0) { x := 1; } if (9007199254740993 == 9007199254740992.0) { x := 2; }"
check "Simple numeric comparisons lost integer precision" (compared["x"] = LanguageSyntax.Integer 1I)
check "Negative fractional mixed comparison differs" (Interpreter.compareValues (LanguageSyntax.Integer -1I) (LanguageSyntax.Real -1.5) > 0)
check "Integer division should preserve positive zero" (System.BitConverter.DoubleToInt64Bits(Interpreter.divideIntegers 0I 3I) = 0L)
check "Integer division should preserve negative zero" (System.BitConverter.DoubleToInt64Bits(Interpreter.divideIntegers 0I -3I) = System.Int64.MinValue)
let frameIO : InterpreterFrame.IO = { Read = (fun () -> LanguageSyntax.Integer 10I); Write = ignore }
let state = InterpreterFrame.run frameIO "function fact(n) { if (n == 0) { return 1; } return n*fact(n-1); } x := fact(25);"
check "Recursive factorial lost integer precision" (state.Values["x"] = LanguageSyntax.Integer 15511210043330985984000000I)
check "Return outside a function should fail" (rejects (fun () -> InterpreterFrame.run frameIO "return 1;" |> ignore))
check "Wrong function arity should fail" (rejects (fun () -> InterpreterFrame.run frameIO "function f(x) { return x; } f();" |> ignore))
check "Short-circuit boolean operator evaluated its right side" ((InterpreterFrame.run frameIO "x := 1 == 1 || missing == 0;").Values["x"] = LanguageSyntax.Boolean true)
check "Unterminated comment should fail" (rejects (fun () -> LanguageSyntax.Parse.program "/* no end" |> ignore))
check "EOF is required" (rejects (fun () -> CalculatorSyntax.Parse.statement "1; 2;" |> ignore))

check "Large integer/real comparison lost precision" (not (InterpreterFrame.equalValues (LanguageSyntax.Integer 9007199254740993I) (LanguageSyntax.Real 9007199254740992.0)))
check "Math domain error should fail" (rejects (fun () -> InterpreterFrame.run frameIO "sqrt(0-1);" |> ignore))
check "Real division by zero should fail" (rejects (fun () -> InterpreterFrame.run frameIO "1.0/0.0;" |> ignore))

// Expected remainders were obtained independently with Python's float % operator.
let realModuloCases =
    [ 7.0, 3.0, 1.0; -7.0, 3.0, 2.0; 7.0, -3.0, -2.0; -7.0, -3.0, -1.0
      1e308, 1e-308, 3.498445546245627e-309
      -1e308, 1e-308, 6.50155445375437e-309
      1e308, -1e-308, -6.50155445375437e-309
      -1e308, -1e-308, -3.498445546245627e-309
      1e308, 3.0, 2.0; 1.0, 1e-300, 8.963186528039712e-301 ]
for left, right, expected in realModuloCases do
    check "Real modulo differs from Python" (Interpreter.floatModulo left right = expected)
    for arithmetic in [Interpreter.arithmetic; InterpreterFrame.arithmetic] do
        check "Interpreter real modulo differs" (arithmetic LanguageSyntax.Modulo (LanguageSyntax.Real left) (LanguageSyntax.Real right) = LanguageSyntax.Real expected)
for left in [0.0; -0.0; 4.0; -4.0] do
    for right in [2.0; -2.0] do
        let expectedBits = if right < 0.0 then System.Int64.MinValue else 0L
        for arithmetic in [Interpreter.arithmetic; InterpreterFrame.arithmetic] do
            let result = arithmetic LanguageSyntax.Modulo (LanguageSyntax.Real left) (LanguageSyntax.Real right)
            check "Real modulo lost the divisor's signed zero" (System.BitConverter.DoubleToInt64Bits(Interpreter.asNumber result) = expectedBits)
for zero in [0.0; -0.0] do
    check "Real modulo accepted a zero divisor" (rejects (fun () -> Interpreter.floatModulo 1.0 zero |> ignore))
check "Simple real modulo accepted zero" (rejects (fun () -> Interpreter.run simpleIO "1.0 % 0.0;" |> ignore))
check "Frame real modulo accepted negative zero" (rejects (fun () -> InterpreterFrame.run frameIO "1.0 % -0.0;" |> ignore))

// Execute the actual accompanying lecture programs, including their printed results.
let exampleOutput = ResizeArray<LanguageSyntax.Value list>()
let exampleIO : InterpreterFrame.IO = { Read = frameIO.Read; Write = exampleOutput.Add }
let runFrameExample name =
    exampleOutput.Clear()
    let result = InterpreterFrame.run exampleIO (File.ReadAllText(chapter "07" ("Examples/" + name + ".sl")))
    result, List.ofSeq exampleOutput
for name, expected in ["pi", System.Math.PI; "e", System.Math.E] do
    let _, printed = runFrameExample name
    let actual = InterpreterFrame.asNumber (printed |> List.last |> List.last)
    check (name + " lecture example differs") (abs(actual - expected) < 1e-14)
let solved, _ = runFrameExample "solve"
let solution = InterpreterFrame.asNumber solved.Values["x"]
check "Fixed-point lecture example differs" (abs(solution - 0.7390851332151607) < 1e-14 && abs(cos solution - solution) < 1e-14)
let _, factorialOutput = runFrameExample "factorial"
check "Factorial lecture example output differs" (factorialOutput.Length = 24 && factorialOutput |> List.last |> List.last = LanguageSyntax.Integer 15511210043330985984000000I)
let _, sumOutput = runFrameExample "sum-for"
check "For-loop lecture sum differs" (sumOutput |> List.last |> List.last = LanguageSyntax.Integer 5050I)
// The set-language examples above are native F# translations and run their own checks.
check "Power-set lecture example differs" (Power.power (Set.ofList [1;2]) = Set.ofList [Set.empty; Set.singleton 1; Set.singleton 2; Set.ofList [1;2]])
check "Prime lecture example differs at 100" ((Primes.primes 100).Count = 25 && Set.maxElement (Primes.primes 100) = 97)
let primes10000 = Primes.primes 10_000
check "Prime lecture example differs at 10000" (primes10000.Count = 1229 && Set.maxElement primes10000 = 9973)

printfn "Parsing checks passed: %d grammar fixtures and %d exhaustive token words plus semantics and failure cases." grammarFiles.Length checkedWords
