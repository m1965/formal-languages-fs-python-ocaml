# ANTLR lecture examples through FsLexYacc

The notebook inventory covers the original pure parser, regular-expression parser and simple-language interpreter. Two additional grammars are taught directly in `Lecture-Notes/antlr.tex`: `Python/ANTLR/Program.g4` defines a calculator with assignments, real-valued arithmetic, and `sqrt`; `Calculator.g4` adds evaluation and prints bare expression statements. `Calculator.fsx` completes that lecture-only example in F#.

Run F# setup from the repository root to build the FsLexYacc projects and export canonical notebook algorithms, then run:

```sh
dotnet fsi --exec FSharp/ANTLR/Calculator.fsx
```

The adapter reuses `Chapter-07/LanguageSyntax` for parsing and the visible `Interpreter-Frame.ipynb` evaluator through its generated `InterpreterFrame.fsx`. It restricts the broader shared AST to the lecture calculator's statements and expressions, converts integer-looking literals to real values as the original `FLOAT` token did, and collects bare-expression values for printing. No second grammar or arithmetic evaluator is introduced.

The original worked program is:

```text
x := 2.3 * 3.4; y := sqrt(2 * x); z := x * x + y * y; sqrt(z);
```

It computes `x = 7.82`, `y ≈ 3.954743986657038`, `z = 76.7924` and prints `sqrt(z) ≈ 8.763127295663345`. Always-active checks validate each value with a stated floating-point tolerance, check `3/2 = 1.5`, and verify undefined names, math-domain errors, zero division, malformed assignments and unsupported control-flow statements.

The original `Differentiator.g4` expression/logarithm/exponential rules correspond to `Chapter-07/02-Differentiator.ipynb`; `RegularExpressions.g4`, `Expr.g4` and `Simple.g4` correspond to the three notebooks in this directory. The authored Java/Jasmin comparison programs for the JVM lecture are in `Chapter-11`.
