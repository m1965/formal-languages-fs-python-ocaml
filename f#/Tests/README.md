# Behavioral verification

Build the edition first, then run these independent checks from the repository root:

```sh
dotnet fsi --exec FSharp/Tests/Automata.fsx
dotnet fsi --exec FSharp/Tests/Parsing.fsx
dotnet fsi --exec FSharp/Tests/ScanningCompiler.fsx
```

The automata suite compares recognition with .NET regular expressions across bounded word sets, then checks minimization, equivalence, rewriting, state elimination and SVG rendering. The parsing suite compares Earley, SLR, LR and a static table on all 3,906 token words of length at most five, and checks ambiguity, nullable productions, numeric semantics and failures. The scanning/compiler suite verifies scanner modes, source spans, parser and code-generation errors, nested control flow and emitted JVM stack instructions. These checks use explicit exceptions and remain active in release execution.

For end-to-end JVM verification, obtain the [Jasmin 3.0.3 dependency bundle](https://repo.maven.apache.org/maven2/ca/mcgill/sable/jasmin/3.0.3/jasmin-3.0.3-jar-with-dependencies.jar) and run:

```sh
JASMIN_JAR=/absolute/path/jasmin-3.0.3-jar-with-dependencies.jar \
  bash FSharp/Chapter-12/verify-jvm.sh
```

The script recompiles and assembles all six translated compiler examples, runs Java with full bytecode verification and compares exact output. A temporary Boolean program also checks negation precedence, skipped division by zero and observable function calls during short-circuit evaluation. Its `JAVA` and `DOTNET` environment variables can select explicit executable paths. The bundle SHA-256 is `78e69b6324cb16c0a6875a057b9a03f52a7b239009a027dca1bda16010fd020b`.

Notebook coverage, FSI execution and actual Jupyter execution are separate checks described in the [edition guide](../README.md). Passing any of them does not establish student comprehension; the [evaluation protocol](../Lecture-Notes/EVALUATION.md) addresses that question.
