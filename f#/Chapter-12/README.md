# Integer C compiler in F#

Build `IntegerC/IntegerC.fsproj`, then execute `Compiler.ipynb` with the F# .NET Interactive kernel. The project generates its scanner and parser from `Lexer.fsl` and `Parser.fsy` using FsLexYacc 11.3.0. The notebook retains the lecture sequence and implements all AST traversals and Jasmin code generation in visible F# cells. The typed AST, generated grammar and source files can be inspected beside the notebook.

All six original C examples are included. The original Python grammar omitted the two `for` exercise forms and ternary conditionals; the F# grammar completes those extensions. `for` loops desugar to existing assignments and `while` nodes. The parser builds without unresolved conflicts, keeps arithmetic precedence explicit, and attaches `else` to the nearest unmatched `if`.

The compiler calculates stack limits, reserves JVM main slot 0 for `String[] args`, initializes local variables, validates declarations and call arities, normalizes Boolean values, short circuits `&&`/`||`, and discards unused expression values. `println` is a one-argument void statement. A non-main function falling through returns zero; this explicit teaching-language choice avoids unverifiable bytecode. It is a small integer teaching language, not a general C implementation.

## Optional JVM verification

F# setup exports `Compiler.fsx` from the notebook. A JDK and the [Jasmin 3.0.3 artifact from Maven Central](https://central.sonatype.com/artifact/ca.mcgill.sable/jasmin/3.0.3) are needed only for bytecode assembly/execution. The standalone artifact is `jasmin-3.0.3-jar-with-dependencies.jar`; [Jasmin's source repository](https://github.com/Sable/jasmin) documents the assembler. Neither Java nor its JAR is embedded in the notebook or repository.

Run:

```sh
JAVA=/path/to/jdk/bin/java JASMIN_JAR=/path/to/jasmin-3.0.3-jar-with-dependencies.jar ./verify-jvm.sh
```

The script compiles all examples, assembles into a temporary directory, runs Java with `-Xverify:all`, and compares exact output. Verified here with OpenJDK 21 and Jasmin 3.0.3: factorials 1! through 10!, `MySum` = 666, `Ternary` = 662, and the identical 25 primes through 97 from all three prime implementations. The notebook also checks malformed input, operator precedence, dangling else, unknown variables, call arity and discarded expression values.

The AST DOT output can be rendered with `dot -Tsvg Examples/MySum.dot -o Examples/MySum.svg` when Graphviz is installed.
