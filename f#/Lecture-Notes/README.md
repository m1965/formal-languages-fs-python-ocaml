# Full F# lecture-notes edition

This is a generated edition of the **14 active chapters** in `Lecture-Notes/formal-languages.tex`. It retains the mathematical theory, proofs, worked grammar/automaton examples, source-language examples and JVM target material. The host implementations and tool explanations are replaced with F# and FsLexYacc. It is a complete textbook source, rather than a migration appendix.

The canonical inputs are the original lecture sources, the replacement fragments in `fragments/`, and the translated notebook/project code selected by `sources.json`. The generator reads actual F# cells and `.fs`, `.fsl` and `.fsy` files. No second set of compiler/parser implementations is authored here. Generated chapter snapshots, extracted listings, reports and PDF/compiler files are ignored under `_build/`.

From the repository root:

```sh
python FSharp/tools/build.py
python FSharp/Lecture-Notes/build.py
```

The entry point is `_build/formal-languages-fsharp.tex`. Its `coverage.json` records the original source hashes, retained ranges, replacement topics and exact code-excerpt hashes. Additional `--source-root <FSharp-directory>` arguments are available only to verify code still located in isolated development worktrees; an integrated checkout needs none.

| Active chapter | Preserved content | F# edition change |
|---|---|---|
| Introduction | Alphabets, strings, language operations, literature and questions | F#/.NET/FsLexYacc course orientation |
| Regular expressions | Definitions, algebraic laws, examples and exercises | .NET practical-syntax references |
| Scanning | Token concepts and the C tokenization example | FsLex specifications, typed payloads, regex API, exam evaluator and recursive lexical modes |
| Finite automata | Whole mathematical chapter and figures | F# subset/Thompson/state-elimination/minimization notebook links |
| Regular languages | Whole mathematical chapter and figures | F# equivalence notebook link |
| Context-free languages | Grammars, derivations, trees, ambiguity, top-down theory and EBNF grammar | Actual mutually recursive F# parser and union AST exercise |
| Generated parsers/interpreter | Calculator grammar, source-language grammar, sum and factorial programs | FsYacc actions, typed trees, immutable environments, explicit I/O and interpreter execution |
| Earley | Algorithm, worked chart, theory and questions | Actual F# chart representation, closure and parser code |
| Shift/reduce | Configurations, transition cases, grammar and abstract action/goto tables | F# parser and typed table representations |
| SLR/LR/LALR theory | Whole theory chapter, examples, comparisons and questions | Canonical F# FIRST/FOLLOW/LR closure listings and links |
| LALR conflicts | Precedence derivation and lookahead transformation | Actual FsYacc grammars/reports and structurally enforced non-associativity |
| Jasmin | Whole JVM/Java/Jasmin target chapter | Explicit distinction between F# host, Integer-C source and JVM target |
| Compiler frontend | Integer-C grammar and source program | Actual FsLex lexer, FsYacc parser, AST and diagnostics boundary |
| Code generation | Mathematical stack effects, expression/statement rules and JVM method conventions | Actual F# context, expressions, statements, functions and whole-program backend |

The exporter checks every original Definition/Theorem/Proposition/Lemma environment and every displayed `eqnarray`, `equation` and `align` block against its generated chapter. The edition preserves roughly **9,000 lines of theory** and extracts **40 canonical F# listings**; `coverage.json` records the precise retained source spans. Long compiler formulas put their left-hand sides on separate rows, with the same formula transformation applied to the preservation check. Old Python host listings and PLY precedence tuples are absent. Historical Java, Integer-C, the interpreted source language and Jasmin examples retain their explicit source/target roles.

FsYacc 11.3.0 differs from the intended non-associative rule: its equal-precedence case shifts. The logic grammar therefore factors equivalence to reject unparenthesized chains. The edition explains that behavior, rather than renaming an old PLY claim. The original state-number illustrations are described as mathematical examples; actual generated state reports belong to the translated notebooks.

Integer-C Boolean operators use short-circuit branches with stack bound `max(1, s1, s2)`. The original eager `iand`/`ior` derivation remains a clearly marked comparison. The JVM arithmetic discussion distinguishes truncation, floor division and Euclidean division, including negative divisors.

To build the complete PDF with an existing TeX installation and Ghostscript:

```sh
python FSharp/Lecture-Notes/pdf.py
```

The script regenerates the edition, preconverts the referenced EPS figures with `epstopdf` into `_build/converted/`, then runs `pdflatex -no-shell-escape`, BibTeX, MakeIndex, and two more LaTeX passes. It derives paths from the checkout and passes subprocess arguments individually, including paths with spaces. Failures stop the build; an old PDF is removed before compilation, and missing glyphs or unresolved references fail validation. Logs and the resulting `_build/formal-languages-fsharp.pdf` remain ignored.

The original Atkinson font, Chancery, CJK, figures and bibliography are retained. For the verified existing TeX Live 2026 installation, the missing packages were installed through the official CTAN repository using TeX Live's package manager:

```sh
tlmgr --repository https://mirror.ctan.org/systems/texlive/tlnet install atkinson cjk skull fontaxes epigraph nextpage
```

Common original lecture packages, `epstopdf`, BibTeX and MakeIndex must also be present in the existing installation. No new TeX distribution is required. Mathematical Unicode symbols in canonical F# strings use TeX glyph declarations; ASCII listings may wrap within long tokens, while Unicode listings wrap at spaces to avoid splitting UTF-8 bytes. Retained target-language listings use `Verbatim` without an external highlighter.

The full local build succeeded with **188 PDF pages**. Representative opening, mathematical theory, automaton diagram, scanner, Unicode parsing-table and long compiler-listing pages were rendered and inspected. The largest compiler formulas were reflowed; some retained mathematical lines still extend slightly into the text margin (up to about 27 pt), and the original font lacks small-caps shapes. These are layout warnings, not missing content or unresolved references. This is a proportional layout check, not a claim that every page has been visually audited. Structural export checks remain available without TeX.

Read [EVALUATION.md](EVALUATION.md) for the proposed comparison of student comprehension. Executable verification and static type checking do not establish equal teaching clarity by themselves.
