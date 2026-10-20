# Regular expressions and finite automata in F#

All 15 notebooks in `Python/Chapter-04-05` have a matching F# notebook here. The original filenames are retained for comparison, including the Python type-annotation introduction, whose content now teaches F# type inference. The six Thompson-construction diagrams retain their original files and mathematical explanations.

Run `python FSharp/tools/build.py` from the repository root before opening these notebooks with the `.NET (F#)` kernel. The build exports reusable `.fsx` libraries from the defining notebook cells. Those exports are generated and ignored by Git: edit the notebook algorithms rather than maintaining two implementations. `AutomataTypes.fsx` owns the shared representations. Loading cells explicitly load it once, followed by the required algorithm modules, because reloading the same F# type definitions through multiple libraries creates incompatible type identities.

The numbered sequence covers subset construction, Thompson construction, state elimination, minimization and equivalence. `RegExp-Parser`, `Rewrite` and `FSM-2-Dot` explain their supporting algorithms. `Minimize-FP` implements the previously missing fixed-point dependency and reuses the first minimizer's quotient construction.

F# discriminated unions replace recursive tuples and unchecked operator strings. Records name the five automaton components. Sets and maps are immutable; epsilon transitions have their own symbol case. The Thompson state counter is local to one conversion, and rewriting patterns have a separate type so uppercase input letters remain literal.

A DFA can have missing transitions for membership testing and conversion to a regular expression: an absent transition rejects the word. Minimization and language difference explicitly require complete DFAs. Subset construction supplies the rejecting empty subset when needed and always returns a complete DFA over its alphabet. Character conversion rejects literals outside the supplied alphabet. The regular-expression parser ignores whitespace and reports other unexpected characters.

Graph construction uses the .NET standard library. Graphviz's native `dot` executable renders the SVG diagrams displayed next to the examples. State encodings and transition text remain available alongside those diagrams. The recursive DFA-to-expression construction follows the lecture's proof and retains its exponential complexity; it is intended for small teaching examples.

Validation from the repository root:

```sh
python FSharp/tools/notebooks.py fsi
dotnet fsi --exec FSharp/Tests/Automata.fsx
python FSharp/tools/notebooks.py jupyter
```

The chapter's behavioral test compares NFA, DFA and minimized-DFA acceptance against the independent .NET regex engine for 16 expressions and every ternary word of length at most five. It also checks all 26 rewrite identities with two substitutions using exact language equivalence, state-elimination round trips, epsilon-only cycles, malformed input, alphabet errors and native Graphviz SVG output. Notebook example cells retain the lecture's equivalence examples, including its deliberately inequivalent final pair.

Two source issues are corrected explicitly in the teaching text: the second Python differentiation function's product rule, and the formula beside the `a*ba*` Thompson example. These are semantic corrections, rather than F# syntax changes. Static checking and these tests establish executable behavior; whether the F# presentation is as understandable to students remains the evaluation question.
