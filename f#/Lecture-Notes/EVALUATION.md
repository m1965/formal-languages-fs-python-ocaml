# Evaluating whether the F# edition is as understandable

The migration's question is pedagogical: can students understand the algorithms and build the same capabilities from the F# notebooks as from the Python notebooks? Successful execution is a prerequisite for that comparison. It is not its result.

Use comparable participants and equivalent tasks in both versions. Record prior Python, F# and functional-programming experience. Counterbalance the order so that solving a task in one language does not make the other language appear easier merely because the algorithm is already familiar. Preserve the same theory, input examples, expected languages and diagrams.

| Task | What the student should explain or produce | Evidence to record |
|---|---|---|
| Tokenize an arithmetic input | Trace longest match, token payloads and source positions; add an identifier token | Correct token stream, time, mistaken rule ordering and unsupported input behavior |
| Determinize an NFA | Predict an epsilon closure and explain the meaning of a subset state | Correct transition/state prediction and an explanation of the empty subset |
| Extend the regular-expression parser | Add a justified grammar case and keep precedence and errors coherent | Accepted/rejected inputs, tree shape, number and cause of corrections |
| Minimize a DFA | Identify unreachable states and explain why two reachable states are equivalent | Correct partition and a separating-word explanation |
| Extend an interpreter | Add a statement or expression without confusing parse structure with evaluation state | Correct execution and an explanation of the affected type/matches |
| Diagnose a parser conflict | Distinguish ambiguity, insufficient lookahead and LALR state merging | A corrected grammar or explicit rejection, plus a causal explanation |
| Trace compiler stack effects | Derive one expression's stack limit and explain a discarded value | Correct instructions/stack bound and a source-to-target trace |

Ask students to explain the code before changing it. During changes, distinguish algorithm errors from syntax/type-system difficulties, setup problems and unfamiliar library APIs. Record whether a type error points to the actual misunderstanding or becomes an additional barrier. An error prevented by the compiler is useful only when the learner can understand how to resolve it.

Compare these specific choices:

- **Recursive unions and records:** Do names such as `Concat`, `Binary`, `States` and `Accepting` clarify structure, or does learning union constructors dominate the task?
- **Type inference:** Can students infer what functions accept and return without needing annotations everywhere? Are explicit boundary signatures sufficient?
- **Sets and maps:** Do immutable operations make the mathematics easier to follow? Do students understand that a map update returns a new value?
- **Local mutation:** Does the Thompson counter or Earley chart remain visibly confined to one computation? Do repeated runs behave predictably?
- **Generated parsers:** Can students connect an `.fsy` production and its typed action to the AST returned by the notebook? Is the generated build step understandable?
- **Notebook flow:** Can students run all examples, inspect diagrams and follow imports without opening generated files merely to discover an algorithm?

Use the original and translated code side by side for a final explanation interview. Ask which version the participant would choose to teach a particular algorithm and why. Keep this qualitative evidence alongside correctness, task time and intervention counts. A shorter implementation, fewer annotations or a preference score alone should not decide the result.

Report observed results per task and participant background. State the sample size, setup, order and limitations. The current repository provides executable translations, coverage reports and behavioral checks; no student study has yet been performed, and equal teaching clarity is not claimed.
