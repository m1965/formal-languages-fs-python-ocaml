# Grammar to HTML with FsLexYacc

The original `Grammar.g4` exercise contains only an unfinished start production. This F# port completes the grammar-of-grammars in `GrammarParser/Parser.fsy`, scans identifiers/quoted literals in `Lexer.fsl`, and generates both with FsLexYacc 11.3.0 through `GrammarParser.fsproj`.

The notebook preserves the rendering stages, composes the small rendering functions after defining them, HTML-escapes literal operators, links variables, and produces HTML from all 63 rules in the original `c-grammar.g`. Empty alternatives are supported and checked. The last kernel-only cell displays the generated table; standalone FSI writes `c-grammar.html` for offline inspection.
