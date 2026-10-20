namespace GrammarSyntax

module Parse =
    let text source =
        let buffer = FSharp.Text.Lexing.LexBuffer<char>.FromString source
        buffer.EndPos <- FSharp.Text.Lexing.Position.FirstLine "grammar"
        let rules =
            try Parser.grammar Lexer.token buffer
            with error -> failwithf "Grammar error at line %d, column %d: %s" buffer.StartPos.Line (buffer.StartPos.Column + 1) error.Message
        if List.isEmpty rules then invalidArg "source" "A grammar needs at least one rule."
        let variables = rules |> List.map (fun rule -> rule.Head) |> Set.ofList
        for rule in rules do
            for symbol in rule.Body do
                match symbol with
                | Nonterminal name when not (Set.contains name variables) -> invalidArg "source" ("Undefined nonterminal: " + name)
                | _ -> ()
        { Start = rules.Head.Head; Rules = rules }
    let file path = text (System.IO.File.ReadAllText path)
