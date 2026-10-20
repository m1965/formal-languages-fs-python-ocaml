namespace SymbolicSyntax

module Parse =
    let expression source =
        let buffer = FSharp.Text.Lexing.LexBuffer<char>.FromString source
        buffer.EndPos <- FSharp.Text.Lexing.Position.FirstLine "symbolic expression"
        try Parser.expression Lexer.token buffer
        with error -> failwithf "Expression syntax error at line %d, column %d: %s" buffer.StartPos.Line (buffer.StartPos.Column + 1) error.Message
