namespace CalculatorSyntax

module Parse =
    let statement source =
        let buffer = FSharp.Text.Lexing.LexBuffer<char>.FromString source
        buffer.EndPos <- FSharp.Text.Lexing.Position.FirstLine "calculator"
        try Parser.statement Lexer.token buffer
        with error -> failwithf "Calculator syntax error at line %d, column %d: %s" buffer.StartPos.Line (buffer.StartPos.Column + 1) error.Message
