namespace LanguageSyntax

module Parse =
    let program source =
        let buffer = FSharp.Text.Lexing.LexBuffer<char>.FromString source
        buffer.EndPos <- FSharp.Text.Lexing.Position.FirstLine "program"
        try Parser.program Lexer.token buffer
        with error -> failwithf "Program syntax error at line %d, column %d: %s" buffer.StartPos.Line (buffer.StartPos.Column + 1) error.Message
    let file path = program (System.IO.File.ReadAllText path)
    let tokens source =
        let buffer = FSharp.Text.Lexing.LexBuffer<char>.FromString source
        buffer.EndPos <- FSharp.Text.Lexing.Position.FirstLine "program"
        let rec loop result =
            match Lexer.token buffer with
            | Parser.EOF -> List.rev result
            | token -> loop ((token, buffer.StartPos.Line, buffer.StartPos.Column + 1) :: result)
        loop []
