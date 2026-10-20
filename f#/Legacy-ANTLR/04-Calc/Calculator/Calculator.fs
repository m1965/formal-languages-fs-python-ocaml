namespace LectureCalculator
open FSharp.Text.Lexing
module Calculator =
    let parse source =
        let buffer = LexBuffer<char>.FromString source
        buffer.EndPos <- Position.FirstLine "input"
        try Parser.program Lexer.token buffer
        with ex -> failwithf "Calculator parse error at line %d, column %d: %s" buffer.StartPos.Line (buffer.StartPos.Column+1) ex.Message
