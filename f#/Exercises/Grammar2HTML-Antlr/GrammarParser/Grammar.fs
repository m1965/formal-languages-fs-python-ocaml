namespace LectureGrammar
open FSharp.Text.Lexing
module Grammar =
    let parse source =
        let buffer = LexBuffer<char>.FromString source
        buffer.EndPos <- Position.FirstLine "grammar"
        try Parser.grammar Lexer.token buffer
        with ex -> failwithf "Grammar parse error at line %d, column %d: %s" buffer.StartPos.Line (buffer.StartPos.Column+1) ex.Message
