namespace GrammarExamples
open FSharp.Text.Lexing
module Parse =
    let private run lexer parser text =
        let buffer = LexBuffer<char>.FromString text
        buffer.EndPos <- Position.FirstLine "input"
        try parser lexer buffer
        with error -> failwithf "Line %d, column %d: %s" buffer.StartPos.Line (buffer.StartPos.Column+1) error.Message
    let expression text = run ExpressionLexer.token ExpressionParser.main text
    let regexp text = run RegExpLexer.token RegExpParser.main text
