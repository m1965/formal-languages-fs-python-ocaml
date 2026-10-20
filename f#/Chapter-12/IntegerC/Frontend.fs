namespace IntegerC
open FSharp.Text.Lexing
module Frontend =
    let parse source =
        let lexbuf = LexBuffer<char>.FromString source
        lexbuf.EndPos <- Position.FirstLine "input"
        try Parser.program Lexer.token lexbuf
        with ex -> failwithf "Parse error at line %d, column %d: %s" lexbuf.StartPos.Line (lexbuf.StartPos.Column+1) ex.Message

    let scan source =
        let lexbuf = LexBuffer<char>.FromString source
        lexbuf.EndPos <- Position.FirstLine "input"
        let rec loop acc =
            match Lexer.token lexbuf with
            | Parser.EOF -> List.rev acc
            | token -> loop ((token,lexbuf.StartPos.Line,lexbuf.StartPos.Column+1)::acc)
        loop []
