namespace LectureScanning
open FSharp.Text.Lexing

module Scanning =
    let buffer source =
        let lexbuf = LexBuffer<char>.FromString source
        lexbuf.EndPos <- Position.FirstLine "input"
        lexbuf

    let arithmetic source =
        let lexbuf = buffer source
        let rec loop acc =
            match Arithmetic.token lexbuf with
            | End -> List.rev acc
            | value -> loop ({Value=value; Line=lexbuf.StartPos.Line; Offset=lexbuf.StartPos.AbsoluteOffset}::acc)
        loop []

    let exam source =
        let lexbuf = buffer source
        let rec loop acc =
            match Exam.token lexbuf with
            | ExamEnd -> List.rev acc
            | value -> loop (value::acc)
        loop []

    let html source =
        let lexbuf = buffer source
        let output = System.Text.StringBuilder()
        let rec loop () =
            match Html.token lexbuf with
            | None -> output.ToString()
            | Some text -> output.Append(text) |> ignore; loop ()
        loop ()

    let latex source =
        let lexbuf = buffer source
        let rec loop acc =
            match Latex.token lexbuf with
            | LatexEnd -> List.rev acc
            | token -> loop (token::acc)
        loop []
