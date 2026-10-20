namespace LectureGrammar
type Item = Variable of string | Literal of string
type Rule = { Head: string; Alternatives: Item list list }
