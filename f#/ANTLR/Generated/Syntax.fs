namespace GrammarExamples

type Expression = Number of int | Binary of string * Expression * Expression
type RegExp = Letter of char | Union of RegExp * RegExp | Product of RegExp * RegExp | Repeat of RegExp
