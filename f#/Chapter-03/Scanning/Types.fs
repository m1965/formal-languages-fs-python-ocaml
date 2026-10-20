namespace LectureScanning

/// Token values are explicit: no attributes are added to a lexer at runtime.
type ArithmeticToken = Number of int | Symbol of char | Illegal of char | End

type LocatedToken = { Value: ArithmeticToken; Line: int; Offset: int }

type ExamToken = Header of string | Maximum of int | Student of string | Points of int | Newline | ExamEnd

type LatexToken = Text of string | Command of string | MathDelimiter of string | Brace of char | Hat | LatexEnd
