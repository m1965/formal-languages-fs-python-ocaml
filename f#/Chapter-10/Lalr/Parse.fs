namespace FormalLanguages.Lalr

open FSharp.Text.Lexing

module Parse =
    let private run lexer parser text =
        let buffer = LexBuffer<char>.FromString text
        buffer.EndPos <- Position.FirstLine "input"
        try parser lexer buffer
        with error ->
            failwithf "Parse error at line %d, column %d: %s" buffer.StartPos.Line (buffer.StartPos.Column + 1) error.Message

    let private ambiguousToken buffer =
        match Lexer.token buffer with
        | Numeric n when LexBuffer<char>.LexemeString buffer |> String.forall System.Char.IsDigit -> AmbiguousParser.NUMBER n
        | Operator "+" -> AmbiguousParser.PLUS
        | Operator "-" -> AmbiguousParser.MINUS
        | Operator "*" -> AmbiguousParser.TIMES
        | Operator "/" -> AmbiguousParser.DIVIDE
        | Operator "^" -> AmbiguousParser.POWER
        | Operator "(" -> AmbiguousParser.LPAREN
        | Operator ")" -> AmbiguousParser.RPAREN
        | End -> AmbiguousParser.EOF
        | other -> failwithf "Unexpected token %A" other

    let ambiguous text = run ambiguousToken AmbiguousParser.main text

    let private resolvedToken buffer =
        match Lexer.token buffer with
        | Numeric n when LexBuffer<char>.LexemeString buffer |> String.forall System.Char.IsDigit -> ResolvedParser.NUMBER n
        | Operator "+" -> ResolvedParser.PLUS
        | Operator "-" -> ResolvedParser.MINUS
        | Operator "*" -> ResolvedParser.TIMES
        | Operator "/" -> ResolvedParser.DIVIDE
        | Operator "^" -> ResolvedParser.POWER
        | Operator "(" -> ResolvedParser.LPAREN
        | Operator ")" -> ResolvedParser.RPAREN
        | End -> ResolvedParser.EOF
        | other -> failwithf "Unexpected token %A" other

    let resolved text = run resolvedToken ResolvedParser.main text

    let private logicToken buffer =
        match Lexer.token buffer with
        | Identifier s -> LogicParser.ID s
        | Operator "↔" -> LogicParser.EQUIV
        | Operator "→" -> LogicParser.IMPLIES
        | Operator "∨" -> LogicParser.OR
        | Operator "∧" -> LogicParser.AND
        | Operator "¬" -> LogicParser.NOT
        | Operator "⊤" -> LogicParser.TRUE
        | Operator "⊥" -> LogicParser.FALSE
        | Operator "(" -> LogicParser.LPAREN
        | Operator ")" -> LogicParser.RPAREN
        | End -> LogicParser.EOF
        | other -> failwithf "Unexpected token %A" other

    let logic text = run logicToken LogicParser.main text

    let private symbolicToken buffer =
        match Lexer.token buffer with
        | Numeric n -> SymbolicParser.NUMBER n
        | Identifier s when not (s.Contains "<" || s.Contains ">") -> SymbolicParser.ID s
        | Operator "+" -> SymbolicParser.PLUS
        | Operator "-" -> SymbolicParser.MINUS
        | Operator "*" -> SymbolicParser.TIMES
        | Operator "/" -> SymbolicParser.DIVIDE
        | Operator "(" -> SymbolicParser.LPAREN
        | Operator ")" -> SymbolicParser.RPAREN
        | Operator ":=" -> SymbolicParser.ASSIGN
        | Operator ";" -> SymbolicParser.SEMICOLON
        | End -> SymbolicParser.EOF
        | other -> failwithf "Unexpected token %A" other

    let symbolic text = run symbolicToken SymbolicParser.main text

    let private lookAheadToken buffer =
        match Lexer.character buffer with
        | Character 'U' -> LookAheadParser.U
        | Character 'V' -> LookAheadParser.V
        | Character 'W' -> LookAheadParser.W
        | Character 'X' -> LookAheadParser.X
        | End -> LookAheadParser.EOF
        | other -> failwithf "Unexpected token %A" other

    let lookAhead text = run lookAheadToken LookAheadParser.main text

    let private lookAheadSolvedToken buffer =
        match Lexer.character buffer with
        | Character 'U' -> LookAheadSolvedParser.U
        | Character 'V' -> LookAheadSolvedParser.V
        | Character 'W' -> LookAheadSolvedParser.W
        | Character 'X' -> LookAheadSolvedParser.X
        | End -> LookAheadSolvedParser.EOF
        | other -> failwithf "Unexpected token %A" other

    let lookAheadSolved text = run lookAheadSolvedToken LookAheadSolvedParser.main text

    let private mysteriousToken buffer =
        match Lexer.character buffer with
        | Character 'v' -> MysteriousParser.V
        | Character 'w' -> MysteriousParser.W
        | Character 'x' -> MysteriousParser.X
        | Character 'y' -> MysteriousParser.Y
        | Character 'z' -> MysteriousParser.Z
        | End -> MysteriousParser.EOF
        | other -> failwithf "Unexpected token %A" other

    let mysterious text = run mysteriousToken MysteriousParser.main text
