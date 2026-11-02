type RegExp =
    | Empty
    | Epsilon
    | Literal of char
    | Concat of RegExp * RegExp
    | Alt of RegExp * RegExp
    | Star of RegExp
