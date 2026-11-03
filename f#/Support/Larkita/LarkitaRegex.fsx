/// Larkita for F#, part 2: the analysis of regular expressions in Python's syntax.
///
/// Lark sorts the terminals by the maximal width of their regular expressions, and Larkita checks
/// whether two terminals can start with the same character.  Python computes both with its parser
/// for regular expressions (re._parser).  This module contains a small parser for the same syntax:
///
///   width re        = (min, max)   the minimal and maximal length of a match (getwidth)
///   firstChars re   = Some chars   the characters a match can start with (lexer._first_chars)
module LarkitaRegex

#load "LarkitaBase.fsx"

open System

/// MAXWIDTH of Python is 2**64; every width at least as large is shown as maxwidth
let maxwidth = Int64.MaxValue / 4L
let maxrepeat = 4294967295L

let add a b = if a >= maxwidth || b >= maxwidth then maxwidth else min (a + b) maxwidth
let mul a b = if a = 0L || b = 0L then 0L elif a >= maxwidth / b then maxwidth else min (a * b) maxwidth

type Item =
    | ILit of int
    | IRange of int * int
    | IOther                                  // a category like \d, or NEGATE

type RNode =
    | Literal of int
    | In of Item list
    | Unit                                    // ANY, CATEGORY, NOT_LITERAL: one character
    | At                                      // ^, $, \b, \A, ...: no character
    | Assert                                  // look-ahead and look-behind
    | Branch of RNode list list
    | Sub of RNode list                       // a group
    | Atomic of RNode list
    | Repeat of int64 * int64 * RNode list    // min, max (maxrepeat: unbounded), body
    | Groupref

exception RegexError

let parse (pattern: string) : RNode list =
    let s = pattern |> Seq.map int |> Array.ofSeq
    let n = s.Length
    let mutable pos = 0
    let peek () = if pos < n then Some s.[pos] else None
    let next () =
        if pos < n then
            let c = s.[pos]
            pos <- pos + 1
            c
        else raise RegexError
    let isC c (ch: char) = c = int ch
    let digit c = c >= 48 && c <= 57
    let hex c = digit c || (c >= 97 && c <= 102) || (c >= 65 && c <= 70)
    let hexValue k =
        let mutable v = 0
        for _ in 1..k do
            let c = next ()
            if not (hex c) then raise RegexError
            v <- v * 16 + Convert.ToInt32(string (char c), 16)
        v
    let mutable verbose = false
    let octalDigit () = match peek () with Some d -> d >= 48 && d <= 55 | None -> false
    let escape (inClass: bool) : RNode =
        let c = next ()
        let ch = if c < 128 then char c else '\000'
        match ch with
        | 'd' | 'D' | 's' | 'S' | 'w' | 'W' -> In [ IOther ]
        | 'b' -> if inClass then Literal 8 else At
        | 'B' | 'A' | 'Z' -> if inClass then raise RegexError else At
        | 'n' -> Literal 10
        | 't' -> Literal 9
        | 'r' -> Literal 13
        | 'f' -> Literal 12
        | 'v' -> Literal 11
        | 'a' -> Literal 7
        | 'x' -> Literal(hexValue 2)
        | 'u' -> Literal(hexValue 4)
        | 'U' -> Literal(hexValue 8)
        | 'N' ->
            while peek () <> None && not (isC (next ()) '}') do ()
            Unit
        | '0' ->
            let mutable v = 0
            let mutable k = 0
            while k < 2 && octalDigit () do
                v <- v * 8 + (next () - 48)
                k <- k + 1
            Literal v
        | _ when c >= 49 && c <= 57 ->
            if inClass then
                let mutable v = c - 48
                let mutable k = 0
                while k < 2 && octalDigit () do
                    v <- v * 8 + (next () - 48)
                    k <- k + 1
                Literal v
            else
                while (match peek () with Some d -> digit d | None -> false) do next () |> ignore
                Groupref
        | _ ->
            if (c >= 65 && c <= 90) || (c >= 97 && c <= 122) then raise RegexError
            Literal c
    let parseClass () : RNode =
        let items = ResizeArray<Item>()
        let negate =
            match peek () with
            | Some c when isC c '^' -> next () |> ignore; true
            | _ -> false
        let mutable first = true
        let mutable go = true
        while go do
            let c = next ()
            if isC c ']' && not first then go <- false
            else
                first <- false
                let lo = if isC c '\\' then escape true else Literal c
                match lo, peek () with
                | Literal a, Some d when isC d '-' && pos + 1 < n && not (isC s.[pos + 1] ']') ->
                    next () |> ignore
                    let c2 = next ()
                    let hi = if isC c2 '\\' then escape true else Literal c2
                    match hi with
                    | Literal b ->
                        if b < a then raise RegexError
                        items.Add(IRange(a, b))
                    | _ -> raise RegexError
                | Literal a, _ -> items.Add(ILit a)
                | In _, _ | Unit, _ -> items.Add IOther
                | _ -> raise RegexError
        let items = List.ofSeq items
        if negate then In(IOther :: items)
        else
            match items with
            | [ ILit a ] -> Literal a
            | _ -> In items
    let readInt () =
        let start = pos
        while (match peek () with Some d -> digit d | None -> false) do next () |> ignore
        if pos = start then None
        else Some(Int64.Parse(String(Array.map char s.[start .. pos - 1])))
    let isRepeat () =
        // '{' starts a repetition only if it has the form {m}, {m,}, {,n} or {m,n}
        let mutable p = pos
        let digits () =
            let st = p
            while p < n && digit s.[p] do p <- p + 1
            p > st
        let d1 = digits ()
        if p < n && isC s.[p] ',' then
            p <- p + 1
            digits () |> ignore
            p < n && isC s.[p] '}'
        else d1 && p < n && isC s.[p] '}'
    let rec parseSub () : RNode list =
        let alts = ResizeArray [ parseSeq () ]
        while (match peek () with Some c -> isC c '|' | None -> false) do
            next () |> ignore
            alts.Add(parseSeq ())
        if alts.Count = 1 then alts.[0] else [ Branch(List.ofSeq alts) ]
    and parseSeq () : RNode list =
        let items = ResizeArray<RNode>()
        let mutable stop = false
        while not stop do
            match peek () with
            | None -> stop <- true
            | Some c when isC c '|' || isC c ')' -> stop <- true
            | Some c ->
                next () |> ignore
                if verbose && (c = 32 || c = 9 || c = 10 || c = 13 || c = 11 || c = 12) then ()
                elif verbose && isC c '#' then
                    while (match peek () with Some d -> d <> 10 | None -> false) do next () |> ignore
                elif isC c '*' || isC c '+' || isC c '?' || (isC c '{' && isRepeat ()) then
                    let mn, mx =
                        if isC c '*' then 0L, maxrepeat
                        elif isC c '+' then 1L, maxrepeat
                        elif isC c '?' then 0L, 1L
                        else
                            let lo = readInt ()
                            let hi =
                                if (match peek () with Some d -> isC d ',' | None -> false) then
                                    next () |> ignore
                                    readInt ()
                                else
                                    match lo with
                                    | Some v -> Some v
                                    | None -> raise RegexError
                            if not (isC (next ()) '}') then raise RegexError
                            let lo = defaultArg lo 0L
                            let hi = defaultArg hi maxrepeat
                            if hi < lo then raise RegexError
                            lo, hi
                    // lazy or possessive
                    match peek () with
                    | Some d when isC d '?' || isC d '+' -> next () |> ignore
                    | _ -> ()
                    if items.Count = 0 then raise RegexError
                    let last = items.[items.Count - 1]
                    match last with
                    | At | Repeat _ -> raise RegexError
                    | _ -> items.[items.Count - 1] <- Repeat(mn, mx, [ last ])
                else items.Add(atom c)
        List.ofSeq items
    and atom (c: int) : RNode =
        if isC c '.' then Unit
        elif isC c '^' || isC c '$' then At
        elif isC c '\\' then escape false
        elif isC c '[' then parseClass ()
        elif isC c '(' then group ()
        else Literal c
    and group () : RNode =
        let close () = if not (isC (next ()) ')') then raise RegexError
        match peek () with
        | Some q when isC q '?' ->
            next () |> ignore
            let c = next ()
            let ch = if c < 128 then char c else '\000'
            match ch with
            | ':' ->
                let body = parseSub ()
                close ()
                Sub body
            | '>' ->
                let body = parseSub ()
                close ()
                Atomic body
            | '=' | '!' ->
                parseSub () |> ignore
                close ()
                Assert
            | '<' ->
                match peek () with
                | Some d when isC d '=' || isC d '!' ->
                    next () |> ignore
                    parseSub () |> ignore
                    close ()
                    Assert
                | _ -> raise RegexError
            | 'P' ->
                let d = next ()
                if isC d '<' then
                    while not (isC (next ()) '>') do ()
                    let body = parseSub ()
                    close ()
                    Sub body
                elif isC d '=' then
                    while not (isC (next ()) ')') do ()
                    Groupref
                else raise RegexError
            | '#' ->
                while not (isC (next ()) ')') do ()
                At
            | '(' ->
                while not (isC (next ()) ')') do ()
                let body = parseSub ()
                close ()
                match body with
                | [ Branch [ yes; no ] ] -> Branch [ yes; no ]
                | yes -> Branch [ yes; [] ]
            | _ ->
                // flags: (?aiLmsux) or (?flags-flags:...)
                pos <- pos - 1
                let flags = Text.StringBuilder()
                let mutable negative = false
                let mutable finished = false
                let mutable scoped = false
                while not finished do
                    let d = next ()
                    if isC d ':' then
                        scoped <- true
                        finished <- true
                    elif isC d ')' then finished <- true
                    elif isC d '-' then negative <- true
                    elif not negative then flags.Append(char d) |> ignore
                let setX = flags.ToString().Contains 'x'
                if scoped then
                    let saved = verbose
                    if setX then verbose <- true
                    let body = parseSub ()
                    verbose <- saved
                    close ()
                    Sub body
                else
                    if setX then verbose <- true
                    At
        | _ ->
            let body = parseSub ()
            close ()
            Sub body
    let result = parseSub ()
    if pos < n then raise RegexError
    result

let rec widthSeq (seq: RNode list) : int64 * int64 =
    List.fold (fun (lo, hi) node -> let i, j = widthNode node in add lo i, add hi j) (0L, 0L) seq

and widthNode (node: RNode) : int64 * int64 =
    match node with
    | Literal _ | In _ | Unit -> 1L, 1L
    | At | Assert -> 0L, 0L
    | Groupref -> 0L, maxwidth
    | Sub body | Atomic body -> widthSeq body
    | Branch alts -> List.fold (fun (lo, hi) alt -> let l, h = widthSeq alt in min lo l, max hi h) (maxwidth, 0L) alts
    | Repeat (mn, mx, body) ->
        let i, j = widthSeq body
        mul i mn, (if mx = maxrepeat && j > 0L then maxwidth else mul j mx)

/// the minimal and maximal width of a match, like re._parser.parse(r).getwidth()
let width (pattern: string) : int64 * int64 =
    try widthSeq (parse pattern)
    with RegexError -> raise (LarkitaBase.GrammarError("Cannot parse the regular expression " + pattern))

exception private Unknown

/// the characters that a match can start with (lexer._first_chars)
let firstChars (pattern: string) : Set<string> option =
    let chr (c: int) = string (char c)
    let rec first (seq: RNode list) : Set<string> * bool =
        let rec go result nodes =
            match nodes with
            | [] -> result, true
            | At :: rest -> go result rest
            | node :: rest ->
                let chars, nullable =
                    match node with
                    | Literal c -> Set.singleton (chr c), false
                    | In items ->
                        let acc =
                            items
                            |> List.fold
                                (fun acc item ->
                                    match item with
                                    | ILit c -> Set.add (chr c) acc
                                    | IRange (a, b) when b - a < 1000 -> Seq.fold (fun acc k -> Set.add (chr k) acc) acc { a .. b }
                                    | _ -> raise Unknown)
                                Set.empty
                        acc, false
                    | Branch alts ->
                        List.fold (fun (acc, nl) alt -> let sub, sn = first alt in Set.union acc sub, nl || sn) (Set.empty, false) alts
                    | Sub body -> first body
                    | Repeat (mn, _, body) ->
                        let sub, nl = first body
                        sub, nl || mn = 0L
                    | _ -> raise Unknown
                let result = Set.union result chars
                if not nullable then result, false else go result rest
        go Set.empty seq
    match (try Some(parse pattern) with RegexError -> None) with
    | None -> None
    | Some nodes ->
        match (try Some(first nodes) with Unknown -> None) with
        | None -> None
        | Some (_, true) -> None
        | Some (chars, false) ->
            Some(Set.fold (fun acc ch -> acc |> Set.add (LarkitaBase.toLower ch) |> Set.add (LarkitaBase.toUpper ch)) chars chars)
