/// Abstract syntax trees as nested tuples.
///
/// Every parser of the notebooks returns its abstract syntax tree as a value of the type nested: a
/// tree is either a leaf, which holds an integer, a floating point number, a string, or a Boolean
/// value, or a node, which is a tuple of trees.  By convention, the first component of a node is the
/// operator, e.g.
///
///   ("+", "a", ("*", "b"))     Node [Leaf (Str "+"); Leaf (Str "a"); Node [Leaf (Str "*"); Leaf (Str "b")]]
///   ("NUMBER", "42")           Node [Leaf (Str "NUMBER"); Leaf (Str "42")]
///
/// printfn "%A" and toString write trees in the tuple notation shown on the left.
///   #load "../Support/Nested.fsx"
///   open Nested
/// Load this file only once per session: loading it again creates a new, incompatible type nested.
/// Therefore, the other scripts of this directory do not load it themselves.
module Nested

open System
open System.Globalization
open System.Text

/// a string in double quotes, with the escape sequences of F#
let quote (s: string) =
    let b = StringBuilder("\"")
    for c in s do
        match c with
        | '"' -> b.Append "\\\"" |> ignore
        | '\\' -> b.Append "\\\\" |> ignore
        | '\n' -> b.Append "\\n" |> ignore
        | '\t' -> b.Append "\\t" |> ignore
        | '\r' -> b.Append "\\r" |> ignore
        | c when Char.IsControl c -> b.AppendFormat("\\u{0:x4}", int c) |> ignore
        | c -> b.Append c |> ignore
    b.Append('"').ToString()

/// the shortest decimal representation of x that is read back as x, e.g. 0.5, 1.0, or 1E+100
let floatToString (x: float) =
    let s = x.ToString("R", CultureInfo.InvariantCulture)
    if Double.IsFinite x && not (s.Contains "." || s.Contains "E") then s + ".0" else s

[<StructuredFormatDisplay("{Display}")>]
type value =
    | Int of int
    | Float of float
    | Str of string
    | Bool of bool
    member this.Display =
        match this with
        | Int n -> string n
        | Float x -> floatToString x
        | Str s -> quote s
        | Bool b -> if b then "true" else "false"
    override this.ToString() = this.Display

[<StructuredFormatDisplay("{Display}")>]
type nested =
    | Leaf of value
    | Node of nested list
    /// the tree in tuple notation; a tuple with one component is written (x,), so that it differs from x
    member this.Display =
        match this with
        | Leaf v -> v.Display
        | Node [ x ] -> "(" + x.Display + ",)"
        | Node xs -> "(" + String.Join(", ", xs |> List.map (fun x -> x.Display)) + ")"
    override this.ToString() = this.Display

let valueToString (v: value) = v.Display

/// toString t: the tree t in tuple notation
let toString (t: nested) = t.Display

/// pformat width t: the tree t in tuple notation.  A tuple that does not fit into a line of the given
/// width is split: its components are written one below the other.
let pformat (width: int) (t: nested) =
    let b = StringBuilder()
    let length (s: string) = StringInfo(s).LengthInTextElements
    let rec format (t: nested) (indent: int) (allowance: int) =
        let rep = t.Display
        match t with
        | Node items when length rep > width - indent - allowance ->
            b.Append '(' |> ignore
            let endchar = if List.length items = 1 then ",)" else ")"
            formatItems items indent (allowance + endchar.Length)
            b.Append endchar |> ignore
        | _ -> b.Append rep |> ignore
    and formatItems (items: nested list) (indent: int) (allowance: int) =
        let indent = indent + 1
        let n = List.length items
        items
        |> List.iteri (fun i item ->
            if i > 0 then b.Append(",\n").Append(String(' ', indent)) |> ignore
            format item indent (if i = n - 1 then allowance else 1))
    format t 0 0
    b.ToString()

/// prints the tree, split into several lines if it is longer than 80 characters
let pprint (t: nested) = printfn "%s" (pformat 80 t)
