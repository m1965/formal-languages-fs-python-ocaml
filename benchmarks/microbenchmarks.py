#!/usr/bin/env python3
"""
Empirical Microbenchmark Suite for Chapter 5:
Compares Python 3.13 (CPython), OCaml 5.4.1 (Bytecode Toplevel & Native ocamlopt),
and F# 9.0 (.NET 9 CoreCLR) on four canonical Formal Language workloads:
  1. Lexical Analysis (Regex / DFA Scanner over 200,000 tokens)
  2. Subset Construction (NFA -> DFA Powerset Construction with immutable sets)
  3. DFA Minimization (Fixpoint Partition Refinement over state equivalence classes)
  4. Symbolic AST Differentiation & Algebraic Simplification (Deep ADT Pattern Matching)
"""
import json
import os
import resource
import statistics
import subprocess
import tempfile
import time
from pathlib import Path

BENCH_DIR = Path(__file__).resolve().parent
DATA_DIR = BENCH_DIR / "results"
DATA_DIR.mkdir(parents=True, exist_ok=True)

# ------------------------------------------------------------------------------
# 1. PYTHON IMPLEMENTATION OF THE 4 BENCHMARK WORKLOADS
# ------------------------------------------------------------------------------
PYTHON_BENCH_CODE = r'''
import time, re, json, resource, sys
sys.setrecursionlimit(20000)

results = {}

# --- Benchmark 1: Lexical Analysis (200,000 tokens) ---
token_spec = [
    ('NUMBER',   r'\d+'),
    ('IDENT',    r'[A-Za-z_][A-Za-z0-9_]*'),
    ('OP',       r'[+\-*/^=()<>!]+'),
    ('WS',       r'[ \t\n]+'),
]
tok_regex = re.compile('|'.join(f'(?P<{name}>{pattern})' for name, pattern in token_spec))
sample_line = "x1 = 42 * (alpha_2 + 137) - beta_9 / 7 ^ 2\n"
input_text = sample_line * 20000  # ~220,000 tokens

t0 = time.perf_counter()
count = 0
for mo in tok_regex.finditer(input_text):
    if mo.lastgroup != 'WS':
        count += 1
t1 = time.perf_counter()
results['lexing_ms'] = (t1 - t0) * 1000.0
results['lexing_tokens'] = count

# --- Benchmark 2: NFA to DFA Subset Construction ---
# Parameterized NFA family recognizing strings where k-th symbol from end is '1' (k=11 -> 2048 DFA states)
k = 11
nfa_states = set(range(k + 2))
alphabet = ('0', '1')
delta = {}
delta[(0, '0')] = frozenset([0])
delta[(0, '1')] = frozenset([0, 1])
for s in range(1, k + 1):
    delta[(s, '0')] = frozenset([s + 1])
    delta[(s, '1')] = frozenset([s + 1])

def move(state_set, ch):
    res = set()
    for q in state_set:
        res |= delta.get((q, ch), frozenset())
    return frozenset(res)

t0 = time.perf_counter()
start = frozenset([0])
visited = {start}
worklist = [start]
dfa_delta = {}
while worklist:
    current = worklist.pop()
    for ch in alphabet:
        nxt = move(current, ch)
        dfa_delta[(current, ch)] = nxt
        if nxt not in visited:
            visited.add(nxt)
            worklist.append(nxt)
t1 = time.perf_counter()
results['nfa2dfa_ms'] = (t1 - t0) * 1000.0
results['nfa2dfa_states'] = len(visited)

# --- Benchmark 3: DFA Minimization (Partition Refinement) ---
# Minimize a redundancy-inflated DFA of 1024 states down to minimal states
num_states = 1024
dfa_trans = {}
for s in range(num_states):
    dfa_trans[(s, '0')] = (s * 2) % 512 + (512 if s >= 512 else 0)
    dfa_trans[(s, '1')] = (s * 2 + 1) % 512 + (512 if s < 512 else 0)
accepting = frozenset(s for s in range(num_states) if (s % 16) == 15)
non_accepting = frozenset(range(num_states)) - accepting

t0 = time.perf_counter()
partitions = [accepting, non_accepting]
state_to_part = {}
for idx, part in enumerate(partitions):
    for s in part:
        state_to_part[s] = idx

changed = True
while changed:
    changed = False
    new_partitions = []
    for part in partitions:
        if len(part) <= 1:
            new_partitions.append(part)
            continue
        groups = {}
        for s in part:
            sig = (state_to_part[dfa_trans[(s, '0')]], state_to_part[dfa_trans[(s, '1')]])
            groups.setdefault(sig, set()).add(s)
        if len(groups) > 1:
            changed = True
        for g in groups.values():
            new_partitions.append(frozenset(g))
    partitions = new_partitions
    for idx, part in enumerate(partitions):
        for s in part:
            state_to_part[s] = idx
t1 = time.perf_counter()
results['minimize_ms'] = (t1 - t0) * 1000.0
results['minimize_blocks'] = len(partitions)

# --- Benchmark 4: Symbolic AST Differentiation & Simplification ---
class Expr: pass
class Num(Expr):
    __slots__ = ('n',)
    __match_args__ = ('n',)
    def __init__(self, n): self.n = n
class Var(Expr):
    __slots__ = ('x',)
    __match_args__ = ('x',)
    def __init__(self, x): self.x = x
class Add(Expr):
    __slots__ = ('l', 'r')
    __match_args__ = ('l', 'r')
    def __init__(self, l, r): self.l = l; self.r = r
class Mul(Expr):
    __slots__ = ('l', 'r')
    __match_args__ = ('l', 'r')
    def __init__(self, l, r): self.l = l; self.r = r

def diff(e, v):
    match e:
        case Num(_): return Num(0)
        case Var(x): return Num(1 if x == v else 0)
        case Add(l, r): return Add(diff(l, v), diff(r, v))
        case Mul(l, r): return Add(Mul(diff(l, v), r), Mul(l, diff(r, v)))

def simplify(e):
    match e:
        case Add(l, r):
            l1, r1 = simplify(l), simplify(r)
            match (l1, r1):
                case (Num(0), _): return r1
                case (_, Num(0)): return l1
                case (Num(a), Num(b)): return Num(a + b)
                case _: return Add(l1, r1)
        case Mul(l, r):
            l1, r1 = simplify(l), simplify(r)
            match (l1, r1):
                case (Num(0), _) | (_, Num(0)): return Num(0)
                case (Num(1), _): return r1
                case (_, Num(1)): return l1
                case (Num(a), Num(b)): return Num(a * b)
                case _: return Mul(l1, r1)
        case _: return e

def count_nodes(e):
    match e:
        case Num(_) | Var(_): return 1
        case Add(l, r) | Mul(l, r): return 1 + count_nodes(l) + count_nodes(r)

# Build nested polynomial expression tree of depth 13
expr = Var('x')
for i in range(1, 13):
    expr = Mul(Add(expr, Num(i)), Add(Var('x'), Num(1)))

t0 = time.perf_counter()
d1 = simplify(diff(expr, 'x'))
d2 = simplify(diff(d1, 'x'))
nodes = count_nodes(d2)
t1 = time.perf_counter()
results['ast_diff_ms'] = (t1 - t0) * 1000.0
results['ast_nodes'] = nodes

ru = resource.getrusage(resource.RUSAGE_SELF)
# On macOS ru_maxrss is in bytes
results['peak_rss_mb'] = ru.ru_maxrss / (1024 * 1024)
print(json.dumps(results))
'''

# ------------------------------------------------------------------------------
# 2. OCAML IMPLEMENTATION OF THE 4 BENCHMARK WORKLOADS
# ------------------------------------------------------------------------------
OCAML_BENCH_CODE = r'''
module IntSet = Set.Make(Int)
module IntSetMap = Map.Make(IntSet)
module IntCharMap = Map.Make(struct
  type t = int * char
  let compare = compare
end)
module SigMap = Map.Make(struct
  type t = int * int
  let compare = compare
end)

type expr =
  | Num of int
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr

(* --- Benchmark 1: Lexical Analysis (200,000 tokens) --- *)
let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_alnum c = is_alpha c || is_digit c
let is_op c =
  match c with
  | '+' | '-' | '*' | '/' | '^' | '=' | '(' | ')' | '<' | '>' | '!' -> true
  | _ -> false
let is_ws c = c = ' ' || c = '\t' || c = '\n'

let scan_tokens (s : string) : int =
  let n = String.length s in
  let rec loop i count =
    if i >= n then count
    else
      let c = String.unsafe_get s i in
      if is_ws c then
        let j = ref (i + 1) in
        while !j < n && is_ws (String.unsafe_get s !j) do incr j done;
        loop !j count
      else if is_digit c then
        let j = ref (i + 1) in
        while !j < n && is_digit (String.unsafe_get s !j) do incr j done;
        loop !j (count + 1)
      else if is_alpha c then
        let j = ref (i + 1) in
        while !j < n && is_alnum (String.unsafe_get s !j) do incr j done;
        loop !j (count + 1)
      else if is_op c then
        let j = ref (i + 1) in
        while !j < n && is_op (String.unsafe_get s !j) do incr j done;
        loop !j (count + 1)
      else loop (i + 1) count
  in
  loop 0 0

let () =
  let sample_line = "x1 = 42 * (alpha_2 + 137) - beta_9 / 7 ^ 2\n" in
  let buf = Buffer.create (String.length sample_line * 20000) in
  for _ = 1 to 20000 do Buffer.add_string buf sample_line done;
  let input_text = Buffer.contents buf in

  let t0 = Unix.gettimeofday () in
  let lex_count = scan_tokens input_text in
  let t1 = Unix.gettimeofday () in
  let lexing_ms = (t1 -. t0) *. 1000.0 in

  (* --- Benchmark 2: NFA to DFA Subset Construction --- *)
  let k = 11 in
  let delta = ref IntCharMap.empty in
  delta := IntCharMap.add (0, '0') (IntSet.singleton 0) !delta;
  delta := IntCharMap.add (0, '1') (IntSet.of_list [0; 1]) !delta;
  for s = 1 to k do
    delta := IntCharMap.add (s, '0') (IntSet.singleton (s + 1)) !delta;
    delta := IntCharMap.add (s, '1') (IntSet.singleton (s + 1)) !delta
  done;
  let move state_set ch =
    IntSet.fold (fun q acc ->
      match IntCharMap.find_opt (q, ch) !delta with
      | Some nxt -> IntSet.union acc nxt
      | None -> acc
    ) state_set IntSet.empty
  in
  let t0 = Unix.gettimeofday () in
  let module StateSetSet = Set.Make(IntSet) in
  let start = IntSet.singleton 0 in
  let visited = ref (StateSetSet.singleton start) in
  let worklist = ref [start] in
  while !worklist <> [] do
    let current = List.hd !worklist in
    worklist := List.tl !worklist;
    List.iter (fun ch ->
      let nxt = move current ch in
      if not (StateSetSet.mem nxt !visited) then begin
        visited := StateSetSet.add nxt !visited;
        worklist := nxt :: !worklist
      end
    ) ['0'; '1']
  done;
  let t1 = Unix.gettimeofday () in
  let nfa2dfa_ms = (t1 -. t0) *. 1000.0 in
  let nfa2dfa_states = StateSetSet.cardinal !visited in

  (* --- Benchmark 3: DFA Minimization --- *)
  let num_states = 1024 in
  let trans0 = Array.init num_states (fun s -> (s * 2) mod 512 + (if s >= 512 then 512 else 0)) in
  let trans1 = Array.init num_states (fun s -> (s * 2 + 1) mod 512 + (if s < 512 then 512 else 0)) in
  let accepting = ref IntSet.empty in
  let non_accepting = ref IntSet.empty in
  for s = 0 to num_states - 1 do
    if s mod 16 = 15 then accepting := IntSet.add s !accepting
    else non_accepting := IntSet.add s !non_accepting
  done;

  let t0 = Unix.gettimeofday () in
  let partitions = ref [!accepting; !non_accepting] in
  let state_to_part = Array.make num_states 0 in
  List.iteri (fun idx part ->
    IntSet.iter (fun s -> state_to_part.(s) <- idx) part
  ) !partitions;

  let changed = ref true in
  while !changed do
    changed := false;
    let new_parts = ref [] in
    List.iter (fun part ->
      if IntSet.cardinal part <= 1 then
        new_parts := part :: !new_parts
      else begin
        let groups = IntSet.fold (fun s acc ->
          let sig_key = (state_to_part.(trans0.(s)), state_to_part.(trans1.(s))) in
          let existing = match SigMap.find_opt sig_key acc with Some g -> g | None -> IntSet.empty in
          SigMap.add sig_key (IntSet.add s existing) acc
        ) part SigMap.empty in
        if SigMap.cardinal groups > 1 then changed := true;
        SigMap.iter (fun _ g -> new_parts := g :: !new_parts) groups
      end
    ) !partitions;
    partitions := List.rev !new_parts;
    List.iteri (fun idx part ->
      IntSet.iter (fun s -> state_to_part.(s) <- idx) part
    ) !partitions
  done;
  let t1 = Unix.gettimeofday () in
  let minimize_ms = (t1 -. t0) *. 1000.0 in
  let minimize_blocks = List.length !partitions in

  (* --- Benchmark 4: Symbolic AST Differentiation & Simplification --- *)
  let rec diff e v =
    match e with
    | Num _ -> Num 0
    | Var x -> if x = v then Num 1 else Num 0
    | Add (l, r) -> Add (diff l v, diff r v)
    | Mul (l, r) -> Add (Mul (diff l v, r), Mul (l, diff r v))
  in
  let rec simplify e =
    match e with
    | Add (l, r) ->
        let l1 = simplify l and r1 = simplify r in
        (match l1, r1 with
         | Num 0, _ -> r1
         | _, Num 0 -> l1
         | Num a, Num b -> Num (a + b)
         | _ -> Add (l1, r1))
    | Mul (l, r) ->
        let l1 = simplify l and r1 = simplify r in
        (match l1, r1 with
         | Num 0, _ | _, Num 0 -> Num 0
         | Num 1, _ -> r1
         | _, Num 1 -> l1
         | Num a, Num b -> Num (a * b)
         | _ -> Mul (l1, r1))
    | _ -> e
  in
  let rec count_nodes e =
    match e with
    | Num _ | Var _ -> 1
    | Add (l, r) | Mul (l, r) -> 1 + count_nodes l + count_nodes r
  in
  let expr = ref (Var "x") in
  for i = 1 to 12 do
    expr := Mul (Add (!expr, Num i), Add (Var "x", Num 1))
  done;
  let t0 = Unix.gettimeofday () in
  let d1 = simplify (diff !expr "x") in
  let d2 = simplify (diff d1 "x") in
  let ast_nodes = count_nodes d2 in
  let t1 = Unix.gettimeofday () in
  let ast_diff_ms = (t1 -. t0) *. 1000.0 in

  Printf.printf "{\"lexing_ms\": %.4f, \"lexing_tokens\": %d, \"nfa2dfa_ms\": %.4f, \"nfa2dfa_states\": %d, \"minimize_ms\": %.4f, \"minimize_blocks\": %d, \"ast_diff_ms\": %.4f, \"ast_nodes\": %d}\n"
    lexing_ms lex_count nfa2dfa_ms nfa2dfa_states minimize_ms minimize_blocks ast_diff_ms ast_nodes
'''

# ------------------------------------------------------------------------------
# 3. F# IMPLEMENTATION OF THE 4 BENCHMARK WORKLOADS
# ------------------------------------------------------------------------------
FSHARP_BENCH_CODE = r'''
open System
open System.Diagnostics
open System.Text

type Expr =
    | Num of int
    | Var of string
    | Add of Expr * Expr
    | Mul of Expr * Expr

let rec diff e v =
    match e with
    | Num _ -> Num 0
    | Var x -> if x = v then Num 1 else Num 0
    | Add (l, r) -> Add (diff l v, diff r v)
    | Mul (l, r) -> Add (Mul (diff l v, r), Mul (l, diff r v))

let rec simplify e =
    match e with
    | Add (l, r) ->
        let l1 = simplify l
        let r1 = simplify r
        match l1, r1 with
        | Num 0, _ -> r1
        | _, Num 0 -> l1
        | Num a, Num b -> Num (a + b)
        | _ -> Add (l1, r1)
    | Mul (l, r) ->
        let l1 = simplify l
        let r1 = simplify r
        match l1, r1 with
        | Num 0, _ | _, Num 0 -> Num 0
        | Num 1, _ -> r1
        | _, Num 1 -> l1
        | Num a, Num b -> Num (a * b)
        | _ -> Mul (l1, r1)
    | _ -> e

let rec countNodes e =
    match e with
    | Num _ | Var _ -> 1
    | Add (l, r) | Mul (l, r) -> 1 + countNodes l + countNodes r

let isDigit c = c >= '0' && c <= '9'
let isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let isAlnum c = isAlpha c || isDigit c
let isOp c =
    match c with
    | '+' | '-' | '*' | '/' | '^' | '=' | '(' | ')' | '<' | '>' | '!' -> true
    | _ -> false
let isWs c = c = ' ' || c = '\t' || c = '\n'

let scanTokens (s: string) =
    let n = s.Length
    let rec loop i count =
        if i >= n then count
        else
            let c = s.[i]
            if isWs c then
                let mutable j = i + 1
                while j < n && isWs s.[j] do j <- j + 1
                loop j count
            elif isDigit c then
                let mutable j = i + 1
                while j < n && isDigit s.[j] do j <- j + 1
                loop j (count + 1)
            elif isAlpha c then
                let mutable j = i + 1
                while j < n && isAlnum s.[j] do j <- j + 1
                loop j (count + 1)
            elif isOp c then
                let mutable j = i + 1
                while j < n && isOp s.[j] do j <- j + 1
                loop j (count + 1)
            else loop (i + 1) count
    loop 0 0

// Warmup JIT
let _ = scanTokens "x = 1 + 2"

// 1. Lexing
let sampleLine = "x1 = 42 * (alpha_2 + 137) - beta_9 / 7 ^ 2\n"
let sb = StringBuilder(sampleLine.Length * 20000)
for _ in 1 .. 20000 do sb.Append(sampleLine) |> ignore
let inputText = sb.ToString()

let sw = Stopwatch.StartNew()
let lexCount = scanTokens inputText
sw.Stop()
let lexingMs = sw.Elapsed.TotalMilliseconds

// 2. NFA to DFA Subset Construction
let k = 11
let delta =
    seq {
        yield ((0, '0'), Set.singleton 0)
        yield ((0, '1'), Set.ofList [0; 1])
        for s in 1 .. k do
            yield ((s, '0'), Set.singleton (s + 1))
            yield ((s, '1'), Set.singleton (s + 1))
    } |> Map.ofSeq

let move (stateSet: Set<int>) (ch: char) =
    stateSet |> Set.fold (fun acc q ->
        match Map.tryFind (q, ch) delta with
        | Some nxt -> Set.union acc nxt
        | None -> acc
    ) Set.empty

sw.Restart()
let start = Set.singleton 0
let mutable visited = Set.singleton start
let mutable worklist = [start]
while not worklist.IsEmpty do
    let current = worklist.Head
    worklist <- worklist.Tail
    for ch in ['0'; '1'] do
        let nxt = move current ch
        if not (Set.contains nxt visited) then
            visited <- Set.add nxt visited
            worklist <- nxt :: worklist
sw.Stop()
let nfa2dfaMs = sw.Elapsed.TotalMilliseconds
let nfa2dfaStates = visited.Count

// 3. DFA Minimization
let numStates = 1024
let trans0 = Array.init numStates (fun s -> (s * 2) % 512 + (if s >= 512 then 512 else 0))
let trans1 = Array.init numStates (fun s -> (s * 2 + 1) % 512 + (if s < 512 then 512 else 0))
let accepting = [ for s in 0 .. numStates - 1 do if s % 16 = 15 then yield s ] |> Set.ofList
let nonAccepting = [ for s in 0 .. numStates - 1 do if s % 16 <> 15 then yield s ] |> Set.ofList

sw.Restart()
let mutable partitions = [accepting; nonAccepting]
let stateToPart = Array.zeroCreate<int> numStates
partitions |> List.iteri (fun idx part ->
    for s in part do stateToPart.[s] <- idx)

let mutable changed = true
while changed do
    changed <- false
    let mutable newParts = []
    for part in partitions do
        if part.Count <= 1 then
            newParts <- part :: newParts
        else
            let groups =
                part |> Set.fold (fun acc s ->
                    let sigKey = (stateToPart.[trans0.[s]], stateToPart.[trans1.[s]])
                    let existing = Map.tryFind sigKey acc |> Option.defaultValue Set.empty
                    Map.add sigKey (Set.add s existing) acc
                ) Map.empty
            if groups.Count > 1 then changed <- true
            for KeyValue(_, g) in groups do
                newParts <- g :: newParts
    partitions <- List.rev newParts
    partitions |> List.iteri (fun idx part ->
        for s in part do stateToPart.[s] <- idx)
sw.Stop()
let minimizeMs = sw.Elapsed.TotalMilliseconds
let minimizeBlocks = partitions.Length

// 4. Symbolic AST Differentiation & Simplification
let mutable expr = Var "x"
for i in 1 .. 12 do
    expr <- Mul (Add (expr, Num i), Add (Var "x", Num 1))

// Warmup JIT on AST
let _ = simplify (diff (Add (Var "x", Num 1)) "x")

sw.Restart()
let d1 = simplify (diff expr "x")
let d2 = simplify (diff d1 "x")
let astNodes = countNodes d2
sw.Stop()
let astDiffMs = sw.Elapsed.TotalMilliseconds

let proc = Process.GetCurrentProcess()
let peakRssMb = float proc.WorkingSet64 / (1024.0 * 1024.0)

printfn "{\"lexing_ms\": %.4f, \"lexing_tokens\": %d, \"nfa2dfa_ms\": %.4f, \"nfa2dfa_states\": %d, \"minimize_ms\": %.4f, \"minimize_blocks\": %d, \"ast_diff_ms\": %.4f, \"ast_nodes\": %d, \"peak_rss_mb\": %.2f}"
    lexingMs lexCount nfa2dfaMs nfa2dfaStates minimizeMs minimizeBlocks astDiffMs astNodes peakRssMb
'''


def run_cmd_with_rss(cmd, env=None, cwd=None):
    """Run a command, returning stdout and peak RSS in MB via /usr/bin/time -l on macOS."""
    full_cmd = ["/usr/bin/time", "-l"] + cmd
    res = subprocess.run(full_cmd, capture_output=True, text=True, env=env, cwd=cwd, check=True)
    stdout = res.stdout.strip().splitlines()[-1]
    data = json.loads(stdout)
    # Parse maximum resident set size from /usr/bin/time -l stderr
    for line in res.stderr.splitlines():
        if "maximum resident set size" in line:
            rss_bytes = int(line.strip().split()[0])
            data["peak_rss_mb"] = round(rss_bytes / (1024.0 * 1024.0), 2)
    return data


def main():
    runs = 5
    summary = {}

    with tempfile.TemporaryDirectory() as tmpdir:
        tmp = Path(tmpdir)

        # 1. Python 3.13
        py_file = tmp / "bench.py"
        py_file.write_text(PYTHON_BENCH_CODE)
        py_exe = str(Path.home() / ".venvs" / "formal-languages" / "bin" / "python3")
        py_runs = [run_cmd_with_rss([py_exe, str(py_file)]) for _ in range(runs)]

        # 2. OCaml 5.4.1 Bytecode (ocamlc) & Native (ocamlopt)
        ml_file = tmp / "bench.ml"
        ml_file.write_text(OCAML_BENCH_CODE)
        subprocess.run(["opam", "exec", "--", "ocamlc", "-I", "+unix", "unix.cma", str(ml_file), "-o", str(tmp / "bench_byte")], check=True)
        subprocess.run(["opam", "exec", "--", "ocamlopt", "-O3", "-I", "+unix", "unix.cmxa", str(ml_file), "-o", str(tmp / "bench_native")], check=True)
        ocaml_byte_runs = [run_cmd_with_rss([str(tmp / "bench_byte")]) for _ in range(runs)]
        ocaml_nat_runs = [run_cmd_with_rss([str(tmp / "bench_native")]) for _ in range(runs)]

        # 3. F# .NET 9 (dotnet fsi & compiled Release)
        fsx_file = tmp / "bench.fsx"
        fsx_file.write_text(FSHARP_BENCH_CODE)
        env = {
            **os.environ,
            "DOTNET_ROOT": "/opt/homebrew/opt/dotnet@9/libexec",
            "PATH": f"/opt/homebrew/bin:/opt/homebrew/opt/dotnet@9/libexec:{os.environ.get('PATH', '')}"
        }
        fsharp_runs = [run_cmd_with_rss(["dotnet", "fsi", "--optimize+", str(fsx_file)], env=env) for _ in range(runs)]

    for label, dataset in [
        ("Python 3.13 (CPython)", py_runs),
        ("OCaml 5.4 (Bytecode)", ocaml_byte_runs),
        ("OCaml 5.4 (Native)", ocaml_nat_runs),
        ("F# 9.0 (.NET 9 CoreCLR)", fsharp_runs),
    ]:
        metrics = {}
        for key in ["lexing_ms", "nfa2dfa_ms", "minimize_ms", "ast_diff_ms", "peak_rss_mb"]:
            vals = [d[key] for d in dataset]
            metrics[key] = {
                "mean": round(statistics.mean(vals), 3),
                "stdev": round(statistics.stdev(vals), 3) if len(vals) > 1 else 0.0,
                "median": round(statistics.median(vals), 3),
                "min": round(min(vals), 3),
                "max": round(max(vals), 3),
            }
        metrics["invariants"] = {
            "lexing_tokens": dataset[0]["lexing_tokens"],
            "nfa2dfa_states": dataset[0]["nfa2dfa_states"],
            "minimize_blocks": dataset[0]["minimize_blocks"],
            "ast_nodes": dataset[0]["ast_nodes"],
        }
        summary[label] = metrics

    out_file = DATA_DIR / "microbenchmarks.json"
    out_file.write_text(json.dumps(summary, indent=2))
    print(json.dumps(summary, indent=2))


if __name__ == "__main__":
    main()
