"""
larkita_benchmarks.py: Benchmarks of Larkita and its OCaml and F# ports for Chapter 4.

The grammar is the calculator grammar of the course (python/Chapter-07/calc.lark, 11 productions),
the input is the same as in parser_benchmarks.py: "1 + 1 + ... ;" with 100 to 10,000 tokens.
Measured are the generation of the canonical LR(1) parser (grammar loading, automaton, output
file) and the parsing throughput (scanning, parsing, and building the abstract syntax tree).

    python3 benchmarks/larkita_benchmarks.py PYTHON

PYTHON is a Python 3.12+ interpreter with Larkita installed (pip install larkita).  The OCaml port
is measured twice: compiled natively with ocamlopt (comparable to the Menhir measurement of
parser_benchmarks.py) and in the bytecode toplevel that the notebooks use.  The F# port runs in
F# Interactive, like the F# notebooks.  The results are written to results/larkita_benchmarks.json.
"""

import json
import re
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
RESULTS = ROOT / "benchmarks" / "results" / "larkita_benchmarks.json"
GRAMMAR = (ROOT / "python" / "Chapter-07" / "calc.lark").read_text()
TOKEN_COUNTS = [100, 500, 1000, 2500, 5000, 10000]
REPETITIONS = 20
GENERATIONS = 20


def parse_results(output: str) -> dict:
    "lines GEN:<ms> and RESULT:<tokens>:<ms> -> dictionary"
    result = {"parse_throughput": {}}
    for line in output.splitlines():
        if line.startswith("GEN:"):
            result["table_generation_ms"] = round(float(line[4:]), 2)
        elif line.startswith("RESULT:"):
            _, n, ms = line.split(":")
            ms = float(ms)
            result["parse_throughput"][n] = {"mean_ms": round(ms, 3), "tokens_per_sec": round(int(n) / (ms / 1000.0), 1)}
    return result


PYTHON_SCRIPT = r'''
import sys, time, tempfile, os
from larkita import Larkita
grammar = open(sys.argv[1]).read()
out = os.path.join(tempfile.mkdtemp(), "calc.out")
times = []
for _ in range({gens}):
    t0 = time.perf_counter(); Larkita(grammar, out_file=out); times.append(time.perf_counter() - t0)
print("GEN:%.4f" % (1000 * sum(times[2:]) / len(times[2:])))
parser = Larkita(grammar, out_file=out)
for n in {counts}:
    expr = " + ".join(["1"] * (n // 2 + 1)) + ";"
    parser.parse(expr)
    t0 = time.perf_counter()
    for _ in range({reps}):
        parser.parse(expr)
    print("RESULT:%d:%.4f" % (n, 1000 * (time.perf_counter() - t0) / {reps}))
'''


def benchmark_python(python: str) -> dict:
    print("[-] Python Larkita ...")
    with tempfile.TemporaryDirectory() as tmp:
        script = Path(tmp) / "bench.py"
        script.write_text(PYTHON_SCRIPT.format(gens=GENERATIONS, counts=TOKEN_COUNTS, reps=REPETITIONS))
        grammar = Path(tmp) / "calc.lark"
        grammar.write_text(GRAMMAR)
        res = subprocess.run([python, str(script), str(grammar)], capture_output=True, text=True, check=True)
    version = subprocess.run([python, "-c", "import larkita, sys; print(larkita.__version__, sys.version.split()[0])"],
                             capture_output=True, text=True).stdout.split()
    return dict(parse_results(res.stdout), larkita=version[0], python=version[1])


OCAML_MAIN = r'''
let grammar = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all
let out = Filename.temp_file "calc" ".out"
let () =
  let times = List.init {gens} (fun _ ->
    let t0 = Unix.gettimeofday () in
    ignore (Larkita.create ~out_file:out grammar);
    Unix.gettimeofday () -. t0) in
  let times = List.tl (List.tl times) in
  Printf.printf "GEN:%.4f\n%!" (1000. *. List.fold_left (+.) 0. times /. float_of_int (List.length times));
  let parser = Larkita.create ~out_file:out grammar in
  List.iter (fun n ->
    let expr = String.concat " + " (List.init (n / 2 + 1) (fun _ -> "1")) ^ ";" in
    ignore (Larkita.parse parser expr);
    let t0 = Unix.gettimeofday () in
    for _ = 1 to {reps} do ignore (Larkita.parse parser expr) done;
    Printf.printf "RESULT:%d:%.4f\n%!" n (1000. *. (Unix.gettimeofday () -. t0) /. float_of_int {reps}))
    [ {counts} ]
'''


def ocaml_library_source() -> str:
    """The OCaml port as one compilable file: the toplevel directives are removed, and the public
    module of larkita.ml is kept without the code that loads the other parts into the toplevel."""
    lib = ROOT / "ocaml" / "lib"
    parts = []
    for name in ["larkita_base", "larkita_regex", "larkita_lexer", "larkita_lr", "larkita_report", "larkita_grammar"]:
        text = (lib / f"{name}.ml").read_text()
        parts.append("\n".join(line for line in text.splitlines() if not line.startswith("#")))
    main = (lib / "larkita.ml").read_text()
    main = main[main.index("type ast = Larkita_base.ast"):main.index("let () =\n  Printexc.register_printer")]
    grammars = (lib / "grammars").resolve()
    parts.append(f'let () = Larkita_grammar.grammars_dir := {json.dumps(str(grammars))}\n' + main)
    return "\n\n".join(parts)


def benchmark_ocaml_native() -> dict:
    print("[-] OCaml port of Larkita (native) ...")
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        (tmp / "larkita_lib.ml").write_text(ocaml_library_source())
        (tmp / "main.ml").write_text("open Larkita_lib\n" + OCAML_MAIN.format(gens=GENERATIONS, reps=REPETITIONS, counts="; ".join(map(str, TOKEN_COUNTS))))
        (tmp / "calc.lark").write_text(GRAMMAR)
        subprocess.run(["ocamlfind", "ocamlopt", "-O3", "-package", "pcre2,uucp,unix", "-linkpkg",
                        "larkita_lib.ml", "main.ml", "-o", "bench"], cwd=tmp, check=True, capture_output=True, text=True)
        res = subprocess.run(["./bench", "calc.lark"], cwd=tmp, capture_output=True, text=True, check=True)
    version = subprocess.run(["ocamlfind", "ocamlopt", "-version"], capture_output=True, text=True).stdout.strip()
    return dict(parse_results(res.stdout), ocaml=version)


def benchmark_ocaml_toplevel() -> dict:
    print("[-] OCaml port of Larkita (bytecode toplevel, as in the notebooks) ...")
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        script = '#use "topfind";;\n#require "unix";;\n#use ' + json.dumps(str(ROOT / "ocaml" / "lib" / "larkita.ml")) + ';;\n'
        script += OCAML_MAIN.format(gens=GENERATIONS, reps=REPETITIONS, counts="; ".join(map(str, TOKEN_COUNTS)))
        (tmp / "bench.ml").write_text(script)
        (tmp / "calc.lark").write_text(GRAMMAR)
        res = subprocess.run(["ocaml", "bench.ml", "calc.lark"], cwd=tmp, capture_output=True, text=True, check=True)
    return parse_results(res.stdout)


FSHARP_SCRIPT = r'''
#load {larkita}
open Larkita
let grammar = System.IO.File.ReadAllText "calc.lark"
let out = System.IO.Path.Combine(System.IO.Path.GetTempPath(), "calc-bench.out")
let watch = System.Diagnostics.Stopwatch()
let times =
    [ for _ in 1 .. {gens} ->
        watch.Restart()
        Larkita.Create(grammar, outFile = out) |> ignore
        watch.Elapsed.TotalMilliseconds ]
printfn "GEN:%.4f" (List.average (List.skip 2 times))
let parser = Larkita.Create(grammar, outFile = out)
for n in [ {counts} ] do
    let expr = String.concat " + " (List.replicate (n / 2 + 1) "1") + ";"
    parse parser expr |> ignore
    watch.Restart()
    for _ in 1 .. {reps} do parse parser expr |> ignore
    printfn "RESULT:%d:%.4f" n (watch.Elapsed.TotalMilliseconds / float {reps})
'''


def benchmark_fsharp() -> dict:
    print("[-] F# port of Larkita (F# Interactive) ...")
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        larkita = json.dumps(str(ROOT / "f#" / "Support" / "Larkita" / "Larkita.fsx"))
        (tmp / "bench.fsx").write_text(FSHARP_SCRIPT.replace("{larkita}", larkita).replace("{gens}", str(GENERATIONS))
                                       .replace("{reps}", str(REPETITIONS)).replace("{counts}", "; ".join(map(str, TOKEN_COUNTS))))
        (tmp / "calc.lark").write_text(GRAMMAR)
        res = subprocess.run(["dotnet", "fsi", "--optimize+", "bench.fsx"], cwd=tmp, capture_output=True, text=True, check=True)
    version = subprocess.run(["dotnet", "--version"], capture_output=True, text=True).stdout.strip()
    return dict(parse_results(res.stdout), dotnet=version)


def main():
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    results = {
        "metadata": {"date": time.strftime("%Y-%m-%d %H:%M:%S"), "token_counts": TOKEN_COUNTS,
                     "repetitions": REPETITIONS, "grammar": "python/Chapter-07/calc.lark (11 productions)"},
        "python_larkita": benchmark_python(sys.argv[1]),
        "ocaml_larkita_native": benchmark_ocaml_native(),
        "ocaml_larkita_toplevel": benchmark_ocaml_toplevel(),
        "fsharp_larkita": benchmark_fsharp(),
    }
    RESULTS.write_text(json.dumps(results, indent=1) + "\n")
    for name, r in results.items():
        if name == "metadata":
            continue
        print(f"{name:24s} generation {r['table_generation_ms']:8.2f} ms  " +
              "  ".join(f"{n}: {v['tokens_per_sec']:,.0f}" for n, v in r["parse_throughput"].items()))


if __name__ == "__main__":
    main()
