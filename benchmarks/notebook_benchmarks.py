#!/usr/bin/env python3
"""
Empirical Jupyter Notebook Macrobenchmark Suite for Chapter 5:
Measures:
  1. Kernel Cold-Start Latency & Baseline Memory Footprint (RSS)
     across ipykernel (Python 3.13), ocaml-jupyter (OCaml 5.4), and dotnet-interactive (F# 9.0).
  2. End-to-End Notebook Execution Time & Cell-Level Execution Breakdown
     across representative matched notebooks from Chapters 01 through 12.
All executions take place in an isolated temporary directory copy so the source
repositories remain completely untouched.
"""
import json
import os
import shutil
import statistics
import tempfile
import time
from pathlib import Path

import nbformat
import psutil
from jupyter_client.manager import KernelManager
from nbclient import NotebookClient

BENCH_DIR = Path(__file__).resolve().parent
REPO_ROOT = BENCH_DIR.parent
DATA_DIR = BENCH_DIR / "results"
DATA_DIR.mkdir(parents=True, exist_ok=True)

ENV = {
    **os.environ,
    "DOTNET_ROOT": "/opt/homebrew/opt/dotnet@9/libexec",
    "PATH": (
        f"{Path.home() / '.venvs/formal-languages/bin'}:"
        f"/Users/markschottle/.dotnet/tools:"
        f"/opt/homebrew/bin:"
        f"/opt/homebrew/opt/dotnet@9/libexec:"
        f"{Path.home() / '.opam/default/bin'}:"
        f"{os.environ.get('PATH', '')}"
    ),
}


def get_process_tree_rss_mb(pid: int) -> float:
    """Return total RSS in MB for a process and all its children."""
    try:
        parent = psutil.Process(pid)
        procs = [parent] + parent.children(recursive=True)
        total_bytes = 0
        for p in procs:
            try:
                total_bytes += p.memory_info().rss
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                pass
        return total_bytes / (1024.0 * 1024.0)
    except (psutil.NoSuchProcess, psutil.AccessDenied):
        return 0.0


def measure_kernel_cold_start(kernel_name: str, noop_code: str, runs: int = 3) -> dict:
    """Measure time to spawn kernel, establish ZeroMQ channels, and execute first trivial cell."""
    startup_times = []
    first_cell_times = []
    warm_cell_times = []
    idle_rss_list = []

    for _ in range(runs):
        t0 = time.perf_counter()
        km = KernelManager(kernel_name=kernel_name)
        km.start_kernel(env=ENV)
        kc = km.client()
        kc.start_channels()
        kc.wait_for_ready(timeout=60)
        t1 = time.perf_counter()
        startup_times.append((t1 - t0) * 1000.0)

        # First cell evaluation (includes JIT/toplevel warmup)
        t2 = time.perf_counter()
        kc.execute_interactive(noop_code, timeout=60)
        t3 = time.perf_counter()
        first_cell_times.append((t3 - t2) * 1000.0)

        # Warm cell evaluation (IPC + minimal eval)
        t4 = time.perf_counter()
        kc.execute_interactive(noop_code, timeout=60)
        t5 = time.perf_counter()
        warm_cell_times.append((t5 - t4) * 1000.0)

        # Measure kernel process tree RSS
        kernel_pid = km.provisioner.process.pid if hasattr(km, "provisioner") and km.provisioner else None
        if kernel_pid:
            idle_rss_list.append(get_process_tree_rss_mb(kernel_pid))

        kc.stop_channels()
        km.shutdown_kernel(now=True)

    return {
        "kernel_ready_ms": {
            "mean": round(statistics.mean(startup_times), 1),
            "stdev": round(statistics.stdev(startup_times), 1) if len(startup_times) > 1 else 0.0,
        },
        "first_cell_ms": {
            "mean": round(statistics.mean(first_cell_times), 1),
            "stdev": round(statistics.stdev(first_cell_times), 1) if len(first_cell_times) > 1 else 0.0,
        },
        "warm_cell_ipc_ms": {
            "mean": round(statistics.mean(warm_cell_times), 2),
            "stdev": round(statistics.stdev(warm_cell_times), 2) if len(warm_cell_times) > 1 else 0.0,
        },
        "total_cold_start_ms": {
            "mean": round(statistics.mean([a + b for a, b in zip(startup_times, first_cell_times)]), 1),
        },
        "idle_rss_mb": {
            "mean": round(statistics.mean(idle_rss_list), 1) if idle_rss_list else 0.0,
        },
    }


def export_fsharp_dependencies(fsharp_root: Path):
    """Export .fsx files from F# notebooks so dependent notebooks can #load them."""
    for path in sorted(fsharp_root.rglob("*.ipynb")):
        nb = json.loads(path.read_text(encoding="utf-8"))
        export = nb.get("metadata", {}).get("fsharp", {})
        if "export" not in export:
            continue
        filename = export["export"]
        module = export.get("module")
        cells = [
            "".join(c["source"])
            for c in nb["cells"]
            if c["cell_type"] == "code" and c.get("metadata", {}).get("fsharp", {}).get("export", True)
        ]
        header = f"// Generated from {path.name}\n"
        if module:
            header += f"module {module}\n\n"
        (path.parent / filename).write_text(header + "\n\n".join(cells) + "\n", encoding="utf-8")


def sanitize_notebook_for_headless(nb_path: Path, lang: str):
    """Disable interactive browser/viewer popups (e.g. 'open ...') during headless benchmarking."""
    nb = nbformat.read(str(nb_path), as_version=4)
    code_cells = 0
    for cell in nb.cells:
        if cell.cell_type == "code":
            code_cells += 1
            src = cell.source
            if lang == "ocaml" and 'Sys.command "open ' in src:
                cell.source = "()"
    nbformat.write(nb, str(nb_path))
    return code_cells


def execute_and_time_notebook(nb_path: Path, kernel_name: str, lang: str) -> dict:
    """Execute a notebook in a fresh kernel and measure total time, pure cell execution time, and cell count."""
    code_cells = sanitize_notebook_for_headless(nb_path, lang)
    nb = nbformat.read(str(nb_path), as_version=4)

    cell_durations = []

    class TimedNotebookClient(NotebookClient):
        def execute_cell(self, cell, cell_index, execution_count=None, store_history=True):
            if cell.cell_type == "code" and cell.source.strip():
                t0 = time.perf_counter()
                res = super().execute_cell(cell, cell_index, execution_count, store_history)
                t1 = time.perf_counter()
                cell_durations.append((t1 - t0) * 1000.0)
                return res
            return super().execute_cell(cell, cell_index, execution_count, store_history)

    client = TimedNotebookClient(
        nb,
        timeout=180,
        kernel_name=kernel_name,
        allow_errors=True,
        resources={"metadata": {"path": str(nb_path.parent)}},
    )

    t_start = time.perf_counter()
    with client.setup_kernel(env=ENV):
        for idx, cell in enumerate(nb.cells):
            client.execute_cell(cell, idx)
        # Sample peak RSS right before kernel teardown
        kernel_pid = (
            client.km.provisioner.process.pid
            if client.km and hasattr(client.km, "provisioner") and client.km.provisioner
            else None
        )
        rss_mb = get_process_tree_rss_mb(kernel_pid) if kernel_pid else 0.0
    t_end = time.perf_counter()

    total_ms = (t_end - t_start) * 1000.0
    cells_ms = sum(cell_durations)
    return {
        "total_wall_ms": round(total_ms, 1),
        "pure_cells_ms": round(cells_ms, 1),
        "startup_overhead_ms": round(total_ms - cells_ms, 1),
        "code_cells": code_cells,
        "rss_mb": round(rss_mb, 1),
    }


BENCHMARK_NOTEBOOKS = [
    ("Ch 03: Lexical Scanning", "Chapter-03/01-RegExp-Scanner.ipynb", "Chapter-03/01-Ply-Scanning-Example.ipynb"),
    ("Ch 03: RegExp Tutorial", "Chapter-03/02-Regexp-Tutorial.ipynb", "Chapter-03/02-Regexp-Tutorial.ipynb"),
    ("Ch 03: Exam Evaluation", "Chapter-03/03-Exam-Evaluation.ipynb", "Chapter-03/03-Exam-Evaluation.ipynb"),
    ("Ch 04-05: NFA to DFA", "Chapter-04-05/01-NFA-2-DFA.ipynb", "Chapter-04-05/01-NFA-2-DFA.ipynb"),
    ("Ch 04-05: RegExp to NFA", "Chapter-04-05/03-Regexp-2-NFA.ipynb", "Chapter-04-05/03-Regexp-2-NFA.ipynb"),
    ("Ch 04-05: DFA to RegExp", "Chapter-04-05/05-DFA-2-RegExp.ipynb", "Chapter-04-05/05-DFA-2-RegExp.ipynb"),
    ("Ch 04-05: DFA Minimization", "Chapter-04-05/07-Minimize.ipynb", "Chapter-04-05/07-Minimize.ipynb"),
    ("Ch 04-05: Equivalence Check", "Chapter-04-05/09-Equivalence.ipynb", "Chapter-04-05/09-Equivalence.ipynb"),
    ("Ch 06: Top-Down Parser", "Chapter-06/1-Top-Down-Parser.ipynb", "Chapter-06/1-Top-Down-Parser.ipynb"),
    ("Ch 06: EBNF Parser", "Chapter-06/2-EBNF-Parser.ipynb", "Chapter-06/2-EBNF-Parser.ipynb"),
    ("Ch 07: Symbolic Differentiator", "Chapter-07/02-Differentiator.ipynb", "Chapter-07/02-Differentiator.ipynb"),
    ("Ch 07/08: Interpreter", "Chapter-08/03-Interpreter.ipynb", "Chapter-07/03-Interpreter.ipynb"),
    ("Ch 08/09: Earley Parser", "Chapter-09/Earley-Parser.ipynb", "Chapter-08/Earley-Parser.ipynb"),
    ("Ch 09/10: SLR Table Generator", "Chapter-10/SLR-Table-Generator.ipynb", "Chapter-09/SLR-Table-Generator.ipynb"),
    ("Ch 10/11: Parser Conflicts", "Chapter-11/01-Conflicts.ipynb", "Chapter-10/01-Conflicts.ipynb"),
    ("Ch 12/13: Integer-C Compiler", "Chapter-13/Compiler.ipynb", "Chapter-12/Compiler.ipynb"),
]


def main():
    print("1. Measuring Kernel Cold-Start & IPC Latencies...")
    kernel_metrics = {
        "Python 3.13 (ipykernel)": measure_kernel_cold_start("python3", "1 + 1"),
        "OCaml 5.4 (ocaml-jupyter)": measure_kernel_cold_start("ocaml-jupyter", "1 + 1;;"),
        "F# 9.0 (.net-fsharp)": measure_kernel_cold_start(".net-fsharp", "1 + 1"),
    }
    print(json.dumps(kernel_metrics, indent=2))

    print("\n2. Running End-to-End Notebook Macrobenchmarks in isolated workspace...")
    notebook_results = {}
    with tempfile.TemporaryDirectory() as tmpdir:
        tmp = Path(tmpdir)
        py_root = tmp / "python"
        ml_root = tmp / "ocaml"
        fs_root = tmp / "f#"
        shutil.copytree(REPO_ROOT / "python", py_root)
        shutil.copytree(REPO_ROOT / "ocaml", ml_root)
        shutil.copytree(REPO_ROOT / "f#", fs_root)
        export_fsharp_dependencies(fs_root)

        for label, py_ml_path, fs_path in BENCHMARK_NOTEBOOKS:
            print(f"  Benchmarking {label} ({py_ml_path} / {fs_path})...")
            notebook_results[label] = {
                "py_ml_path": py_ml_path,
                "fs_path": fs_path,
                "python": execute_and_time_notebook(py_root / py_ml_path, "python3", "python"),
                "ocaml": execute_and_time_notebook(ml_root / py_ml_path, "ocaml-jupyter", "ocaml"),
                "fsharp": execute_and_time_notebook(fs_root / fs_path, ".net-fsharp", "fsharp"),
            }
            print(
                f"    -> Python: {notebook_results[label]['python']['total_wall_ms']} ms "
                f"(cells: {notebook_results[label]['python']['pure_cells_ms']} ms) | "
                f"OCaml: {notebook_results[label]['ocaml']['total_wall_ms']} ms "
                f"(cells: {notebook_results[label]['ocaml']['pure_cells_ms']} ms) | "
                f"F#: {notebook_results[label]['fsharp']['total_wall_ms']} ms "
                f"(cells: {notebook_results[label]['fsharp']['pure_cells_ms']} ms)"
            )

    full_output = {
        "kernel_cold_start": kernel_metrics,
        "notebooks": notebook_results,
    }
    out_file = DATA_DIR / "notebook_benchmarks.json"
    out_file.write_text(json.dumps(full_output, indent=2))
    print(f"\nSaved complete results to {out_file}")


if __name__ == "__main__":
    main()
