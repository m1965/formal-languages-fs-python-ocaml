#!/usr/bin/env python3
"""
Static Corpus & Structural Analysis of the Python, OCaml, and F# Notebook Editions:
Collects cell counts, code lines (SLOC), comment lines, and directive counts
across all notebooks to contextualize execution performance in Chapter 5.
"""
import json
from pathlib import Path

BENCH_DIR = Path(__file__).resolve().parent
REPO_ROOT = BENCH_DIR.parent
DATA_DIR = BENCH_DIR / "results"
DATA_DIR.mkdir(parents=True, exist_ok=True)


def analyze_edition(root: Path, lang: str) -> dict:
    notebooks = [
        p for p in sorted(root.rglob("*.ipynb"))
        if ".ipynb_checkpoints" not in p.parts and ".validation" not in p.parts
    ]
    total_nb = len(notebooks)
    total_code_cells = 0
    total_md_cells = 0
    total_sloc = 0
    total_Blank_or_comment = 0

    for nb_path in notebooks:
        nb = json.loads(nb_path.read_text(encoding="utf-8"))
        for cell in nb.get("cells", []):
            ctype = cell.get("cell_type")
            if ctype == "markdown":
                total_md_cells += 1
            elif ctype == "code":
                total_code_cells += 1
                lines = "".join(cell.get("source", [])).splitlines()
                for line in lines:
                    s = line.strip()
                    if not s:
                        total_Blank_or_comment += 1
                    elif lang == "python" and s.startswith("#"):
                        total_Blank_or_comment += 1
                    elif lang == "fsharp" and s.startswith("//") and not s.startswith("#"):
                        total_Blank_or_comment += 1
                    elif lang == "ocaml" and s.startswith("(*") and s.endswith("*)"):
                        total_Blank_or_comment += 1
                    else:
                        total_sloc += 1

    return {
        "notebooks": total_nb,
        "code_cells": total_code_cells,
        "markdown_cells": total_md_cells,
        "sloc": total_sloc,
        "avg_sloc_per_nb": round(total_sloc / total_nb, 1) if total_nb else 0.0,
        "avg_cells_per_nb": round(total_code_cells / total_nb, 1) if total_nb else 0.0,
    }


def main():
    stats = {
        "Python": analyze_edition(REPO_ROOT / "python", "python"),
        "OCaml": analyze_edition(REPO_ROOT / "ocaml", "ocaml"),
        "FSharp": analyze_edition(REPO_ROOT / "f#", "fsharp"),
    }
    out = DATA_DIR / "corpus_metrics.json"
    out.write_text(json.dumps(stats, indent=2))
    print(json.dumps(stats, indent=2))


if __name__ == "__main__":
    main()
