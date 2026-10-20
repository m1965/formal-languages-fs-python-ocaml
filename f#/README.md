# Formal Languages in F#

This edition ports the course's Python notebooks to F# so students can compare readability, type inference and explicit algebraic data types. The original notebooks and lecture notes remain the comparison baseline. FsLexYacc replaces PLY; .NET libraries replace Python runtime dependencies.

All **56 tracked source notebooks** have a counterpart: 50 from `Python`, five from `ANTLR4-Python` and one from `Exercises`. Their relative names are retained under `FSharp`, `FSharp/Legacy-ANTLR` and `FSharp/Exercises`. The [full lecture edition](Lecture-Notes/README.md) covers all 14 active textbook chapters and takes its 40 code listings directly from these notebooks and parser projects. Supporting source-language examples, figures and JVM lessons accompany the relevant chapters.

## Start a Jupyter session

Install a **.NET 9 SDK** from [Microsoft](https://dotnet.microsoft.com/download/dotnet/9.0), and Anaconda or Miniconda. Then, from the repository root:

```sh
conda env create -f FSharp/environment.yml
conda activate formal-languages-fsharp
dotnet tool restore
dotnet tool run dotnet-interactive jupyter install
python FSharp/tools/build.py
jupyter lab
```

Select **.NET (F#)** for each notebook. The tool manifest pins .NET Interactive 1.0.632301, which targets .NET 9. FsLexYacc projects pin version 11.3.0. The build script enables major runtime roll-forward for the generator tools; students do not need to install their older .NET 6 runtime. The SDK itself is selected by `global.json`.

After activating the Conda environment, `bash FSharp/setup.sh` also restores the kernel, builds the edition and checks coverage in one step.

`graphviz` is installed by the Conda environment and supplies `dot` for diagrams. Java is included for the compiler's JVM target. Python only hosts Jupyter and the export/validation tooling; all teaching algorithms execute as F#/.NET code.

The setup follows [.NET Interactive's Jupyter workflow](https://github.com/dotnet/interactive/blob/main/docs/NotebookswithJupyter.md) and [FsLexYacc's build integration](https://fsprojects.github.io/FsLexYacc/).

## Notebook dependencies

Run `python FSharp/tools/build.py` after editing algorithm notebooks or grammar specifications. Reusable algorithm cells are exported to adjacent `.fsx` files using notebook `metadata.fsharp.export` and `metadata.fsharp.module`. Demonstration cells carry `metadata.fsharp.export: false`. Generated scripts and parser build outputs are ignored; edit the notebook or `.fsl`/`.fsy` source, not generated files.

Run each notebook from top to bottom with a fresh kernel. File paths in cells are relative to the notebook directory. Examples use local course data; any interactive input or network access is offered as an explicit optional call so a full notebook run can finish unattended.

## Check the edition

```sh
python FSharp/tools/build.py
python FSharp/tools/notebooks.py check
python FSharp/tools/notebooks.py fsi
python FSharp/tools/notebooks.py jupyter
```

`check` verifies complete coverage against the tracked source notebook inventory, kernel metadata, cell IDs, cleared outputs, and removal of Python/shell cells. `fsi` runs code through the F# compiler and checks deterministic examples using explicit exceptions, which stay active in both execution environments. `jupyter` additionally runs every cell in the actual .NET F# kernel, including rich display cells; executed notebooks are saved under ignored `FSharp/.validation/` for inspection.

Use `--match Chapter-07` to run one chapter, or `--dotnet /path/to/dotnet` with the build and execution scripts. Grammar conflict examples intentionally produce generator warnings: their notebooks explain and test those conflicts rather than hiding them.

The [behavioral suites](Tests/README.md) compare independent algorithms and exercise numeric semantics, malformed input and compiler output. The GitHub workflow runs the build, all notebooks in both environments, these suites and exact JVM output checks. A local run and a GitHub workflow run are separate verification results.

The [JVM comparison lesson](Chapter-11/README.md) and [lecture calculator](ANTLR/README.md) also supply standalone F# scripts. The workflow runs these supporting examples, including the original multi-statement calculator program with `sqrt`.

## Evaluation

Passing execution checks establishes runnable examples, not equal readability. The [comparison protocol](Lecture-Notes/EVALUATION.md) uses paired tasks: predict a result, explain a transition, modify a grammar and diagnose an error. Record student completion time, mistakes, explanation quality and comments about type errors. Students should receive a short introduction to F# syntax, `Set`, `Map`, pattern matching and discriminated unions before evaluating the edition.
