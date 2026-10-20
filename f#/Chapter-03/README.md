# Scanning in F#

Run `dotnet build Scanning/Scanning.fsproj` before executing these notebooks, or use the repository F# setup script. FsLexYacc 11.3.0 generates the four scanners from the `.fsl` files; the notebook cells show the specifications and the transformations they teach. The SDK must be on `PATH`; with only a newer runtime installed, `DOTNET_ROLL_FORWARD=Major` allows the generator's .NET 6 tool to run.

The regular-expression tutorial uses the .NET regex engine. Exam data is a typed record list, and its grade plot is a portable SVG. The formerly unfinished LaTeX exercise now renders its documented subset to native MathML, with no MathJax/CDN dependency. The HTML example deliberately retains the small teaching subset rather than promising a full DOM parser. The harvester defaults to an offline page graph and exposes an optional `HttpClient` boundary.

`exam.txt` is a deterministic Advanced Witchcraft fixture added because the original notebook references a missing file. The source Python notebooks are preserved for comparison.
