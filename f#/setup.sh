#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
: "${DOTNET:=dotnet}"
: "${PYTHON:=python}"
"$DOTNET" tool restore
"$DOTNET" tool run dotnet-interactive jupyter install
"$PYTHON" FSharp/tools/build.py --dotnet "$DOTNET"
"$PYTHON" FSharp/tools/notebooks.py check
