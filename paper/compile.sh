#!/usr/bin/env bash
# Compile the monograph (main.tex -> main.pdf) with latexmk.
#
# Usage:
#   ./compile.sh          build (incremental)
#   ./compile.sh force    force a full rebuild
#   ./compile.sh clean    remove auxiliary files (keeps main.pdf)
#   ./compile.sh watch    rebuild continuously on changes
set -euo pipefail

cd "$(dirname "$0")"

LATEXMK=(latexmk -pdf -interaction=nonstopmode -halt-on-error -file-line-error)

case "${1:-build}" in
  build) "${LATEXMK[@]}" main.tex ;;
  force) "${LATEXMK[@]}" -g main.tex ;;
  clean) latexmk -c main.tex ;;
  watch) "${LATEXMK[@]}" -pvc main.tex ;;
  *)
    echo "Usage: $0 [build|force|clean|watch]" >&2
    exit 1
    ;;
esac
