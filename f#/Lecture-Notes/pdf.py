#!/usr/bin/env python3
"""Compile the complete edition using an existing TeX installation, without shell escape."""
from __future__ import annotations
import argparse
from pathlib import Path
import shutil
import subprocess
import build as edition


def run(command: list[str], output: Path, log_name: str) -> str:
    result = subprocess.run(command, cwd=output, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, text=True, errors='replace')
    (output / log_name).write_text(result.stdout)
    if result.returncode:
        raise RuntimeError(f'{command[0]} failed; see {output / log_name}\n' +
                           '\n'.join(result.stdout.splitlines()[-20:]))
    return result.stdout


def compile_pdf(output: Path, roots: list[Path]) -> Path:
    missing = [tool for tool in ['pdflatex', 'bibtex', 'makeindex', 'epstopdf', 'gs']
               if shutil.which(tool) is None]
    if missing:
        raise RuntimeError('An existing TeX/Ghostscript installation is required; missing: ' + ', '.join(missing))
    edition.build(output, roots)
    text = '\n'.join(path.read_text() for path in output.glob('*.tex'))
    eps_files = sorted(path for path in (edition.ORIGINALS / 'Abbildungen').glob('*.eps')
                       if str(path) in text or str(path.with_suffix('')) in text)
    for source in eps_files:
        target = output / 'converted' / (source.stem + '-eps-converted-to.pdf')
        if not target.exists() or target.stat().st_mtime < source.stat().st_mtime:
            run(['epstopdf', '--outfile=' + str(target), str(source)], output,
                'eps-' + source.stem + '.log')
    tex = ['pdflatex', '-no-shell-escape', '-halt-on-error', '-interaction=nonstopmode',
           'formal-languages-fsharp.tex']
    pdf = output / 'formal-languages-fsharp.pdf'
    # A failed compile must never be mistaken for an earlier successful artifact.
    pdf.unlink(missing_ok=True)
    run(tex, output, 'pdf-pass1.log')
    run(['bibtex', 'formal-languages-fsharp'], output, 'pdf-bibliography.log')
    run(['makeindex', 'formal-languages-fsharp.idx'], output, 'pdf-index.log')
    run(tex, output, 'pdf-pass2.log')
    final = run(tex, output, 'pdf-pass3.log')
    if 'There were undefined references' in final or 'Missing character:' in final:
        raise RuntimeError('The PDF contains unresolved references or missing glyphs; see pdf-pass3.log')
    if not pdf.is_file():
        raise RuntimeError('TeX reported success without creating the PDF')
    return pdf


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, default=edition.HERE / '_build')
    parser.add_argument('--source-root', action='append', type=Path, default=[])
    args = parser.parse_args()
    print(compile_pdf(args.output.resolve(), [edition.REPOSITORY / 'FSharp', *args.source_root]))


if __name__ == '__main__':
    main()
