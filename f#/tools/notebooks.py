#!/usr/bin/env python3
"""Export canonical teaching cells and validate the complete F# notebook edition."""
import argparse
import json
import os
import shutil
from pathlib import Path
import re
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[2]
EDITION = ROOT / 'FSharp'


def notebooks():
    return sorted(path for path in EDITION.rglob('*.ipynb')
                  if '.validation' not in path.relative_to(EDITION).parts)


def read_notebook(path):
    with path.open(encoding='utf-8') as source:
        return json.load(source)


def code_cells(notebook):
    return (cell for cell in notebook['cells'] if cell['cell_type'] == 'code')


def cell_source(cell):
    return ''.join(cell['source'])


def export_dependencies():
    """Notebook algorithms are the source of truth; generated scripts are ignored."""
    for path in notebooks():
        notebook = read_notebook(path)
        export = notebook['metadata'].get('fsharp', {})
        if 'export' not in export:
            continue
        filename = export['export']
        if Path(filename).name != filename or not filename.endswith('.fsx'):
            raise ValueError(f'{path}: export must name an adjacent .fsx file')
        module = export.get('module')
        if module and not re.fullmatch(r'[A-Z][A-Za-z0-9_]*', module):
            raise ValueError(f'{path}: invalid module name {module}')
        cells = [cell_source(cell) for cell in code_cells(notebook)
                 if cell.get('metadata', {}).get('fsharp', {}).get('export', True)]
        header = f'// Generated from {path.name}; edit the notebook.\n'
        if module:
            header += f'module {module}\n\n'
        (path.parent / filename).write_text(header + '\n\n'.join(cells) + '\n', encoding='utf-8')


def target_path(source):
    if source.startswith('Python/'):
        return EDITION / source.removeprefix('Python/')
    if source.startswith('ANTLR4-Python/'):
        return EDITION / 'Legacy-ANTLR' / source.removeprefix('ANTLR4-Python/')
    return EDITION / source


def dotnet_environment(command):
    executable = shutil.which(command)
    if executable is None:
        raise ValueError(f'Cannot find .NET SDK executable: {command}')
    directory = str(Path(executable).resolve().parent)
    return {**os.environ, 'PATH': directory + os.pathsep + os.environ.get('PATH', ''),
            'DOTNET_ROOT': directory, 'DOTNET_ROLL_FORWARD': 'Major'}


def check_structure():
    sources = subprocess.check_output(['git', 'ls-files', '*.ipynb'], cwd=ROOT, text=True).splitlines()
    sources = [source for source in sources if not source.startswith('FSharp/')]
    missing = [str(target_path(source).relative_to(ROOT)) for source in sources
               if not target_path(source).is_file()]
    if missing:
        raise ValueError('Missing ports:\n' + '\n'.join(missing))
    for path in notebooks():
        notebook = read_notebook(path)
        if notebook.get('nbformat') != 4:
            raise ValueError(f'{path}: expected notebook format 4')
        if notebook['metadata'].get('kernelspec', {}).get('name') != '.net-fsharp':
            raise ValueError(f'{path}: expected .net-fsharp kernel')
        identifiers = [cell.get('id', '') for cell in notebook['cells']]
        if len(set(identifiers)) != len(identifiers) or any(not re.fullmatch(r'[A-Za-z0-9_-]{1,64}', identifier) for identifier in identifiers):
            raise ValueError(f'{path}: missing or duplicate notebook cell IDs')
        for cell in code_cells(notebook):
            source = cell_source(cell)
            # Grammar specifications may contain %token/%nonassoc inside F# strings.
            outside_strings = re.sub(r'(?s)""".*?"""|@"(?:""|[^"])*"|"(?:\\.|[^"\\])*"', '""', source)
            if re.search(r'^\s*(%[A-Za-z]|![a-z]|import |from .* import |def |class )', outside_strings, re.M):
                raise ValueError(f'{path}: Python or shell command remains in F# cell')
            if re.search(r'(?m)^\s*assert(?:\s|\()', outside_strings):
                raise ValueError(f'{path}: use an always-active check instead of assert')
            if cell.get('outputs') or cell.get('execution_count') is not None:
                raise ValueError(f'{path}: clear stale output before committing')
    print(f'Coverage: {len(sources)} source notebooks, {len(notebooks())} F# notebooks; structure valid.')


def execute_fsi(dotnet, selected):
    failed = []
    for path in selected:
        notebook = read_notebook(path)
        # Keep #load and file I/O relative to the original notebook, just as in Jupyter.
        with tempfile.NamedTemporaryFile(mode='w', prefix='.validate-', suffix='.fsx',
                                         dir=path.parent, encoding='utf-8', delete=False) as script:
            script.write('\n\n;;\n\n'.join(cell_source(cell) for cell in code_cells(notebook)
                                  if cell.get('metadata', {}).get('fsharp', {}).get('fsi', True)))
            filename = script.name
        try:
            result = subprocess.run([dotnet, 'fsi', '--define:DEBUG', '--exec', filename], cwd=path.parent,
                                    capture_output=True, text=True, timeout=180,
                                    env=dotnet_environment(dotnet))
            if result.returncode:
                failed.append(str(path.relative_to(ROOT)))
                print(f'FAIL {failed[-1]}\n{result.stdout}\n{result.stderr}', flush=True)
            else:
                print(f'PASS {path.relative_to(ROOT)}', flush=True)
        except subprocess.TimeoutExpired:
            failed.append(str(path.relative_to(ROOT)))
            print(f'TIMEOUT {failed[-1]}', flush=True)
        finally:
            Path(filename).unlink(missing_ok=True)
    if failed:
        raise ValueError(f'{len(failed)} notebooks failed')
    print(f'Executed {len(selected)} notebooks with F# Interactive.')


def execute_jupyter(dotnet, selected):
    from nbclient import NotebookClient
    import nbformat
    failed = []
    for path in selected:
        notebook = nbformat.read(path, as_version=4)
        output = EDITION / '.validation' / path.relative_to(EDITION)
        output.parent.mkdir(parents=True, exist_ok=True)
        try:
            NotebookClient(notebook, timeout=180, kernel_name='.net-fsharp',
                           resources={'metadata': {'path': str(path.parent)}}).execute(env=dotnet_environment(dotnet))
            print(f'KERNEL PASS {path.relative_to(ROOT)}', flush=True)
        except Exception as error:
            failed.append(str(path.relative_to(ROOT)))
            details = [item.get('evalue', '') for cell in notebook.cells
                       for item in cell.get('outputs', []) if item.get('output_type') == 'error']
            print(f'KERNEL FAIL {failed[-1]}\n{str(error)[:1500]}\n' + '\n'.join(details)[:2500], flush=True)
        finally:
            nbformat.write(notebook, output)
    if failed:
        raise ValueError(f'{len(failed)} notebooks failed in the .NET F# kernel')
    print(f'Executed {len(selected)} notebooks in the .NET F# Jupyter kernel.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('action', choices=['export', 'check', 'fsi', 'jupyter'])
    parser.add_argument('--dotnet', default='dotnet')
    parser.add_argument('--match', default='', help='Only execute paths containing this substring')
    args = parser.parse_args()
    if args.action != 'export':
        check_structure()
    export_dependencies()
    selected = [path for path in notebooks() if args.match in str(path.relative_to(EDITION))]
    if args.action in ('fsi', 'jupyter') and not selected:
        raise ValueError(f'No notebook paths match {args.match!r}')
    if args.action == 'fsi':
        execute_fsi(args.dotnet, selected)
    elif args.action == 'jupyter':
        execute_jupyter(args.dotnet, selected)


if __name__ == '__main__':
    try:
        main()
    except (ValueError, subprocess.CalledProcessError) as error:
        sys.exit(str(error))
