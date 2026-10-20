"""Execute all OCaml notebooks without a browser and report the cells that fail.

Usage (from the directory OCaml):   python3 tools/check.py [notebook ...]

Since the OCaml kernel type checks every cell before executing it, this is also the type check of
the notebooks.  The notebooks are executed in a temporary copy of the directory OCaml, so that no
file is changed, and the cells that open a file in a viewer (`Sys.command "open ..."`) are skipped.
A cell fails if it raises an error although it is not tagged with "raises-exception", or if it is
tagged with "raises-exception" but does not raise an error.
"""
import glob, json, os, shutil, subprocess, sys, tempfile

root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
notebooks = sys.argv[1:] or sorted(glob.glob('Chapter-*/**/*.ipynb', root_dir=root, recursive=True))

with tempfile.TemporaryDirectory() as tmp:
    copy = os.path.join(tmp, 'OCaml')
    shutil.copytree(root, copy)
    failures = 0
    for nb_path in notebooks:
        path = os.path.join(copy, nb_path)
        nb = json.load(open(path))
        for cell in nb['cells']:
            if cell['cell_type'] == 'code' and 'Sys.command "open ' in ''.join(cell['source']):
                cell['source'] = '()'
        json.dump(nb, open(path, 'w'))
        result = subprocess.run(['jupyter', 'nbconvert', '--to', 'notebook', '--execute', '--allow-errors',
                                 '--ExecutePreprocessor.timeout=1200', '--inplace', path],
                                cwd=os.path.dirname(path), capture_output=True, text=True)
        if result.returncode != 0:
            print(f'{nb_path}: nbconvert failed\n{result.stderr[-2000:]}')
            failures += 1
            continue
        bad = []
        for i, cell in enumerate(json.load(open(path))['cells']):
            if cell['cell_type'] != 'code':
                continue
            raised = any(o['output_type'] == 'error' for o in cell.get('outputs', []))
            expected = 'raises-exception' in cell.get('metadata', {}).get('tags', [])
            if raised != expected:
                source = ''.join(cell['source']).strip().split('\n')[0]
                bad.append(f'  cell {i}: {"unexpected error" if raised else "expected an error"}: {source[:70]}')
        print(f'{nb_path}: {"ok" if not bad else "FAILED"}')
        for line in bad:
            print(line)
        failures += bool(bad)
    print(f'\n{len(notebooks) - failures} of {len(notebooks)} notebooks ok')
    sys.exit(1 if failures else 0)
