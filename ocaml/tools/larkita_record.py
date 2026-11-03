"""Records how the Python package Larkita behaves, so that the OCaml port can be compared with it.

    python3 larkita_record.py OUTPUT.json SOURCE...

Every SOURCE is a directory with Larkita's unit tests (a package ``tests``), a Python script, or a
Jupyter notebook.  It is executed with a wrapped version of the class ``Larkita`` that records

  * every grammar together with its options and the outcome of its construction
    (the output file, or the exception that was raised),
  * every call of ``parse`` and ``lex`` together with its result or its exception.

The OCaml script larkita_replay.ml replays the recorded calls, and diff_larkita.py compares the results.
Needs Python 3.12 and Larkita (pip install larkita).
"""

import builtins
import contextlib
import io
import json
import os
import runpy
import sys
import unittest
import warnings

import larkita
import larkita.larkita as larkita_module
from larkita import Larkita as _Larkita

CASES = []

_FLAGS = {2: 'i', 8: 'm', 16: 's', 64: 'x'}


def _exception(e):
    result = {'type': type(e).__name__, 'message': str(e)}
    for attribute in ('line', 'column'):
        if hasattr(e, attribute):
            result[attribute] = getattr(e, attribute)
    return result


class RecordingLarkita(_Larkita):
    def __init__(self, grammar, **options):
        if not isinstance(grammar, str):
            options.setdefault('source_path', getattr(grammar, 'name', None))
            grammar = grammar.read()
        case = {'grammar': grammar, 'cwd': os.getcwd(), 'calls': [], 'skip': None}
        recorded = {}
        for key, value in options.items():
            if key == 'g_regex_flags':
                recorded[key] = ''.join(f for bit, f in sorted(_FLAGS.items()) if value & bit)
            elif key == 'import_paths':
                if any(callable(p) for p in value):
                    case['skip'] = 'callable import path'
                recorded[key] = [os.path.abspath(p) for p in value if not callable(p)]
            elif key == 'regex':
                if value:
                    case['skip'] = 'regex module'
            elif key in ('start', 'lexer', 'source_path', 'out_file', 'merge_states'):
                recorded[key] = value
            else:
                case['skip'] = 'option ' + key
        case['options'] = recorded
        self._case = case
        CASES.append(case)
        try:
            super().__init__(grammar, **options)
        except Exception as e:
            case['outcome'] = _exception(e)
            out_file = recorded.get('out_file') or (os.path.splitext(options['source_path'])[0] + '.out'
                                                    if options.get('source_path') else 'grammar.out')
            case['report'] = _read(out_file)
            raise
        case['outcome'] = None
        case['report'] = _read(self.out_file)

    def parse(self, text):
        call = {'kind': 'parse', 'text': text}
        self._case['calls'].append(call)
        try:
            result = super().parse(text)
        except Exception as e:
            call['error'] = _exception(e)
            raise
        call['result'] = repr(result)
        return result

    def lex(self, text):
        call = {'kind': 'lex', 'text': text}
        self._case['calls'].append(call)
        try:
            tokens = list(super().lex(text))
        except Exception as e:
            call['error'] = _exception(e)
            raise
        call['result'] = [[t.type, t.value, t.line, t.column] for t in tokens]
        return iter(tokens)


def _read(path):
    try:
        with open(path, encoding='utf8') as f:
            return f.read()
    except OSError:
        return None


def _install():
    larkita.Larkita = RecordingLarkita
    larkita_module.Larkita = RecordingLarkita


def run_tests(directory):
    sys.path.insert(0, directory)
    old = os.getcwd()
    os.chdir(directory)
    try:
        import tests.test_larkita as module
        module.Larkita = RecordingLarkita
        suite = unittest.defaultTestLoader.loadTestsFromModule(module)
        unittest.TextTestRunner(stream=io.StringIO()).run(suite)
    finally:
        os.chdir(old)
        sys.path.remove(directory)


def run_script(path):
    old = os.getcwd()
    os.chdir(os.path.dirname(os.path.abspath(path)))
    try:
        with contextlib.redirect_stdout(io.StringIO()), warnings.catch_warnings():
            warnings.simplefilter('ignore')
            runpy.run_path(os.path.basename(path), run_name='__main__')
    except BaseException as e:
        print('  %s: %s' % (path, type(e).__name__), file=sys.stderr)
    finally:
        os.chdir(old)


def run_notebook(path):
    "Executes the code cells of a notebook; cells that fail are skipped."
    with open(path, encoding='utf8') as f:
        notebook = json.load(f)
    old = os.getcwd()
    os.chdir(os.path.dirname(os.path.abspath(path)))
    namespace = {'__name__': '__main__'}

    def no_input(prompt=''):
        raise EOFError

    saved_input = builtins.input
    builtins.input = no_input
    try:
        for cell in notebook['cells']:
            if cell['cell_type'] != 'code':
                continue
            source = ''.join(cell['source'])
            lines = [line for line in source.split('\n') if not line.lstrip().startswith(('%', '!'))]
            try:
                with contextlib.redirect_stdout(io.StringIO()), warnings.catch_warnings():
                    warnings.simplefilter('ignore')
                    exec(compile('\n'.join(lines), path, 'exec'), namespace)
            except BaseException:
                pass
    finally:
        builtins.input = saved_input
        os.chdir(old)


def main():
    output, *sources = sys.argv[1:]
    _install()
    for source in sources:
        if os.path.isdir(source):
            run_tests(os.path.abspath(source))
        elif source.endswith('.ipynb'):
            run_notebook(source)
        else:
            run_script(source)
    with open(output, 'w', encoding='utf8') as f:
        json.dump(CASES, f, ensure_ascii=False, indent=1)
    print('%d grammars, %d calls recorded' % (len(CASES), sum(len(c['calls']) for c in CASES)))


if __name__ == '__main__':
    main()
