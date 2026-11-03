"""Compares the OCaml port of Larkita with the Python package Larkita.

    python3 tools/diff_larkita.py [--impl ocaml|fsharp] PYTHON SOURCE...

PYTHON is a Python 3.12+ interpreter with Larkita installed; every SOURCE is passed to
larkita_record.py (Larkita's test directory, scripts, or notebooks).  The script records the
behaviour of Python's Larkita, replays it with the OCaml port (larkita_replay.ml), and reports every
difference: in the output files, in the abstract syntax trees, in the tokens, and in the exceptions.
Run it in the directory ocaml.
"""

import difflib
import json
import os
import subprocess
import sys
import tempfile


def expected_out_file(case):
    options = case['options']
    if options.get('out_file'):
        return options['out_file']
    if options.get('source_path'):
        return os.path.splitext(options['source_path'])[0] + '.out'
    return 'grammar.out'


def normalize(error, mine, theirs):
    if error is None:
        return None
    error = dict(error)
    if 'message' in error:
        error['message'] = error['message'].replace(mine, theirs)
    return error


def main():
    args = sys.argv[1:]
    impl = 'ocaml'
    if args[:1] == ['--impl']:
        impl, args = args[1], args[2:]
    python, *sources = args
    tools = os.path.dirname(os.path.abspath(__file__))
    with tempfile.TemporaryDirectory() as tmp:
        cases_file = os.path.join(tmp, 'cases.json')
        results_file = os.path.join(tmp, 'results.json')
        subprocess.run([python, os.path.join(tools, 'larkita_record.py'), cases_file, *sources], check=True)
        if impl == 'fsharp':
            replay = os.path.join(os.path.dirname(os.path.dirname(tools)), 'f#', 'Support', 'Larkita', 'tests', 'larkita_replay.fsx')
            subprocess.run(['dotnet', 'fsi', replay, cases_file, results_file, tmp], check=True)
        else:
            subprocess.run(['ocaml', os.path.join(tools, 'larkita_replay.ml'), cases_file, results_file, tmp], check=True,
                           cwd=os.path.dirname(tools))
        with open(cases_file, encoding='utf8') as f:
            cases = json.load(f)
        with open(results_file, encoding='utf8') as f:
            results = json.load(f)

    failures = compared = skipped = 0
    for i, (case, result) in enumerate(zip(cases, results)):
        if case['skip'] or result.get('skipped'):
            skipped += 1
            continue
        problems = []
        mine, theirs = result['out_file'], expected_out_file(case)
        if normalize(result['outcome'], mine, theirs) != case['outcome']:
            problems.append('construction: python %r\n                 ocaml  %r' % (case['outcome'], result['outcome']))
        if case['report'] and result['report'] is not None and case['report'] != result['report']:
            diff = difflib.unified_diff(case['report'].splitlines(), result['report'].splitlines(), 'python', 'ocaml',
                                        lineterm='', n=1)
            problems.append('output file differs:\n' + '\n'.join(list(diff)[:30]))
        for call, mine_call in zip(case['calls'], result['calls']):
            compared += 1
            if 'result' in call:
                if mine_call.get('result') != call['result']:
                    problems.append('%s %r:\n    python %r\n    ocaml  %r' % (call['kind'], call['text'][:60], call['result'],
                                                                          mine_call.get('result', mine_call.get('error'))))
            else:
                theirs_error = call['error']
                mine_error = normalize(mine_call.get('error'), mine, theirs)
                if mine_error != theirs_error:
                    problems.append('%s %r:\n    python %r\n    ocaml  %r' % (call['kind'], call['text'][:60], theirs_error,
                                                                          mine_error or mine_call.get('result')))
        if problems:
            failures += 1
            print('=' * 70)
            print('Case %d (%s)' % (i, case['options'].get('source_path') or case['grammar'][:60].replace('\n', ' ')))
            for p in problems:
                print('  ' + p)
    print('%d grammars (%d skipped), %d calls compared, %d grammars with differences'
          % (len(cases), skipped, compared, failures))
    sys.exit(1 if failures else 0)


if __name__ == '__main__':
    main()
