#!/usr/bin/env python3
"""Produce the F# edition from original theory and canonical F# teaching sources."""
from __future__ import annotations
import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
import re

HERE = Path(__file__).resolve().parent
REPOSITORY = HERE.parents[1]
ORIGINALS = REPOSITORY / 'Lecture-Notes'
EXPECTED = ['introduction', 'regular-expressions', 'ply-lex', 'finite-state-machines',
            'regular-languages', 'context-free-languages', 'ply-yacc-interpreter',
            'earley-parser', 'shift-reduce-parser', 'lr-parser', 'ply-yacc-lalr',
            'jasmin', 'compiler', 'compiler-2']


def read_source(path: Path) -> str:
    raw = path.read_bytes()
    try:
        return raw.decode('utf-8')
    except UnicodeDecodeError:
        # Some original chapters use ISO-8859-1 umlauts rather than UTF-8.
        return raw.decode('latin-1')


def live_text(text: str) -> str:
    """Remove TeX comment lines, including the disabled old Python implementations."""
    return '\n'.join(line for line in text.splitlines() if not line.lstrip().startswith('%')) + '\n'


def unique_position(text: str, marker: str) -> int:
    if text.count(marker) != 1:
        raise ValueError(f'Expected one source anchor {marker!r}, found {text.count(marker)}')
    return text.index(marker)


def span(text: str, start: str, end: str | None = None) -> str:
    first = unique_position(text, start)
    last = unique_position(text, end) if end else len(text)
    if last < first:
        raise ValueError(f'Invalid source span {start!r} to {end!r}')
    return text[first:last]


def figure(text: str, label: str) -> str:
    position = unique_position(text, '\\label{' + label + '}')
    first = text.rfind('\\begin{figure}', 0, position)
    last = text.find('\\end{figure}', position)
    if first < 0 or last < 0:
        raise ValueError(f'Missing figure boundaries for {label}')
    return text[first:last + len('\\end{figure}')]


def remove_figures(text: str) -> str:
    return re.sub(r'\\begin\{figure\}.*?\\end\{figure\}', '', text, flags=re.S)


def braced_end(text: str, first: int) -> int:
    """Find the end of a TeX argument, including nested commands in link labels."""
    depth, position = 0, first
    while position < len(text):
        character = text[position]
        if character == '\\':
            position += 2
            continue
        if character == '{':
            depth += 1
        elif character == '}':
            depth -= 1
            if depth == 0:
                return position + 1
        position += 1
    raise ValueError('Unclosed TeX argument in notebook link')


def replace_link_labels(text: str, pattern: str, replace) -> str:
    parts, first = [], 0
    for match in re.finditer(pattern, text):
        parts.extend([text[first:match.start()], replace(match)])
        first = braced_end(text, match.end() - 1)
    return ''.join(parts) + text[first:]


def rewrite_notebook_links(text: str, fsharp_roots: list[Path]) -> str:
    def link(match: re.Match[str]) -> str:
        original_path = match.group(1)
        # Repair already broken historical links while preserving their notebook topic.
        corrections = {
            'Chapter-10/Look-Ahead.ipynb': 'Chapter-10/03-Look-Ahead.ipynb',
            'Chapter-10/Look-Ahead-Solved.ipynb': 'Chapter-10/04-Look-Ahead-Solved.ipynb',
            'Compiler.ipynb': 'Chapter-12/Compiler.ipynb',
        }
        relative = corrections.get(original_path, original_path)
        if not any((root / relative).is_file() for root in fsharp_roots):
            raise ValueError(f'No translated notebook for original link {relative}')
        return '\\href{../../' + relative + '}{\\nolinkurl{FSharp/' + relative + '}}'
    pattern = r'\\href\{https://github\.com/karlstroetmann/Formal-Languages/(?:blob|tree)/master/(?:Python|Ply)/([^}]+\.ipynb)\}\{'
    text = replace_link_labels(text, pattern, link)
    # The Earley chapter points at the old ANTLR notebook although its algorithm is independent.
    text = replace_link_labels(text, r'\\href\{https://github\.com/karlstroetmann/Formal-Languages/blob/master/ANTLR4-Python/Earley-Parser/Earley-Parser.ipynb\}\{',
                              lambda _: r'\href{../../Chapter-08/Earley-Parser.ipynb}{\nolinkurl{FSharp/Chapter-08/Earley-Parser.ipynb}}')
    return text


def neutral_prose(text: str) -> str:
    # Applies only to preserved theoretical narrative, never to source code.
    text = text.replace('\\textsl{Python}', '\\texttt{F\\#}')
    text = text.replace('\\simtextsc{Python}', '\\texttt{F\\#}')
    for old in ['\\simtextsc{Ply}', '\\textsc{Ply}', '\\texttt{Ply}', '\\textsl{Ply}']:
        text = text.replace(old, '\\texttt{FsYacc}')
    return text


def generated_verbatim(text: str) -> str:
    # Retained minted listings are Java/C/Jasmin target examples, not F# host code.
    text = re.sub(r'\\begin\{minted\}(?:\[[\s\S]*?\])?\{(?!python)[^}]+\}',
                  r'\\begin{Verbatim}[fontsize=\\small,breaklines=true,frame=lines]', text)
    text = text.replace('\\end{minted}', '\\end{Verbatim}')
    return text


def wrap_compiler_arrays(text: str) -> str:
    """Move long left-hand sides to their own row without changing any formula."""
    pattern = r'^(\s*)(\\textsl\{compile\\_stmnt\}[^\n]*)(\n)'
    def row(match: re.Match[str]) -> str:
        indent, formula, newline = match.groups()
        if '\\mytt{else}' not in formula and '\\mytt{while}' not in formula:
            return match.group()
        if ' & = & ' in formula:
            lhs, rhs = formula.split(' & = & ', 1)
            return indent + '\\multicolumn{5}{l}{' + lhs + '} \\\\' + '\n' + indent + '& = & ' + rhs + newline
        return indent + '\\multicolumn{5}{l}{' + formula + '} \\\\' + newline
    return re.sub(pattern, row, text, flags=re.M)


def notebook_code(path: Path) -> str:
    notebook = json.loads(path.read_text())
    return '\n\n'.join(''.join(cell['source']) for cell in notebook['cells']
                       if cell['cell_type'] == 'code' and cell.get('metadata', {}).get('fsharp', {}).get('fsi', True))


def snippet_code(spec: dict, roots: list[Path]) -> tuple[str, Path]:
    source = next((root / spec['path'] for root in roots if (root / spec['path']).is_file()), None)
    if source is None:
        raise FileNotFoundError(f'Missing F# source {spec["path"]}; build and integrate the translated chapters first.')
    if source.suffix == '.ipynb':
        notebook = json.loads(source.read_text())
        if 'cell_contains' in spec:
            candidates = [''.join(c['source']) for c in notebook['cells']
                          if c['cell_type'] == 'code' and spec['cell_contains'] in ''.join(c['source'])]
            if len(candidates) != 1:
                raise ValueError(f'Nonunique notebook cell selector {spec["cell_contains"]!r} in {source}')
            code = candidates[0]
        else:
            code = notebook_code(source)
    else:
        code = read_source(source)
    if 'start' in spec:
        code = span(code, spec['start'], spec.get('end'))
    if not code.strip():
        raise ValueError(f'Empty code excerpt from {source}')
    return code.rstrip() + '\n', source


def validate_export(output: Path, roots: list[Path]) -> None:
    """Check generated book structure, cross references, links and host-code boundaries."""
    files = sorted(output.glob('*.tex'))
    combined = '\n'.join(path.read_text() for path in files)
    labels = re.findall(r'\\label\{([^}]+)\}', combined)
    references = re.findall(r'\\(?:ref|pageref)\{([^}]+)\}', combined)
    duplicates = [label for label, count in Counter(labels).items() if count > 1]
    unresolved = sorted(set(references) - set(labels) - {'LastPage'})
    if duplicates or unresolved:
        raise ValueError(f'Invalid TeX cross references: duplicates={duplicates}, unresolved={unresolved}')
    for path in files:
        text = path.read_text()
        for relative in re.findall(r'\\href\{\.\./\.\./([^}]+\.ipynb)\}', text):
            if not any((root / relative).is_file() for root in roots):
                raise FileNotFoundError(f'Notebook link in {path.name} does not exist: {relative}')
        structural = re.sub(r'\\begin\{(?:Verbatim|verbatim)\}[\s\S]*?\\end\{(?:Verbatim|verbatim)\}', '', text)
        stack = []
        for match in re.finditer(r'\\(begin|end)\{([^}]+)\}', structural):
            direction, name = match.groups()
            if direction == 'begin':
                stack.append(name)
            elif not stack or stack.pop() != name:
                raise ValueError(f'Unbalanced TeX environment {name} in {path.name}')
        if stack:
            raise ValueError(f'Unclosed TeX environments in {path.name}: {stack}')
        if '\\begin{minted}' in text:
            raise ValueError(f'Unconverted syntax-highlighter dependency in {path.name}')
        for source in re.findall(r'\\VerbatimInput(?:\[[^]]*\])?\{([^}]+)\}', text):
            listing = output / source
            if not listing.is_file():
                raise FileNotFoundError(f'Missing canonical listing: {source}')
            if re.search(r'^\s*(?:def |import ply|from \w+ import |class \w+\()', listing.read_text(), re.M):
                raise ValueError(f'Python host listing in generated F# source excerpt: {source}')


def build(output: Path, fsharp_roots: list[Path]) -> dict:
    output.mkdir(parents=True, exist_ok=True)
    (output / 'snippets').mkdir(exist_ok=True)
    (output / 'converted').mkdir(exist_ok=True)
    master = live_text(read_source(ORIGINALS / 'formal-languages.tex'))
    active = re.findall(r'^\\include\{([^}]+)\}', master, re.M)
    if active != EXPECTED:
        raise ValueError(f'Active chapter list changed: {active}; review the edition mapping before exporting.')
    originals = {name: live_text(read_source(ORIGINALS / (name + '.tex'))) for name in active}
    manifest = json.loads((HERE / 'sources.json').read_text())
    generated_snippets = {}

    def fragment(name: str) -> str:
        text = read_source(HERE / 'fragments' / (name + '.tex'))
        def expand(match: re.Match[str]) -> str:
            key = match.group(1)
            if key not in manifest:
                raise ValueError(f'Unknown snippet {key}')
            code, path = snippet_code(manifest[key], fsharp_roots)
            filename = key + path.suffix.replace('.ipynb', '.fs')
            (output / 'snippets' / filename).write_text(code)
            generated_snippets[key] = {'path': manifest[key]['path'], 'sha256': hashlib.sha256(code.encode()).hexdigest(), 'lines': len(code.splitlines())}
            # pdfTeX/fvextra can split multibyte UTF-8 when breaking anywhere.
            anywhere = ',breakanywhere=true' if code.isascii() else ''
            return '\\VerbatimInput[fontsize=\\small,breaklines=true' + anywhere + ',numbers=left,frame=lines]{snippets/' + filename + '}'
        text = re.sub(r'@@snippet:([a-z0-9-]+)@@', expand, text)
        def original_figure(match: re.Match[str]) -> str:
            return figure(originals[match.group(1)], match.group(2))
        text = re.sub(r'@@figure:([^:]+):([^@]+)@@', original_figure, text)
        return text

    chapters = {}
    coverage = []
    for name, original in originals.items():
        preserved = []
        replacements = []
        def keep(start: str, end: str | None = None) -> str:
            value = span(original, start, end)
            preserved.append({'start': start, 'end': end, 'lines': len(value.splitlines())})
            return value
        if name == 'introduction':
            text = keep('\\chapter{', 'This lecture explores') + fragment('introduction')
            text += keep('\\section{Basic Definitions}', '\\section{Overview}') + fragment('overview')
            text += keep('\\section{Literature}')
            replacements = ['opening tool introduction', 'course overview']
        elif name == 'regular-expressions':
            text = original.replace('The syntax of regular expressions as used in \\textsl{Python}', 'The syntax supported by the .NET regular-expression library')
            preserved.append({'start': 'whole theoretical chapter', 'end': None, 'lines': len(original.splitlines())})
        elif name == 'ply-lex':
            text = fragment('scanner-opening') + keep('A \\textcolor{blue}{scanner}', '\\section{The Structure') + fragment('scanning')
            replacements = ['PLY token definitions and line-based commentary', 'Python regex syntax', 'exam scanner', 'exclusive lexer states']
        elif name in ['finite-state-machines', 'regular-languages', 'lr-parser']:
            text = original
            preserved.append({'start': 'whole theoretical chapter', 'end': None, 'lines': len(original.splitlines())})
            if name == 'lr-parser':
                text += fragment('lr-implementation')
        elif name == 'context-free-languages':
            text = keep('\\chapter{', '\\noindent\nFigure \\ref{fig:differentiate.stlx}') + fragment('recursive-descent')
            text += keep('\\exerciseEng\nDevelop an \\simtextsc{EBNF} grammar')
            text = text.replace('a nested tuple that represents a regular expression', 'a discriminated union representing a regular expression')
            replacements = ['Python EBNF parser and line-based explanation']
        elif name == 'ply-yacc-interpreter':
            text = fragment('generated-parsers')
            preserved.append({'start': 'calculator and source-language grammar/program figures', 'end': None,
                              'lines': sum(len(figure(original, label).splitlines()) for label in ['fig:calculator.g', 'fig:Pure.g', 'fig:sum.sl', 'fig:factorial.sl'])})
            replacements = ['PLY scanner/parser API and semantic actions', 'nested tuple ASTs', 'Python interpreter, I/O and environment']
        elif name == 'earley-parser':
            text = keep('\\chapter{', '\\section{Implementing Earley') + fragment('earley')
            text += keep('\\section{Check Your Understandig}')
            replacements = ['Python implementation reference']
        elif name == 'shift-reduce-parser':
            boundary = original.index('\\label{fig:Shift-Reduce-Parser-Pure.ipynb}')
            first_figure = original.rfind('\\begin{figure}', 0, boundary)
            preserved.append({'start': 'chapter start', 'end': 'first Python implementation figure', 'lines': len(original[:first_figure].splitlines())})
            text = original[:first_figure] + fragment('shift-reduce')
            tables = span(original, 'The tables  \\ref{tab:action}', '\\caption{Grammar rules coded')
            last_figure = tables.rfind('\\begin{figure}')
            text += tables[:last_figure]
            preserved.append({'start': 'abstract action/goto explanation, grammar and tables', 'end': 'host encoding figures', 'lines': len(tables[:last_figure].splitlines())})
            text += fragment('parse-table')
            replacements = ['Python parser class and stack mutations', 'Python/SetlX grammar/action/goto encodings']
        elif name == 'ply-yacc-lalr':
            text = fragment('lalr-opening')
            # Preserve the complete precedence derivation independently of old tool reports.
            theory = span(original, '\\begin{enumerate}\n\\item Since the operators', '\\section{Resolving Shift-Reduce')
            theory = theory.replace('Next, we explain in detail how \\simtextsc{Ply} uses operator precedence relations to resolve shift-reduce conflicts.',
                                    'We now derive the intended precedence-based action selection mathematically. The implementation note below explains the generator-specific non-associativity difference.')
            theory = theory.replace('First, \\simtextsc{Ply} assigns', 'First, a precedence declaration assigns')
            theory = theory.replace('Therefore, \\simtextsc{Ply} will resolve this\n            shift-reduce conflict by putting an error entry into the action table.',
                                    'The intended non-associative language therefore requires an error entry. FsYacc 11.3.0 does not implement this case by its declaration alone; the factored grammar below enforces it structurally.')
            theory = theory.replace('and \\simtextsc{Ply} prints a warning message', 'and the generator reports a warning')
            text += theory + fragment('lalr-implementation')
            # Keep formal lookahead transformations and state-merger discussion; discard old reports.
            text += keep('\\section{Resolving Shift-Reduce', '\\subsection{Mysterious Reduce-Reduce Conflicts}')
            text += fragment('mysterious')
            text += '\\section{Exercises}\nUse FsYacc to implement the propositional-logic grammar, explain its operator precedences, and show why unparenthesized chained equivalence must be rejected. Compare the two lookahead grammars and identify the state cores that cause the mysterious reduce-reduce conflict.\n'
            preserved.append({'start': 'precedence derivation with non-associativity correction', 'end': 'conflict-resolution section', 'lines': len(theory.splitlines())})
            replacements = ['PLY reports and state-number claims', 'Python precedence tuples', 'PLY-specific mysterious-conflict anecdote', 'non-associativity assumption']
        elif name == 'jasmin':
            text = original + fragment('jasmin-host')
            # These comparisons are intentionally retained as target-language context.
            first = unique_position(text, '        In mathematics, integer division, which is also known as')
            last = text.index('      }. ', first)
            text = text[:first] + fragment('division-conventions').rstrip() + '\n' + text[last:]
            text = text.replace('differs from the implementation in \\textsl{Java}.  For example,',
                                'differs from the implementation in \\textsl{Python}.  For example,')
            preserved.append({'start': 'entire JVM/Java/Jasmin target-language chapter', 'end': None, 'lines': len(original.splitlines())})
            replacements = ['floor/Euclidean division clarification', 'Java/Python remainder comparison typo']
        elif name == 'compiler':
            text = keep('\\chapter{', '\\section{Developing the  Scanner') + fragment('compiler-frontend')
            replacements = ['Python scanner/parser, tuple AST, diagnostics and file API']
        elif name == 'compiler-2':
            text = fragment('codegen-opening')
            # Each rule's mathematical specification precedes its Python implementation figure.
            sections = re.split(r'(?=^\\(?:subsection|subsubsection)\{)', original, flags=re.M)
            for section in sections:
                if not section.strip():
                    continue
                if section.startswith('\\subsection{Compiling a Program}'):
                    text += fragment('compile-program')
                    replacements.append('Python whole-program file orchestration')
                    continue
                python = re.search(r'\\begin\{figure\}.*?\\begin\{minted\}.*?\{python3\}', section, re.S)
                if python:
                    # A lazy environment match must not consume preceding target examples.
                    position = section.index('\\begin{minted}')
                    cutoff = section.rfind('\\begin{figure}', 0, position)
                    theory = section[:cutoff]
                else:
                    theory = section
                theory = remove_figures(theory) if 'nested tuple' in theory else theory
                if section.startswith('\\subsection{Translation of Arithmetic'):
                    theory = theory.replace('Concretely, this is a dictionary', 'Concretely, the F\\# implementation uses an immutable map')
                    theory = theory.replace("\\mytt{st['x'] = 2}", '\\mytt{Map.find "x" context.Symbols = 2}')
                if section.startswith('\\subsubsection{Translation of Binary Boolean Operators}'):
                    # Keep the original stack derivation as an explicitly eager alternative.
                    theory = theory.replace('\\subsubsection{Translation of Binary Boolean Operators}',
                                            '\\paragraph{An eager JVM alternative from the original derivation}')
                    theory = theory[:theory.index('Figure \\ref{fig:Compiler.ipynb:compile:iand}')]
                    text += fragment('short-circuit')
                    theory += ('\nThe original formula above uses a plus sign in its Boolean expression position; '
                               'it denotes the eager conjunction described here, not arithmetic addition. '
                               'The F\\# compiler uses the preceding short-circuit branches instead of this eager alternative.\n')
                    replacements.append('Boolean operator derivation aligned with short-circuit backend')
                if section.startswith('\\subsubsection{Translation of a Function Call}'):
                    lhs = r'\mytt{compile\_expr}\bigl(f(a_1,\cdots,a_n), \mytt{st}, \mytt{class\_name}\bigr) :='
                    first = theory.index('$' + lhs)
                    last = theory.index('$', first + 1)
                    formula = theory[first + 1:last].replace(lhs, '\\begin{aligned}[t]\n&' + lhs[:-3] + '\\\\\n&:=', 1)
                    theory = theory[:first] + '$' + formula + '\\end{aligned}$' + theory[last + 1:]
                    first = unique_position(theory, '\\item If $f$ is the function \\mytt{println}')
                    last = theory.index('\\end{enumerate}', first)
                    theory = theory[:first] + fragment('println-call') + theory[last:]
                    replacements.append('one-argument void println and invokevirtual convention')
                if section.startswith('\\subsubsection{Translation of Expression Statements}'):
                    theory = fragment('expression-statements')
                    replacements.append('discarded expression values and stack-neutral statements')
                if section.startswith('\\subsubsection{Translation of a Return Statement}'):
                    theory += ('\nThe rule above applies to functions with an integer return value. '
                               'JVM main has a void descriptor; its F\\# compilation emits '
                               '\\mytt{pop} followed by \\mytt{return} after evaluating a source return expression.\n')
                if section.startswith('\\subsection{Translation of a Function Definition}'):
                    theory += ('The JVM method conventions below describe function signatures and local frames. '
                               'Section \\ref{sec:fsharp-code-generation} gives the complete '
                               '\\texttt{compileFunction} implementation, which allocates local slots, '
                               'computes stack limits and follows each target method\'s return convention.\n')
                theory = theory.replace('$ \\quad and \\quad\n$', '$, \\\\[0.1cm]\n\\hspace*{1.3cm}\n$')
                theory = wrap_compiler_arrays(theory)
                text += theory
                preserved.append({'start': section.splitlines()[0], 'end': 'before Python implementation figure', 'lines': len(theory.splitlines())})
            conventions = span(original, 'In order to understand how this works we have to understand what type of code',
                               'We proceed to discuss the implementation shown')
            text += '\n\\subsection{JVM method conventions}\n' + conventions
            preserved.append({'start': 'JVM method signature and frame conventions', 'end': 'Python line-by-line explanation', 'lines': len(conventions.splitlines())})
            text += fragment('code-generation')
            replacements += ['all Python expression/Boolean/statement/function implementations', 'line-number explanations tied to Python tuple layout']
        else:
            raise ValueError(f'Unmapped chapter {name}')
        if name == 'compiler-2':
            text = re.sub(r'Figure \\ref\{fig:Compiler\.ipynb:[^}]+\}(?: on page\s*\\pageref\{[^}]+\})?',
                          r'the F\\# implementation in Section \\ref{sec:fsharp-code-generation}', text)
        text = text.replace('\\label{fig:chapter-image}', '\\label{fig:' + name + ':chapter-image}')
        # Labels for replaced figures still resolve to their F# replacement discussions.
        original_labels = set(re.findall(r'\\label\{([^}]+)\}', original))
        current_labels = set(re.findall(r'\\label\{([^}]+)\}', text))
        missing = sorted(original_labels - current_labels - {'fig:chapter-image'})
        if missing:
            text += '\n\\paragraph{Implementation cross references}\nThe following source-edition labels refer to the F\\# implementations and explanations in this chapter.\n'
            text += '\n'.join('\\label{' + label + '}' for label in missing) + '\n'
        # Host-specific code was replaced above; narrative references and links now describe the edition.
        if name != 'jasmin':
            text = neutral_prose(text)
        text = rewrite_notebook_links(text, fsharp_roots)
        text = text.replace('https://docs.python.org/3/library/re.html', 'https://learn.microsoft.com/dotnet/standard/base-types/regular-expressions')
        text = text.replace('Abbildungen/', str(ORIGINALS / 'Abbildungen') + '/')
        text = generated_verbatim(text)
        if re.search(r'\{python3?\}|^\s*(?:def |import ply|from \w+ import |class \w+\(|(?:gotoTable|actionTable)\[)', text, re.M):
            raise ValueError(f'Live Python/legacy host implementation remains in {name}')
        if '@@' in text:
            raise ValueError(f'Unexpanded chapter directive in {name}')
        for match in re.finditer(r'\\begin\{(Definition|Theorem|Satz|Proposition|Lemma|Korollar|Corollary|eqnarray\*?|equation\*?|align\*?|array|displaymath)\}[\s\S]*?\\end\{\1\}', original):
            expected = neutral_prose(match.group()) if name != 'jasmin' else match.group()
            if name == 'compiler-2':
                expected = wrap_compiler_arrays(expected)
            expected = expected.replace('Abbildungen/', str(ORIGINALS / 'Abbildungen') + '/')
            if expected not in text:
                raise ValueError(f'Mathematical environment was not retained in {name}: {match.group(1)}')
        chapters[name] = text
        (output / (name + '.tex')).write_text(text)
        coverage.append({'chapter': name, 'original_sha256': hashlib.sha256(original.encode()).hexdigest(),
                         'original_lines': len(original.splitlines()), 'preserved': preserved,
                         'replaced': replacements, 'generated_lines': len(text.splitlines())})
    # Keep original typography and mathematical macros. Listings use fancyvrb/fvextra.
    master = master.replace('\\usepackage{minted}', '\\usepackage{fvextra}')
    master = master.replace('\\usepackage{fvextra}', '\\usepackage{fvextra}\n' + read_source(HERE / 'fragments' / 'pdf-support.tex'))
    master = master.replace('An Introduction via \\textsc{Ply}', 'An Introduction via F\\# and FsLexYacc')
    master = master.replace('{\\large --- 2025 ---}', '{\\large --- F\\# evaluation edition ---}')
    cover_start = unique_position(master, '  These lecture notes, along with their')
    cover_end = master.index('\\end{minipage}', cover_start)
    master = master[:cover_start] + read_source(HERE / 'fragments' / 'cover-repository.tex') + master[cover_end:]
    master = master.replace('\\bibliography{cs}', '\\bibliography{' + str(ORIGINALS / 'cs') + '}')
    master = master.replace('Abbildungen/', str(ORIGINALS / 'Abbildungen') + '/')
    master = master.replace('\\maketitle', '\\maketitle\n\\input{edition-note}')
    (output / 'formal-languages-fsharp.tex').write_text(master)
    (output / 'edition-note.tex').write_text(fragment('edition-note'))
    report = {'active_chapters': active, 'chapters': coverage, 'snippets': generated_snippets,
              'preserved_theory_lines': sum(part['lines'] for chapter in coverage for part in chapter['preserved']),
              'python_host_listing_count': 0}
    validate_export(output, fsharp_roots)
    report['structural_checks'] = {'mathematical_environments': 'preserved', 'tex_environments': 'balanced', 'cross_references': 'resolved', 'notebook_links': 'exist', 'canonical_source_listings': 'exist', 'python_host_listings': 'absent'}
    (output / 'coverage.json').write_text(json.dumps(report, indent=2) + '\n')
    return report


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, default=HERE / '_build')
    parser.add_argument('--source-root', action='append', type=Path, default=[],
                        help='Additional FSharp root for isolated worktree verification; normal builds need none.')
    args = parser.parse_args()
    report = build(args.output.resolve(), [REPOSITORY / 'FSharp', *args.source_root])
    print(f'F# lecture edition: {len(report["active_chapters"])} active chapters, '
          f'{report["preserved_theory_lines"]} preserved theory lines, '
          f'{len(report["snippets"])} canonical F# source excerpts; no Python host listings.')
    print(args.output.resolve() / 'formal-languages-fsharp.tex')


if __name__ == '__main__':
    main()
