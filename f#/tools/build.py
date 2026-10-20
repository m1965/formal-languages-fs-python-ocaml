#!/usr/bin/env python3
"""Build FsLexYacc projects, notebook dependencies and the full lecture edition."""
import argparse
import subprocess
import sys

from notebooks import EDITION, ROOT, export_dependencies, dotnet_environment

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--dotnet', default='dotnet')
args = parser.parse_args()
for project in sorted(EDITION.rglob('*.fsproj')):
    subprocess.run([args.dotnet, 'build', str(project), '--nologo'], cwd=ROOT, check=True,
                   env=dotnet_environment(args.dotnet))
export_dependencies()
subprocess.run([sys.executable, str(EDITION / 'Lecture-Notes' / 'build.py')],
               cwd=ROOT, check=True)
