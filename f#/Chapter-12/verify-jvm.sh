#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
: "${JAVA:=java}"
: "${JASMIN_JAR:?Set JASMIN_JAR to the Jasmin 3.0.3 jar-with-dependencies file}"
: "${DOTNET:=dotnet}"
verification_dir=$(mktemp -d)
trap 'rm -rf "$verification_dir"' EXIT
verify_class() {
    local name=$1 expected=$2 assembler=$3 actual
    "$JAVA" -jar "$JASMIN_JAR" -d "$verification_dir" "$assembler"
    actual=$("$JAVA" -Xverify:all -cp "$verification_dir" "$name")
    if [[ "$actual" != "$expected" ]]; then
        printf 'Unexpected output from %s\n%s\n' "$name" "$actual" >&2
        exit 1
    fi
    printf 'JVM verification and exact output passed: %s\n' "$name"
}
# Keep regression fixtures temporary; compile them with the notebook's real compiler.
cat > "$verification_dir/BooleanSemantics.c" <<'C_SOURCE'
int probe() {
    println(77);
    return 2;
}
int main() {
    int x;
    int y;
    x = 1;
    y = 2;
    println(!x < y);
    println(!(x < y));
    println(!x < y || y == 2);
    println(!(x < y || y == 2));
    println(0 && 1 / 0);
    println(1 || 1 / 0);
    println(0 && probe());
    println(1 || probe());
    println(1 && probe());
    println(0 || probe());
}
C_SOURCE
# #load resolves relative to the runner; place it beside the exported notebook.
runner=$(mktemp "$PWD/.verify-boolean-XXXXXX.fsx")
trap 'rm -rf "$verification_dir"; rm -f "$runner"' EXIT
cat > "$runner" <<'FS_SOURCE'
#load "Compiler.fsx"
let examples = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "Examples")
for file in System.IO.Directory.EnumerateFiles(examples, "*.c") |> Seq.sort do
    Compiler.compileProgram file |> ignore
Compiler.compileProgram fsi.CommandLineArgs.[1] |> ignore
FS_SOURCE
"$DOTNET" fsi --exec "$runner" "$verification_dir/BooleanSemantics.c" >/dev/null
primes=$'2\n3\n5\n7\n11\n13\n17\n19\n23\n29\n31\n37\n41\n43\n47\n53\n59\n61\n67\n71\n73\n79\n83\n89\n97'
for name in Fakultaet MySum Primes PrimesFor PrimesFor2 Ternary; do
    case "$name" in
        Fakultaet) expected=$'1\n2\n6\n24\n120\n720\n5040\n40320\n362880\n3628800' ;;
        MySum) expected=666 ;;
        Ternary) expected=662 ;;
        *) expected=$primes ;;
    esac
    verify_class "$name" "$expected" "Examples/$name.jas"
done

verify_class BooleanSemantics $'0\n0\n1\n0\n0\n1\n0\n1\n77\n1\n77\n1' "$verification_dir/BooleanSemantics.jas"
