#!/bin/bash

set -euo pipefail

SELF_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
MAIN_DIR="$SELF_DIR/.."
TESTS_DIR="$MAIN_DIR/tests"
INPUT_DIR="$TESTS_DIR/input"
OUTPUT_DIR="$TESTS_DIR/output"

if $(which python3 &> /dev/null); then
  python_bin=python3
else
  python_bin=python
fi
tezos_utils="$MAIN_DIR/tezos-utils"

for test in $(ls -- "$INPUT_DIR"); do
  testname=$(basename -s .tz "$test")
  testin="$INPUT_DIR/$testname.tz"
  testexp="$OUTPUT_DIR/$testname.dot"
  "$python_bin" "$tezos_utils" convert -i michelson -o dot "$testin" "$testexp"
done
