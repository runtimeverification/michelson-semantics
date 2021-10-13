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

BUILD_TESTS=
[ $# -eq 1 ] && [ "$1" == "build" ] && BUILD_TESTS=on

for test in $(ls -- "$INPUT_DIR"); do
  testname=$(basename -s .tz "$test")
  testin="$INPUT_DIR/$testname.tz"
  testout="$OUTPUT_DIR/$testname.dot.testout"
  testexp="$OUTPUT_DIR/$testname.dot"
  if [ -n "$BUILD_TESTS" ]; then
    "$python_bin" "$tezos_utils" convert -i michelson -o dot "$testin" "$testexp"
  else
    "$python_bin" "$tezos_utils" convert -i michelson -o dot "$testin" "$testout"
    diff -q "$testout" "$testexp"
  fi
done
