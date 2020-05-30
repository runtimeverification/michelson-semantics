#!/usr/bin/env bash

set -euo pipefail

# either validates that all expected tests pass or marks which tests fail
# when the first argument is "record", then do mark failing tests
# when there are no arguments, then validate all expected tests pass

SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
RECORD_FILE="$SCRIPT_DIRECTORY/tests/failing.cross"
TEST_DIR="$SCRIPT_DIRECTORY/tests/unit"

failed=0
record=0


if [ $# -ge 1 ] && [ "${1:-}" == "record" ]; then
  record=1
  > "$RECORD_FILE"
elif [ $# -ge 1 ] || [ ! -f "$RECORD_FILE" ]; then
  echo "usage: run-tests.sh [record]"
  echo "       Must execute:"
  echo "       > run-tests.sh record"
  echo "       BEFORE running:"
  echo "       > run-tests.sh"
  exit 1
fi

for test in $(ls "$TEST_DIR");
do
  # run test and get actual return value
  echo "Cross Validating: $test"
  cross_validate_output='0'
  "$SCRIPT_DIRECTORY/cross-validate.sh" "$TEST_DIR/$test" || cross_validate_output="$?"
  actual=$(( "$cross_validate_output" ? 1 : 0 ))
  # get expected value of test
  if [ $record -eq 1 ]; then
    expected=0
  else
    output_grep='0'
    grep -Fxq -- "$test" "$RECORD_FILE" || output_grep="$?"
    expected=$(( "$output_grep" ? 0 : 1 ))
  fi
  # check whether test results agree and record result
  if [ $actual -ne $expected ]; then
    echo "FAILED $test"
    failed=1
    if [ $record -eq 1 ]; then
      echo "$test" >> "$RECORD_FILE"
    fi
  fi
done

if [ $record -eq 0 ] && [ $failed -eq 1 ]; then
  echo "test cross-validation failed on above tests"
  exit 1
fi
