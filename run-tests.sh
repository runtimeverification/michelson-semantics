#!/usr/bin/env bash

set -euo pipefail

# either validates that all expected tests pass or marks which tests fail
# when the first argument is "record", then do mark failing tests
# when there are no arguments, then validate all expected tests pass

SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
FAILING_FILE="$SCRIPT_DIRECTORY/tests/failing.cross"
TEST_DIR="$SCRIPT_DIRECTORY/tests/unit"

failed=0

for test in $(find $TEST_DIR -name '*.tzt');
do
  ! grep "${test#$TEST_DIR}" "$FAILING_FILE" &> /dev/null || continue
  # run test and get actual return value
  echo "Cross Validating: $test"
  cross_validate_output='0'
  $SCRIPT_DIRECTORY/cross-validate.sh "$test"
done
