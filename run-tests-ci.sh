#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"

# do test initialization
source "$SCRIPT_DIRECTORY/common.sh"
set +u
source "$SCRIPT_DIRECTORY/start.sh"
set -u

# run tests
FAILING_FILE="$SCRIPT_DIRECTORY/tests/failing.cross"
TEST_DIR="$SCRIPT_DIRECTORY/tests/unit"

for test in $(find $TEST_DIR -name '*.tzt'); do
  ! grep "${test#$TEST_DIR}" "$FAILING_FILE" &> /dev/null || continue
  # run test and get actual return value
  echo "Cross Validating: $test"
  cross_validate_output='0'
  $SCRIPT_DIRECTORY/cross-validate.sh "$test"
done

# wind down tests
"$SCRIPT_DIRECTORY/shutdown.sh"
