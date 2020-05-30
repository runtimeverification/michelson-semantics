#!/usr/bin/env bash

set -euo pipefail

notif() { echo "== $@" >&2 ; }
fatal() { echo "[FATAL] $@" ; exit 1 ; }

command="$1" ; shift

SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"

# do test initialization
set +u
source "$SCRIPT_DIRECTORY/start.sh"
set -u

# run tests
FAILING_FILE="$SCRIPT_DIRECTORY/tests/failing.cross"
TEST_DIR="$SCRIPT_DIRECTORY/tests/unit"

for test in $(find $TEST_DIR -name '*.tzt'); do
  ! grep "${test#$TEST_DIR}" "$FAILING_FILE" &> /dev/null || continue
  # run test and get actual return value
  notif "Running '$command': $test"
  cross_validate_output='0'
  case "$command" in
    fix-address) $SCRIPT_DIRECTORY/fix-address.sh "$test" ;;
    run-tezos)   $SCRIPT_DIRECTORY/run-tezos.sh   "$test" ;;
    *) fatal "Unknown command: $command"                  ;;
  esac
done

# wind down tests
"$SCRIPT_DIRECTORY/shutdown.sh"
