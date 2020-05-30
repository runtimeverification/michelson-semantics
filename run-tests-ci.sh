#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"

# do test initialization
source "$SCRIPT_DIRECTORY/common.sh"
set +u
source "$SCRIPT_DIRECTORY/start.sh"
set -u

# run tests
"$SCRIPT_DIRECTORY/run-tests.sh"

# wind down tests
"$SCRIPT_DIRECTORY/shutdown.sh"
