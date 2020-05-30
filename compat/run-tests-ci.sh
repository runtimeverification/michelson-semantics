#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
REPO_DIRECTORY="$SCRIPT_DIRECTORY/.."

# do test initialization
source "$REPO_DIRECTORY/common.sh"
set +u
source "$REPO_DIRECTORY/start.sh"
set -u

# run tests
"$SCRIPT_DIRECTORY/run-tests.sh"

# wind down tests
"$REPO_DIRECTORY/shutdown.sh"
