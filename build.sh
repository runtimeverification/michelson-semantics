#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"

source "$SCRIPT_DIRECTORY/common.sh"
source "$SCRIPT_DIRECTORY/gen-k.sh"

kompile --directory "$SCRIPT_DIRECTORY" $KOMPILE_OPTS $* "$BUILD_DIRECTORY/unit-test.k" && "$SCRIPT_DIRECTORY/compat/build.sh"
