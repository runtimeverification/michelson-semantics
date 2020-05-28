#!/bin/bash
set -e
SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
source "$SCRIPT_DIRECTORY/common.sh"

kompile --backend haskell --directory "$SCRIPT_DIRECTORY" "$BUILD_DIRECTORY/unit-test.k"
