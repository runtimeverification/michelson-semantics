#!/bin/bash

set -euo pipefail

SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
source "$SCRIPT_DIRECTORY/common.sh"
source "$SCRIPT_DIRECTORY/gen-k.sh"

kompile --backend haskell --directory "$SCRIPT_DIRECTORY" "$BUILD_DIRECTORY/symbolic-unit-test.k"
