#!/bin/bash
set -e
SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
source "$SCRIPT_DIRECTORY/common.sh"
"$SCRIPT_DIRECTORY/compat/kompile-all.sh"
"$SCRIPT_DIRECTORY/unparser/kompile.sh"
kompile $KOMPILE_OPTS $* "$SCRIPT_DIRECTORY/unit-test.k"
