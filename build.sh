#!/bin/bash
SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
source "$SCRIPT_DIRECTORY/common.sh"
"$SCRIPT_DIRECTORY/compat/kompile-all.sh"
kompile $KOMPILE_OPTS $* "$SCRIPT_DIRECTORY/unit-test.k"
