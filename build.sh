#!/bin/bash
set -e
SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
source "$SCRIPT_DIRECTORY/common.sh"
kompile $KOMPILE_OPTS $* "$SCRIPT_DIRECTORY/unit-test.k" && "$SCRIPT_DIRECTORY/compat/kompile-all.sh"
