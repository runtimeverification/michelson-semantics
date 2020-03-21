#!/bin/bash
SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
source "$SCRIPT_DIRECTORY/common.sh"
krun --directory "$SCRIPT_DIRECTORY" --parser "$SCRIPT_DIRECTORY/parser.sh" $*
