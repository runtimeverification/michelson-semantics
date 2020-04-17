#!/bin/bash
SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
source "$SCRIPT_DIRECTORY/common.sh"

OUT_DIRECTORY="$(mktemp -d)"

trap "rm -rf $OUT_DIRECTORY" EXIT 

export NO_PARSER="YES"

find "$SCRIPT_DIRECTORY/tests/unit" -name "*.tzt" -print0 | xargs -0 -n 1 -P 8 -I'{}' bash -c "\"$SCRIPT_DIRECTORY/run.sh\" \"{}\" > \"$OUT_DIRECTORY/out_\$(basename \"{}\")\" || (echo \"FAILURE {}\" ; cat \"$OUT_DIRECTORY/out_\$(basename \"{}\")\" ; exit 255)" 

