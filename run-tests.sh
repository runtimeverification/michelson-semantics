#!/bin/bash
SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
source "$SCRIPT_DIRECTORY/common.sh"

OUT_DIRECTORY="$(mktemp -d)"

trap "rm -rf $OUT_DIRECTORY" EXIT 

for Test in "$SCRIPT_DIRECTORY/tests/unit/"*.tzt ; do
    OUT_FILE="$OUT_DIRECTORY/$(basename "$Test")" ;
    echo "$Test" ;
    if ! "$SCRIPT_DIRECTORY/run.sh" "$Test" > "$OUT_FILE" 2>&1 ; then
        echo "FAILURE:"
        cat "$OUT_FILE" ;
        exit 1
    fi 
done
