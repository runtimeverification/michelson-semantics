#!/bin/bash
TEMP_FILE="$(mktemp)"
trap "rm -f $TEMP_FILE" EXIT
echo "query $2 $3 ; " | cat - $1 > "$TEMP_FILE"
krun --directory "$(dirname "$(readlink -f "$BASH_SOURCE")")" "$TEMP_FILE"
