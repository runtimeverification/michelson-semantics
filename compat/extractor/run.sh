#!/bin/bash
TEMP_FILE="$(mktemp)"

trap "rm -f $TEMP_FILE" EXIT

echo "query $2 $3 ; " | cat - $1 > "$TEMP_FILE"

krun --directory "$(dirname "$(readlink -f "$BASH_SOURCE")")" -o program "$TEMP_FILE" | tr '\n' ' ' | sed 's/\.AnnotationList//g;s/.*<stack>\(.*\)<\/stack>.*/\1/;s/^\s*//;s/\s*$//;s/true/True/g;s/false/False/g;/#NoGroup/d'
RET="${PIPESTATUS[0]}"
echo
exit $RET
