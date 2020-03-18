#!/bin/bash
SCRIPT_DIR="$(dirname "$(readlink -f "$BASH_SOURCE")")"
EXTRACTOR_DIR="$SCRIPT_DIR/compat/extractor/"

if which tezos-client>/dev/null 2>&1; then
    set -e ;
    TEMP_DIR="$(mktemp -d)" ;
    trap "rm -rf $TEMP_DIR" EXIT ;
    CODE_FILE="$TEMP_DIR/code"
    SUB_FILE="$TEMP_DIR/sub" ;
    EXPANDED_FILE="$TEMP_DIR/expanded"
    "$EXTRACTOR_DIR/run.sh" "$1" 'code_or_contract' 'true' | sed 's/({/{/g;s/})/}/g;s/true/True/g;s/false/False/g;s/.*<k>\(.*\)<\/k>.*/\1/;s/(\s*\(0x\w*\|[+-]*[0-9][0-9]*\|"\([^"]\|\\"\)*"\)\s*)/\1/g' > $CODE_FILE ;
    paste -d'/' <(sed -E 's/\s+/\\s*/g;s/\(/\\s*(\\s*/g;s/\)/\\s*)\\s*/g;' "$CODE_FILE") <(tezos-client expand macros in "$(cat $CODE_FILE)" 2>/dev/null | tr -d '\n') | sed -E 's|(.*)|s/\1/|' > "$SUB_FILE" ;
    kast --directory "$SCRIPT_DIR" -o program "$1"  | tr -d '\n' | sed 's/({/{/g;s/})/}/g;s/true/True/g;s/false/False/g;s/.*<k>\(.*\)<\/k>.*/\1/;s/(\s*\(0x\w*\|[+-]*[0-9][0-9]*\|"\([^"]\|\\"\)*"\)\s*)/\1/g' |  sed -f "$SUB_FILE" > "$EXPANDED_FILE" ;
    kast --directory "$SCRIPT_DIR" --expand-macros "$EXPANDED_FILE" || cat "$EXPANDED_FILE" > /dev/stderr ;
else
    echo 'tezos-client not found, using normal parsing' >/dev/stderr ;
    kast "$1" --directory "$SCRIPT_DIR" --expand-macros ;
fi
