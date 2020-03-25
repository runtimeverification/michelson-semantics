#!/bin/bash
SCRIPT_DIR="$(dirname "$(readlink -f "$BASH_SOURCE")")"
EXTRACTOR_DIR="$SCRIPT_DIR/compat/extractor/"

if which tezos-client>/dev/null 2>&1; then
    set -e ; 
    TEMP_DIR="$(mktemp -d)" ;
    trap "rm -rf $TEMP_DIR" EXIT ;
    GROUP_FILE="$TEMP_DIR/group" ; 
    EXPANDED_FILE="$TEMP_DIR/expanded" ;
    "$EXTRACTOR_DIR/run.sh" "$1" 'code_or_contract' 'false' > "$GROUP_FILE" ;
    GROUP="$(grep -Eo '^\s*(code|contract)' "$GROUP_FILE")" ;
    tezos-client 'expand' 'macros' 'in' "$(sed -E 's/^\s*(code|contract)\s*(.*);/\2/' "$GROUP_FILE")" | tr -d '\n' | sed -E "s/(.*)/$GROUP {\\1} ;/;s/{{/{/g;s/}}/}/g" | cat - "$1" > "$EXPANDED_FILE" ;
    kast --directory "$SCRIPT_DIR" --expand-macros "$EXPANDED_FILE" || cat "$EXPANDED_FILE" > /dev/stdout;
else
    echo 'tezos-client not found, using normal parsing' >/dev/stderr ;
    kast "$1" --directory "$SCRIPT_DIR" --expand-macros ;
fi
