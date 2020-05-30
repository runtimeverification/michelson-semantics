#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(dirname "$(readlink -f "$BASH_SOURCE")")"
KOMPILE_OPTS="${KOMPILE_OPTS:-}"

function kompile_defn {
    DEFN_MAIN="$SCRIPT_DIR/$1";
    DEFN_DIR="$SCRIPT_DIR/.$(basename "$DEFN_MAIN" '.k')" ;
    mkdir -p "$DEFN_DIR" ;
    kompile "$DEFN_MAIN" $KOMPILE_OPTS -ccopt $SCRIPT_DIR/decode.cpp --hook-namespaces MICHELSON --directory "$DEFN_DIR"
}

if [[ "$#" -lt '1' ]]; then
    kompile_defn 'contract-expander.k' &
    kompile_defn 'extractor.k' &
    kompile_defn 'input-creator.k' &
    kompile_defn 'output-compare.k' &
    wait ;
else
    for defn in "$@"; do
        kompile_defn "$1"
    done
fi
