#!/bin/bash
SCRIPT_DIR="$(dirname "$(readlink -f "$BASH_SOURCE")")"
kompile "$SCRIPT_DIR/output-compare.k" $KOMPILE_OPTS -ccopt $SCRIPT_DIR/decode.cpp --hook-namespaces MICHELSON --directory "$SCRIPT_DIR" 
