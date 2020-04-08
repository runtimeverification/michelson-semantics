#!/bin/bash
SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
source "$SCRIPT_DIRECTORY/common.sh"
TEMP_DIR="$(mktemp -d)" ;
trap 'rm -rf "$TEMP_DIR"' EXIT ;
"$SCRIPT_DIRECTORY/parser.sh" "$*" > "$TEMP_DIR/input.kore"
llvm-krun -d "$SCRIPT_DIRECTORY/unit-test-kompiled" -c PGM "$TEMP_DIR/input.kore" Pgm korefile -o "$TEMP_DIR/output.kore" || cat "$TEMP_DIR/output.kore"
