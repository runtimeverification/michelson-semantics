#!/bin/bash
SCRIPT_DIR="$(dirname "$(readlink -f "$BASH_SOURCE")")"
"$SCRIPT_DIR/contract-expander/kompile.sh" &
"$SCRIPT_DIR/extractor/kompile.sh" &
"$SCRIPT_DIR/input-creator/kompile.sh" &
"$SCRIPT_DIR/output-compare/kompile.sh" &
wait
