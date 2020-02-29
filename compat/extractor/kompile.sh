#!/bin/bash
SCRIPT_DIR="$(dirname "$(readlink -f "$BASH_SOURCE")")"
kompile "$SCRIPT_DIR/extractor.k" $KOMPILE_OPTS --directory "$SCRIPT_DIR"
