#!/bin/bash
SCRIPT_DIR="$(dirname "$(readlink -f "$BASH_SOURCE")")"
kompile "$SCRIPT_DIR/input-creator.k" $KOMPILE_OPTS --syntax-module UNIT-TEST-SYNTAX --directory "$SCRIPT_DIR"
