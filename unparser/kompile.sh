#!/bin/bash
SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
kompile --backend llvm "$SCRIPT_DIRECTORY/michelson-unparser-test.k" --directory "$SCRIPT_DIRECTORY" 
