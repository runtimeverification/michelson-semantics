#!/bin/bash
SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
krun --directory "SCRIPT_DIRECTORY" "$1" -o none
