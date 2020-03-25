#!/bin/bash
krun -o none --directory "$(dirname "$(readlink -f "$BASH_SOURCE")")" "$1"
