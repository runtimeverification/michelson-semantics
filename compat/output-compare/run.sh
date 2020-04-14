#!/bin/bash
krun --directory "$(dirname "$(readlink -f "$BASH_SOURCE")")" $*
