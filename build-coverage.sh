#!/usr/bin/env bash
"$(dirname "$(readlink -f "$BASH_SOURCE")")/build.sh" --coverage
