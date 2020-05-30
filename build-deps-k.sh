#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
source "$SCRIPT_DIRECTORY/common.sh"

CURRENT_DIRECTORY="$(pwd)"

trap "cd '$CURRENT_DIRECTORY'" EXIT

K_DIRECTORY="$SCRIPT_DIRECTORY/ext/k"

git submodule update --init --recursive

cd "$K_DIRECTORY"
mvn package -DskipTests
