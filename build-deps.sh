#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
source "$SCRIPT_DIRECTORY/common.sh"

CURRENT_DIRECTORY="$(pwd)"

trap "cd '$CURRENT_DIRECTORY'" EXIT

./build-deps-k.sh
./build-deps-tezos.sh
