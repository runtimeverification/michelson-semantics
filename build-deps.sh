#!/bin/bash
set -e
SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
source "$SCRIPT_DIRECTORY/common.sh"

CURRENT_DIRECTORY="$(pwd)"

trap "cd '$CURRENT_DIRECTORY'" EXIT

K_DIRECTORY="$SCRIPT_DIRECTORY/ext/k"
TEZOS_DIRECTORY="$SCRIPT_DIRECTORY/ext/tezos"

git submodule update --init --recursive
cd "$K_DIRECTORY"
mvn package
cd "$TEZOS_DIRECTORY"
opam init --bare
make build-deps
eval `opam env`
make
