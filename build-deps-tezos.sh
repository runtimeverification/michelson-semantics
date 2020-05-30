#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"

CURRENT_DIRECTORY="$(pwd)"

trap "cd '$CURRENT_DIRECTORY'" EXIT

TEZOS_DIRECTORY="$SCRIPT_DIRECTORY/ext/tezos"

git submodule update --init --recursive

cd "$TEZOS_DIRECTORY"
# ocaml setup
# --bare               - do not install any base ocaml installation now
# --no-setup           - do not ask to setup hooks
# --disable-sandboxing - do not use bwrap which is not supported on our container
opam init --bare --no-setup --disable-sandboxing
make build-deps
eval `opam env`
make
