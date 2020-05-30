#!/usr/bin/env bash

set -euo pipefail
shopt -s expand_aliases

SCRIPT_DIR="$(dirname "$(readlink -f "$BASH_SOURCE")")"
TEZOS_DIR="$(readlink -f "$SCRIPT_DIR/ext/tezos")"

export PATH="$TEZOS_DIR:$PATH"

"$TEZOS_DIR/src/bin_node/tezos-sandboxed-node.sh" 1 --connections 1 >/dev/null 2>&1 &
node_pid=$!
sleep 5

if ! kill -0 "$node_pid" >/dev/null 2>&1 ; then
    echo 'There seems to already be a tezos sandboxed node running, exitting' ;
    kill -2 $$ # https://stackoverflow.com/questions/6112540/return-an-exit-code-without-closing-shell
fi

echo "$node_pid" > ".node-pid"

set +u
eval `"$TEZOS_DIR/src/bin_client/tezos-init-sandboxed-client.sh" 1`
set -u
tezos-activate-alpha
tezos-autocomplete
