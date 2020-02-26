#!/bin/bash

shopt -s expand_aliases

SCRIPT_DIR="$(dirname "$(readlink -f "$BASH_SOURCE")")"

export PATH="$SCRIPT_DIR/tezos:$PATH"

"$SCRIPT_DIR/tezos/src/bin_node/tezos-sandboxed-node.sh" 1 --connections 1 >/dev/null 2>&1 &
node_pid=$!
sleep 5

if ! kill -0 "$node_pid" >/dev/null 2>&1 ; then
    echo 'There seems to already be a tezos sandboxed node running, exitting' ;
    kill -2 $$ # https://stackoverflow.com/questions/6112540/return-an-exit-code-without-closing-shell
fi

echo "$node_pid" > ".node-pid"

eval `"$SCRIPT_DIR/tezos/src/bin_client/tezos-init-sandboxed-client.sh" 1`
tezos-activate-alpha
tezos-autocomplete
