#!/usr/bin/env bash

set -euo pipefail
shopt -s expand_aliases

notif() { echo "== $@" >&2 ; }
fatal() { echo "[FATAL] $@" ; exit 1 ; }

SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
TEZOS_DIR="$(readlink -f "$SCRIPT_DIRECTORY/ext/tezos")"
export PATH="$TEZOS_DIR:$PATH"

TEZOS_NODE_PID=

list_files() {
    if [[ "$#" -gt 0 ]]; then
        while [[ "$#" -gt 0 ]]; do
            echo "$1"
            shift
        done
    else
        for f in $(find $TEST_DIR -name '*.tzt'); do
            fshort="${f#$TEST_DIR}"
            if ! grep $fshort $FAILING_FILE &> /dev/null; then
                echo $fshort
            fi
        done
    fi
}

start_tezos() {
    "$TEZOS_DIR/src/bin_node/tezos-sandboxed-node.sh" 1 --connections 1 >/dev/null 2>&1 &
    TEZOS_NODE_PID=$!
    sleep 5

    if ! kill -0 "$TEZOS_NODE_PID" >/dev/null 2>&1 ; then
        echo 'There seems to already be a tezos sandboxed node running, exitting' ;
        kill -2 $$ # https://stackoverflow.com/questions/6112540/return-an-exit-code-without-closing-shell
    fi

    set +u
    eval $("$TEZOS_DIR/src/bin_client/tezos-init-sandboxed-client.sh" 1)
    set -u

    tezos-activate-alpha
    tezos-autocomplete
}

stop_tezos() {
    kill -15 "$TEZOS_NODE_PID"
}

start_tezos

FAILING_FILE="$SCRIPT_DIRECTORY/tests/failing.cross"
TEST_DIR="$SCRIPT_DIRECTORY/tests/unit"

command="$1" ; shift

for test in $(list_files "$@"); do
  notif "Running '$command': $test"
  case "$command" in
    fix-address) $SCRIPT_DIRECTORY/fix-address.sh "$test" ;;
    run-tezos)   $SCRIPT_DIRECTORY/run-tezos.sh   "$test" ;;
    *) fatal "Unknown command: $command"                  ;;
  esac
done

stop_tezos
