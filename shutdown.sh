#!/usr/bin/env bash

set -euo pipefail

NODE_PID="$(dirname "$(readlink -f "$BASH_SOURCE")")/.node-pid"
kill -15 "$(cat $NODE_PID)"
rm "$NODE_PID"
