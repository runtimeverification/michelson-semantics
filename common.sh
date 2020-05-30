set -euo pipefail
SCRIPT_DIR="$(dirname "$(readlink -f "$BASH_SOURCE")")"
export PATH="$SCRIPT_DIR/ext/k/k-distribution/bin:$PATH"
export KOMPILE_OPTS="--backend llvm --gen-bison-parser -ccopt $SCRIPT_DIR/hex.cpp -ccopt $SCRIPT_DIR/time.cpp -ccopt -std=c++14 --hook-namespaces TIME  -I $SCRIPT_DIR/.build -I $SCRIPT_DIR/compat"
export LUA_PATH="$SCRIPT_DIR/ext/pandoc-tangle/?.lua;;"
export TANGLER="$SCRIPT_DIR/ext/pandoc-tangle/tangle.lua"
