SCRIPT_DIR="$(dirname "$(readlink -f "$BASH_SOURCE")")"
export PATH="$(dirname "$(readlink -f "$BASH_SOURCE")")/ext/k/k-distribution/bin:$PATH"
export KOMPILE_OPTS="--backend llvm -ccopt $SCRIPT_DIR/time.cpp -ccopt -std=c++14 --hook-namespaces TIME"
