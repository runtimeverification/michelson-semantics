SCRIPT_DIR="$(dirname "$(readlink -f "$BASH_SOURCE")")"
PLUG_SRC_DIR="$SCRIPT_DIR/ext/blockchain-k-plugin/plugin-c"

# declare compiler options without prefix
C_FILES=("$SCRIPT_DIR/hex.cpp" "$SCRIPT_DIR/time.cpp" "$PLUG_SRC_DIR/blake2.cpp" "$PLUG_SRC_DIR/hash.cpp" "$PLUG_SRC_DIR/secp256k1.cpp")
C_LIBS=(-lcryptopp -lprocps -lsecp256k1)
C_OTHERS=(-std=c++14)
HOOKS="TIME HASH SECP256K1"

# generate the prefixed compiler options for kompile
C_OPTS=$(for opt in ${C_FILES[@]} ${C_LIBS[@]} ${C_OTHERS[@]}; do echo -n "-ccopt $opt "; done)

# export relevant environment variables
export PATH="$SCRIPT_DIR/ext/k/k-distribution/bin:$PATH"
export LUA_PATH="$SCRIPT_DIR/ext/pandoc-tangle/?.lua;;"
export TANGLER="$SCRIPT_DIR/ext/pandoc-tangle/tangle.lua"
export KOMPILE_OPTS=(--backend llvm --gen-bison-parser $C_OPTS --hook-namespaces "$HOOKS" --debug -I "$SCRIPT_DIR/.build" -I "$SCRIPT_DIR/compat")
