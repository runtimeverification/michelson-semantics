#!/bin/bash
set -e
SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"
source "$SCRIPT_DIRECTORY/common.sh"

BUILD_DIRECTORY="$SCRIPT_DIRECTORY/.build"

rm -rf "$BUILD_DIRECTORY"
mkdir -p "$BUILD_DIRECTORY"


MAIN_DEF_FILES=$(cat <<EOF
michelson-common.md 
michelson-config.md 
michelson-internal-syntax.md 
michelson.md 
michelson-syntax.md 
unit-test.md 
unit-test-syntax.md
symbolic-unit-test.md 
symbolic-unit-test-syntax.md
EOF
)

for f in $MAIN_DEF_FILES ; do
    pandoc --from markdown --to "$TANGLER" --metadata=code:.k "$SCRIPT_DIRECTORY/$f" > "$BUILD_DIRECTORY/$(basename $f .md).k"
done

kompile --backend haskell --directory "$SCRIPT_DIRECTORY" "$BUILD_DIRECTORY/symbolic-unit-test.k"
