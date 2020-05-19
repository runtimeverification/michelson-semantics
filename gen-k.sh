#!/usr/bin/env bash

SCRIPT_DIRECTORY="$(dirname "$(readlink -f "$BASH_SOURCE")")"

export BUILD_DIRECTORY="$SCRIPT_DIRECTORY/.build"

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
michelson-types.md
michelson-claims.md
symbolic-unit-test.md
symbolic-configuration.md
symbolic-unit-test-syntax.md
EOF
)

for f in $MAIN_DEF_FILES ; do
    pandoc --from markdown --to "$TANGLER" --metadata=code:.k "$SCRIPT_DIRECTORY/$f" > "$BUILD_DIRECTORY/$(basename $f .md).k"
done
