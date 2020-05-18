#!/usr/bin/env bash
SCRIPT_DIR="$(dirname "$(readlink -f "$BASH_SOURCE")")"
COMPAT_DIR="$SCRIPT_DIR/compat/"

if [[ -z "$NO_PARSER" ]] && which tezos-client>/dev/null 2>&1; then
    JSON_FILE="$(mktemp)"
    trap "rm $JSON_FILE" EXIT

    "$COMPAT_DIR/run.sh" extractor "$1" > "$JSON_FILE"
    CODE="$(python3 "$COMPAT_DIR/extract-group.py" "$JSON_FILE" code true)"
    CONTRACT="$(python3 "$COMPAT_DIR/extract-group.py" "$JSON_FILE" contract true)"

    GROUP="$([[ ! -z "$CODE" ]] && echo code || echo contract)"
    CONTENTS="$CODE $CONTRACT"

    echo "$GROUP $(tezos-client expand macros in "$CONTENTS" 2>/dev/null) ;" | cat - "$1" | "$SCRIPT_DIR/unit-test-kompiled/parser_PGM"
else
    "$SCRIPT_DIR/unit-test-kompiled/parser_PGM" "$1"
fi
