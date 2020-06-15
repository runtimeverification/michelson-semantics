#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(dirname "$(readlink -f "$BASH_SOURCE")")"

notif() { echo "== $@" >&2 ; }
fatal() { echo "[FATAL] $@" ; exit 1 ; }

test_file="$1" ; shift
test_file_extracted="$test_file.extracted"
test_file_input="$test_file.input"
test_file_expanded="$test_file.expanded"

test_file_output="$test_file.output"

[[ -f "$test_file"       ]] || fatal "File doesn't exist: $test_file"
[[ -f "$test_file_input" ]] || fatal "File doesn't exist: $test_file_input"

extract() {
    python3 "$SCRIPT_DIR/extract-group.py" "$test_file_extracted" "$@"
}

TYPECHECK_OUTPUT="$TEMP_DIR/typecheck"
RAW_TYPES="$TEMP_DIR/rawtypes"
TYPES_FILE="$TEMP_DIR/types"
EXECUTION="$TEMP_DIR/execution"
ERROR_FILE="$TEMP_DIR/error"
RAW_DATA="$TEMP_DIR/rawdata"
DATA_FILE="$TEMP_DIR/data"
FIXED_ADDRS_OUTPUT="$TEMP_DIR/others_fixed"
REAL_OUTPUT_FILE="$TEMP_DIR/actual"
EXPECTED_OUTPUT_FILE="$TEMP_DIR/expected"

$TEZOS_CLIENT typecheck script "$(cat $test_file_expanded)" --details  > $TYPECHECK_OUTPUT 2>&1 \
    || fatal "Contract did not typecheck: $test_file"

pcregrep -oM '(?<=\[)\s*@exitToken[^]]*' "$TYPECHECK_OUTPUT" > "$RAW_TYPES"
FOUND_TYPES="$?"

sed -E 's/ : /\n/g;s/@%|@%%|%@|[@:%][_a-zA-Z][_0-9a-zA-Z\.%@]*//g' "$RAW_TYPES" > "$TYPES_FILE"

AMOUNT="$(python -c 'import sys ; print("0" if len(sys.argv) < 2 or sys.argv[1].strip() == "" else "%f" % (float(sys.argv[1]) / 1000000.0))' "$(extract amount true)")"

TRACE_CLI=()
if [ ! -z "$FAKE_SOURCE" ] ; then
    REAL_SOURCE="$(grep "$FAKE_SOURCE" $ORIGINATION_SUBS | sed -E 's|s/[^/]*/([^/]*)/|\1|')" ;
    if [ ! -z "$REAL_SOURCE" ] ; then
        TRACE_CLI+=(--payer $REAL_SOURCE)
    fi ;
fi

if [ ! -z "$FAKE_SENDER" ] ; then
    REAL_SENDER="$(grep "$FAKE_SENDER" $ORIGINATION_SUBS | sed -E 's|s/[^/]*/([^/]*)/|\1|')" ;
    if [ ! -z "$REAL_SENDER" ] ; then
        TRACE_CLI+=(--source $REAL_SENDER)
    fi ;
fi

$TEZOS_CLIENT run script "$(cat $test_file_expanded)" on storage Unit and input "$(cat "$test_file_input")" --amount "$AMOUNT" --trace-stack "${TRACE_CLI[@]}" > "$EXECUTION" 2>&1 || true
# For some reason, the cli argument for "SENDER" is "--source" and "SOURCE" is "--payer"

pcregrep -oM '(?<=\[)\s*Unit\s*@exitToken[^]]*' "$EXECUTION" > "$RAW_DATA"
FOUND="$?"

sed -E "s/@%|@%%|%@|[@:%][_a-zA-Z][_0-9a-zA-Z\.%@]*//g" "$RAW_DATA" > "$DATA_FILE" ;
extract 'other_contracts' 'false' '{}' | sed -f "$ALL_SUBS" > "$FIXED_ADDRS_OUTPUT" ;
extract 'output' 'false' '{}' | sed -f "$ALL_SUBS" > "$EXPECTED_OUTPUT_FILE" ;

if [ "$FOUND" -eq "0" ]; then
    if [ "$FOUND_TYPES" -ne "0" ]; then
        echo "Found unexpected exitToken" ;
        exit 1 ;
    fi ;
    python "$SCRIPT_DIR/combine.py" $TYPES_FILE $DATA_FILE > $REAL_OUTPUT_FILE ;
else
    if grep -q "script reached FAILWITH instruction" "$EXECUTION" >/dev/null 2>&1; then
        grep -oP "(?<=^with ).*$" "$EXECUTION" | sed -E 's/(.*)/real_output ( Failed \1 ) ;/' > "$REAL_OUTPUT_FILE" ;
    elif grep -o "Overflowing addition of [0-9.]* tez and [0-9.]* tez" "$EXECUTION" >"$ERROR_FILE" 2>/dev/null; then
        sed -E 's/Overflowing addition of ([0-9.]*) tez and ([0-9.]*) tez/real_output ( MutezOverflow \1 \2 ) ;/' "$ERROR_FILE" | tr -d '.' > "$REAL_OUTPUT_FILE" ;
    elif grep -o "Underflowing subtraction of [0-9.]* tez and [0-9.]* tez" "$EXECUTION" >"$ERROR_FILE" 2>/dev/null; then
        sed -E 's/Underflowing subtraction of ([0-9.]*) tez and ([0-9.]*) tez/real_output ( MutezUnderflow \1 \2 ) ;/' "$ERROR_FILE" | tr -d '.' > "$REAL_OUTPUT_FILE" ;
    elif grep -q "unexpected arithmetic overflow" "$EXECUTION" >/dev/null 2>&1; then
        cat "$EXECUTION" | tr '\n' ' ' | grep -o "\[[^]]*\]" | tail -n 1 | tr -d '[]' | sed -E 's/(.*)/real_output ( GeneralOverflow \1 ) ;/' > "$REAL_OUTPUT_FILE"
    fi ;
fi

echo | cat "$FIXED_ADDRS_OUTPUT" "$REAL_OUTPUT_FILE" "$EXPECTED_OUTPUT_FILE" - > "$test_file_output" ;
