#!/usr/bin/env bash

set -euo pipefail

notif() { echo "== $@" >&2 ; }
fatal() { echo "[FATAL] $@" ; exit 1 ; }

test_file="$1" ; shift
test_file_extracted="$test_file.extracted"
test_file_input="$test_file.input"

notif "Cross Validating: $test_file"
SCRIPT_DIR="$(dirname "$(readlink -f "$BASH_SOURCE")")"

function output_if_failing {
    bash -c "$1"
    local r="$?"
    if [ $r -ne 0 ] ; then
        echo "$test_file: (exit code $r) $2" ;
        exit 1
    fi
}

TEMP_DIR="$SCRIPT_DIR/.failure"

rm -rf "$TEMP_DIR"
mkdir -p "$TEMP_DIR"

KNOWN_FAKES="$SCRIPT_DIR/addresses.txt"

FOUND_ADDRESSES="$TEMP_DIR/addresses"
REAL_ADDRESSES_UNSORTED="$TEMP_DIR/contracts_unsorted"
REAL_ADDRESSES="$TEMP_DIR/contracts"
FAKE_ADDRESSES="$TEMP_DIR/fake_addresses"
FAKE_ADDRESS_SUBS="$TEMP_DIR/fake_address_subs"
FIXED_FAKE_ADDRESS_CONTRACT="$TEMP_DIR/fixed_fake_address"
ORIGINATION_OUTPUTS="$TEMP_DIR/originations"
ORIGINATION_SUBS="$TEMP_DIR/origination_subs"
ALL_SUBS="$TEMP_DIR/subs"
FIXED_ADDRESS_CONTRACT="$TEMP_DIR/fixed_addrs"
EXPANDED_FILE="$TEMP_DIR/expanded"
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
OUTPUT_FILE="$TEMP_DIR/actual-and-expected"
COMPARE_FILE="$TEMP_DIR/comparison"

function extract {
    python3 "$SCRIPT_DIR/extract-group.py" "$test_file_extracted" "$@"
}

if ! grep -o '@Address([^)"]*)' "$test_file" | sort | uniq > "$FOUND_ADDRESSES"; then
    touch "$FOUND_ADDRESSES"
fi

# Point is to cross-validate our results with the reference interpreter.
# Reference interpreter does not understand our format, we need to convert from our format to theirs.
#
# extractor, expander, input-creator all convert our format to theirs.
# extractor turns the unit tests into a structured JSON for easily grabbing the parts.
# expander takes our account address format and expands it out into proper Tezos account addresses.
# input-creator takes the version with expanded addresses and converts it into their format.
#
# output-compare is a slight extension of our syntax to compare the output stacks for equality between our format and theirs.

extract other_contracts true | tr -d '{}' | tr ';' '\n' | sed -E 's/^\s*//;s/\s*$//;s/Elt\s*"([^"]*)"\s*(.*)/\1#\2/;/^\s*$/d' > "$REAL_ADDRESSES_UNSORTED"

FAKE_SENDER=$(extract sender true)
if [ ! -z $FAKE_SENDER ] && ! ( grep "$FAKE_SENDER" "$REAL_ADDRESSES_UNSORTED" ); then
    echo "$FAKE_SENDER#unit" >> "$REAL_ADDRESSES_UNSORTED"
fi

FAKE_SOURCE=$(extract source true)
if [ ! -z $FAKE_SOURCE ] && ! ( grep "$FAKE_SOURCE" "$REAL_ADDRESSES_UNSORTED" ); then
    echo "$FAKE_SOURCE#unit" >> "$REAL_ADDRESSES_UNSORTED"
fi

FAKE_SELF=$(extract self true)
tezos-client run script "parameter unit ; storage (option address) ; code { DROP ; SELF ; ADDRESS ; CONTRACT unit ; IF_SOME { ADDRESS ; SOME ; NIL operation ; PAIR } {FAIL} }" on storage None and input Unit
REAL_SELF="$(tezos-client run script "parameter unit ; storage (option address) ; code { DROP ; SELF ; ADDRESS ; CONTRACT unit ; IF_SOME { ADDRESS ; SOME ; NIL operation ; PAIR } {FAIL} }" on storage None and input Unit 2>&1 | grep "Some" | sed -E 's/\s*\(Some "([^"]*)"\)\s*/\1/')"

sort "$REAL_ADDRESSES_UNSORTED" | uniq > "$REAL_ADDRESSES"

diff --new-line-format="" --unchanged-line-format="" "$FOUND_ADDRESSES" <(cut -d'#' -f 1 "$REAL_ADDRESSES") > "$FAKE_ADDRESSES"

if [ ! -z "$FAKE_SELF" ] ; then
    sed -i "/$FAKE_SELF/d" "$FAKE_ADDRESSES" ;
fi

paste -d'/' "$FAKE_ADDRESSES" <(head -n "$(wc -l "$FAKE_ADDRESSES" | grep -o "^[0-9]*")" $KNOWN_FAKES  ) | sed -E 's|(.*)|s/\1/|' > "$FAKE_ADDRESS_SUBS"

if [ ! -z $FAKE_SELF ] ; then
    echo "s|$FAKE_SELF|$REAL_SELF|" >> "$FAKE_ADDRESS_SUBS"
fi

output_if_failing "python3 '$SCRIPT_DIR/originate.py' '$REAL_ADDRESSES' > '$ORIGINATION_OUTPUTS'" "Failed to originate contracts"
paste -d '/' <(cut -d'#' -f1 "$REAL_ADDRESSES") <(grep -Po '(?<=New contract )[a-zA-Z0-9_]*' "$ORIGINATION_OUTPUTS") | sed -E 's|(.*)|s/\1/|;s|s///||' > "$ORIGINATION_SUBS"



cat "$FAKE_ADDRESS_SUBS" "$ORIGINATION_SUBS" > "$ALL_SUBS"
sed -f "$ALL_SUBS" "$test_file" > "$FIXED_ADDRESS_CONTRACT"

notif "Expanding: $test_file"
./kmich run --backend contract-expander $FIXED_ADDRESS_CONTRACT --output none > $EXPANDED_FILE \
    || fatal "Expanding failed: $test_file"
output_if_failing "tezos-client typecheck script '$(cat "$EXPANDED_FILE")' --details  >$TYPECHECK_OUTPUT 2>&1" "Contract did not typecheck"

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

tezos-client run script "$(cat $EXPANDED_FILE)" on storage Unit and input "$(cat "$test_file_input")" --amount "$AMOUNT" --trace-stack "${TRACE_CLI[@]}" > "$EXECUTION" 2>&1 || true
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


echo | cat "$FIXED_ADDRS_OUTPUT" "$REAL_OUTPUT_FILE" "$EXPECTED_OUTPUT_FILE" - > "$OUTPUT_FILE" ;

notif "Comparing output: $test_file"
./kmich run --backend output-compare $OUTPUT_FILE --output none > $COMPARE_FILE \
    || fatal "Comparing output failed: $test_file"

echo "$test_file Passed"
