#!/usr/bin/env bash

set -euo pipefail

notif() { echo "== $@" >&2 ; }
fatal() { echo "[FATAL] $@" ; exit 1 ; }

test_file="$1" ; shift
test_file_extracted="$test_file.extracted"
test_file_input="$test_file.input"

test_file_address="$test_file.address"

[[ -f "$test_file"           ]] || fatal "File doesn't exist: $test_file"
[[ -f "$test_file_extracted" ]] || fatal "File doesn't exist: $test_file_extracted"
[[ -f "$test_file_input"     ]] || fatal "File doesn't exist: $test_file_input"

notif "Fixing Address: $test_file"
SCRIPT_DIR="$(dirname "$(readlink -f "$BASH_SOURCE")")"

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

function extract {
    python3 "$SCRIPT_DIR/extract-group.py" "$test_file_extracted" "$@"
}

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

python3 $SCRIPT_DIR/originate.py $REAL_ADDRESSES > $ORIGINATION_OUTPUTS \
    || fatal "Failed to originate contracts: $test_file"
paste -d '/' <(cut -d'#' -f1 "$REAL_ADDRESSES") <(grep -Po '(?<=New contract )[a-zA-Z0-9_]*' "$ORIGINATION_OUTPUTS") | sed -E 's|(.*)|s/\1/|;s|s///||' > "$ORIGINATION_SUBS"

cat "$FAKE_ADDRESS_SUBS" "$ORIGINATION_SUBS" > "$ALL_SUBS"
sed -f "$ALL_SUBS" "$test_file" > "$test_file_address"
