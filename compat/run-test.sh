#!/bin/bash

UT="$1"
SCRIPT_DIR="$(dirname "$(readlink -f "$BASH_SOURCE")")"
FAIL_DIR="$SCRIPT_DIR/.failure"


function output_if_failing {
    bash -c "$1"
    local r="$?"
    if [ $r -ne 0 ] ; then
        echo "$UT: (exit code $r) $2" ;
        rm -rf "$FAIL_DIR" ;
        cp -r "$TEMP_DIR" "$FAIL_DIR" ;
        exit 1
    fi
}

TEMP_DIR="$(mktemp -d '.tezos-ut.XXXXX' -p "$SCRIPT_DIR")"

trap "rm -rf $TEMP_DIR" EXIT

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
INPUT_FILE="$TEMP_DIR/input"
EXECUTION="$TEMP_DIR/execution"
RAW_DATA="$TEMP_DIR/rawdata"
DATA_FILE="$TEMP_DIR/data"
FIXED_ADDRS_OUTPUT="$TEMP_DIR/others_fixed"
REAL_OUTPUT_FILE="$TEMP_DIR/actual"
EXPECTED_OUTPUT_FILE="$TEMP_DIR/expected"
OUTPUT_FILE="$TEMP_DIR/actual-and-expected"
COMPARE_FILE="$TEMP_DIR/comparison"


grep -o '@Address([^)"]*)' "$UT" | sort | uniq > "$FOUND_ADDRESSES"

OTHER_EXTRACTOR_CMD=$(cat <<EOF
'$SCRIPT_DIR/extractor/run.sh' '$UT' other_contracts true |  
    tr -d '{}' | 
    tr ';' '\n' | 
    sed -E '/#NoGroup/d;s/^\s*//;s/\s*$//;s/Elt\s*"([^"]*)"\s*(.*)/\1#\2/;/^\s*$/d' > '$REAL_ADDRESSES_UNSORTED'
EOF
)

output_if_failing "$OTHER_EXTRACTOR_CMD" "Failed to extract dependencies on other contracts."

FAKE_SENDER="$("$SCRIPT_DIR/extractor/run.sh" "$UT" 'sender' 'true' | sed '/#NoGroup/d;s/"\([^"]*\)"/\1/')"
if [ ! -z $FAKE_SENDER ] && ! ( grep "$FAKE_SENDER" "$REAL_ADDRESSES_UNSORTED" ); then
    echo "$FAKE_SENDER#unit" >> "$REAL_ADDRESSES_UNSORTED"
fi

FAKE_SOURCE="$("$SCRIPT_DIR/extractor/run.sh" "$UT" 'source' 'true' | sed '/#NoGroup/d;s/"\([^"]*\)"/\1/')"
if [ ! -z $FAKE_SOURCE ] && ! ( grep "$FAKE_SOURCE" "$REAL_ADDRESSES_UNSORTED" ); then
    echo "$FAKE_SOURCE#unit" >> "$REAL_ADDRESSES_UNSORTED"
fi

FAKE_SELF="$("$SCRIPT_DIR/extractor/run.sh" "$UT" 'self' 'true' | sed '/#NoGroup/d;s/"\([^"]*\)"/\1/')"
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


REAL_SENDER="$(grep "$FAKE_SENDER" $ORIGINATION_SUBS | sed -E 's|s/[^/]*/([^/]*)/|\1|')"
REAL_SOURCE="$(grep "$FAKE_SOURCE" $ORIGINATION_SUBS | sed -E 's|s/[^/]*/([^/]*)/|\1|')"

cat "$FAKE_ADDRESS_SUBS" "$ORIGINATION_SUBS" > "$ALL_SUBS"
sed -f "$ALL_SUBS" "$UT" > "$FIXED_ADDRESS_CONTRACT"

output_if_failing "'$SCRIPT_DIR/contract-expander/run.sh' '$FIXED_ADDRESS_CONTRACT' > '$EXPANDED_FILE'" "Contract did not expand properly"
output_if_failing "tezos-client typecheck script '$(cat "$EXPANDED_FILE")' --details  >$TYPECHECK_OUTPUT 2>&1" "Contract did not typecheck"
output_if_failing "pcregrep -oM '(?<=\[ )@exitToken[^\\]]*' '$TYPECHECK_OUTPUT' > '$RAW_TYPES'" 'Could not find @exitToken in typecheck output'
sed -E 's/ : /\n/g;s/@%|@%%|%@|[@:%][_a-zA-Z][_0-9a-zA-Z\.%@]*//g' $RAW_TYPES > $TYPES_FILE


AMOUNT="$(output_if_failing "'$SCRIPT_DIR/extractor/run.sh' '$UT' amount true" "Failed to extract amount")"
AMOUNT="$(python -c "import sys ; print('%f' % (float(sys.argv[1]) / 1000000.0) if len(sys.argv) > 1 else 0)" $AMOUNT)"

if [ ! -z "$REAL_SOURCE" ] ; then
    SOURCE_CLI="--payer $REAL_SOURCE"
fi

if [ ! -z "$REAL_SENDER" ] ; then
    SENDER_CLI="--source $REAL_SENDER"
fi

output_if_failing "'$SCRIPT_DIR/input-creator/run.sh' '$UT' > '$INPUT_FILE'" "Could not generate input"

tezos-client run script "$(cat $EXPANDED_FILE)" on storage Unit and input $(cat "$INPUT_FILE") --amount "$AMOUNT" --trace-stack $SENDER_CLI $SOURCE_CLI > "$EXECUTION" 2>&1
# For some reason, the cli argument for "SENDER" is "--source" and "SOURCE" is "--payer"

output_if_failing "pcregrep -oM '(?<=\[)\s*Unit\s*@exitToken[^\\]]*' $EXECUTION > $RAW_DATA" "Could not find @exitToken in execution output"
sed -E 's/@%|@%%|%@|[@:%][_a-zA-Z][_0-9a-zA-Z\.%@]*//g' "$RAW_DATA" > "$DATA_FILE"

python "$SCRIPT_DIR/combine.py" $TYPES_FILE $DATA_FILE > $REAL_OUTPUT_FILE
"$SCRIPT_DIR/extractor/run.sh" "$1" 'output' 'false' | sed -f "$ALL_SUBS" > "$EXPECTED_OUTPUT_FILE"

"$SCRIPT_DIR/extractor/run.sh" "$UT" 'other_contracts' 'false' 2>/dev/null | sed -f "$ALL_SUBS" > "$FIXED_ADDRS_OUTPUT"

echo | cat "$FIXED_ADDRS_OUTPUT" "$REAL_OUTPUT_FILE" "$EXPECTED_OUTPUT_FILE" - > "$OUTPUT_FILE"

output_if_failing "'$SCRIPT_DIR/output-compare/run.sh' '$OUTPUT_FILE' > '$COMPARE_FILE'" "Output did not compare correctly"

echo "$1 Passed"
