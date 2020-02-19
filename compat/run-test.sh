#!/bin/bash
UT="$1"

function output_if_failing {
    bash -c "$1"
    local r="$?"
    if [ $r -ne 0 ] ; then
        echo "$UT: (exit code $r) $2" ;
        exit 1
    fi
}

SCRIPT_DIR="$(dirname $0)"
TEMP_DIR="$(mktemp -d '.tezos-ut.XXXXX' -p "$SCRIPT_DIR")"

#trap "rm -rf $TEMP_DIR" EXIT

KNOWN_FAKES="$SCRIPT_DIR/addresses.txt"

FOUND_ADDRESSES="$TEMP_DIR/addresses"
REAL_ADDRESSES="$TEMP_DIR/contracts"
FAKE_ADDRESSES="$TEMP_DIR/fake_addresses"
FAKE_ADDRESS_SUBS="$TEMP_DIR/fake_address_subs"
FIXED_FAKE_ADDRESS_CONTRACT="$TEMP_DIR/fixed_fake_address"
ORIGINATION_OUTPUTS="$TEMP_DIR/originations"
ORIGINATION_SUBS="$TEMP_DIR/origination_subs"
FIXED_ADDRESS_CONTRACT="$TEMP_DIR/fixed_addrs"
EXPANDED_FILE="$TEMP_DIR/expanded"
TYPECHECK_OUTPUT="$TEMP_DIR/typecheck"
RAW_TYPES="$TEMP_DIR/rawtypes"
TYPES_FILE="$TEMP_DIR/types"
EXECUTION="$TEMP_DIR/execution"
RAW_DATA="$TEMP_DIR/rawdata"
DATA_FILE="$TEMP_DIR/data"
REAL_OUTPUT_FILE="$TEMP_DIR/actual"
EXPECTED_OUTPUT_FILE="$TEMP_DIR/expected"
OUTPUT_FILE="$TEMP_DIR/actual-and-expected"
COMPARE_FILE="$TEMP_DIR/comparison"


grep -o '@Address([^)"]*)' "$UT" | sort | uniq > "$FOUND_ADDRESSES"

OTHER_EXTRACTOR_CMD=$(cat <<EOF
'$SCRIPT_DIR/extractor/run.sh' '$UT' other_contracts true |  
    tr -d '{}' | 
    tr ';' '\n' | 
    sed '/#NoGroup/d' |
    sed 's/^\s*//;s/\s*$//' | 
    sed -E 's/Elt\s*"([^"]*)"\s*(.*)/\1#\2/' | 
    sort | 
    uniq > '$REAL_ADDRESSES'
EOF
)

output_if_failing "$OTHER_EXTRACTOR_CMD" "Failed to extract dependencies on other contracts."
diff --new-line-format="" --unchanged-line-format="" "$FOUND_ADDRESSES" <(cut -d'#' -f 1 "$REAL_ADDRESSES") > "$FAKE_ADDRESSES"
paste -d'/' "$FAKE_ADDRESSES" <(head -n "$(wc -l "$FAKE_ADDRESSES" | grep -o "^[0-9]*")" $KNOWN_FAKES  ) | sed -E 's|(.*)|s/\1/|' > "$FAKE_ADDRESS_SUBS"

output_if_failing "python3 '$SCRIPT_DIR/originate.py' '$REAL_ADDRESSES' > '$ORIGINATION_OUTPUTS'" "Failed to originate contracts"
paste -d '/' <(cut -d'#' -f1 "$REAL_ADDRESSES") <(grep -Po '(?<=New contract )[a-zA-Z0-9_]*' "$ORIGINATION_OUTPUTS") | sed -E 's|(.*)|s/\1/|' > "$ORIGINATION_SUBS"

cat "$FAKE_ADDRESS_SUBS" "$ORIGINATION_SUBS" | sed -f /dev/stdin "$UT" > "$FIXED_ADDRESS_CONTRACT"

output_if_failing "'$SCRIPT_DIR/contract-expander/run.sh' '$FIXED_ADDRESS_CONTRACT' > '$EXPANDED_FILE'" "Contract did not expand properly"
output_if_failing "tezos-client typecheck script '$(cat "$EXPANDED_FILE")' --details  >$TYPECHECK_OUTPUT 2>&1" "Contract did not typecheck"
output_if_failing "pcregrep -oM '(?<=\[ )@exitToken[^\\]]*' '$TYPECHECK_OUTPUT' > '$RAW_TYPES'" 'Could not find @exitToken in typecheck output'
sed -E 's/ : /\n/g;s/@%|@%%|%@|[@:%][_a-zA-Z][_0-9a-zA-Z\.%@]*//g' $RAW_TYPES > $TYPES_FILE


tezos-client run script "$(cat $EXPANDED_FILE)" on storage Unit and input Unit --trace-stack > "$EXECUTION" 2>&1
output_if_failing "pcregrep -oM '(?<=\[)\s*Unit\s*@exitToken[^\\]]*' $EXECUTION > $RAW_DATA" "Could not find @exitToken in execution output"
sed -E 's/@%|@%%|%@|[@:%][_a-zA-Z][_0-9a-zA-Z\.%@]*//g' "$RAW_DATA" > "$DATA_FILE"

python "$SCRIPT_DIR/combine.py" $TYPES_FILE $DATA_FILE > $REAL_OUTPUT_FILE
"$SCRIPT_DIR/extractor/run.sh" "$1" 'output' 'false' | sed -f "$ORIGINATION_SUBS" > $EXPECTED_OUTPUT_FILE

echo | cat "$REAL_OUTPUT_FILE" "$EXPECTED_OUTPUT_FILE" - > "$OUTPUT_FILE"

output_if_failing "'$SCRIPT_DIR/output-compare/run.sh' '$OUTPUT_FILE' > '$COMPARE_FILE'" "Output did not compare correctly"

echo "$1 Passed"
