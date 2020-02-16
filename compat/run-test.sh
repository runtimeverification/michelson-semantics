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

FOUND_ADDRESSES="$TEMP_DIR/contracts"
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


output_if_failing "'$SCRIPT_DIR/other-extractor/run.sh' '$UT' > '$FOUND_ADDRESSES'" "Failed to extract dependencies on other contracts."
output_if_failing "python3 '$SCRIPT_DIR/originate.py' '$FOUND_ADDRESSES' > '$ORIGINATION_OUTPUTS'" "Failed to originate contracts"
paste -d '/' <(cut -d'#' -f1 "$FOUND_ADDRESSES") <(grep -Po '(?<=New contract )[a-zA-Z0-9_]*' "$ORIGINATION_OUTPUTS") | sed -E 's|(.*)|s/\1/|' > "$ORIGINATION_SUBS"
sed -f "$ORIGINATION_SUBS" "$UT" > "$FIXED_ADDRESS_CONTRACT"

output_if_failing "'$SCRIPT_DIR/contract-expander/run.sh' '$FIXED_ADDRESS_CONTRACT' > '$EXPANDED_FILE'" "Contract did not expand properly"
output_if_failing "tezos-client typecheck script '$(cat "$EXPANDED_FILE")' --details  >$TYPECHECK_OUTPUT 2>&1" "Contract did not typecheck"
output_if_failing "pcregrep -oM '(?<=\[ )@exitToken[^\\]]*' '$TYPECHECK_OUTPUT' > '$RAW_TYPES'" 'Could not find @exitToken in typecheck output'
sed -E 's/ : /\n/g;s/@%|@%%|%@|[@:%][_a-zA-Z][_0-9a-zA-Z\.%@]*//g' $RAW_TYPES > $TYPES_FILE


tezos-client run script "$(cat $EXPANDED_FILE)" on storage Unit and input Unit --trace-stack > "$EXECUTION" 2>&1
output_if_failing "pcregrep -oM '(?<=\[)\s*Unit\s*@exitToken[^\\]]*' $EXECUTION > $RAW_DATA" "Could not find @exitToken in execution output"
sed -E 's/@%|@%%|%@|[@:%][_a-zA-Z][_0-9a-zA-Z\.%@]*//g' "$RAW_DATA" > "$DATA_FILE"

python "$SCRIPT_DIR/combine.py" $TYPES_FILE $DATA_FILE > $REAL_OUTPUT_FILE
"$SCRIPT_DIR/output-extractor/run.sh" "$1" | sed -f "$ORIGINATION_SUBS" > $EXPECTED_OUTPUT_FILE

echo | cat "$REAL_OUTPUT_FILE" "$EXPECTED_OUTPUT_FILE" - > "$OUTPUT_FILE"

output_if_failing "'$SCRIPT_DIR/output-compare/run.sh' '$OUTPUT_FILE' > '$COMPARE_FILE'" "Output did not compare correctly"

echo "$1 Passed"
