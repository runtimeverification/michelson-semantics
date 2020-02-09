#!/bin/bash
function output_if_failing {
    bash -c "$1"
    local r="$?"
    if [ $r -ne 0 ] ; then
        echo "$2: (exit code $r)" ;
        exit 1
    fi
}

SCRIPT_DIR="$(dirname $0)"
TEMP_DIR="$(mktemp -d '.tezos-ut.XXXXX' -p "$SCRIPT_DIR")"

trap "rm -rf $TEMP_DIR" EXIT

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

output_if_failing "'$SCRIPT_DIR/contract-expander/run.sh' '$1' > '$EXPANDED_FILE'" "Contract did not expand properly"
output_if_failing "tezos-client typecheck script '$(cat "$EXPANDED_FILE")' --details  >$TYPECHECK_OUTPUT 2>&1" "Contract did not typecheck"
output_if_failing "pcregrep -oM '(?<=\[ )@exitToken[^\\]]*' '$TYPECHECK_OUTPUT' > '$RAW_TYPES'" 'Could not find @exitToken in typecheck output'
sed -E 's/ : /\n/g;s/@%|@%%|%@|[@:%][_a-zA-Z][_0-9a-zA-Z\.%@]*//g' $RAW_TYPES > $TYPES_FILE

tezos-client run script "$(cat $EXPANDED_FILE)" on storage Unit and input Unit --trace-stack > "$EXECUTION" 2>&1
output_if_failing "pcregrep -oM '(?<=\[)\s*Unit\s*@exitToken[^\\]]*' $EXECUTION > $RAW_DATA" "Could not find @exitToken in execution output"
sed -E 's/@%|@%%|%@|[@:%][_a-zA-Z][_0-9a-zA-Z\.%@]*//g' "$RAW_DATA" > "$DATA_FILE"

python "$SCRIPT_DIR/combine.py" $TYPES_FILE $DATA_FILE > $REAL_OUTPUT_FILE
"$SCRIPT_DIR/output-extractor/run.sh" "$1" > $EXPECTED_OUTPUT_FILE

echo | cat "$REAL_OUTPUT_FILE" "$EXPECTED_OUTPUT_FILE" - > "$OUTPUT_FILE"

output_if_failing "'$SCRIPT_DIR/output-compare/run.sh' '$OUTPUT_FILE' > '$COMPARE_FILE'" "Output did not compare correctly"

echo "$1 Passed"
