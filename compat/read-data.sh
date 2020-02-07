#!/bin/bash
tezos-client run script "$($(dirname $0)/contract-expander/run.sh $1)" on storage Unit and input Unit --trace-stack 2>&1 | pcregrep -oM "(?<=\[)\s*Unit\s*@exitToken[^\\]]*" | sed -E 's/^\s*((\S*\s*)*\S*)\s*$/\1/'
