#!/bin/bash
tezos-client typecheck script "$($(dirname $0)/contract-expander/run.sh $1)" --details 2>&1 | pcregrep -oM "(?<=\[ )@exitToken[^\\]]*" | sed 's/ : /\n/g'
