#!/bin/bash
krun --directory "$(dirname $0)" -o program $1 | 
    tr '\n' ' ' | 
    sed 's/.*<k>\(.*\)<\/k>.*/\1/' | 
    tr -d '{}' | 
    tr ';' '\n' | 
    sed 's/^\s*//;s/\s*$//' |
    sed -E 's/Elt\s*"([^"]*)"\s*(.*)/\1#\2/' |
    sort |
    uniq
RET="${PIPESTATUS[0]}"
echo
exit $RET
