#!/bin/bash
krun --directory "$(dirname $0)" -o program $1 | tr '\n' ' ' | sed -E 's/.*<k>(.*)<\/k>.*/\1/;s/\(\s*(0x\w*|\d+|"([^"]|\\")*")\s*\)/\1/g'
exit ${PIPESTATUS[0]}
