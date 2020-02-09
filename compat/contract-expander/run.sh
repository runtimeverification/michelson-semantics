#!/bin/bash
krun --directory "$(dirname $0)" -o program $1 | tr '\n' ' ' | sed -E 's/.*<k>(.*)<\/k>.*/\1/' | sed 's/(\s*\(0x\w*\|[+-]*[0-9][0-9]*\|"\([^"]\|\\"\)*"\)\s*)/\1/g'
exit ${PIPESTATUS[0]}
