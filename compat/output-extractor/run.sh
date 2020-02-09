#!/bin/bash
krun --directory "$(dirname $0)" -o program $1 | tr '\n' ' ' | sed 's/.*<k>\(.*\)<\/k>.*/\1/'
exit ${PIPESTATUS[0]}
