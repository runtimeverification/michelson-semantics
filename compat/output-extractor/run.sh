#!/bin/bash
krun --directory "$(dirname $0)" -o program $1 | tr '\n' ' ' | sed 's/true/True/;s/false/False/;s/.*<k>\(.*\)<\/k>.*/\1/'
exit ${PIPESTATUS[0]}
