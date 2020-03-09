#!/bin/bash
krun --directory "$(dirname $0)" -o program $1 | tr '\n' ' ' | sed 's/\.AnnotationList//g;s/({/{/g;s/})/}/g;s/true/True/g;s/false/False/g;s/.*<k>\(.*\)<\/k>.*/\1/;s/(\s*\(0x\w*\|[+-]*[0-9][0-9]*\|"\([^"]\|\\"\)*"\)\s*)/\1/g;s/^\s*//;s/\s*$//'
exit ${PIPESTATUS[0]}
