#!/bin/bash
krun --directory "$(dirname $0)" -o program $1 | tr '\n' ' ' | sed -E 's/.*<k>\s*((\S*\s*)*\S*)\s*<\/k>.*/\1/' | sed 's/\s*$//'
exit "${PIPESTATUS[0]}"
