#/bin/bash
file=$(mktemp) && sed 's/#.*$//g' $1 >$file && kast $file
rm $file
