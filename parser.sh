#/bin/bash
file=$(mktemp) && sed 's/#.*$//g' $1 | sed -E 's/[@:%](|@|%|%%|[_a-zA-Z][_0-9a-zA-Z\.]*)//g' > $file && kast $file
rm $file
