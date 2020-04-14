#!/bin/bash
DEFN_DIR="$(dirname "$(readlink -f "$BASH_SOURCE")")/.$1/$1-kompiled/"
TEMP_DIR="$(mktemp -d)"
trap "rm -rf $TEMP_DIR" EXIT
"$DEFN_DIR/parser_PGM" "$2" > "$TEMP_DIR/in.kore"
llvm-krun -d "$DEFN_DIR" -c PGM "$TEMP_DIR/in.kore" Pgm korefile -c IO '\dv{SortString{}}("on")' String kore -o "$TEMP_DIR/out.kore"
RET="$?"

if [ $RET -ne "0" ] ; then
    kast -d "$DEFN_DIR/.." -i kore -o pretty "$TEMP_DIR/out.kore" > /dev/stderr;
fi

exit $RET
