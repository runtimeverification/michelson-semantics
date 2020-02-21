#!/bin/bash
SCRIPT_DIR=
./tezos/tezos-codec decode 005-PsBabyM1.operation.internal from $(echo $1 | sed 's/0x//')
