#!/usr/bin/python3
import subprocess, sys, os
TEMPLATE = os.environ['TEZOS_CLIENT'] + " originate contract {0} transferring 0 from bootstrap1 running 'parameter {1} ; storage unit ; code {{ DROP ; UNIT ; NIL operation ; PAIR }}' --burn-cap 10000000 --force"
with open(sys.argv[1], "r") as f:
    lines = f.readlines()
    if len(lines) == 1 and lines[0].strip() == "#NoGroup":
        sys.exit(0)
    contracts = [line.split("#") for line in lines]
    if len(contracts) > 0 and contracts[-1][0].isspace():
        contracts = contracts[:-1]
    for contract in contracts:
        assert len(contract) == 2, contract
    for cmd in [TEMPLATE.format("t" + str(counter), contracts[counter][1].strip()) for counter in range(len(contracts))]:
        originate = subprocess.run(cmd, shell=True, executable='/bin/bash', stderr=subprocess.STDOUT, check=True)
