#!/usr/bin/python3
import subprocess, sys
TEMPLATE = "tezos-client originate contract {0} transferring 0 from bootstrap1 running 'parameter {1} ; storage unit ; code {{ DROP ; UNIT ; NIL operation ; PAIR }}' --burn-cap 10000000 --force"
with open(sys.argv[1], "r") as f:
    contracts = [line.split("#") for line in f.readlines()]
    for contract in contracts:
        assert len(contract) == 2, contract
    for cmd in [TEMPLATE.format("t" + str(counter), contracts[counter][1].strip()) for counter in range(len(contracts))]:
        originate = subprocess.Popen(cmd, shell=True, executable='/bin/bash', stderr=subprocess.STDOUT)
        while originate.poll() is None:
            subprocess.call("tezos-client bake for bootstrap1", shell=True, executable='/bin/bash', stderr=subprocess.STDOUT)
        assert originate.returncode == 0
