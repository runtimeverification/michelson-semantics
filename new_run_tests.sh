#!/bin/bash
set -e ; for d in ./tests/unit/* ; do echo $d ; krun $d ; done
