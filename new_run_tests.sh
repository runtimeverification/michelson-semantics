bash -c 'set -e ; for d in ./tests/unit/* ; do echo $d ; krun $d ; done' 
