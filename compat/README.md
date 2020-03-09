# Building

Execute `./kompile.sh` in each of the four subdirectories to build the K definitions needed to execute the compatibility script

# Using

Execute `source start.sh` to start a sandboxed tezos node and setup the shell environment to execute unit tests.
Afterwards, run a test on the official Tezos implementation by executing `./run-test TZT_FILE`
Run `./shutdown.sh` to shutdown the sandboxed tezos node.
