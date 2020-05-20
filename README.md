# K-Michelson Semantics

## Building and Installation

To build K-Michelson, please follow the instructions below. Note that:

- all commands should be executed in the K-Michelson software archive root
- only Linux is officially supported; other systems may have limited support

K-Michelson has two direct dependencies:

1. [K framework](https://github.com/kframework/k) - for building the K-Michelson
  interpreter and executing Michelson code using the K-Michelson interpreter
2. [Tezos client](http://tezos.gitlab.io/index.html) for cross-validation of
  K-Michelson test execution against test execution on the reference Michelson
  interpreter.

To simplify installation, we package pinned versions of (1-2) under the `/ext`
directory. As part of the installation process, we must first build these
dependencies.

### Installing the Prerequisites

The K framework and Tezos client each have a number of their own dependencies
that must be installed before they can be built. On an Ubuntu system, do the
following to install all needed dependencies:

```
sudo apt-get install rsync git m4 build-essential patch unzip bubblewrap wget  \
pkg-config libgmp-dev libev-dev libhidapi-dev libmpfr-dev flex bison z3        \
libz3-dev maven python3 cmake gcc zlib1g-dev libboost-test-dev libyaml-dev     \
libjemalloc-dev openjdk-8-jdk clang-8 lld-8 llvm-8-tools
```

For other Linux distributions, you may need to modify the package names as well
as the package installation command. Consult your distribution documentation
for details.

Note that in the above command, the JDK and Clang packages typically can be
substituted with more recent versions without any issues.

### Building K-Michelson

Afterwards, do the following to build K-Michelson and its dependencies:

```
./build-deps && ./build.sh
```

Presently, K-Michelson is only automatically built with the LLVM backend of the
K framework. However, K-Michelson may also be built with the Haskell backend. To
do so, first build the dependencies and then run:

```
source common.sh
kompile --backend haskell unit-test.k
```

However, this will disable certain llvm-specific hooks, such as timestamp parsing.

The OCaml and Java backends are not presently officially supported.

## Running Tests

After building, the test suite may be run by the following command:

```
./run-tests.sh
```

Individual tests may be run by doing the following:

```
./run.sh <testname>
```

## Using the semantics

The semantics accept Michelson contracts or unit tests in a variant of the format discussed [here](https://gitlab.com/tezos/tezos/-/merge_requests/1487/diffs).  The primary change can be seen in the addition of the `contract`, `parameter_value` and `storage_value` primitive applications, which capture the remaining initial state of a Michelson contract execution.  Either these three primitive applications, or the `code`, `input` and `output` applications detailed in the Unit Test syntax must be present in any input file for the semantics.  The former results in a full contract execution, the latter in the execution of a small snippet.

As an example, here is a contract input file implementing a sum-to-n program:

```
parameter_value 300000 ;
storage_value 0 ;
contract {
  storage nat ;
  parameter nat ;
  code { LEFT nat ;
         LOOP_LEFT { DUP ;
                     DIP { CDR } ;
                     CAR ;
                     DUP ;
                     DIP { ADD } ;
                     PUSH nat 1 ;
                     SWAP ;
                     SUB ;
                     ISNAT ;
                     IF_NONE { RIGHT (pair nat nat) } { PAIR ; LEFT nat } } ; NIL operation; PAIR } }
```

This contract computes the sum of 1 to its parameter value, plus its storage value, and places the result in its storage.

As an example of a unit test format file, here is a test for the `DIG` instruction:


```
code { DIG 1 } ;
input { Stack_elt int 1 ; Stack_elt int 2 ; Stack_elt int 3 ; Stack_elt int 4 ; Stack_elt int 5 ; Stack_elt int 6 } ;
output { Stack_elt int 2 ; Stack_elt int 1 ; Stack_elt int 3 ; Stack_elt int 4 ; Stack_elt int 5 ; Stack_elt int 6 }
```

Note that the unit test format allows the user to specify an entire input and output stack, rather than using the normal Michelson parameter/storage system.

## Project Structure

### Michelson Semantics Definition

[michelson-syntax.md](./michelson-syntax.md) contains the specification for the syntax of a Michelson contract and the other input data.

[michelson-config.md](./michelson-config.md) describes the template state of a Michelson contract.

[michelson-common.md](./michelson-common.md) specifies most of the K-Michelson internal datatypes.

[michelson.md](./michelson.md) specifies the semantics of the Michelson language as rewrite rules over the syntax, configuration and datatypes defined in the previous files.

[unit-test.md](./unit-test.md) and [unit-test-syntax.md](./unit-test-syntax.md) extend the semantics and syntax of the Michelson language to include unit testing facilities, such as the ability to specify an initial and final stack, and to check that the final stack matches the expected result.

`time.cpp` and `hex.cpp` implement backend hooks to perform timestamp translation (i.e. from an ISO-8601 human readable timestamp to a Unix timestamp) and print binary blobs as hexadecimal strings. They are used by the K semantics internally.

### Michelson Tests

All tests are located under in the `tests` directory.
See the [README](./tests/README.md) for more details.
For information on running the cross-validation testing
harness, consult this [README](./compat/README.md).
