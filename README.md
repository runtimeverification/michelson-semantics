K-Michelson Semantics
=====================

Building and Installation
-------------------------

To build K-Michelson, please follow the instructions below. Note that:

-   all commands should be executed in the K-Michelson software archive root
-   only Linux is officially supported; other systems may have limited support

K-Michelson has two direct dependencies:

1.  [K framework](https://github.com/kframework/k) - for building the K-Michelson
    interpreter and executing Michelson code using the K-Michelson interpreter
2.  [Tezos client](http://tezos.gitlab.io/index.html) for cross-validation of
    K-Michelson test execution against test execution on the reference Michelson
    interpreter.

To simplify installation, we package pinned versions of (1-2) under the `/ext`
directory. As part of the installation process, we must first build these
dependencies.

### Installing the Prerequisites

The K framework and Tezos client each have a number of their own dependencies
that must be installed before they can be built. On an Ubuntu system, do the
following to install all needed dependencies:

```sh
sudo apt-get install rsync git m4 build-essential patch unzip bubblewrap wget  \
pkg-config libgmp-dev libev-dev libhidapi-dev libmpfr-dev flex bison z3        \
libz3-dev maven python3 cmake gcc zlib1g-dev libboost-test-dev libyaml-dev     \
libjemalloc-dev openjdk-8-jdk clang-8 lld-8 llvm-8-tools pcregrep pandoc
```

Note that the JDK and Clang packages referenced above typically can be
substituted with more recent versions without any issues.

You will also need a recent version of [Haskell stack](https://docs.haskellstack.org/en/stable/install_and_upgrade).
You can either install the Ubuntu package and upgrade it locally:

```
sudo apt-get install haskell-stack
stack upgrade --binary-only
```

or else get the latest version with their installer script by doing:

```
curl -sSL https://get.haskellstack.org/ | sh
```

You will additionally need to install a recent version of the [OCaml package manager, opam](https://opam.ocaml.org/doc/Install.html).
For Ubuntu, the recommended installation steps are:

```
sudo apt-get install software-properties-common
sudo add-apt-repository ppa:avsm/ppa
sudo apt-get install opam
```

For other Linux distributions, you may need to adapt the above instructions,
especially package names and package installation commands.
Consult your distribution documentation for details as well as the links
above for guidance on installing Haskell Stack as well as opam.

### Building K-Michelson

Build the K Framework (if you don't have a global install) and Tezos dependencies:

```sh
git submodule update --init --recursive
make deps
```

The following command will build all versions of the semantics including:

-   The LLVM backend for running `*.tzt` programs,
-   The Haskell backend for running proofs about the semantics,
-   The Haskell backend for running symbolic tests, and
-   The compatibility layers for validating the semantics against the Tezos reference client.

```sh
make build -j8
```

Running Tests
-------------

There are three major test-suites you may be interested in running.

The unit tests (running individual `*.tzt` programs and checking their exit code):

```sh
make test-unit -j8
```

The symbolic unit tests (running individual `*.tzt` programs and checking their
output using `lib/michelson-test-check`). Note that their are three flavours of
symbolic unit tests. Those with the `stuck.tzt` extension, test ill-formed
programs and are expected to get stuck before completing exectution. Those with
the `fail.tzt` extension, are "broken" tests where assertions are expected to
fail.

```sh
make test-symbolic -j8
```

The proof tests:

```sh
make test-prove -j2
```

The validation tests against the Tezon reference client:

```sh
make test-cross -j8
```

Using the Semantics
-------------------

### Runner Script

The `kmich` script is provided as a way to access the semantics directly.
You can do `./kmich help` for the most up-to-date information about how to use this script.

### Example

The semantics accept Michelson contracts or unit tests in a variant of the format discussed [here](https://gitlab.com/tezos/tezos/-/merge_requests/1487/diffs).
The primary change can be seen in the addition of the `contract`, `parameter_value` and `storage_value` primitive applications, which capture the remaining initial state of a Michelson contract execution.
Either these three primitive applications, or the `code`, `input` and `output` applications detailed in the Unit Test syntax must be present in any input file for the semantics.
The former results in a full contract execution, the latter in the execution of a small snippet.

As an example, here is a contract input file implementing a sum-to-n program (at `sum-to-n.tzt`):

```tzt
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
You can run this contract using `kmich` as follows:

```sh
./kmich run sum-to-n.tzt
```

As an example of a unit test format file, here is a test for the `DIG` instruction (at `dig.tzt`):

```tzt
code { DIG 1 } ;
input { Stack_elt int 1 ; Stack_elt int 2 ; Stack_elt int 3 ; Stack_elt int 4 ; Stack_elt int 5 ; Stack_elt int 6 } ;
output { Stack_elt int 2 ; Stack_elt int 1 ; Stack_elt int 3 ; Stack_elt int 4 ; Stack_elt int 5 ; Stack_elt int 6 }
```

Note that the unit test format allows the user to specify an entire input and output stack, rather than using the normal Michelson parameter/storage system.
You can run this program using:

```sh
./kmich run dig.tzt
```

Project Structure
-----------------

### Michelson Semantics Definition

The [michelson/](./michelson/) directory contains files related to the Michelson language, contracts and input data

* [michelson/syntax.md](./michelson/syntax.md) contains the specification for the syntax of a Michelson contract and the other input data.
* [michelson/configuration.md](./michelson/configuration.md) describes the template state of a Michelson contract.
* [michelson/common.md](./michelson/common.md) specifies most of the K-Michelson internal datatypes.
* [michelson/michelson.md](./michelson/michelson.md) specifies the semantics of the Michelson language as rewrite rules over the syntax, configuration and datatypes defined in the previous files.

[unit-test/unit-test.md](./unit-test/unit-test.md) and [unit-test/syntax.md](./unit-test/syntax.md) extend the semantics and syntax of the Michelson language to include unit testing facilities, such as the ability to specify an initial and final stack, and to check that the final stack matches the expected result.

Similarly, [symbolic/symbolic.md](./symbolic/symbolic.md) and [symbolic/syntax.md](./symbolic/syntax.md) extend the unit-tests with the ability to specify symbolic variables, and contract and look invariants.

[compat.md](./compat.md) is a compatability layer between KMichelson and the Tezon Reference client used for doing cross-validation between the two.

`hooks/time.cpp` and `hooks/hex.cpp` implement backend hooks to perform timestamp translation (i.e. from an ISO-8601 human readable timestamp to a Unix timestamp) and print binary blobs as hexadecimal strings.
They are used by the K semantics internally.

Miscellaneous Documentation
---------------------------

We store bits and pieces of useful information here that. These are primarily
of interest to developers.

### Semantics Initializatation

As is the case with other languages, the K-Michelson semantics needs different
drivers to support its use in different tools. Currently, we have the following
drivers:

- a driver that executes Michelson contracts extended with inputs
- a driver that executes Michelson unit tests
- a driver that executes symbolic Michelson unit tests

Each of these drivers must perform slightly different initialization routines.
However, these initialization routines have a common structure:

1. Parse the input file and put it into the `<k>` cell
2. Load each of the input file groups into a specific configuration cell
3. Apply the driver `#Init` rule

We give an overview of the steps performed by each driver:

1. Create fresh variables for any symbolic inputs (symbolic semantics only)
2. Execute the precondition blocks
3. Execute the main program block
4. Bind the symbolic variables in the expected output stack against the
   actual output stack (symbolic semantics only)
4. Execute the postcondition blocks

In particular, it is worth considering the steps necessary to execute a block:

1. Load the block input stack and expected output stack
2. Type-check the block against the input stack and expected output stack
3. Convert the input/output stacks into internal forms used for execution
4. Run the block with the converted input stack
5. Check that our actual output stack matches the expected output stack
