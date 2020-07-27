K-Michelson Semantics
=====================

Building and Installation
-------------------------

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

```sh
sudo apt-get install rsync git m4 build-essential patch unzip bubblewrap wget  \
pkg-config libgmp-dev libev-dev libhidapi-dev libmpfr-dev flex bison z3        \
libz3-dev maven python3 cmake gcc zlib1g-dev libboost-test-dev libyaml-dev     \
libjemalloc-dev openjdk-8-jdk clang-8 lld-8 llvm-8-tools pcregrep
```

Note that the JDK and Clang packages referenced above typically can be
substituted with more recent versions without any issues.

You will also need a recent version of
[Haskell stack](https://docs.haskellstack.org/en/stable/install_and_upgrade).
You can either install the Ubuntu package and upgrade it locally:

```
sudo apt-get install haskell-stack
stack upgrade --binary-only
```

or else get the latest version with their installer script by doing:

```
curl -sSL https://get.haskellstack.org/ | sh
```

You will additionally need to install a recent version of the
[OCaml package manager, opam](https://opam.ocaml.org/doc/Install.html). For
Ubuntu, the recommended installation steps are:

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

Build the K Framework (if you don't have a global install) and Tezos
dependencies:

```sh
git submodule update --init --recursive
make deps
```

The following command will build all versions of the semantics including:

- The LLVM backend for running Michelson unit tests (`.tzt` extension);
- The Haskell backend for running symbolic Michelson unit tests (also `.tzt`
  extension);
- The compatibility layers for validating the semantics against the Tezos
  reference client.

```sh
make build -j8
```

Running Tests
-------------

There are three major test-suites you may be interested in running.

The unit tests (running individual `*.tzt` programs and checking their exit
code):

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

The validation tests against the Tezos reference client:

```sh
make test-cross -j8
```

Using the Semantics
-------------------

### Runner Script

The `kmich` script is provided as a way to access the semantics directly.
You can do `./kmich help` for the most up-to-date information about how to use
this script.

### Example

The semantics accepts unit tests in the format discussed
[here](https://gitlab.com/tezos/tezos/-/merge_requests/1487/diffs).
As an example of a unit test file, here is a test for the `DIG`
instruction (at `tests/unit/dig_01.tzt`):

```tzt
code { DIG 1 } ;
input { Stack_elt int 1 ; Stack_elt int 2 ; Stack_elt int 3 ;
        Stack_elt int 4 ; Stack_elt int 5 ; Stack_elt int 6 } ;
output { Stack_elt int 2 ; Stack_elt int 1 ; Stack_elt int 3 ;
         Stack_elt int 4 ; Stack_elt int 5 ; Stack_elt int 6 }
```

Note that the unit test format allows the user to specify an entire input and
output stack, rather than using the normal Michelson parameter/storage system.
You can run this program using:

```sh
./kmich interpret tests/unit/dig_01.tzt
```

Project Structure
-----------------

### Michelson Semantics Definition

The `michelson/` directory contains files related to the Michelson language,
contracts, and input data

- [michelson/syntax.md](./michelson/syntax.md) contains the specification for
  the syntax of a Michelson contract and the other input data.
- [michelson/internal-syntax.md](./michelson/internal-syntax.md) contains the
  additional syntax used by the core Michelson semantics as well as the unit
  test semantics.
- [michelson/configuration.md](./michelson/configuration.md) describes the
  template state of a Michelson contract.
- [michelson/common.md](./michelson/common.md) specifies most of the
  K-Michelson internal datatypes.
- [michelson/michelson.md](./michelson/michelson.md) specifies the semantics of
  the Michelson language as rewrite rules over the syntax, configuration and
  datatypes defined in the previous files.

[unit-test/syntax.md](./unit-test/syntax.md) and
[unit-test/unit-test.md](./unit-test/unit-test.md) extend the syntax and
semantics of the Michelson language to include unit testing facilities, such as
the ability to specify an initial and final stack, to check that the final
stack matches the expected result, to make assertions about the Michelson
code for verification purposes, and to represent symbolic values in tests.

[compat.md](./compat.md) is a compatability layer between KMichelson and the
Tezon Reference client used for doing cross-validation between the two.

`hooks/time.cpp` and `hooks/hex.cpp` implement backend hooks to perform
timestamp translation (i.e. from an ISO-8601 human readable timestamp to a Unix
timestamp) and print binary blobs as hexadecimal strings.  They are used by the
K semantics internally.
