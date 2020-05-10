# Cross-validation Testing Harness

This directory contains the K-Michelson cross-validation testing harness.
This test harness is designed to test that K-Michelson and the official
Tezos Michelson implementation agree on all unit tests.

Note: all commands shown below are assuemd to executed in the root directory of
the source distribution. We test our scripts using the `bash` shell; for best
results, try using `bash`.

## Build Instructions

To build the semantics as well as this cross-validation testing harness, run:

```
./build.sh
```

## Usage Instructions

Set up the Tezos sandbox by running and shell environment by running:

```
source ./start.sh
```

Afterwards, check that K-Michelson and the official Tezos implementation
agree on a unit test by executing:

```
./compat/run-test.sh TZT_FILE
```

To run all tests at once, do:

```
./compat/run-tests.sh
```

To shutdown the sandboxed Tezos nodes, run:

```
./shutdown.sh
```

## Developer Instructions

After updating the semantics, run:

```
./compat/run-tests.sh record
```

to update the list of unit tests that are expected to diverge between
K-Michelson and the reference implementation. Note that, in general, for any
commit that we make, it is expected that the list of diverging tests should
decrease.

To run all of the tests with automatic sandbox setup and tear-down, run:

```
./compat/run-tests-ci.sh
```
