User Guide
==========

Welcome to K-Michelson: a formal verification framework for Michelson using
assertions written in Michelson!

This guide assumes you have successfully installed K-Michelson according to
the instructions in [INSTALL.md](INSTALL.md).

Table of Contents
-----------------

1.  [Table of Contents](#table-of-contents)
2.  [Introduction to K-Michelson](#introduction-to-k-michelson)
3.  [Running Existing K-Michelson Tests](#running-existing-k-michelson-tests)
4.  [Writing Your Own K-Michelson Tests](#writing-your-own-k-michelson-tests)
5.  [Cross-Validating K-Michelson](#cross-validating-k-michelson)
6.  [K-Michelson Test Grammar Reference](#k-michelson-test-grammar-reference)

Introduction to K-Michelson
---------------------------

The main purpose of this toolkit is to enable Michelson smart contract
developers to test their contracts more easily and thoroughly.

Testing with K-Michelson is easier because it is a local client with a simple
test format (`.tzt`) which is just a slight extension of (`.tz`) scripts that
you are used to writing. That means you do _not_ need to:

-   spin up a local test net;

-   define a transaction set to produce your desired blockchain state;

-   bake any blocks.

Testing with K-Michelson is more thorough because it enables you to:

-   test at a finer level of granularity than is possible with (`.tz`) tests,
    i.e., Michelson expressions that are not valid scripts can be tested;

-   test `operation` and `big_map` emitting instructions, which is difficult
    to achieve normally because we cannot `COMPARE` such values;

-   verify correctness of _concrete and symbolic_ Michelson programs, i.e.,
    even Michelson programs with unknown values can be proved correct for all
    possible, valid choices of unknowns (if this is still unclear, see the
    example symbolic test below).

A secondary purpose of K-Michelson is to provide a formal, executable, and
human-readable semantics of the Michelson blockchain programming language
using the K Framework. In this sense, it acts as:

-   a programming language design aid. Since K semantics are easy to modify
    and extend, it can be used to explore modifications to the language. Since
    K semantics are exectuable, it can be used to test potential
    modifications.

-   an additional reference implementation for cross-validation testing
    purposes; by cross-validating alternate Michelson interpreter results,
    we can increase confidence in the language design correctness.

See [Cross-Validating K-Michelson](#cross-validating-k-michelson) for more
information.

### Test Formats

We now survey the two available test formats:

1.  concrete tests using the `.tzt` format;

    Ex. This test asserts adding `5` to `5` produces `10`.

    ```tzt
    code { ADD } ;
    input { Stack_elt int 5 ; Stack_elt int 5 } ;
    output { Stack_elt int 10 }
    ```

2.  symbolic tests using an extended `.tzt` format;

    Ex. This test asserts that `N + 1 > 0` for any natural number `N`.

    ```tzt
    code { ADD ; CMPGT }
    input { Stack_elt nat $N ; Stack_elt nat 1 ; Stack_elt nat 0 ; }
    output { Stack_elt bool True }
    ```

    Ex. This test is somewhat more complex due to the presence of loops;
    it asserts that its input and output are always identical.

    ```tzt
    input  { Stack_elt nat $N } ;
    output { Stack_elt nat $C } ;
    code   { DUP ;
             PUSH nat 0 ;
             SWAP ;
             INT ;
             GT ;
             LOOP @I { PUSH nat 1 ;
                       ADD ;
                       DUP ;
                       DUP 3 ;
                       SWAP ;
                       CMPLT
                     } ;
             DIP { DROP }
           } ;
    invariant @I
      { Stack_elt bool $GUARD ; Stack_elt nat $C ; Stack_elt nat $N  }
      { { PUSH nat $N ; PUSH nat $C ; COMPARE ; LE }
      ; { PUSH nat $N ; PUSH nat $C ; COMPARE ; LT ;
          PUSH bool $GUARD ;
          COMPARE ; EQ
        }
      } ;
    postcondition { { PUSH nat $N ; PUSH nat $C ; COMPARE ; EQ } }
    ```

Note that, in many cases, type (1) tests are already sufficient to explore a
wide range of script behaviors. Type (2) tests should only be used when a full
_proof of correctness_ is required.

Since K-Michelson is derived from the K Framework, it inherits native support
for `kprove`-style tests, but a full definition of such tests is outside the
scope of this document. Consult the K Framework documentation for more details
if needed.

### Test Field Overview

Essentially, a K-Michelson test file can be understood a set of fields.
We survey only the most important fields here; a complete listing of test
fields can be found in the
[K-Michelson Test Grammar Reference](#k-michelson-test-grammar-reference).

The following fields are useful for all kinds of K-Michelson tests:

-   `code` specifies the Michelson expression to be tested.

-   `input` specifies the input stack (possibly with symbolic values).

-   `output` specifies the expected output stack (possibly with symbolic
    values).

The following fields are primarily useful for _symbolic_ tests:

-   `precondition` specifies the test preconditions, i.e., a list of boolean
    functions that must hold before `code` executes for the test to succeed;
    in particular, preconditions can constrain symbolic input values.

-   `postcondition` specifies the test postconditions, i.e., a list of boolean
    functions that must hold after `code` executes for the test to succeed;
    in particular, postconditions can constrian symbolic output values.

-   `invariant` specifies loop invariants for `LOOP` and `LOOP_LEFT`; in
    particular, loop invariants are necessary when looping with symbolic
    values.

See
[Writing Your Own K-Michelson Tests](#writing-your-own-k-michelson-tests)
for more details on how to use these fields.

Running Existing K-Michelson Tests
----------------------------------

Since K-Michelson is derived from the K Framework, running tests just means
calling one of the K utilities, e.g., `krun`, `kprove`, etc. However, due to
their general nature, these utilities must accept a wide range of options that
K-Michelson does not need.

Thus, we provide a runner script `kmich` which invokes the underlying K
utilities with the correct options automatically.

To see the full range of options avaialable, run:

```
./kmich help
```

When the `kmich` runner finishes executing a test, it returns an exit code
indicating whether an error occurred, i.e., `0` indicates success and a
non-zero code indicates an error.

### Running a Concrete Unit Test

Concrete unit tests reside in the `/tests/unit` folder in this archive.
The file `/tests/unit/concate_bytes_00.tzt` is a unit test for the `CONCAT`
instruction.

```tzt
code { CONCAT } ;
input { Stack_elt bytes 0xFF ; Stack_elt bytes 0xcd } ;
output { Stack_elt bytes 0xffcd }
```

Concrete unit tests are run using the `kmich interpret` subcommand, e.g.,

```sh
./kmich interpret tests/unit/concat_bytes_00.tzt
```

### Running a Symbolic Unit Test

Symbolic unit tests reside in the `/tests/symbolic` folder in this archive.
The file `/tests/symbolic/add-party.tzt` is a unit test which tests whether
adding two numbers of opposite parity (an even and odd number) always produces
an odd number:

```tzt
code { ADD } ;
input { Stack_elt nat $I1 ; Stack_elt nat $I2 } ;
output { Stack_elt nat $I3 } ;

precondition { { PUSH nat 2 ; PUSH nat $I1 ; EDIV ;
                 IF_NONE { PUSH bool False }
                         { CDR ; PUSH nat 1 ; COMPARE ; EQ } } ;

               { PUSH nat 2 ; PUSH nat $I2 ; EDIV ;
                 IF_NONE { PUSH bool False }
                         { CDR ; PUSH nat 0 ; COMPARE ; EQ } } } ;

postcondition { { PUSH nat 2 ; PUSH nat $I3 ; EDIV ;
                  IF_NONE { PUSH bool False }
                          { CDR ; PUSH nat 1 ; COMPARE ; EQ } } }
```

Symbolic unit tests are run using the `kmich symbtest` subcommand, e.g.,

```sh
./kmich symbtest tests/symbolic/add-parity.tzt
```

Writing Your Own K-Michelson Tests
----------------------------------

An easy way to boostrap writing a new test is to copy and existing test and
modify it. This helps ensure that the test file has the correct format.

**TODO**: finish writing this section.

Cross-Validating K-Michelson
----------------------------

As part of its goal to be a human-readable language semantics and aid
programming language design, K-Michelson supports a cross-valdiation test
mode. In this mode, concrete unit tests (under `/tests/unit`) are executed
in the following manner:

1.   The unit test is executed by K-Michelson.

2.   The same unit test is executed by `tezos-client`, the reference Michelson
     interpreter.

3.   The results are compared. If they are identical, the test succeeds;
     otherwise, the test fails.

To use this mode, use the `/lib/tezos-client-unit-test` command, e.g.,

```sh
./lib/tezos-client-unit-test tests/unit/concat_bytes_00.tzt
```

Note that this mode has additional dependencies and build requirements
compared to the concrete or symbolic unit tests. See [INSTALL.md](INSTALL.md)
for details.

K-Michelson Test Grammar Reference
----------------------------------

Type (1)-(2) tests use the `.tzt` format, first defined in
[here](https://gitlab.com/tezos/tezos/-/merge_requests/1487/diffs).
The `.tzt` format is a slight extension of the `.tz` format.
For convenience, we briefly explain the `.tzt` grammar here.

The `.tzt` format can be understood as schemas applied to the
[Micheline format](http://tezos.gitlab.io/whitedoc/micheline.html).
More abstractly, it is an unordered set of typed fields that optionally
contains associated data.

#### Michelson Grammar Extensions

In the standard Michelson grammar, there are no literals for the `operation`
and `big_map` types. The `.tzt` format adds support for these literals because
tests may need to refer to these kinds of values.

1.  `operation` literals have the following form:

    - `Create_contract contract (option key_hash) mutez T byte` where
      `contract`'s storage type is `T`

    - `Transfer_tokens T mutez address byte` where the `contract` value at
       `address` has parameter type `T`

    - `Set_delegate (option key_hash) byte`

    where in each case the final `byte` argument represents a cryptographic
    nonce.

2.  `big_map` literals have the following form:

    - a natural number identifier referring to an indexed `big_map` in
      the `big_maps` field (see description below)

      Ex. `2`

    - a standard `map` literal

      Ex. `{ Elt 1 True ; Elt 3 False }` (literal of type `big_map nat bool`)

    - a pair of a `big_map` identifier and a map literal (the map literal
      represents a difference list)

      Ex. `Pair 2 { Elt 1 False }` (refers to the `big_map`
      `{ Elt 1 False ; Elt 3 False }` if `2` identifies the map
      `{ Elt 1 True  ; Elt 3 False }`)

#### Field Types

We list the set of possible types below. Note that each field can accept only
_one_ type.

-   expression - an arbitrary Michelson expression

    Ex. `{ PUSH int 1; PUSH int -2; ADD }`

    Ex. `{}`

-   stack - a typed Michelson stack which is equivalent to a list of stack
    stack elements of the form `Stack_elt type value`

    Ex. `{ Stack_elt int -1 ; Stack_elt (set int) { Elt 0 ; Elt 3 } }`

    Ex. `{ Stack_elt bool True }`

    Ex. `{}`

-   type - any Michelson type

    Ex. `bool :flag`

    Ex. `pair nat int`

-   timestamp - a Michelson timestamp literal

    Ex. `0`

    Ex. `"2019-09-26T10:59:51Z"`

-   mutez - a Michelson mutez literal

    Ex. `0` (minimum value)

    Ex. `9223372036854775807` (maximum value)

-   address - a Michelson address literal

    Ex. `"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"`

    Ex. `"KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi"`

-   chain_id - a Michelson chain_id literal

    Ex. `0x00000000` (minimum value)

    Ex. `0xFFFFFFFF` (maximum value)

-   contract type map - a map of contract addresses to their parameter types
    where each map entry has the form `Contract address type`

    Ex. `{ Contract "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi" nat ;
           Contract "KT1HgAM3pNzkqd1Ps8iunMGNopFRFKHWoPdW" (list int) }`

    Ex. `{ Contract "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi" nat }`

    Ex. `{}`

-   big_map index map - a map of natural number indices to big_map literals
    where each map entry has the form `Big_map nat type type map_literal`

    Ex. `{ Big_map 1 string int  { Elt "bar" 1 ; Elt "foo" 2 } ;
           Big_map 2 string bool { Elt "a" True ; Elt "b" False } }`

    Ex. `{}`

-   predicate list - a list of Michelson predicates where each predicate
    has the form `{ expression }` where the expression takes an empty input
    stack and produces a stack containing a single boolean

    Ex. `{ { PUSH int $I ; EQ } ;
           { PUSH list int $L ; SIZE ; GT } }`

    Ex. `{}`

    Ex. `{ { ADD ; EQ } }` (**invalid**: input stack type non-empty)

    Ex. `{ { PUSH nat 0 ; PUSH bool True } }` (**invalid**: output stack is
    not single boolean value)

-   stack predicate list - a pair of a stack binder, i.e. a fully symbolic
    stack, and a predicate list such that the bound stack variables are
    available in the predicate list

    Ex.
    ```
    { Stack_elt bool $CONT ; Stack_elt nat $N }
    { { PUSH nat $N ; INT ; GT ; PUSH bool $CONT ; CMPEQ } }
    ```
    (this stack predicate list describes, e.g., a loop variable `N` with loop
    continuation guard `CONT`, such that the loop will continue to iterate
    while `N > 0`)

#### Field List

The set of required fields and their types is listed below:

-   `code` (expression) the Michelson code to be tested

-   `input` (stack) the input stack supplied to the code in the `code` field

-   `output` (stack) the expected output stack of the `code` field given the
    input stack defined in `input`

The format also allows for optional fields which have default values. The set
of optional fields is defined below:

-   `parameter` (type, default `unit`) defines the contract type pushed by the
    `SELF` instruction

-   `now` (timestamp, default `0`) defines the result timestamp of the `NOW`
    instruction

-   `sender` (address, default `"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"`)
    defines the result of the `SENDER` instruction

-   `source` (address, default `"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"`)
    defines the result of the `SOURCE` instruction

-   `chain_id` (chain_id, default `0x7a06a770`) defines the result of the
    `CHAIN_ID` instruction

-   `self` (address, default `"KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi"`) defines
    the address of the contract pushed by the `SELF` instruction

-   `amount` (mutez, default `0`) defines the amount of mutez returned by the
    `AMOUNT` instruction

-   `balance` (mutez, default `0`) defines the amount of mutez returned by the
    `BALANCE` instruction

-   `other_contracts` (contract type map, default `{}`) defines the set of
    other contracts which are available for this contract to invoke.

-   `big_maps` (big_map index map, default `{}`) defines a mapping from
    natural number indices to `big_map` literals; this allows us to write
    `big_map` literals in the other fields as just an index.

    This feature is helpful when the `big_map` literal is large.

-   `precondition` (predicate list, default `{}`) defines a list of predicates
    which constrain any symbolic values in the `input` stack.

-   `postcondition` (predicate list, default `{}`) defines a list of predicates
    which constrain any symbolic values in the `output` stack.

-   `invariant @L` (stack predicate list, default `{} {}`) defines a loop
    invariant for a `LOOP` or `LOOP_LEFT` instruction annotated with a
    special `@L` annotation.

The format admits some fields which are _ignored_ for compatibility reasons.
The set of ignored fields is defined below:

-   `storage` (type) defines the storage type of the contract in `.tz` files
