User Guide
==========

Welcome to K-Michelson: a formal verification framework for Michelson using
assertions written in Michelson!

This guide assumes you have successfully installed K-Michelson according to
the instructions in [INSTALL.md](INSTALL.md).

The purpose of this toolkit is to aid Michelson smart contract developers by
providing a local testing framework that supports both concrete and symbolic
tests.

We will cover the following topics:

1.  How to run existing K-Michelson tests
2.  How to write your own K-Michelson tests based on new or existing Michelson
    scripts

Testing Overview
----------------

In total, this framework includes three kinds of tests:

1. concrete unit tests using the `.tzt` format (defined below)
2. symbolic unit tests using an extended `.tzt` format
3. `kprove`-style tests

with type (1) tests being the least expressive/rigorous and type (3) tests
being the most expressive and rigorous. In many cases, concrete unit tests are
already sufficient to explore a wide range of script behaviors. Type (2)-(3)
tests should only be used when a full _proof of correctness_ is required.

### Test Grammar

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

The format admits some fields which are _ignored_ for compatibility reasons.
The set of ignored fields is defined below:

-   `storage` (type) defines the storage type of the contract in `.tz` files

Example Tests
-------------

Running an Existing Test
------------------------

The `kmich` script is provided as a way to access the semantics directly.
You can do `./kmich help` for the most up-to-date information about how to use
this script.

### Example

The semantics accepts unit tests in the format discussed
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
