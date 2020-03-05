# K-Michelson Tests

## Introduction

The K-Michelson test suite is structured so that different kinds of tests are
grouped together into separate directories. In particular, we currently have
the following test categories:

- contracts - tests containing Tezos contracts; not just Michelson expressions
- macros - tests that perform macro expansion
- obsolete - tests that are deprecated and subject to removal
- proofs - tests that perform symbolic reasoning over the Michelson semantics
  using K that cover (possibly infinite) classes of concrete test cases
- unit - tests containing Michelson expressions that cover each Michelson
  feature independently to the extent possible

## Unit Test Name Structure

To help with eyeballing coverage of the unit tests over the Michelson semantics,
we have adopted a highly regular structure for Michelson unit test names.

The test name structure has two flavors:

1. For opcodes that consume values, we have the format:

`opcode_[type1[-type2]]_NN.tzt`

where `type1` and `type2` are the types of values consumed on the stack and `NN`
are two natural numbers. For convenience, when an instruction only consumes one
type of value, we omit its type information.

2. For opcodes that produce values, we have the format:

`opcode_[type1[-type2]]_NN.tzt`

where `type1` and `type2` are the type parameters for the generated value when
they exist and `NN` are two natural numbers. Opcodes that produce values with
type parameters include:

```
CONS
CONTRACT
EMPTY_BIG_MAP
EMPTY_MAP
EMPTY_SET
NIL
NONE
PAIR
PUSH
SOME
```

Opcodes that produce values without type parameters include:

```
ADDRESS
BALANCE
SELF
UNIT
```

### Eyeballing Unit Test Coverage

For a given opcode, we can gather all of the tests related to it via shell
commands, e.g. `ls opcode*`, and check whether they meaningfully cover the
various cases.

In some cases, an opcode has very different behavior based on its arguments.
In such a case, we can match on the opcode plus argument to check coverage,
e.g. `ls iter_list*` and `ls iter_map*` show all tests corresponding to
an `ITER` opcode applied to a list or map respectively.
