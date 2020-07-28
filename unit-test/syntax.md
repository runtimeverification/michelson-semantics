This file extends the Michelson contract format to include the .tzt unit
testing format proposed by the Tezos foundation
[here](https://gitlab.com/tezos/tezos/-/merge_requests/1487/diffs).
The first module, `UNIT-TEST-SYNTAX` attempts to follow the proposal as closely
as possible. The second, `SYMBOLIC-UNIT-TEST-SYNTAX` further extends the syntax
with contructs needed for symbolic tests.

```k
requires "michelson/internal-syntax.md"
requires "michelson/syntax.md"
```

Concrete Unit Test Syntax
=========================

In order to test Michelson code at a finer level than a full smart
contract script, the following extension of the Michelson language can
be used. It adds syntax to specify an instruction (or sequence of
instructions) to test, a concrete input stack and the expected output
stack.

These unit tests can be useful for both smart contract developers that
need to independently test various parts of the smart contracts they
develop and to the developers of new implementations of the Michelson
interpreter that need to check that their new implementations behave
as the reference implementation by passing a conformance test suite.

```k
module UNIT-TEST-SYNTAX
  imports UNIT-TEST-COMMON-SYNTAX
  imports MICHELSON-SYNTAX
endmodule
```

```k
module UNIT-TEST-COMMON-SYNTAX
  imports MICHELSON-COMMON-SYNTAX
  imports MICHELSON-INTERNAL-SYNTAX
```

Syntax of concrete stacks
-------------------------

A concrete stack is written as a Micheline sequence whose elements are
of the form ``Stack_elt ty x`` where ``x`` is a Michelson value and
``ty`` is its type. For example, ``{ Stack_elt bool True ; Stack_elt
nat 42 }`` is a concrete stack of length 2 whose top element is the
boolean ``True`` and the bottom element is the natural number ``42``.

```k
  syntax StackElement ::= "Stack_elt" Type Data
  syntax StackElementList ::= List{ StackElement, ";" } [klabel(StackElementList)]
  syntax LiteralStack ::= "{" StackElementList "}"
```

While the input stack must be a full literal, the output stack may be either a
literal or a failed stack (see `michelson-internal-syntax.k` for details),
indicating that the code snippet should fail in a specific way during execution.

```k
  syntax OutputStack ::= LiteralStack | FailedStack
```

The wildcard pattern `_` can be used to omit part of the output stack. This
is typically used to omit the cryprographic nonce in values of type `operation`.

TODO: Parsing `_`s is somewhat tricky in K so we use `#Any` instead for now.

```k
  syntax Data ::= "#Any"
```

Unit test groups
----------------

Unit test files allow the following additional groups, in any order:

`input` (mandatory): the input stack is the initial stack to pass to the Code
block or instruction.

```k
  syntax Group ::= InputGroup
  syntax InputGroup ::= "input" LiteralStack
```

`code` (mandatory): the instruction or sequence of instructions to test.
We promote `CodeDecl` from the contract syntax to a `Group`.

```k
  syntax Group ::= CodeGroup
  syntax CodeGroup ::= CodeDecl
```

`output` (mandatory): the expected output stack:

```k
  syntax Group ::= OutputGroup
  syntax OutputGroup ::= "output" OutputStack
```

`parameter` (optional, defaults to `unit`): the type of the parameter of the
contract pushed by the `SELF` instruction.

```k
  syntax Group ::= ParameterGroup
  syntax ParameterGroup ::= ParameterDecl
```

Besides these, the `now`, `sender`, `source`, `chain_id`, `self`, `amount`, and
`balance`, `other_contracts`, and `big_maps` groups are defined in
[michelson/syntax.md](michelson/syntax.md)

```k
endmodule
```

Symbolic Unit Test Syntax
=========================

We further extend the unit-test syntax with additional groups for representing
pre- and post-conditions as well as invariants to enable verification.

```k
module SYMBOLIC-UNIT-TEST-SYNTAX
  imports UNIT-TEST-SYNTAX
  imports SYMBOLIC-UNIT-TEST-COMMON-SYNTAX

  syntax SymbolicData ::= r"$[_a-zA-Z][_0-9a-zA-Z]*" [token]
endmodule
```

```k
module SYMBOLIC-UNIT-TEST-COMMON-SYNTAX
  imports UNIT-TEST-COMMON-SYNTAX
```

We extend `Data` to allow symbolic variables.

```k
  syntax SymbolicData [token]
  syntax Data ::= SymbolicData
```

We represent pre- and post-conditions as lists of blocks, where each block
contains a well-typed Michelson expression that consumes an empty input stack
and produces an output stack of type `Stack_elt bool _`.

```k
  syntax PreconditionGroup ::= "precondition" "{" BlockList "}"
  syntax PostconditionGroup ::= "postcondition" "{" BlockList "}"
  syntax Group ::= PreconditionGroup | PostconditionGroup
```

An invariant is an annotated pair of a stack shape (represented as a
`LiteralStack`) and a list of predicates (represented as `BlockList`).
The annotation determines to which looping instruction sequence (i.e. `LOOP`,
`LOOP_LEFT`, or `ITER`) an invariant corresponds.
When the program reaches the head of an annotated loop, we check that the actual
stack matches the specified stack shape.
Any symbolic variables in the stack shape are bound to the corresponding values
in the actual stack.
The predicate list represents invariants that must hold over all bound symbolic
variables.


```k
  syntax InvariantsGroup ::= "invariant" VariableAnnotation Invariant
  syntax Invariant ::= LiteralStack "{" BlockList "}"
  syntax Group ::= InvariantsGroup
```

```k
endmodule
```
