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
  imports MICHELSON-SYNTAX
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
  syntax StackElementList ::= StackElement | StackElement ";" StackElementList
  syntax LiteralStack ::= EmptyBlock | "{" StackElementList "}"
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
`balance`, `other_contracts`, and ``big_maps` groups are defined in
[michelson/syntax.md](michelson/syntax.md)

```k
endmodule
```

```k
module SYMBOLIC-UNIT-TEST-SYNTAX
  imports UNIT-TEST-SYNTAX

  syntax BlockList ::= Block | Block ";" BlockList
  syntax Blocks ::= EmptyBlock | "{" BlockList "}"

  syntax Invariants ::= EmptyBlock | "{" InvariantList "}"
  syntax InvariantList ::= Invariant | Invariant ";" InvariantList
  syntax Invariant ::= VariableAnnotation Blocks

  syntax PreconditionGroup ::= "precondition" Blocks
  syntax PostconditionGroup ::= "postcondition" Blocks
  syntax InvariantsGroup ::= "invariants" Invariants

  syntax Group ::= PreconditionGroup | PostconditionGroup | InvariantsGroup
  syntax SymbolicData ::= r"$[_a-zA-Z][_0-9a-zA-Z]*" [token]

  syntax Data ::= SymbolicData
endmodule
```
