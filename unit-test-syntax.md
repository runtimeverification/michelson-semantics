This module extends the Michelson contract format to include the .tzt unit testing format proposed by the Tezos foundation [here](https://gitlab.com/tezos/tezos/-/merge_requests/1487/diffs).  Note that, unlike normal contract execution, unit tests *may* use internal syntax.  They furthermore specify entire input and expected output stacks, rather than only a parameter and storage value.

```k
requires "michelson-internal-syntax.k"
requires "michelson-syntax.k"

module UNIT-TEST-SYNTAX
  imports MICHELSON-SYNTAX
  imports MICHELSON-INTERNAL-SYNTAX

```

Here we describe the format of a stack literal. These are used both to set up an input stack for a Michelson code snippit, and to describe the expected output stack.

```k
  syntax StackElement ::= "Stack_elt" Type Data
  syntax StackElementList ::= StackElement | StackElement ";" StackElementList
  syntax LiteralStack ::= EmptyBlock | "{" StackElementList "}"
```

While the input stack must be a full literal, the output stack may be either a literal or a failed stack (see `michelson-internal-syntax.k` for details), indicating that the code snippit should fail in a specific way during execution.

```k
  syntax OutputStack ::= LiteralStack | FailedStack
```

To fully implement the .tzt format, we add four new groups to the loading group set.  These specify:

- Code, specifying an instruction or block of Michelson code to execute (note that Blocks are also instructions)
- Input, the initial stack to pass to the Code block or instruction.
- Output, the expectation for the final stack after the contents of Code have finished executing.

```k
  syntax InputGroup ::= "input" LiteralStack
  syntax CodeGroup ::= CodeDecl
  syntax OutputGroup ::= "output" OutputStack

  syntax Group ::= InputGroup | CodeGroup | OutputGroup | ParameterDecl
```

Finally, we declare the #Any wildcard as described in the .tzt format proposal.  #Any can be placed in the expected output group in place of any data element, in which case any data "below" that #Any element will be ignored during comparison. (e.g. (Pair #Any 1) will match with (Pair 1 1), (Pair 2 1), etc.)

```k
  syntax Data ::= "#Any"

endmodule
```
