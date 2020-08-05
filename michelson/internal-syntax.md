This file extends the normal Michelson syntax to describe Blockchain operations
and error conditions. This module should remain separate from MICHELSON-SYNTAX
to prevent normal contracts from using these productions, which are intended
only for unit tests. However, this cannot be merged into the UNIT-TEST-SYNTAX
module, as the MICHELSON module needs to be able to use them, and MICHELSON
should not depend upon UNIT-TEST-SYNTAX.

```k
requires "michelson/syntax.md"

module MICHELSON-INTERNAL-SYNTAX
  imports MICHELSON-COMMON-SYNTAX

```

Operation literals
------------------

Here we define literals of type `operation`.
They are not representable in standard Michelson code;
instead, they can only be pushed on the stack by the Michelson
`CREATE_CONTRACT`, `TRANSFER_TOKENS`, and `SET_DELEGATE` instructions.

1. We need an `operation` literal to create a new contract:

```k
  syntax BlockchainOperation ::=
    "Create_contract" "(" Int "," Contract "," OptionData "," Mutez "," Data ")"
```

- Nonce (`int`): A cryptographic nonce attached to each new `operation` literal
  created; no two operations created separately will ever share the same nonce 
- Contract (`contract`): The source code of the contract to originate. The type
  of this contract will determine the expected type of the initial storage
- Delegate (`option key_hash`): An optional delegate specified by key hash
- Initial Balance (`mutez`): An initial balance to transfer to the new contract
- Initial Storage (`T`): An initial storage value, expected to be the same type
  as specified in the originated contract
[//]: # (A code together with parameter and storage types is usually called a "script")

2. We need an `operation` literal to transfer mutez to a contract (including
   the case of invoking a contract):

```k
  syntax BlockchainOperation ::=
    "Transfer_tokens" "(" Int "," Data "," Mutez "," Address ")"
```

- Nonce (`int`): A cryptographic nonce attached to each new `ooeration` literal
  created; no two operations created separately will ever share the same nonce
- Parameter (`T`): The parameter passed to the contract being invoked
  (or Unit, if the target contract is an implicit account)
- Amount (`mutez`): The quantity of mutez to transfer to the target contract
- Address (`address`): The address of the target contract

3. We need an `operation` literal to set the delegate of an account.

```k
  syntax BlockchainOperation ::= "Set_delegate" "(" Int "," OptionData ")"
```

- Nonce (`int`): A cryptographic nonce attached to each new `operation` literal
  created; no two operations created separately will ever share the same nonce 
- Delegate (`option key_hash`): An optional new delegate specified by key hash;
  if `None`, then this operation clears the current delegate of the contract

We need to be able to represent `operation` literals stored on the stack.
Thus, we make `BlockchainOperation` a subsort of `Data`.

```k
  syntax SimpleData ::= BlockchainOperation
```

Failed Stack Literals
---------------------

We need to represent the stack of Michelson code that fails to execute properly.

1. We need to represent a stack resulting from a `FAILWITH` instruction. Its
   argument is the element on top of the stack when `FAILWITH` was executed.

```k
  syntax FailedStack ::= "(" "Failed" Data ")"
```

2. We need to represent a stack resulting from an operation on mutez which
   would have created a mutez value greater than the maximum representable
   value of 2^63 - 1. Its arguments are the two input mutez values involved in
   the operation.

```k
  syntax FailedStack ::= "(" "MutezOverflow" Int Int ")"
```

3. Similarly, we need to represent a stack resulting from an operation on mutez
   which would have created a mutez value less than the minimum representable
   value of 0. Its arguments are the two input mutez values involved in the
   operation.

```k
  syntax FailedStack ::= "(" "MutezUnderflow" Int Int ")"
```

4. This production represents a stack resulting from a non-mutez overflow, such
   as those produced by the `LSL` and `LSR` instructions when their shift
   argument is greater than 256.

```k
  syntax FailedStack ::= "(" "GeneralOverflow" Int Int ")"
```

Michelson Assertions
--------------------

When running Michelson unit tests, we store pre- and post-conditions as lists
of Michelson sequences that produce a stack of the form `Stack_elt bool`.

```k
  syntax BlockList ::= List{Block, ";"} [klabel(BlockList)]
```

```k
endmodule
```
