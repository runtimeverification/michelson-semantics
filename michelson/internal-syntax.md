This file extends the normal Michelson syntax to describe Blockchain operations and error conditions.  This module should remain separate from MICHELSON-SYNTAX to prevent normal contracts from using these productions, which are intended only for unit tests.  But, this cannot be merged into the UNIT-TEST-SYNTAX module, as the MICHELSON module needs to be able to use them, and MICHELSON should not depend upon UNIT-TEST-SYNTAX.

```k
requires "michelson/syntax.md"

module MICHELSON-INTERNAL-SYNTAX
  imports MICHELSON-SYNTAX

```

This BlockchainOperation describes a contract origination operation.  Its arguments are:

- Nonce (`int`): A cryptographic nonce attached to each new BlockchainOperation created.  No two operations created separately (i.e. by two different `CREATE_CONTRACT` executions) will ever share the same nonce, but an operation duplicated with the `DUP` instruction will.

- Contract (`contract`): The source code of the contract to originate.  The type of this contract will determine the expected type of the initial storage.

[//]: # (A code together with parameter and storage types is usually called a "script")

- Delegate (`option key_hash`): An optional delegate specified by key hash.
- Initial Balance (`mutez`): An initial balance to transfer to the new contract.
- Initial Storage (`T`): An initial storage value, expected to be the same type as specified in the originated contract.

```k
  syntax BlockchainOperation ::= "Create_contract" "(" Int "," Contract "," OptionData "," Mutez "," Data ")"
```

This BlockchainOperation describes a balance transfer (and, consequentially, a contract invocation) operation.  Its arguments are:

- Nonce (`int`): A cryptographic nonce attached to each new BlockchainOperation created.  No two operations created separately (i.e. by two different `TRANSFER_TOKENS` executions) will never share the same nonce, but an operation duplicated with the `DUP` instruction will.
- Parameter (`T`): The parameter passed to the contract being invoked (or Unit, if the target contract is an implicit account).
- Amount (`mutez`): The quantity of mutez to transfer to the target contract.
- Address (`address`): The address of the target contract.

```k
  syntax BlockchainOperation ::= "Transfer_tokens" "(" Int "," Data "," Mutez "," Address ")"
```

This BlockchainOperation describes a delegation operation.  Its arguments are:

- Nonce (`int`): A cryptographic nonce attached to each new BlockchainOperation created.  No two operations created separately (i.e. by two different `SET_DELEGATE` executions) will never share the same nonce, but an operation duplicated with the `DUP` instruction will.
- Delegate (`option key_hash`): An optional new delegate specified by key hash.  If None, then this operation clears the current delegate of the contract.

```k
  syntax BlockchainOperation ::= "Set_delegate" "(" Int "," OptionData ")"
```

This production represents a stack resulting from the `FAILWITH` instruction.  Its argument is the data element on top of the stack when the `FAILWITH` was executed.

```k
  syntax FailedStack ::= "(" "Failed" Data ")"
```

This production represents a stack resulting from an operation on mutez which would have created a mutez value greater than the maximum representable value of 2^63 - 1.  Its arguments are the two input mutez values involved in the operation.

```k
  syntax FailedStack ::= "(" "MutezOverflow" Int Int ")"
```

This production represents a stack resulting from an operation on mutez which would have created a mutez value less than the minimum representable value of 0.  Its arguments are the two input mutez values involved in the operation.

```k
  syntax FailedStack ::= "(" "MutezUnderflow" Int Int ")"
```

This production represents a stack resulting from a non-mutez overflow, such as those produced by the `LSL` and `LSR` instructions when their shift argument is greater than 256.

```k
  syntax FailedStack ::= "(" "GeneralOverflow" Int Int ")"
```

All blockchain operations are data.  This line is necessary so that BlockchainOperations may be placed on a stack in a unit test.

```k
  syntax Data ::= BlockchainOperation
```

For representing pre- and post-conditions, we use this syntax:

```k
  syntax BlockList ::= Block | Block ";" BlockList
  syntax Blocks ::= EmptyBlock | "{" BlockList "}"
```

```k
endmodule
```
