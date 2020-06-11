The configuration of a K semantics captures all of the mutable state which rules may act upon.  In a Michelson contract, this includes the current continuation derived from contract code, the current stack, and various pieces of state related to the blockchain.  Note that, while in general all cells of a `K` configuration are mutable, once contract execution has begun only the `k`, `stack`, `nonce` and `returncode` cells will actually mutate.  The other cells are set at most once and then remain the same throughout execution.

```k
requires "michelson/syntax.md"
requires "michelson/common.md"
requires "michelson/types.md"

module MICHELSON-CONFIG
  imports MICHELSON-SYNTAX
  imports MICHELSON-COMMON
  imports MICHELSON-TYPES
  imports DOMAINS
```

The `<k>` cell is the main cell of a K semantics, which represents the current [continuation](https://en.wikipedia.org/wiki/Continuation) of a program.
Initially, the continuation is just the code corresponding to an entire script/contract, which is derived from the source code of the file passed on the command line after being parsed according to the grammar described in the MICHELSON-SYNTAX module and preprocessed by a number of macro rules.
As the program executes, its current continuation changes to represent the currently to-be-executed computation.
We represent this by dynamically modifying the Michelson code contained in the `<k>` cell, e.g. the continuation `<k> ADD ; SUB </k>` reduces by an application of the semantic rule defining the `ADD` instruction to the continuation `<k> SUB </k>`.
Eventually, in case of a normal execution, the current continuation becomes empty, i.e., reduces to `.K`, meaning that the entire requested computation finished without triggering an exception.

```k
  configuration <michelsonTop>
                  <k> $PGM:Pgm </k>
```

The `<stack>` cell contains the data on the stack of the current Michelson program, and is initially empty.  The initial contract loader rules will populate it before handing over control to the contract execution rules by loading the code in the `contract` group (or `code`, in the event of a unit test).  Its initial value in the case of a full contract execution will be a pair of the parameter and storage values passed to the contract through the `parameter_value` and `storage_value` applications.

```k
                  <stack> .K </stack>
                  <stacktypes> .TypeSeq </stacktypes>
```

This cell contains the type of the parameter of this contract.  It may be left unset during the execution of a unit test.

```k
                  <paramtype> #NotSet </paramtype>
```

This cell contains the value of the parameter of this contract.  It may be left unset during the execution of a unit test.

```k
                  <paramvalue> #NoData </paramvalue>
```

This cell contains the type of the storage of this contract.  It may be left unset during the execution of a unit test.


```k
                  <storagetype> #NotSet </storagetype>
```

This cell contains the value of the storage of this contract.  It may be left unset during the execution of a unit test.


```k
                  <storagevalue> #NoData </storagevalue>
```

This cell contains the mutez balance of the current contract.  This value will be pushed to the stack on the execution of a `BALANCE` instruction.

```k
                  <mybalance> #Mutez(0) </mybalance>
```

This cell contains the quantity of mutez transferred to this contract to begin the current execution.  This value will be pushed to the stack on the execution of an `AMOUNT` instruction.

```k
                  <myamount> #Mutez(0) </myamount>
```

This cell contains the timestamp of the ["block whose validation triggered this execution"](https://tezos.gitlab.io/whitedoc/michelson.html).  This value will be pushed to the stack on the execution of a `NOW` instruction.

```k
                  <mynow> #Timestamp(0) </mynow>
```

This cell contains the address of the current contract.  When the `SELF` instruction is executed, this value will be combined with the `<paramtype>` cell to produce a `contract` value to push to the stack.  This value can be accessed by the snippet `SELF ; ADDRESS`.

[//]: # (Note that next proposal will probably add a SELF_ADDRESS instruction for this: https://gitlab.com/cryptiumlabs/tezos/-/merge_requests/75)

```k
                  <myaddr> #Address("InvalidMyAddr") </myaddr>
```

This cell contains a mapping from addresses to contract data structures which represents the set of addresses "known" to this contract for the purposes of the `CONTRACT` instruction.  While in principle a Michelson contract may lookup any contract on the blockchain with that instruction, for the purposes of K-Michelson actually exposing all contracts on the chain would be infeasible.  Furthermore, a developer may wish to reason about interacting with a contract that has not actually been originated yet using K-Michelson.  As a consequence, we instead require that any contract to be looked up with the `CONTRACT` instruction is added to this map using the `other_contracts` group.

The values in `<myaddr>`, `<sourceaddr>`, and `<senderaddr>` are *not* required to be in this map. But, if they are not, then looking them up with the `CONTRACT` instruction will return `None`.

```k
                  <knownaddrs> .Map </knownaddrs>
```

This cell contains the address of the source contract (the one which initiated the entire transaction, and paid its fees).  It will be pushed to the stack by the execution of the `SOURCE` instruction.

```k
                  <sourceaddr> #Address("InvalidSourceAddr") </sourceaddr>
```

This cell contains the address of the sender contract (the one which transferred tokens to this one and directly caused its execution).  It will be pushed to the stack by the execution of the `SENDER` instruction.


```k
                  <senderaddr> #Address("InvalidSenderAddr") </senderaddr>
```

This cell contains the `chain_id` of the current contract.  It will be pushed to the stack by the execution of the `CHAIN_ID` instruction.

```k
                  <mychainid> #ChainId(0x) </mychainid>
```

This cell contains a counter nonce which will be attached to any new BlockchainOperations forged by this contract, and incremented each time such that no two BlockchainOperations created by different executions of an operation producing instruction (`TRANSFER_TOKENS`, `SET_DELEGATE` and `CREATE_CONTRACT`) will have the same nonce (this restriction does not apply to operations copied with `DUP`).

```k
                  <nonce> #Nonce(0) </nonce>
```

This cell contains a mapping of any `big_map` structures attached to the current contract, represented by a map of Integers (the `big_map`'s id) to maps).  If a `big_map` is specified by index anywhere in the contract, that index must be set here.  Note that, since big\_maps are immutable in Michelson, this cell is **not** mutated during execution.  Hence, updates to a big\_map at a given index will not be reflected.

[//]: # (I find it a bit strange to say that big_map are immutable; the purpose of the UPDATE instruction is really to update the big_map; it is however true that the big_map is not updated during the execution but right after execution.)

```k
                  <bigmaps> .Map </bigmaps>
```

This cell indicates to krun whether or not the contract or unit test under execution has actually terminated properly as opposed to getting stuck due to a type or other well-formedness error, or a bug in the semantics.  The exit code of krun will match the final contents of this cell.

```k
                  <returncode exit=""> 1 </returncode>
                </michelsonTop>
endmodule
```
