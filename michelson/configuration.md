Michelson Interpreter State
===========================

K-Michelson: an intra-contract semantics
----------------------------------------

Recall that Michelson is a blockchain programming language (i.e., the
programming language used on the Tezos blockchain). As such, the initial state
for any Michelson script execution is provided by the current blockchain head
upon which it is executed. This means that as blocks are added to the chain,
executing the same script on the new chain head may produce different results.

As such, we can choose to model Michelson code execution on two levels, i.e.:

1. modeling how Michelson code will execute by limiting ourselves to a
   particular blockchain head, or;
2. we can additionally model how the Tezos blockchain evolves by modeling:
   - Michelson script-driven (internal) vs. off-chain (external) originated
     transactions including: acount creation, delegation, Tezos tranfer, etc...
   - block baking
   - protocol amendment
   - etc...

We refer to (1) as a _intra-contract_ semantics and (2) as a _inter-contract_
semantics.

The important thing to note from all of this: K-Michelson is an intra-contract
semantics --- thus we avoid worrying about lots of details involved in modeling
blockchain state.

### Intra-contract semantic styles

Within the intra-contract semantic framework, we have two main alternatives to
consider. We can either model:

1. Contract semantics --- executing a Michelson script as it would appear in a
   contract account on the Tezos blockchain, i.e., one that consumes an initial
   stack of the form:

   `Stack_elt (pair 'parameter_type 'storage_type)`

   and produces a final stack of the form:

   `Stack_elt (pair (list operation) 'storage_type)`;

2. General semantics --- exeucting an arbitray snippet of Michelson code that
   consumes an iput stack of arbitrary type and outputs a Michelson stack of
   arbitrary type.

Note that, as the name suggests, a general semantics is more general than a
contract semantics, since it can model arbitrary Michelson expressions ---
including Michelson expressions which are equivalent to standard contracts.

In particular, K-Michelson is an _general_, intra-contract semantics. This means
we can represent the execution of arbitrary Michelson expressions as they would
appear in an arbitrary contract on an arbitrary blockchain head.

The K-Michelson Configuration
-----------------------------

In K, the `configuration` captures a system's state, while `rule`s define how a
system transitions from one state to the next state. In this file, we define a
configuration for Michelson which captures three kinds of execution state:

1. Tezos blockchain context: all of the Tezos blockchain state that is
   accessible to a Michelson script, such as the current block creation time,
   the current script address, etc...
2. Michelson runtime state: additional runtime state needed to execute a
   Michelson script such as the Michelson script's current continuation and
   stack, etc...
3. Additional test state: additional state needed to execute Michelson code in
   precise configurations such as expected input and output stacks, pre- and
   post-conditions, etc...

Note that for K-Michelson, all type (1) configuration cells are _write-once_ in
the sense that, once they are initialized, they are never changed. This is
precisely because K-Michelson is an _intra-contract_ semantics, as we mentioned
above.

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

We first define a special directive `#Init` which we will describe later.

```k
syntax KItem ::= "#Init"
```

Here we declare our K-Michelson state configuration. We separate it into type
(1)-(3) configuration cells as listed above. By convention, we nest all state
cells inside a topmost cell, which we call `<michelsonTop>`.

```k
  configuration <michelsonTop>
```

### Tezos Blockchain Context

As mentioned above, in K-Michelson, the Tezos blockchain context state cells are
all _write-once_, i.e. after initialization, they are never changed.

1. We need to store the parameter type of the contract containing the Michelson
   code to be executed. For standard Michelson contract execution, we also need
   to know the parameter value that was passed in so we can properly initialize
   the Michelson input stack.

    ```k
                  <paramtype> #NotSet </paramtype>
                  <paramvalue> #NoData </paramvalue>
    ```

2. We also need to store the storage type of the contract containing the
   Michelson code to be executed as well as the contracts current storage
   value.

    ```k
                  <storagetype> #NotSet </storagetype>
                  <storagevalue> #NoData </storagevalue>
    ```

3. Each contract has a remaining balance of mutez. The `BALANCE` instruction
   pushes this value on the stack.

    ```k
                  <mybalance> #Mutez(0) </mybalance>
    ```

4. Each contract is given a certain quantity of mutez when it is first
   executed.  The `AMOUNT` instruction pushes this value to the stack.

    ```k
                  <myamount> #Mutez(0) </myamount>
    ```

5. Each Michelson block has an associated creation timestamp. Blocks also
   contain a list of transactions, including contract executions. Each
   Michelson contract execution can access the timestamp of the block which
   invoked it.  The `NOW` instruction pushes this value on the stack.

    ```k
                  <mynow> #Timestamp(0) </mynow>
    ```

6. Each Michelson contract has an address. The `SELF` instruction combines this
   value with the `<paramtype>` cell to produce a `contract` value that is
   pushed on the stack. This value can be accessed by the snippet `SELF ;
   ADDRESS`. Note that a later amendment to the Tezos protocol may add a
   `SELF_ADDRESS` instruction that would allow this value to be accessed
   directly.

    ```k
                  <myaddr> #Address("InvalidMyAddr") </myaddr>
    ```

7. At any given moment, the Tezos current block has a certain number of
   accounts with various parameter types. This cell stores the addresses and
   types of all contract accounts on the curent Tezos block. The `CONTRACT`
   instruction allows these values to be accessed.

   N.B. The values in `<myaddr>`, `<sourceaddr>`, and `<senderaddr>` are not
   required to be in this map. If they are not, then looking them up with the
   `CONTRACT` instruction will return `None`.

    ```k
                  <knownaddrs> .Map </knownaddrs>
    ```

8. Each contract has a source contract (the one which initiated the entire
   transaction and paid its fees). The`SOURCE` instruction pushes this address
   on the stack.

    ```k
                  <sourceaddr> #Address("InvalidSourceAddr") </sourceaddr>
    ```

9. Each contract has a sender contract (the one which transferred tokens to
   this one and directly caused its execution). The `SENDER` instruction pushes
   this value on the stack.

    ```k
                  <senderaddr> #Address("InvalidSenderAddr") </senderaddr>
    ```

10. Each Tezos network (the global main network, global testing network, as
    well as any local sandboxed networks for development) have their own
    blockchain. A chain identifier identifies this execution context. The
    `CHAIN_ID` instruction pushes the current chain identifier on the stack.

    ```k
                  <mychainid> #ChainId(0x) </mychainid>
    ```

11. Each contract account contains a nonce which will be attached to any new
    `operation` that this contract forges such that no two `operation`s will
    share the same nonce (this restriction does not apply to any `operation`
    that was copied via `DUP`). The operation nonce is not directly observable
    by Michelson.

    ```k
                  <nonce> #Nonce(0) </nonce>
    ```

12. Tezos provides two kinds of map-like data structures, `map` and `big_map`.
    As maps, they are functionally equivalent --- the difference is that
    `big_map`s are accessed and updated lazily, i.e. `big_map`s are:

    - passed by reference;
    - particular values are only accessed on demand;
    - only updated in-place after contract execution --- any contract internal
      updates are stored as a list of differences.

    As such, the Tezos blockchain context must maintain a table of pointers to
    `big_map`s. This table is stored in the `<bigmaps>` configuration cell
    below.  This table is represented by a map of integers (i.e. `big_map` ids)
    to maps.

    N.B. The semantics behavior is undefined if a `big_map` index occurs in a
    Michelson code expression that is not set in this cell.

    ```k
                  <bigmaps> .Map </bigmaps>
    ```

13. The `<script>` cell stores the code of the smart contract to be executed.
    However, since K-Michelson is a general semantics, the script cell may
    include any valid Michelson expression, including expressions that
    represent fragments of contracts and which are not complete contracts.

    ```k
                  <script> #NoData </script>
    ```

### Michelson Runtime State

As mentioned above, Michelson script execution requires additional runtime
state. We list the configuration cells storing this kind of state below.

1. The `<k>` cell is the heart of a K semantics. It represents the current
[continuation](https://en.wikipedia.org/wiki/Continuation) of a program.

   Initially, the continuation is just the Michelson contract/expression code
   itself paired with the initialization function `#Init` (defined below).

   As the program executes, its current continuation changes to represent the
   currently to-be-executed computation. We represent this by dynamically
   modifying the Michelson code contained in the `<k>` cell, e.g. the
   continuation `<k> ADD ; SUB </k>` reduces by an application of the semantic
   rule defining the `ADD` instruction to the continuation `<k> SUB </k>`.
   Eventually, in case of a normal execution, the current continuation becomes
   empty.

   The special constant `#Init` is used to control initialization of each
   semnatics. It works by having each driver module define a single rule of the
   form: `rule #Init => ...`. This is needed because different drivers may need
   to perform different pre-processing tasks.

    ```k
                  <k> $PGM:Pgm ~> #Init </k>
    ```


2. The `<stack>` cell contains the data on the stack of the current Michelson
   program, and is initially empty. When we are executing a standard Michelson
   contract, its initial form will always be:

   `Stack_elt (pair 'parameter_type 'storage_type)`.

   In the general case, the stack may be initialized to any well-typed
   Michelson stack.

   Since currently, the Michelson stack values are stored in a way that loses
   some type information, we have an additional cell to capture type
   information precisely.

    ```k
                  <stack> .K </stack>
                  <stacktypes> .TypeSeq </stacktypes>
    ```

### Additional Test State

Finally, we have configuration cells which represent additional state only
useful for testing Michelson in precise configurations or for verification
of Michelson code. We list these configuration cells here:

1. The `<inputstack>` cell contains the initial input stack for the given
   Michelson execution as prescribed by the Michelson unit test format. Storing
   this separately from the `<stack>` cell lets us remember the initial stack
   information without worry that it will be clobbered during code execution.
   It is useful for test execution, type-checking, and debugging purposes.

    ```k
                  <inputstack> .K </inputstack>
    ```

2. The `<expected>` cell contains the expected output stack for the given
   Michelson execution as prescribed by the Michelson unit test format. It is
   used for test validation, type-checking, and debugging purposes.

    ```k
                  <expected> .K </expected>
    ```

3. Theese cells contain pre- and post-conditions, which are useful when doing
   verification of Michelson expressions with symbolic input and output values.

    ```k
                  <pre> {}:Blocks </pre>
                  <post> {}:Blocks </post>
    ```

4. This cell lists the bindings between symbolic variables and their values. It
   is only used when symbolically executing/verifying Michelson scripts.

    ```k
                  <symbols> .Map </symbols>
    ```

5. This cell stores the return code of the K-Michelson interpreter. It tracks
   whether the Michelson code in question terminated properly as opposed to
   getting stuck due to a type-error, ill-formed input, or a bug in the
   semantics. It is initially set to `1` and changes to `0` when the script
   executes successfully.

    ```k
                  <returncode exit=""> 1 </returncode>
    ```


6. The following cell is a debugging aid, indicating whether an `#Assume`
   statement failed. It is primarily used during Michelson code verification.

    ```k
                  <assumeFailed> false </assumeFailed>
    ```

Here we finalize the configuration declaration by closing the topmost
configuration cell.

    ```k
                </michelsonTop>
    ```

```k
endmodule
```
