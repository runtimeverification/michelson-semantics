```k
requires "common.md"
requires "types.md"
```

Michelson Interpreter State
===========================

```k
module MICHELSON-CONFIG
  imports MICHELSON-COMMON
  imports DOMAINS
```

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
                  <mychainid> #ChainId(.Bytes) </mychainid>
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

    ```k
                  <stack> (.Stack):InternalStack </stack>
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
                  <inputstack> { .StackElementList } </inputstack>
    ```

2. The `<expected>` cell contains the expected output stack for the given
   Michelson execution as prescribed by the Michelson unit test format. It is
   used for test validation, type-checking, and debugging purposes.

    ```k
                  <expected> ({ .StackElementList }):OutputStack </expected>
    ```

3. These cells contain pre- and post-conditions, as well as loop invariants.
   These are useful when doing verification of Michelson expressions with
   symbolic input and output values.

    ```k
                  <pre> .BlockList </pre>
                  <post> .BlockList </post>
                  <invs> .Map </invs>
    ```

4. In the symbolic semantics, the cutpoint cell contains a list of cutpoints
   that have been visited.

    ```internalized-rl
                  <cutpoints> .Set </cutpoints>
    ```

5. This cell lists the bindings between symbolic variables and their values. It
   is only used when symbolically executing/verifying Michelson scripts.

    ```k
                  <symbols> .Map </symbols>
    ```

6. This cell stores the return code of the K-Michelson interpreter. It tracks
   whether the Michelson code in question terminated properly as opposed to
   getting stuck due to a type-error, ill-formed input, or a bug in the
   semantics. It is initially set to `1` and changes to `0` when the script
   executes successfully.

   ```k
                 <returncode exit=""> 1 </returncode>
   ```

7. The following cell is a debugging aid, indicating whether an `#Assume`
   statement failed. It is primarily used during Michelson code verification.

    ```k
                  <assumeFailed> false </assumeFailed>
    ```

8. The following cell is a debugging aid. It stores a sequence of strings
   corresponding to all `TRACE` instructions reached during program execution.

    ```k
                  <trace> .K </trace>
    ```

Here we finalize the configuration declaration by closing the topmost
configuration cell.

```k
                </michelsonTop>
```

```k
endmodule
```

Michelson Semantics
===================

This is the main execution semantics for Michelson. It contains the rewrite
rule semantics for the all Michelson instructions, as well as logic for
transforming values from their Micheline representations to K internal
representations. It also contains the .tzt file loading and contract
initialization logic.


We implement the unit test section of the .tzt format described by the
Tezos foundation
[here](https://gitlab.com/tezos/tezos/-/merge_requests/1487/diffs). This file
implements the behavior of the 'code,' 'input,' and 'output' applications
discussed in that document.

```k
module MICHELSON
  imports MICHELSON-CONFIG
  imports MATCHER
```

Semantics Initialization
------------------------

`#Init` takes care of initialization.

```k
  rule <k> #Init
        => #ConvertBigMapsToNative
        ~> #ConvertParamToNative
        ~> #ConvertStorageToNative
        ~> #ExecutePreConditions
        ~> #InitInputStack
        ~> #ExecuteScript
        ~> #ExecutePostConditions
           ...
       </k>
```

### Group Loading

Below are the rules for loading specific groups.

#### Default Contract Groups

```k
  rule <k> parameter T => .K ... </k>
       <paramtype> #NotSet => T </paramtype>

  rule <k> storage T => .K ... </k>
       <storagetype> #NotSet => T </storagetype>

  rule <k> code C => .K ... </k>
       <script> #NoData => C </script>
```

#### Extended Unit Test Gruops

```k
  rule <k> G:Group ; Gs:Groups => G:Group ~> Gs ... </k>

  rule <k> now I => .K ... </k>
       <mynow> #Timestamp(0 => I) </mynow>

  rule <k> sender A => .K ... </k>
       <senderaddr> #Address("InvalidSenderAddr" => A) </senderaddr>

  rule <k> source A => .K ...  </k>
       <sourceaddr> #Address("InvalidSourceAddr" => A) </sourceaddr>

  rule <k> chain_id M => .K ... </k>
       <mychainid> #ChainId(_ => M) </mychainid>

  rule <k> self A => .K ... </k>
       <myaddr> #Address("InvalidMyAddr" => A) </myaddr>

  rule <k> amount I => .K ... </k>
       <myamount> #Mutez(0 => I) </myamount>
    requires #IsLegalMutezValue(I)

  rule <k> balance I => .K ... </k>
       <mybalance> #Mutez(0 => I) </mybalance>
    requires #IsLegalMutezValue(I)

  rule <k> other_contracts { M } => .K ... </k>
       <knownaddrs> .Map => #OtherContractsMapEntryListToKMap(M) </knownaddrs>

  rule <k> big_maps { M } => .K ... </k>
       <bigmaps> .Map => #BigMapsEntryListToKMap(M) </bigmaps>

  syntax Map ::= #BigMapsEntryListToKMap(BigMapEntryList) [function]
  syntax Map ::= #BigMapsEntryToKMap(BigMapEntry) [function]

  rule #BigMapsEntryListToKMap(.BigMapEntryList) => .Map
  rule #BigMapsEntryListToKMap(E ; Es) => #BigMapsEntryToKMap(E) #BigMapsEntryListToKMap(Es)

  syntax KItem ::= "#BigMap" "(" MapLiteral "," Type ")"
  rule #BigMapsEntryToKMap(Big_map I T1 T2 { }            ) => I |-> #BigMap({ }, big_map .AnnotationList T1 T2)
  rule #BigMapsEntryToKMap(Big_map I T1 T2 ML:NeMapLiteral) => I |-> #BigMap(ML,  big_map .AnnotationList T1 T2)

  rule <k> invariant Annot { Stack } { Blocks } => . ... </k>
       <invs> .Map
           => (Annot |-> { Stack } { Blocks })
              ...
       </invs>

  rule <k> input LS => .K ... </k>
       <inputstack> { .StackElementList } => LS </inputstack>

  rule <k> output OS => .K ... </k>
       <expected> { .StackElementList } => OS </expected>

  rule <k> precondition { Bs } => .K ... </k>
       <pre> .BlockList => Bs </pre>

  rule <k> postcondition { Bs } => .K ... </k>
       <post> .BlockList => Bs </post>
```

### Micheline to Native Conversion

```k
  syntax KItem ::= "#ConvertBigMapsToNative"
  rule <k> #ConvertBigMapsToNative => .K ... </k>
       <bigmaps> BigMaps => #ConvertBigMapsToNative(BigMaps) </bigmaps>

  syntax Map ::= "#ConvertBigMapsToNative" "(" Map ")" [function]

  rule #ConvertBigMapsToNative(.Map) => .Map
  rule #ConvertBigMapsToNative(I |-> #BigMap(D, T) BigMaps)
   => I |-> #MichelineToNative(D, T, .Map, .Map) #ConvertBigMapsToNative(BigMaps)
```

```k
  syntax KItem ::= "#ConvertParamToNative"
  rule <k> #ConvertParamToNative => .K ... </k>
       <paramvalue> D:Data => #MichelineToNative(D, #ConvertToType(T), .Map, BigMaps) </paramvalue>
       <paramtype>  T      => #ConvertToType(T)                                       </paramtype>
       <bigmaps> BigMaps </bigmaps>

  rule <k> #ConvertParamToNative => .K ... </k>
       <paramvalue> #NoData                </paramvalue>
       <paramtype>  T => #ConvertToType(T) </paramtype>

  syntax KItem ::= "#ConvertStorageToNative"
  rule <k> #ConvertStorageToNative => .K ... </k>
       <storagevalue> D:Data => #MichelineToNative(D, #ConvertToType(T), .Map, BigMaps) </storagevalue>
       <storagetype>  T      => #ConvertToType(T)                                       </storagetype>
       <bigmaps> BigMaps </bigmaps>

  rule <k> #ConvertStorageToNative => .K ... </k>
       <storagevalue> #NoData                </storagevalue>
       <storagetype>  T => #ConvertToType(T) </storagetype>

  syntax Type ::= #ConvertToType(PreType) [function]
  rule #ConvertToType(#NotSet) => unit .AnnotationList
  rule #ConvertToType(T:Type)  => T
```

### Type Checking

Executing Michelson code without type information leads to non-determinism.
For example, the `CONCAT` instruction, when applied to an empty list, produces
either an empty `string` or empty `bytes`. Without knowing the type of the list,
the resulting type of value is unknown.

To correctly check the typing of a unit test, we need the following info:

1. the contract parameter type --- only used in typing the `SELF` instruction
2. the input stack types --- which depend on (1) because `lambda`
3. the output stack types --- which depend on (1) for the same reason
4. a Michelson script

Currently, we implement runtime type checking. We may adopt static type
checking at a later time. All type checking requires looking at the top
fragment of of the stack only, with the exception of instructions which
have code in their immediate arguments (1-2) or execute `lambda`s (3).

1. Checking if-like instructions `IF` and `IF_X` requires checking that both
   branches produce the same the stack type.
2. Checking iterating instructions `LOOP` and `LOOP_LEFT`, `ITER`, and `MAP`
   requires checking thet loop body code preserves the original stack type.
3. Checking function-call instruction `EXEC` requires checking the code
   returns a singleton stack of the correct type.

We currently do not implement the full typing semantics for cases (1)-(2).
Practically, this means that only *executed* code is type-checked, i.e. untaken
branches are not checked and loops are only checked for the exact number of
iterations for which they are run. This means that, even if some execution
succeeds, executing the other branch of an if instruction or executing a loop
a different number of times may cause the program to get stuck.

**TODO**: the type of a `MAP` instruction over an empty `list` or `map` is
currently incorrectly calculated, as we assume it is equivalent to the original
`map` or `list` that was passed in, which disagrees with the intended
semantics that the type matches whatever the to-be executed code block does.

#### Typing `FAILWITH`

The `FAILWITH` instruction is interesting because its resulting type is the
*universal stack type* which unifies with all other stack types. However, due
to its unique semantics, it poisons all further computation, so that entire
program returns a `(Failed D)`. For this reason, from our experimentation, we
have observed the reference implementation makes two requirements on `FAILWITH`
instruction placement:

1.  It must occur last in the code block it appears in (this makes sense,
    because all code following it is unreachable, i.e., dead).
2.  It may not occur in positions that would prevent a concrete type from
    being inferred. From our understanding, this can happen in two ways:

    - code of the form `IF_X { ... ; FAILWITH } { ... ; FAILWITH }` which
      prevents inferring a concrete stack type for the `IF_X` instruction;
    - code of the form `MAP { ... ; FAILWITH }` which prevents the inferring
      a concrete value type for the resulting `map key_ty val_ty` or `list ty`
      that it produces.

Currently, we do enforce these restrictions, but we may do so in a future
version.

### Stack Initialization

```k
  syntax KItem ::= "#InitInputStack"
  rule <k> #InitInputStack => #StackToNative(Actual, .Stack) ... </k>
       <inputstack> { Actual } </inputstack>

  syntax KItem ::= #StackToNative(StackElementList, Stack)
  // -----------------------------------------------------
  rule <k> #StackToNative(Stack_elt T D ; Stack, Stack')
        => #StackToNative(Stack, [ #Name(T) #MichelineToNative(D, T, Addrs, BigMaps) ] ; Stack')
           ...
       </k>
       <knownaddrs> Addrs </knownaddrs>
       <bigmaps> BigMaps </bigmaps>
    requires notBool isSymbolicData(D)

  rule <k> #StackToNative(.StackElementList, Stack) => .K ... </k>
       <stack> _ => reverseStack(Stack) </stack>
```

```internalized-rl
  rule <k> (.K => #CreateSymbol(X, T))
        ~> #StackToNative(Stack_elt T X:SymbolicData ; Stack, Stack')
           ...
       </k>
       <symbols> Symbols  </symbols>
    requires notBool X in_keys(Symbols)

  rule <k> #StackToNative(Stack_elt T X:SymbolicData ; Stack, Stack')
        => #StackToNative(Stack, [ TN D ] ; Stack')
           ...
       </k>
       <symbols> X |-> #TypedSymbol(TN, D) ... </symbols>
    requires TN ==K #Name(T)
```

### Code Execution

```k
  syntax KItem ::= "#ExecutePreConditions"
  rule <k> #ExecutePreConditions => ASSUME { Preconditions } ... </k>
       <pre> Preconditions </pre>

  syntax KItem ::= "#ExecutePostConditions"
  rule <k> #ExecutePostConditions
        => BIND Expected { ASSERT { Postconditions } }
           ...
       </k>
       <expected> Expected </expected>
       <post> Postconditions </post>

  syntax KItem ::= "#ExecuteScript"
  rule <k> #ExecuteScript => Script ... </k>
       <script> Script </script>
```

Execution Semantics
===================

Miscellaneous
-------------

When the `<k>` cell is empty, we consider execution successful.

```k
  rule <k> . </k>
       <returncode> 1 => 0 </returncode>
```

We handle sequence and block semantics here.

```k
  rule I:Instruction ; Is => I ~> Is [structural]
  rule {}                 => .K      [structrual]
  rule { Is:DataList }    => Is      [structural]
```

For now, we ignore annotations.

```k
  syntax Instruction ::= #HandleAnnotations(AnnotationList)
  rule #HandleAnnotations(_) => .
```

Control Structures
------------------

### User-defined Exceptions

The `FAILWITH` instruction lets users terminate execution at any point.

```k
  rule <k> FAILWITH A ~> Rk
        => #HandleAnnotations(A)
        ~> Aborted("FAILWITH instruction reached", D, Rk, Rs)
        ~> Rk
       </k>
       <stack> [ _Type D:Data ] ; Rs => ( Failed D ) </stack>
```

`Aborted()` contains error information when a contract fails at runtime.

```k
  syntax Error ::= Aborted(message: String,
                           stackTop: KItem,
                           restOfStack: K,
                           restOfContinuation: K)
```

It then consumes the rest of the program:

```k
  rule <k> Aborted(_, _, _, _) ~> (_:DataList => .K) ... </k>
  rule <k> Aborted(_, _, _, _) ~> (_:Data => .K) ... </k>
```

Currently, if a program aborts due to the FAILWITH instruction, we throw away
the abortion debug info:

```k
  rule <k> (Aborted(_, _, _, _) => .K) ~> #ExecutePostConditions ... </k>
```

### Conditionals

The control flow instruction's implementations in K should look extremely
similar to their formal description in the [Michelson
documentation](https://tezos.gitlab.io/whitedoc/michelson.html#control-structures).
Keeping this similarity, unless absolutely prevented for performance or K style
reasons, was a major design goal of the semantics.

```k
  rule <k> IF A  BT _  => #HandleAnnotations(A) ~> BT ... </k>
       <stack> [ bool true  ] ; SS => SS </stack>

  rule <k> IF A  _  BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> [ bool false ] ; SS => SS </stack>
```

### Loops

Here we handle concrete loop semantics.

```k
  rule <k> LOOP .AnnotationList Body
        => Body ~> LOOP .AnnotationList Body
           ...
       </k>
       <stack> [ bool true  ] ; SS => SS </stack>

  rule <k> LOOP .AnnotationList _ => .K ... </k>
       <stack> [ bool false ] ; SS => SS </stack>
```

```k
  rule <k> LOOP_LEFT .AnnotationList Body
        => Body
        ~> LOOP_LEFT .AnnotationList Body
           ...
        </k>
       <stack> [ (or LX _) Left D ] ; SS
           =>  [ LX D ] ; SS
       </stack>

  rule <k> LOOP_LEFT .AnnotationList _ => .K ... </k>
       <stack> [ (or _ RX) Right D ] ; SS
            => [ RX D ] ; SS
       </stack>
```

Here we handle symbolic loop semantics.

```internalized-rl
  rule <k> LOOP A .AnnotationList Body
        => CUTPOINT(!Id, Invariant) ;
           LOOP .AnnotationList {
             Body ;
             CUTPOINT(!Id, Invariant)
           }
           ...
       </k>
       <invs> A |-> Invariant ... </invs>

  rule <k> LOOP_LEFT A .AnnotationList Body
        => CUTPOINT(!Id, Invariant) ;
           LOOP_LEFT .AnnotationList {
             Body ;
             CUTPOINT(!Id, Invariant)
           }
           ...
       </k>
       <invs> A |-> Invariant ... </invs>
```

### Stack Manipulation

It is sometimes useful to create "pseudo-instructions" like this to schedule
operations to happen in the future.

```k
  syntax Instruction ::= #Push(TypeName,Data)
  rule <k> #Push(T,D) => . ... </k>
       <stack> SS => [ T D ] ; SS </stack>
```

The `DIP` instruction uses the `#Push` pseudo-instruction to replace the
element it pops off for its block.

```k
  rule <k> DIP A B => #HandleAnnotations(A) ~> B ~> #Push(T,D) ... </k>
       <stack> [ T D ] ; SS => SS </stack>

  rule <k> DIP A 0 B => #HandleAnnotations(A) ~> B ... </k>

  rule <k> DIP A N B
         => #HandleAnnotations(A)
         ~> DIP .AnnotationList { DIP .AnnotationList N -Int 1 B }
            ...
       </k>
    requires N >Int 0
```

`DROP n` is implemented in a recursive style, like in the Michelson
documentation.

```k
  rule <k> DROP A =>  #HandleAnnotations(A) ... </k>
       <stack> _:StackElement ; SS => SS </stack>

  rule <k> DROP A I
        => #HandleAnnotations(A)
        ~> DROP .AnnotationList
        ~> DROP .AnnotationList I -Int 1
           ...
       </k>
    requires I >Int 0

  rule <k> DROP A 0 => #HandleAnnotations(A) ... </k>
```

`DUP` and `SWAP` are essentially lifted directly from the docs.

```k
  rule <k> DUP A => #HandleAnnotations(A) ... </k>
       <stack> X:StackElement ; SS => X ; X ; SS </stack>

  rule <k> SWAP A => #HandleAnnotations(A) ... </k>
       <stack> X:StackElement ; Y:StackElement ; SS
            => Y ; X ; SS
       </stack>
```

`DIG n` and `DUG n` are both implemented using two internal instructions:
`X_DOWN` and `X_UP` which descend down to the `n`th stack position and then
climb back up, respectively.

```k
  rule <k> DIG A N => #HandleAnnotations(A) ~> DIG_DOWN(N, .Stack) ... </k>

  syntax Instruction ::= "DIG_DOWN" "(" Int "," Stack ")"
                       | "DIG_UP" "(" Stack "," StackElement ")"
  // -----------------------------------------------------------
  rule <k> DIG_DOWN(N, A) => DIG_DOWN(N -Int 1, F ; A) ... </k>
       <stack> F ; SS => SS </stack>
    requires N >Int 0

  rule <k> DIG_DOWN(0, A) => DIG_UP(A, F) ... </k>
       <stack> F ; SS => SS </stack>

  rule <k> DIG_UP(F ; A, T) => DIG_UP(A, T) ... </k>
       <stack> SS => F ; SS </stack>

  rule <k> DIG_UP(.Stack, T) => . ... </k>
       <stack> SS => T ; SS </stack>

  rule <k> DUG A N => #HandleAnnotations(A) ~> DUG_DOWN(N, .Stack, T) ... </k>
       <stack> T ; SS => SS </stack>

  syntax Instruction ::= "DUG_DOWN" "(" Int "," Stack "," StackElement ")"
                       | "DUG_UP" "(" K ")"
  // ---------------------------------------------------------------------
  rule <k> DUG_DOWN(N, S, R) => DUG_DOWN(N -Int 1, T ; S, R) ... </k>
       <stack> T ; SS => SS </stack>
    requires N >Int 0

  rule <k> DUG_DOWN(0, S, R) => DUG_UP(S) ... </k>
       <stack> SS => R ; SS </stack>

  rule <k> DUG_UP(T:StackElement ; S) => DUG_UP(S) ... </k>
       <stack> SS => T ; SS </stack>

  rule <k> DUG_UP(.Stack) => .K ... </k>
```

#### `PUSH`-like Instructions

`PUSH` puts its syntactic argument on the stack *when it is a `Value`*.

```k
  rule <k> PUSH A T X => #HandleAnnotations(A) ... </k>
       <stack> SS => [ #Name(T) X ] ; SS </stack>
    requires isValue(#Name(T), X)
```

If it is not a `Value`, `PUSH` converts its argument to a `Value`, either by
converting the parse-time representation to an internal one or else by looking
up/creating a new symbol in the symbol table.

```k
  rule <k> PUSH _ T (X => #MichelineToNative(X, T, .Map, .Map)) ... </k>
    requires notBool isValue(#Name(T), X)
     andBool notBool isSymbolicData(X)
```

```internalized-rl
  rule <k> PUSH _ T (X:SymbolicData => D)  ... </k>
       <symbols> X |-> #TypedSymbol(TN, D) ... </symbols>
    requires #Name(T) ==K TN

  rule <k> (.K => #CreateSymbol(X, T)) ~> PUSH A T X:SymbolicData  ... </k>
       <symbols> Symbols  </symbols>
    requires notBool X in_keys(Symbols)
```

`UNIT` and `LAMBDA` are specialized versions of `PUSH`.

```k
  rule <k> UNIT A => #HandleAnnotations(A) ... </k>
       <stack> SS => [ unit  Unit] ; SS </stack>

  rule <k> LAMBDA A T1 T2 C => #HandleAnnotations(A) ... </k>
       <stack> SS
            => [ (lambda #Name(T1) #Name(T2)) #Lambda(#Name(T1), #Name(T2), C) ] ; SS
       </stack>
```

### Lambda Evaluation

An `EXEC` instruction replaces the stack and schedules the restoration of the
old stack after the completion of the lambda code.

```k
  rule <k> EXEC B
        => #HandleAnnotations(B)
        ~> Code
        ~> #PostExecStackFix(RetType, SS)
           ...
       </k>
       <stack> [ ArgType D ]
             ; [ (lambda ArgType RetType) #Lambda(ArgType, RetType, Code) ]
             ; SS
            => [ ArgType D ]
       </stack>

  syntax Instruction ::= #PostExecStackFix(TypeName,Stack)
  // -----------------------------------------------------
  rule <k> #PostExecStackFix(RetType, SS) => .K ... </k>
       <stack> [ RetType D ] => [ RetType D ] ; SS </stack>
```

`APPLY` demonstrates why lambdas have their type information preserved, as
otherwise we would be unable to produce an appropriate `PUSH` instruction for
the expanded lambda.

```k
  rule <k> APPLY A => #HandleAnnotations(A) ... </k>
       <stack> [ T0 D ] ;
               [ (lambda (pair T0 T1) T2)
                 #Lambda((pair T0 T1), T2, { C } )
               ] ;
               SS
            => [ (lambda T1 T2) #Lambda(T1, T2, { PUSH .AnnotationList #Type(T0) D ;
                                                  PAIR .AnnotationList ;
                                                  { C }
                                                } )
               ] ;
               SS
       </stack>
```

Core Operations
---------------

### Generic Comparison

```k
  rule <k> EQ A => #HandleAnnotations(A) ... </k>
       <stack> [ int I ] ; SS => [ bool I ==Int 0 ] ; SS </stack>

  rule <k> NEQ A => #HandleAnnotations(A) ... </k>
       <stack> [ int I ] ; SS => [ bool I =/=Int 0 ] ; SS </stack>

  rule <k> LT A => #HandleAnnotations(A) ... </k>
       <stack> [ int I ] ; SS => [ bool I <Int 0 ] ; SS </stack>

  rule <k> GT A => #HandleAnnotations(A) ... </k>
       <stack> [ int I ] ; SS => [ bool I >Int 0 ] ; SS </stack>

  rule <k> LE A => #HandleAnnotations(A) ... </k>
       <stack> [ int I ] ; SS => [ bool I <=Int 0 ] ; SS </stack>

  rule <k> GE A => #HandleAnnotations(A) ... </k>
       <stack> [ int I ] ; SS => [ bool I >=Int 0 ] ; SS </stack>
```

We add lemmas for symbolic reasoning which normalize integer comparisons to
greater than; this lets us avoid writing symmetric lemmas for both cases.

```symbolic
    rule A  <Int B => B >Int  A [simplification]
    rule A <=Int B => B >=Int A [simplification]
```

### Boolean Operations

```k
  rule <k> OR A => #HandleAnnotations(A) ... </k>
       <stack> [ bool B1 ]
             ; [ bool B2 ]
             ; SS
            => [ bool (B1 orBool B2) ] ; SS
       </stack>

  rule <k> AND A => #HandleAnnotations(A) ... </k>
       <stack> [ bool B1 ]
             ; [ bool B2 ]
             ; SS
            => [ bool (B1 andBool B2) ] ; SS
       </stack>

  rule <k> XOR A => #HandleAnnotations(A) ... </k>
       <stack> [ bool B1 ]
             ; [ bool B2 ]
             ; SS
             => [ bool (B1 xorBool B2) ] ; SS
       </stack>

  rule <k> NOT A => #HandleAnnotations(A) ... </k>
       <stack> [ bool B ] ; SS
            => [ bool (notBool B) ] ; SS
       </stack>
```

For symbolic reasoning purposes, we add a few lemmas about boolean logic.

```symbolic
  rule false      ==Bool B            => notBool B    [simplification]
  rule true       ==Bool B            => B            [simplification]
  rule B          ==Bool false        => notBool B    [simplification]
  rule B          ==Bool true         => B            [simplification]
  rule notBool (notBool B)            => B            [simplification]
  rule notBool B1 ==Bool notBool B2   => B1 ==Bool B2 [simplification]
```

### Integer and Natural Operations

Michelson `int` and `nat` datatypes are both represnted using the K `Int` type.
These operations map directly to their K equivalents.

```k
  rule <k> NEG A => #HandleAnnotations(A) ... </k>
       <stack> [ _:NumTypeName I ] ; SS => [ int 0 -Int I ] ; SS </stack>

  rule <k> ABS A => #HandleAnnotations(A) ... </k>
       <stack> [ int I ] ; SS => [ nat absInt(I) ] ; SS </stack>

  rule <k> ISNAT A => #HandleAnnotations(A) ... </k>
       <stack> [ int I ] ; SS => [ (option nat) Some I ] ; SS </stack>
       requires I >=Int 0

  rule <k> ISNAT A => #HandleAnnotations(A) ... </k>
       <stack> [ int I ] ; SS => [ (option nat) None ] ; SS </stack>
       requires I <Int 0

  rule <k> INT A => #HandleAnnotations(A) ... </k>
       <stack> [ (nat => int) _:Int ] ; _ </stack>

  rule <k> ADD A => #HandleAnnotations(A) ... </k>
       <stack> [ T1:NumTypeName I1 ] ;
               [ T2:NumTypeName I2 ] ;
               SS
            => [ BinOpNumType(T1,T2) I1 +Int I2 ] ;
               SS
       </stack>

  rule <k> SUB A => #HandleAnnotations(A) ... </k>
       <stack> [ _:NumTypeName I1 ] ;
               [ _:NumTypeName I2 ] ;
               SS
            => [ int I1 -Int I2 ] ;
               SS
       </stack>

  rule <k> MUL A => #HandleAnnotations(A) ... </k>
       <stack> [ T1:NumTypeName I1 ] ;
               [ T2:NumTypeName I2 ] ;
               SS
            => [ BinOpNumType(T1,T2) I1 *Int I2 ] ;
               SS
       </stack>

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> [ T1:NumTypeName _:Int ] ;
               [ T2:NumTypeName 0 ] ;
               SS
            => [ (option (pair BinOpNumType(T1,T2) nat)) None ] ;
               SS
       </stack>

  rule <k> EDIV A  => #HandleAnnotations(A) ... </k>
       <stack> [ T1:NumTypeName I1:Int ] ;
               [ T2:NumTypeName I2:Int ] ;
               SS
            => [ (option (pair BinOpNumType(T1,T2) nat))
                   Some (Pair (I1 /Int I2) (I1 %Int I2))
               ] ;
               SS
       </stack>
       requires I2 =/=Int 0

  syntax NumTypeName ::= BinOpNumType(NumTypeName, NumTypeName) [function, functional]
  rule BinOpNumType(_:NumTypeName, int) => int
  rule BinOpNumType(int, _:NumTypeName) => int
  rule BinOpNumType(nat, nat) => nat
```

For symbolic reasoning purposes, we add some simple lemmas about arithmetic:

```symbolic
  rule V:Int -Int V:Int => 0 [simplification]
```

Bitwise operations on Michelson `int`s map directly onto K `Int` functions.
The `LSL` and `LSR` operations produce exceptions when their shift arugment
overflows.

```k
  rule <k> OR A => #HandleAnnotations(A)  ... </k>
       <stack> [ nat I1 ] ; [ nat I2 ] ; SS => [ nat I1 |Int I2 ] ; SS </stack>

  rule <k> AND A => #HandleAnnotations(A) ... </k>
       <stack> [ _:NumTypeName I1 ] ; [ nat I2 ] ; SS => [ nat I1 &Int I2 ] ; SS </stack>

  rule <k> XOR A => #HandleAnnotations(A) ... </k>
       <stack> [ nat I1 ] ; [ nat I2 ] ; SS => [ nat I1 xorInt I2 ] ; SS </stack>

  rule <k> NOT A => #HandleAnnotations(A) ... </k>
       <stack> [ _:NumTypeName I ] ; SS => [ int ~Int I ] ; SS </stack>

  rule <k> LSL A => #HandleAnnotations(A) ... </k>
       <stack> [ nat X ] ; [ nat S ] ; SS => [ nat X <<Int S ] ; SS </stack>
    requires S <=Int 256

  rule <k> LSL A ~> Rk
        => #HandleAnnotations(A)
        ~> Aborted("LSL out of range", S, Rk, Rs)
        ~> Rk
        </k>
       <stack> [ nat C:Int ] ; [ nat S:Int ] ; Rs => ( GeneralOverflow C S ) </stack>
    requires S >Int 256

  rule <k> LSR A => #HandleAnnotations(A) ... </k>
       <stack> [ nat X ] ; [ nat S ] ; SS => [ nat X >>Int S ] ; SS </stack>
    requires S <=Int 256

  rule <k> LSR A ~> Rk
        => #HandleAnnotations(A)
        ~> Aborted("LSR out of range", S, Rk, Rs)
        ~> Rk
        </k>
       <stack> [ nat X ] ; [ nat S ] ; Rs => ( GeneralOverflow X S ) </stack>
       requires S >Int 256
```

### `COMPARE` Instruction

The `COMPARE` instruction is defined over all comparable datatypes.

```k
  rule <k> COMPARE A => #HandleAnnotations(A) ... </k>
       <stack> [ TY V1 ] ; [ TY V2 ] ; SS => [ int #DoCompare(V1, V2) ] ; SS </stack>
    requires #IsComparable(TY)

  syntax Bool ::= #IsComparable(TypeName) [function, functional]
  // Comparables
  rule #IsComparable(_:NumTypeName) => true
  rule #IsComparable(string) => true
  rule #IsComparable(bytes) => true
  rule #IsComparable(mutez) => true
  rule #IsComparable(bool) => true
  rule #IsComparable(key_hash) => true
  rule #IsComparable(timestamp) => true
  rule #IsComparable(address) => true
  rule #IsComparable(pair T1 T2) => #IsComparable(T1) andBool #IsComparable(T2)

  rule #IsComparable(option _) => true

  // Nullary Incomparables
  rule #IsComparable(key) => false
  rule #IsComparable(unit) => false
  rule #IsComparable(signature) => false
  rule #IsComparable(operation) => false
  rule #IsComparable(chain_id) => false

  // Unary Incomparables
  rule #IsComparable(list _) => false
  rule #IsComparable(set _) => false
  rule #IsComparable(contract _) => false

  // Bianry Incomparables
  rule #IsComparable(or _ _) => false
  rule #IsComparable(lambda _ _) => false
  rule #IsComparable(map _ _) => false
  rule #IsComparable(big_map _ _) => false
```

We define `COMPARE` in terms of a `#DoCompare` function.

```k
  syntax Int ::= #DoCompare(Data, Data) [function]

  rule #DoCompare(V, V) => 0

  rule #DoCompare(false, true) => -1
  rule #DoCompare(true, false) => 1

  rule #DoCompare(I1:Int, I2:Int) => -1 requires I1 <Int I2
  rule #DoCompare(I1:Int, I2:Int) => 0 requires I1 ==Int I2
  rule #DoCompare(I1:Int, I2:Int) => 1 requires I1 >Int I2

  rule #DoCompare(S1:String, S2:String) => -1 requires S1 <String S2
  rule #DoCompare(S1:String, S2:String) => 0 requires S1 ==String S2
  rule #DoCompare(S1:String, S2:String) => 1 requires S1 >String S2

  rule #DoCompare(None,    Some _ ) => -1
  rule #DoCompare(Some _,  None   ) =>  1
  rule #DoCompare(Some V1, Some V2) => #DoCompare(V1, V2)

  rule #DoCompare((Pair A1 _ ), (Pair B1 _ )) => -1                 requires #DoCompare(A1, B1) ==Int -1
  rule #DoCompare((Pair A1 A2), (Pair B1 B2)) => #DoCompare(A2, B2) requires #DoCompare(A1, B1) ==Int 0
  rule #DoCompare((Pair A1 _ ), (Pair B1 _ )) => 1                  requires #DoCompare(A1, B1) ==Int 1

  rule #DoCompare(B1:Bytes, B2:Bytes) => #DoCompare(Bytes2Int(B1, BE, Unsigned), Bytes2Int(B2, BE, Unsigned))
  rule #DoCompare(#KeyHash(S1), #KeyHash(S2)) => #DoCompare(S1, S2)
  rule #DoCompare(#Mutez(I1), #Mutez(I2)) => #DoCompare(I1, I2)
  rule #DoCompare(#Timestamp(I1), #Timestamp(I2)) => #DoCompare(I1, I2)
  rule #DoCompare(#Address(S1), #Address(S2)) => #DoCompare(S1, S2)
```

The `#DoCompare` function requires additional lemmas for symbolic execution.

```symbolic
  rule 0 ==Int  #DoCompare(V1, V2) => V1  ==K V2  [simplification]
  rule 0 =/=Int #DoCompare(V1, V2) => V1 =/=K V2  [simplification]
  rule #DoCompare(V1, V2)  ==Int 0 => V1  ==K V2  [simplification]
  rule #DoCompare(V1, V2) =/=Int 0 => V1 =/=K V2  [simplification]

  rule 0  >Int #DoCompare(I1:Bool, I2:Bool) => (notBool I1) andBool I2 [simplification]
  rule 0 >=Int #DoCompare(I1:Bool, I2:Bool) => I1 impliesBool I2       [simplification]
  rule #DoCompare(I1:Bool, I2:Bool) >=Int 0 => I2 impliesBool I1       [simplification]
  rule #DoCompare(I1:Bool, I2:Bool)  >Int 0 => I1 andBool (notBool I2) [simplification]

  rule 0  >Int #DoCompare(I1:Int, I2:Int) => I2 >Int I1  [simplification]
  rule 0 >=Int #DoCompare(I1:Int, I2:Int) => I2 >=Int I1 [simplification]
  rule #DoCompare(I1:Int, I2:Int) >=Int 0 => I1 >=Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) >Int 0  => I1 >Int I2  [simplification]

  rule 0  >Int #DoCompare(I1:String, I2:String) => I1 <String I2  [simplification]
  rule 0 >=Int #DoCompare(I1:String, I2:String) => I1 <=String I2 [simplification]
  rule #DoCompare(I1:String, I2:String) >=Int 0 => I1 >=String I2 [simplification]
  rule #DoCompare(I1:String, I2:String)  >Int 0 => I1 >String I2  [simplification]

  // TODO: at some point this rule should be builtin
  rule X ==String X => true [simplification]
```

### String Operations

```k
  rule <k> CONCAT A => #HandleAnnotations(A) ... </k>
       <stack> [string S1] ; [string S2] ; SS => [string S1 +String S2] ; SS </stack>

  rule <k> CONCAT A => #HandleAnnotations(A) ... </k>
       <stack> [(list string) L] ; SS => [string #ConcatStrings(L, "")] ; SS </stack>

  rule <k> SIZE A => #HandleAnnotations(A) ... </k>
       <stack> [string S] ; SS => [nat lengthString(S)] ; SS </stack>

  syntax String ::= #ConcatStrings(InternalList, String) [function]
  // --------------------------------------------------------------
  rule #ConcatStrings(.InternalList, A) => A
  rule #ConcatStrings([ S1 ] ;; DL,  A) => #ConcatStrings(DL, A +String S1)
```

The actual out of bounds conditions here are determined by experimentation.
Earlier versions of the semantics didn't check if O was in bounds, resulting in
`Slice("", 0, 0) => Some ""` rather than the correct
`#SliceString("", 0, 0) => None`

```k
  rule <k> SLICE A => #HandleAnnotations(A) ... </k>
       <stack> [nat O] ; [nat L] ; [string S] ; SS => [option string #SliceString(S, O, L)] ; SS </stack>

  syntax OptionData ::= #SliceString(String, Int, Int) [function]

  rule #SliceString(S, O, L) => Some substrString(S, O, O +Int L)
    requires O >=Int 0
     andBool L >=Int 0
     andBool O <Int lengthString(S)
     andBool (O +Int L) <=Int lengthString(S)

  rule #SliceString(_, _, _) => None [owise]
```

### Bytes Operations

The bytes instructions have a stubbed implementation for the time being, since
the actual serialization format is not formally unspecified.

```k
  rule <k> PACK A => #HandleAnnotations(A) ... </k>
       <stack> [T V] ; SS => [bytes #Packed(T,V)] ; SS </stack>

  rule <k> UNPACK A _ => #HandleAnnotations(A) ... </k>
       <stack> [bytes #Packed(T,V)] ; SS => [option T Some V] ; SS </stack>
```

The `CONCAT` operation over two bytes is relatively straightforward since we
already have helper functions to extract bytes content.

```k
  rule <k> CONCAT A => #HandleAnnotations(A) ... </k>
       <stack> [bytes B1] ; [bytes B2] ; SS => [bytes B1 +Bytes B2] ; SS </stack>
```

`CONCAT` over lists of bytes is somewhat more involved, since we need to
distinguish this case from lists of strings.

```k
  rule <k> CONCAT A => #HandleAnnotations(A) ... </k>
       <stack> [(list bytes) L] ; SS => [bytes #ConcatBytes(L, .Bytes)] ; SS </stack>

  syntax Bytes ::= #ConcatBytes(InternalList, Bytes) [function]
  // ----------------------------------------------------------
  rule #ConcatBytes(.InternalList, A) => A
  rule #ConcatBytes([ B ] ;; DL,   A) => #ConcatBytes(DL, A +Bytes B)
```

`SIZE` is relatively simple, except that we must remember to divide by two,
since bytes length is measured in terms of number of bytes, not characters in
the hex string.

```k
  rule <k> SIZE A => #HandleAnnotations(A) ... </k>
       <stack> [bytes B] ; SS => [nat lengthBytes(B)] ; SS </stack>
```

The remaining operations are defined in terms of the same operations on
strings, allowing for code reuse.

```k
  rule <k> SLICE A => #HandleAnnotations(A) ... </k>
       <stack> [nat O:Int] ; [nat L:Int] ; [bytes B:Bytes] ; SS => [option bytes #SliceBytes(B, O, L)] ; SS </stack>

  syntax OptionData ::= #SliceBytes(Bytes, Int, Int) [function]
  // ----------------------------------------------------------
  rule #SliceBytes(S, O, L) => Some substrBytes(S, O, O +Int L)
    requires O >=Int 0
     andBool L >=Int 0
     andBool O <Int lengthBytes(S)
     andBool (O +Int L) <=Int lengthBytes(S)

  rule #SliceBytes(_, _, _) => None [owise]
```

### Pair Operations

```k
  rule <k> PAIR A => #HandleAnnotations(A) ... </k>
       <stack> [LTy L] ; [RTy R] ; SS => [pair LTy RTy Pair L R] ; SS </stack>

  rule <k> UNPAIR A => #HandleAnnotations(A) ... </k>
       <stack> [pair LTy RTy Pair L R] ; SS => [LTy L] ; [RTy R] ; SS </stack>

  rule <k> CAR A => #HandleAnnotations(A) ... </k>
       <stack> [pair LTy _ Pair L _] ; SS => [LTy L] ; SS </stack>

  rule <k> CDR A => #HandleAnnotations(A) ... </k>
       <stack> [pair _ RTy Pair _ R] ; SS => [RTy R] ; SS </stack>
```

### Set Operations

```k
  rule <k> EMPTY_SET A T:Type => #HandleAnnotations(A) ... </k>
       <stack> SS => [set #Name(T) .Set] ; SS </stack>

  rule <k> MEM A => #HandleAnnotations(A) ... </k>
       <stack> [T X] ; [set T S:Set] ; SS => [bool X in S] ; SS </stack>

  // True to insert, False to remove.
  rule <k> UPDATE A => #HandleAnnotations(A) ... </k>
       <stack> [T D] ; [bool true] ; [set T S:Set] ; SS => [set T (SetItem(D) S)] ; SS </stack>

  rule <k> UPDATE A => #HandleAnnotations(A) ... </k>
       <stack> [T D] ; [bool false] ; [set T SetItem(D) S] ; SS => [set T S] ; SS </stack>

  rule <k> UPDATE A => #HandleAnnotations(A) ... </k>
       <stack> [T D] ; [bool false] ; [set T S:Set] ; SS => [set T S:Set] ; SS </stack>
       requires notBool(D in S)

  rule <k> SIZE A => #HandleAnnotations(A) ... </k>
       <stack> [set _ S:Set] ; SS => [nat size(S)] ; SS </stack>
```

Note that, according to the Michelson documentation, set iteration order is
actually defined (the set is iterated over in ascending order).
For simplicity we implement this by repeatedly selecting the minimal element.

```k
  rule <k> ITER A _ => #HandleAnnotations(A) ... </k>
       <stack> [set _ .Set] ; SS => SS </stack>

  rule <k> ITER A Body
        => #HandleAnnotations(A)
        ~> Body
        ~> #Push(set T,S -Set SetItem(#MinimalElement(Set2List(S))))
        ~> ITER .AnnotationList Body
        ...
        </k>
       <stack> [set T S] ; SS => [T #MinimalElement(Set2List(S))] ; SS </stack>
    requires size(S) >Int 0

  syntax Data ::= #MinimalElement(List) [function]
  syntax Data ::= #MinimalElementAux(List, Data) [function]

  rule #MinimalElement(ListItem(H) L) => #MinimalElementAux(L, H)
  rule #MinimalElementAux(.List, M) => M
  rule #MinimalElementAux(ListItem(H) L, M)
    => #MinimalElementAux(L, M) requires #DoCompare(M, H) <=Int 0
  rule #MinimalElementAux(ListItem(H) L, M)
    => #MinimalElementAux(L, H) requires #DoCompare(M, H) ==Int 1
```

### Shared Map/Big Map Operations

Internally, we represent `map`s and `big_map`s identically using K maps.
For this reason, many map operations share an identical representation upto
typing (shared operations use a generic `MapTypeName`).

```k
  rule <k> GET A => #HandleAnnotations(A) ...  </k>
       <stack> [KT K] ; [_:MapTypeName KT VT M:Map] ; SS => [option VT #lookup(M, K, VT)] ; SS </stack>
    requires isValue(KT, K)
```

```k
  syntax OptionData ::= #lookup(Map, Data, TypeName) [function, smtlib(lookup)]
  // --------------------------------------------------------------------------
  rule #lookup(M, K, VT) => Some {M[K]}:>Data requires K in_keys(M) andBool         isValue(VT,{M[K]}:>Data)
  rule #lookup(M, K, _ ) => None              requires notBool K in_keys(M)
```

This rule is not supported by the LLVM backend, so we only include it in Haskell builds.

```symbolic
  rule #lookup(M, K, VT) => #Bottom           requires K in_keys(M) andBool notBool isValue(VT,{M[K]}:>Data)
```

```symbolic
  rule #lookup(M [ K1 <- V1    ], K2, VT) => Some {V1}:>Data    requires K1 ==K  K2 andBool         isValue(VT,{V1}:>Data) [simplification]
  rule #lookup(M [ K1 <- V1    ], K2, VT) => #Bottom            requires K1 ==K  K2 andBool notBool isValue(VT,{V1}:>Data) [simplification]
  rule #lookup(M [ K1 <- undef ], K2, _ ) => None               requires K1 ==K  K2                                        [simplification]
  rule #lookup(M [ K1 <- _     ], K2, VT) => #lookup(M, K2, VT) requires K1 =/=K K2                                        [simplification]
  rule #lookup(M [ K1 <- undef ], K2, VT) => #lookup(M, K2, VT) requires K1 =/=K K2                                        [simplification]
```

```k
  rule <k> MEM A => #HandleAnnotations(A) ~> #Assume(isValue(VT, {M[K]}:>Data)) ... </k>
       <stack> [KT K] ; [_:MapTypeName KT VT M:Map] ; SS => [bool true] ; SS </stack>
    requires isValue(KT, K)
     andBool K in_keys(M)

  rule <k> MEM A => #HandleAnnotations(A) ...  </k>
       <stack> [KT K] ; [_:MapTypeName KT _ M:Map] ; SS => [bool false] ; SS </stack>
    requires notBool K in_keys(M)

  rule <k> UPDATE A => #HandleAnnotations(A)  ... </k>
       <stack> [KT K] ; [option VT Some V] ; [MT:MapTypeName KT VT M:Map] ; SS => [MT KT VT M[K <- V]] ; SS </stack>
    requires isValue(KT, K)
     andBool isValue(VT, V)

  rule <k> UPDATE A => #HandleAnnotations(A)  ... </k>
       <stack> [KT K] ; [option VT None] ; [MT:MapTypeName KT VT M:Map] ; SS => [MT KT VT M[K <- undef]] ; SS </stack>
```

### Map Specific Operations

```k
  rule <k> EMPTY_MAP A KT VT => #HandleAnnotations(A) ... </k>
       <stack> SS => [#Name(map A KT VT) .Map] ; SS </stack>

  rule <k> SIZE A => #HandleAnnotations(A)  ... </k>
       <stack> [map _ _ M:Map] ; SS => [nat size(M)] ; SS </stack>
```

The `MAP` operation over maps is defined via psuedoinstruction `#DoMap`.

```k
  rule <k> MAP A Body
        => #HandleAnnotations(A)
        ~> #DoMap(#MapOpInfo(KT, VT, NoneType, M, .Map, Body))
           ...
       </k>
       <stack> [map KT VT M] ; SS => SS </stack>
```

`#DoMap` takes a `MapOpInfo` struct that holds the original map and newly built
map, as well as typing information. The map instruction proceeds as follows.
Let the map have `p` entries in sorted order:

```
{ Elt key₁ val₁ ; Elt key₂ val₂ ; ... ; Elt keyₚ valₚ }
```

We apply the map `code`, replacing the map on top of the stack with each
map entry in sorted order, and then build the new map by pairing each key
with the corresponding `code` generated value (with a possibly new type),
as described by the `MAP` typing rule below.

```
          Γ ⊢ code :: ( pair key_ty val_ty1 ) : A ⇒ val_ty2 : A
:------------------------------------------------------------------------
      Γ ⊢ MAP code :: map key_ty val_ty1 : A ⇒ map key_ty val_ty2 : A
```

```k
  syntax MapOpInfo ::= #MapOpInfo(keyType     :TypeName,
                                  origValType :TypeName,
                                  newValType  :MaybeTypeName,
                                  origMap     :Map,
                                  newMap      :Map,
                                  mapBody     :Block)

  syntax Instruction ::= #DoMap(MapOpInfo)
  // -------------------------------------
  rule <k> #DoMap(#MapOpInfo(KT, VT, NVT, M1, M2, B))
        => B
        ~> #DoMapAux(#MinKey(M1),
                     #MapOpInfo(KT, VT, NVT, M1[#MinKey(M1) <- undef], M2, B))
           ...
       </k>
       <stack> SS
            => [pair KT VT Pair #MinKey(M1) {M1[#MinKey(M1)]}:>Data] ;
               SS
       </stack>
    requires size(M1) >Int 0

  rule <k> #DoMap(#MapOpInfo(KT, VT, NVT, .Map, M, _)) => .K ... </k>
       <stack> SS => [map KT #DefaultType(NVT,VT) M] ; SS </stack>

  syntax Instruction ::= #DoMapAux(Data, MapOpInfo)
  // ----------------------------------------------
  rule <k> #DoMapAux(K, #MapOpInfo(KT, VT, NVT, M1, M2, B))
        => #DoMap(#MapOpInfo(KT, VT, NVT', M1, M2[K <- V], B))
        ...
       </k>
       <stack> [NVT' V] ; SS => SS </stack>
    requires #CompatibleTypes(NVT,NVT')

  syntax Data ::= #MinKey(Map) [function]
  // ------------------------------------
  rule #MinKey(M) => #MinimalElement(keys_list(M))
```

We define auxiliary functions for computing the result type of `MAP`.

```k
  syntax MaybeTypeName ::= TypeName
                         | "NoneType"

  syntax Bool ::= #CompatibleTypes(MaybeTypeName, TypeName) [function]
  // -----------------------------------------------------------------
  rule #CompatibleTypes(NoneType,    _ ) => true
  rule #CompatibleTypes(T1:TypeName, T2) => T1 ==K T2

  syntax TypeName ::= #DefaultType(MaybeTypeName, TypeName) [function]
  // -----------------------------------------------------------------
  rule #DefaultType(T:TypeName, _) => T
  rule #DefaultType(NoneType, T) => T
```

`ITER` is relatively easy to implement using a straightforward recursive style,
since it does not need to track the new map while keeping it off the stack.

```k
  rule <k> ITER A _ => #HandleAnnotations(A)  ... </k>
       <stack> [map _ _ .Map] ; SS => SS </stack>

  rule <k> ITER A Body
        => #HandleAnnotations(A)
        ~> Body
        ~> #Push(map KT VT, M[#MinKey(M) <- undef])
        ~> ITER .AnnotationList Body
           ...
       </k>
       <stack> [map KT VT M:Map] ; SS
            => [pair KT VT Pair #MinKey(M) {M[#MinKey(M)]}:>Data] ; SS
       </stack>
    requires size(M) >Int 0
```

### Big Map Specific Operations

```k
  rule <k> EMPTY_BIG_MAP A KT VT => #HandleAnnotations(A) ... </k>
       <stack> SS => [#Name(big_map A KT VT) .Map] ; SS </stack>
```

### Option Operations

```k
  rule <k> SOME A => #HandleAnnotations(A)  ... </k>
       <stack> [T X] ; SS => [option T Some X] ; SS </stack>

  rule <k> NONE A T:Type => #HandleAnnotations(A)  ... </k>
       <stack> SS => [option #Name(T) None] ; SS </stack>

  rule <k> IF_NONE A BT _  => #HandleAnnotations(A) ~> BT ... </k>
       <stack> [option _ None] ; SS => SS </stack>

  rule <k> IF_NONE A _  BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> [option T Some V] ; SS => [T V] ; SS </stack>
```

### Union Operations

```k
  rule <k> LEFT A RTy:Type => #HandleAnnotations(A)  ... </k>
       <stack> [LTy X:Data] ; SS => [or LTy #Name(RTy) Left X] ; SS </stack>

  rule <k> RIGHT A LTy:Type => #HandleAnnotations(A) ... </k>
       <stack> [RTy X:Data] ; SS => [or #Name(LTy) RTy Right X] ; SS </stack>

  rule <k> IF_LEFT A BT _  => #HandleAnnotations(A) ~> BT ... </k>
       <stack> [or LTy _   Left V] ; SS => [LTy V] ; SS </stack>

  rule <k> IF_LEFT A _  BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> [or _   RTy Right V] ; SS => [RTy V] ; SS </stack>
```

### List Operations

```k
  rule <k> CONS A => #HandleAnnotations(A)  ... </k>
       <stack> [T V] ; [list T L] ; SS => [list T [ V ] ;; L] ; SS </stack>

  rule <k> NIL A T => #HandleAnnotations(A)  ... </k>
       <stack> SS => [list #Name(T) .InternalList] ; SS </stack>

  rule <k> IF_CONS A BT _  => #HandleAnnotations(A) ~> BT ... </k>
       <stack> [list T [ E ] ;; L] ; SS => [T E] ; [list T L] ; SS </stack>

  rule <k> IF_CONS A _  BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> [list _ .InternalList ] ; SS => SS </stack>

  rule <k> SIZE A => #HandleAnnotations(A)  ... </k>
       <stack> [list _ L:InternalList] ; SS => [nat size(L,0)] ; SS </stack>

  rule <k> ITER A _ =>  #HandleAnnotations(A) ~>. ... </k>
       <stack> [list _ .InternalList] ; SS => SS </stack>

  rule <k> ITER A Body
        => #HandleAnnotations(A)
        ~> Body
        ~> #Push(list T,L)
        ~> ITER .AnnotationList Body
           ...
       </k>
       <stack> [list T [ E ] ;; L] ; SS => [T E] ; SS </stack>
```

The `MAP` operation over `list`s is defined in terms of a helper function.

```k
  rule <k> MAP A Body
        => #HandleAnnotations(A)
        ~> #DoMap(T, NoneType, L, .InternalList, Body)
           ...
       </k>
       <stack> [list T L] ; SS => SS </stack>

  syntax Instruction ::= #DoMap(TypeName, MaybeTypeName, InternalList, InternalList, Block)
                       | #DoMapAux(TypeName, MaybeTypeName, InternalList, InternalList, Block)
  // -----------------------------------------------------------------------------------------
  rule <k> #DoMap(T, NT, .InternalList, Acc, _) => .K ... </k>
       <stack> SS => [list #DefaultType(NT,T) #ReverseList(Acc, .InternalList)] ; SS </stack>

  rule <k> #DoMap(T, NT, [ E ] ;; L, Acc, B)
        => B
        ~> #DoMapAux(T, NT, L, Acc, B)
           ...
       </k>
       <stack> SS => [T E] ; SS </stack>

  rule <k> #DoMapAux(T, NT, L, Acc, B)
        => #DoMap(T, NT', L, [ E ] ;; Acc, B)
           ...
       </k>
       <stack> [NT' E] ; SS => SS </stack>
    requires #CompatibleTypes(NT,NT')

  syntax InternalList ::= #ReverseList(InternalList, InternalList) [function, functional]
  // ------------------------------------------------------------------------------------
  rule #ReverseList(E ;; L,        L') => #ReverseList(L, E ;; L')
  rule #ReverseList(.InternalList, L') => L'
```

Domain Specific operations
--------------------------

### Timestamp Operations

Timestamps are simply wrapped ints in Michelson, so the implementation of
simple arithmetic over them is straightforward. The differing argument types
however forces us to use two rules for each operation.

```k
  rule <k> ADD A => #HandleAnnotations(A) ... </k>
       <stack> [timestamp #Timestamp(I1)] ; [int I2] ; SS => [timestamp #Timestamp(I1 +Int I2)] ; SS </stack>

  rule <k> ADD A => #HandleAnnotations(A) ... </k>
       <stack> [int I1] ; [timestamp #Timestamp(I2)] ; SS => [timestamp #Timestamp(I1 +Int I2)] ; SS </stack>

  rule <k> SUB A => #HandleAnnotations(A) ... </k>
       <stack> [timestamp #Timestamp(I1)] ; [int I2] ; SS => [timestamp #Timestamp(I1 -Int I2)] ; SS </stack>

  rule <k> SUB A => #HandleAnnotations(A) ... </k>
       <stack> [timestamp #Timestamp(I1)] ; [timestamp #Timestamp(I2)] ; SS => [int I1 -Int I2] ; SS </stack>
```

### Blockchain Operations

```k
  rule <k> CREATE_CONTRACT A:AnnotationList { C } => #HandleAnnotations(A) ... </k>
       <stack> [option key_hash Delegate:OptionData] ;
               [mutez Initial:Mutez] ;
               [_ Storage:Data] ;
               SS
            => [operation Create_contract { C } Delegate Initial Storage O ] ;
               [address #Address("@Address(" +String Int2String(!_:Int) +String ")")] ;
               SS
       </stack>
       <nonce> #Nonce(O) => #NextNonce(#Nonce(O)) </nonce>

  rule <k> TRANSFER_TOKENS AL => #HandleAnnotations(AL) ... </k>
       <stack> [T D] ; [mutez M] ; [contract T #Contract(A, _)] ; SS
            => [operation Transfer_tokens D M A O] ; SS
       </stack>
       <nonce> #Nonce(O) => #NextNonce(#Nonce(O)) </nonce>

  rule <k> SET_DELEGATE A => #HandleAnnotations(A) ... </k>
       <stack> [option key_hash D] ; SS => [operation Set_delegate D O] ; SS </stack>
       <nonce> #Nonce(O) => #NextNonce(#Nonce(O)) </nonce>
```

Each `operation` value must have a unique nonce.
The nonce generation process is unspecified by the Michelson semantics.
We implement it here.

```k
  syntax OperationNonce ::= #NextNonce(OperationNonce) [function]
  rule #NextNonce(#Nonce(I)) => #Nonce(I +Int 1)
```

These instructions push fresh `contract` literals on the stack corresponding
to the given addresses/key hashes.

```k
  rule <k> CONTRACT AL T => #HandleAnnotations(AL) ... </k>
       <stack> [address A] ; SS => [option contract #Name(T) Some {M[A]}:>Data] ; SS </stack>
       <knownaddrs> M </knownaddrs>
    requires A in_keys(M)
     andBool #TypeFromContractStruct({M[A]}:>Data) ==K T

  rule <k> CONTRACT AL T => #HandleAnnotations(AL) ... </k>
       <stack> [address _] ; SS => [option contract #Name(T) None] ; SS </stack>
       <knownaddrs> _ </knownaddrs> [owise]

  rule <k> IMPLICIT_ACCOUNT AL => #HandleAnnotations(AL) ... </k>
       <stack> [key_hash #KeyHash(A)] ; SS
            => [contract unit #Contract(#Address(A), unit .AnnotationList)] ; SS
       </stack>

  syntax Type ::= #TypeFromContractStruct(Data) [function]
  rule #TypeFromContractStruct(#Contract(_, T)) => T
```

These instructions push blockchain state on the stack.

```k
  rule <k> BALANCE A => #HandleAnnotations(A) ... </k>
       <stack> SS => [mutez B] ; SS </stack>
       <mybalance> B </mybalance>

  rule <k> ADDRESS AL => #HandleAnnotations(AL) ... </k>
       <stack> [contract TN #Contract(A, T)] ; SS => [address A] ; SS </stack>
    requires TN ==K #Name(T)

  rule <k> SOURCE AL => #HandleAnnotations(AL) ... </k>
       <stack> SS => [address A] ; SS </stack>
       <sourceaddr> A </sourceaddr>

  rule <k> SENDER AL => #HandleAnnotations(AL) ... </k>
       <stack> SS => [address A] ; SS </stack>
       <senderaddr> A </senderaddr>

  rule <k> SELF AL => #HandleAnnotations(AL) ... </k>
       <stack> SS => [contract #Name(T) #Contract(A, T)] ; SS </stack>
       <paramtype> T </paramtype>
       <myaddr> A </myaddr>

  rule <k> AMOUNT A => #HandleAnnotations(A) ... </k>
       <stack> SS => [mutez M] ; SS </stack>
       <myamount> M </myamount>

  rule <k> CHAIN_ID A => #HandleAnnotations(A) ... </k>
       <stack> SS => [chain_id C] ; SS </stack>
       <mychainid> C </mychainid>

  rule <k> NOW A => #HandleAnnotations(A) ... </k>
       <stack> SS => [timestamp N] ; SS </stack>
       <mynow> N </mynow>
```

### Cryptographic Operations

The cryptographic operations are simply stubbed for now.

```k
  syntax String ::= #Blake2BKeyHash(String) [function]
  rule #Blake2BKeyHash(S) => S

  rule <k> HASH_KEY A => #HandleAnnotations(A) ... </k>
       <stack> [key #Key(S)] ; SS => [key_hash #KeyHash(#Blake2BKeyHash(S))] ; SS </stack>

  rule <k> BLAKE2B A => #HandleAnnotations(A) ... </k>
       <stack> [bytes B:MichelsonBytes] ; SS => [bytes #Blake2B(B)] ; SS </stack>

  rule <k> SHA256 A => #HandleAnnotations(A) ... </k>
       <stack> [bytes B:MichelsonBytes] ; SS => [bytes #SHA256(B)] ; SS </stack>

  rule <k> SHA512 A => #HandleAnnotations(A) ... </k>
       <stack> [bytes B:MichelsonBytes] ; SS => [bytes #SHA512(B)] ; SS </stack>

  syntax MichelsonBytes ::= #SignBytes(Key, Signature, MichelsonBytes)

  /*
  // FIXME: The haskell backend does not support distinguishing these rules.
  rule <k> CHECK_SIGNATURE A => #HandleAnnotations(A) ... </k>
       <stack> #Key(K)
            ~> #Signature(S)
            ~> #SignBytes(#Key(K), #Signature(S), _)
            => true
               ...
       </stack>

  rule <k> CHECK_SIGNATURE A => #HandleAnnotations(A) ... </k>
       <stack> #Key(_)
            ~> #Signature(_)
            ~> _:MichelsonBytes
            => false
               ...
       </stack> [owise]
  */
```

### Mutez Operations

Mutez operations need to check their results since `mutez` is not an unlimited
precision type.
This internal instruction checks and produces the appropriate error case if the
value is invalid.

```k
  syntax Instruction ::= #ValidateMutezAndPush(Mutez, Int, Int)
  // ----------------------------------------------------------
  rule <k> #ValidateMutezAndPush(#Mutez(I), _, _) => . ... </k>
       <stack> SS => [mutez #Mutez(I)] ; SS </stack>
       requires #IsLegalMutezValue(I)

  rule <k> #ValidateMutezAndPush(#Mutez(I), I1, I2) ~> Rk
        => Aborted("Mutez out of bounds", I, Rk, Rs) ~> Rk </k>
       <stack> Rs => #FailureFromMutezValue(#Mutez(I), I1, I2) </stack>
    requires notBool #IsLegalMutezValue(I)

  syntax FailedStack ::= #FailureFromMutezValue(Mutez, Int, Int) [function]
  // ----------------------------------------------------------------------
  rule #FailureFromMutezValue(#Mutez(I), I1, I2)
    => ( MutezOverflow I1 I2 ) requires I >=Int #MutezOverflowLimit
  rule #FailureFromMutezValue(#Mutez(I), I1, I2)
    => ( MutezUnderflow I1 I2 ) requires I <Int 0
```

Other than the mutez validation step, these arithmetic rules are essentially
identical to those defined over integers.

```k
  rule <k> ADD A
        => #ValidateMutezAndPush(#Mutez(I1 +Int I2), I1, I2)
        ~> #HandleAnnotations(A)
           ...
       </k>
       <stack> [mutez #Mutez(I1)] ; [mutez #Mutez(I2)] ; SS => SS </stack>

  rule <k> SUB A
        => #ValidateMutezAndPush(#Mutez(I1 -Int I2), I1, I2)
        ~> #HandleAnnotations(A)
           ...
       </k>
       <stack> [mutez #Mutez(I1)] ; [mutez #Mutez(I2)] ; SS => SS </stack>

  rule <k> MUL A
        => #ValidateMutezAndPush(#Mutez(I1 *Int I2), I1, I2)
        ~> #HandleAnnotations(A)
           ...
       </k>
       <stack> [mutez #Mutez(I1)] ; [nat I2] ; SS => SS </stack>

  rule <k> MUL A
        => #ValidateMutezAndPush(#Mutez(I1 *Int I2), I1, I2)
        ~> #HandleAnnotations(A)
           ...
       </k>
       <stack> [nat I1] ; [mutez #Mutez(I2)] ; SS => SS </stack>

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> [mutez #Mutez(_)] ; [mutez #Mutez(0)] ; SS => [option pair nat mutez None] ; SS </stack>

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> [mutez #Mutez(_)] ; [nat 0] ; SS => [option pair mutez mutez None] ; SS </stack>

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> [mutez #Mutez(I1)] ;
               [mutez #Mutez(I2)] ;
               SS
            => [option pair nat mutez Some (Pair (I1 /Int I2) #Mutez(I1 %Int I2))] ;
               SS
       </stack>
    requires I2 >Int 0

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> [mutez #Mutez(I1)] ;
               [nat I2] ;
               SS
            => [option pair mutez mutez Some (Pair #Mutez(I1 /Int I2) #Mutez(I1 %Int I2))] ;
               SS
       </stack>
    requires I2 >Int 0
```

Debugging Operations
--------------------

We introduce several pseudo-instructions that are used for debugging:

-   `STOP` is an instruction that cannot be evaluated and causes the program to
    get stuck
-   `PAUSE` non-determinstically chooses to either do nothing or else `STOP`;
    it optionally traces at its pause point.
-   `TRACE` appends its string content to the `<trace>` cell as a debugging aid
    for complex programs.

```k
  rule <k> PAUSE    => .K                ... </k>
  rule <k> PAUSE    => STOP              ... </k>
  rule <k> PAUSE(S) => TRACE(S) ~> PAUSE ... </k>
  rule <k> TRACE(S) => .K ... </k>
       <trace> K:K => (K ~> S) </trace>
```

Internal Operations
-------------------

These operations are used internally for implementation purposes.

### `ASSUME`/`ASSERT` Instructions

```k
  syntax Instruction ::= "ASSERT" "{" BlockList "}"
                       | "ASSUME" "{" BlockList "}"
```

```k
  rule <k> ASSERT { .BlockList } => .K ... </k>
  rule <k> ASSERT { B; Bs }
        => B ~> #AssertTrue ~> ASSERT { Bs } ~> #RestoreStack(SS)
           ...
       </k>
       <stack> SS => .Stack </stack>
```

```k
  rule <k> ASSUME { .BlockList } => .K ... </k>
  rule <k> ASSUME { B; Bs }
        => B ~> #AssumeTrue ~> ASSUME { Bs } ~> #RestoreStack(SS)
           ...
       </k>
       <stack> SS => .Stack </stack>
```

```k
  syntax Instruction ::= #RestoreStack(K)
  rule <k> #RestoreStack(SS) => .K ... </k>
       <stack> _ => SS </stack>
```

```k
  syntax Instruction ::= "#AssertTrue"
  rule <k> #AssertTrue => #Assert(B) ... </k>
       <stack> [bool B:Bool] ; SS => SS </stack>
```

```k
  syntax Instruction ::= "#AssumeTrue"
  rule <k> #AssumeTrue => #Assume(B) ... </k>
       <stack> [bool B:Bool] ; SS => SS </stack>
```

```k
  syntax KItem ::= #Assert(BoolExp) [strict, result(Bool)]
  rule <k> #Assert(true)  => .             ... </k>
  rule <k> #Assert(false) => #AssertFailed ... </k>
  syntax KItem ::= "#AssertFailed" [klabel(#AssertFailed), symbol]
```

```k
  syntax KItem ::= #Assume(BoolExp) [strict, result(Bool)]
  rule <k> #Assume(true)  => .             ... </k>
  rule <k> #Assume(false) ~> _:K => . </k>
       <assumeFailed> _ => true </assumeFailed> [transition]
```


Note that the first value is a `KItem` and not heated/cooled. This is to work
around the need for sort coersion in the map `GET` operation.

```k
  syntax BoolExp ::= Bool
                   | KItem "==" Data

  rule <k> D1 == D2 => (D2 ~> D1 == #hole) ... </k> requires notBool isValue(D2)
  rule <k> (D2 ~> D1 == #hole) => D1 == D2 ... </k> requires isValue(D2)
  rule <k> D1 == D2 => D1 ==K D2 ... </k>           requires isValue(D2)

  rule isBool(_L == _R) => false [simplification]
```

### `CUTPOINT` Instruction

A cutpoint is a semantic construct that internalizes the notion of a
reachability logic circularity (or claim).
When we reach a cutpoint, we need to generalize our current state into one
which corresponds to the reachability logic circularity that we wish to use.

```internalized-rl
  syntax Instruction ::= CUTPOINT( id: Int, invariant: Invariant)

  rule <k> CUTPOINT(I, { Shape } { Predicates })
        => BIND { Shape } { ASSERT { Predicates }}
        ~> #GeneralizeStack(Shape, .Stack)
        ~> BIND { Shape } { ASSUME { Predicates }}
           ...
       </k>
       <cutpoints> (.Set => SetItem(I)) VisitedCutpoints </cutpoints>
    requires notBool I in VisitedCutpoints

  rule <k> CUTPOINT(I, { Shape } { Predicates })
        => BIND { Shape } { ASSERT { Predicates }}
        ~> #Assume(false)
           ...
       </k>
       <cutpoints> VisitedCutpoints </cutpoints>
    requires I in VisitedCutpoints
```

In stack-based languages like Michelson, state generalization means that we
abstract out pieces of the stack which are non-invariant during loop execution.

```internalized-rl
  syntax KItem ::= #GeneralizeStack(StackElementList, Stack)
  rule <k> #GeneralizeStack(.StackElementList, SS) => . ... </k>
       <stack> _ => reverseStack( SS ) </stack>

  rule <k> #GeneralizeStack(Stack_elt T D ; SS, SS')
        => #GeneralizeStack(SS, [#Name(T) D] ; SS')
           ...
       </k>
    requires notBool isSymbolicData(D)

  rule <k> (.K => #MakeFresh(T))
        ~> #GeneralizeStack(Stack_elt T D:SymbolicData ; SS, SS')
           ...
       </k>

  rule <k> ( V
          ~> #GeneralizeStack(Stack_elt T D:SymbolicData ; SS, SS')
           )
        =>   #GeneralizeStack(Stack_elt T V ; SS, SS')
           ...
       </k>
    requires isValue(#Name(T),V)
```

### `BIND` Instruction

```k
  syntax Instruction ::= "BIND" OutputStack Block
  rule <k> BIND Shape Block
        => #Bind(#LiteralStackToStack(Shape), Stack)
        ~> Block
        ~> #RestoreSymbols(Symbols)
           ...
       </k>
       <symbols> Symbols </symbols>
       <stack> Stack </stack>
```

```k
  syntax KItem ::= #Bind(InternalStack, InternalStack)

  rule <k> #Bind(.Stack, .Stack) => .K ... </k>

  rule <k> #Bind(S1:FailedStack, S2:FailedStack) => .K ... </k>
    requires #Matches(S1, S2)

  rule <k> #Bind( [ T S:SymbolicData ] ; SS  => SS
                , [ T D ]              ; SS' => SS'
                )
           ...
       </k>
       <symbols> Syms => S |-> #TypedSymbol(T, D) Syms </symbols>
    requires notBool S in_keys(Syms)

  rule <k> #Bind( [ T S:SymbolicData ] ; SS  => SS
                , [ T D1 ]             ; SS' => SS'
                )
           ...
       </k>
       <symbols> S |-> #TypedSymbol(T, D2) ... </symbols>
    requires D1 ==K D2

  rule <k> #Bind( [ T ED ] ; SS  => SS
                , [ T AD ] ; SS' => SS'
                )
           ...
       </k>
       <knownaddrs> KnownAddrs </knownaddrs>
       <bigmaps> BigMaps </bigmaps>
    requires #ConcreteMatch(ED, #Type(T), KnownAddrs, BigMaps, AD)

  // NOTE: this function protects against unification errors
  syntax Bool ::= #ConcreteMatch(Data, Type, Map, Map, Data) [function]
  rule #ConcreteMatch(_:SymbolicData, _, _, _, _) => false
  rule #ConcreteMatch(ED, T, Addrs, BigMaps, AD) => #Matches(#MichelineToNative(ED,T,Addrs,BigMaps),AD)
    requires notBool isSymbolicData(ED)
```

```k
  syntax KItem ::= #RestoreSymbols(Map)
  rule <k> #RestoreSymbols(Symbols) => .K ... </k>
       <symbols> _ => Symbols </symbols>
```

Macro Evaluation
----------------

### Simple Macro Evaluation

The following macros have one-step mappings to Michelson code.

```k
  rule <k> CMPEQ _  => COMPARE .AnnotationList ; EQ  .AnnotationList ... </k>
  rule <k> CMPNEQ _ => COMPARE .AnnotationList ; NEQ .AnnotationList ... </k>
  rule <k> CMPLT _  => COMPARE .AnnotationList ; LT  .AnnotationList ... </k>
  rule <k> CMPGT _  => COMPARE .AnnotationList ; GT  .AnnotationList ... </k>
  rule <k> CMPLE _  => COMPARE .AnnotationList ; LE  .AnnotationList ... </k>
  rule <k> CMPGE _  => COMPARE .AnnotationList ; GE  .AnnotationList ... </k>

  rule <k> IFEQ     _ B1 B2 => EQ     .AnnotationList ; IF .AnnotationList B1 B2 ... </k>
  rule <k> IFNEQ    _ B1 B2 => NEQ    .AnnotationList ; IF .AnnotationList B1 B2 ... </k>
  rule <k> IFLT     _ B1 B2 => LT     .AnnotationList ; IF .AnnotationList B1 B2 ... </k>
  rule <k> IFGT     _ B1 B2 => GT     .AnnotationList ; IF .AnnotationList B1 B2 ... </k>
  rule <k> IFLE     _ B1 B2 => LE     .AnnotationList ; IF .AnnotationList B1 B2 ... </k>
  rule <k> IFGE     _ B1 B2 => GE     .AnnotationList ; IF .AnnotationList B1 B2 ... </k>
  rule <k> IFCMPEQ  _ B1 B2 => CMPEQ  .AnnotationList ; IF .AnnotationList B1 B2 ... </k>
  rule <k> IFCMPNEQ _ B1 B2 => CMPNEQ .AnnotationList ; IF .AnnotationList B1 B2 ... </k>
  rule <k> IFCMPLT  _ B1 B2 => CMPLT  .AnnotationList ; IF .AnnotationList B1 B2 ... </k>
  rule <k> IFCMPGT  _ B1 B2 => CMPGT  .AnnotationList ; IF .AnnotationList B1 B2 ... </k>
  rule <k> IFCMPLE  _ B1 B2 => CMPLE  .AnnotationList ; IF .AnnotationList B1 B2 ... </k>
  rule <k> IFCMPGE  _ B1 B2 => CMPGE  .AnnotationList ; IF .AnnotationList B1 B2 ... </k>

  rule <k> FAIL _ => UNIT .AnnotationList ; FAILWITH .AnnotationList ... </k>

  rule <k> ASSERT        _ => IF       .AnnotationList {} { FAIL .AnnotationList } ... </k>
  rule <k> ASSERT_EQ     _ => IFEQ     .AnnotationList {} { FAIL .AnnotationList } ... </k>
  rule <k> ASSERT_NEQ    _ => IFNEQ    .AnnotationList {} { FAIL .AnnotationList } ... </k>
  rule <k> ASSERT_LT     _ => IFLT     .AnnotationList {} { FAIL .AnnotationList } ... </k>
  rule <k> ASSERT_LE     _ => IFLE     .AnnotationList {} { FAIL .AnnotationList } ... </k>
  rule <k> ASSERT_GT     _ => IFGT     .AnnotationList {} { FAIL .AnnotationList } ... </k>
  rule <k> ASSERT_GE     _ => IFGE     .AnnotationList {} { FAIL .AnnotationList } ... </k>
  rule <k> ASSERT_CMPEQ  _ => IFCMPEQ  .AnnotationList {} { FAIL .AnnotationList } ... </k>
  rule <k> ASSERT_CMPNEQ _ => IFCMPNEQ .AnnotationList {} { FAIL .AnnotationList } ... </k>
  rule <k> ASSERT_CMPLT  _ => IFCMPLT  .AnnotationList {} { FAIL .AnnotationList } ... </k>
  rule <k> ASSERT_CMPLE  _ => IFCMPLE  .AnnotationList {} { FAIL .AnnotationList } ... </k>
  rule <k> ASSERT_CMPGT  _ => IFCMPGT  .AnnotationList {} { FAIL .AnnotationList } ... </k>
  rule <k> ASSERT_CMPGE  _ => IFCMPGE  .AnnotationList {} { FAIL .AnnotationList } ... </k>
  rule <k> ASSERT_NONE   _ => IF_NONE  .AnnotationList {} { FAIL .AnnotationList } ... </k>
  rule <k> ASSERT_SOME   _ => IF_SOME  .AnnotationList {} { FAIL .AnnotationList } ... </k>
  rule <k> ASSERT_LEFT   _ => IF_LEFT  .AnnotationList {} { FAIL .AnnotationList } ... </k>
  rule <k> ASSERT_RIGHT  _ => IF_RIGHT .AnnotationList {} { FAIL .AnnotationList } ... </k>

  rule <k> IF_SOME  _ B1 B2 => IF_NONE .AnnotationList B2 B1 ... </k>
  rule <k> IF_RIGHT _ B1 B2 => IF_LEFT .AnnotationList B2 B1 ... </k>

  rule <k> SET_CAR _ => CDR .AnnotationList ; SWAP .AnnotationList ; PAIR .AnnotationList ... </k>
  rule <k> SET_CDR _ => CAR .AnnotationList ;                        PAIR .AnnotationList ... </k>

  rule <k> MAP_CAR _ Body
        => DUP .AnnotationList ;
           CDR .AnnotationList ;
           DIP .AnnotationList { CAR .AnnotationList ; Body } ;
           SWAP .AnnotationList ;
           PAIR .AnnotationList
           ...
       </k>

  rule <k> MAP_CDR _ Body
        => DUP .AnnotationList ;
           CDR .AnnotationList ;
           Body ;
           SWAP .AnnotationList ;
           CAR .AnnotationList ;
           PAIR .AnnotationList
           ...
       </k>

  // NOTE: there is no DUP 1 macro --- presumably becuase this is equal to DUP
  rule <k> DUP _ 2 => DIP .AnnotationList { DUP .AnnotationList } ; SWAP .AnnotationList ... </k>
  rule <k> DUP _ N => DIP .AnnotationList (N -Int 1) { DUP .AnnotationList } ; DIG .AnnotationList N ... </k>
    requires N >Int 2
```

### Complex Macro Evaluation

The following macros require two or more steps to fully process.
We first convert the macro token into a string and trim off any unneeded
prefixes/suffixes.

```k
  syntax DataList ::= #DUP(String)                          [function]
                    | #CDAR(String)                         [function]
                    | "#SET_CDAR" "(" String ")"            [function]
                    | "#MAP_CDAR" "(" String ","  Block ")" [function]
                    | #DIP(String, Block)                   [function]

  rule <k> M:DUPMacro     _   => #DUP(     #Trim(1,1,#DUPMacroToString(M)))        ... </k>
  rule <k> M:CDARMacro    _   => #CDAR(    #Trim(1,1,#CDARMacroToString(M)))       ... </k>
  rule <k> M:SetCDARMacro _   => #SET_CDAR(#Trim(5,1,#SetCDARMacroToString(M)))    ... </k>
  rule <k> M:MapCDARMacro _ B => #MAP_CDAR(#Trim(5,1,#MapCDARMacroToString(M)), B) ... </k>
  rule <k> M:DIPMacro     _ B => #DIP(     #Trim(1,1,#DIPMacroToString(M)),     B) ... </k>
```

Complex macro evaluation proceeds by interpreting the trimmed string as
Michelson instructions.

1.  The `DU+P` macro is equivalent to `DUP n` where `n` is the number of `U`s,
    where `DUP n` duplicates the `n`th stack element. Thus, we can infer a
    typing rule of the form:

    `x1 ... xn S => x1 xn ... xn S`.

    ```k
    rule #DUP(S) => DUP .AnnotationList lengthString(S)
    ```

2.  The `C[AD]+R` macro evaluates to successive `CAR` or `CDR` instructions for
    each `A` or `D` in the string. Thus, we can infer a typing rule of the
    form:

    `ptype(...x...) S => x S`

    where `ptype` is a nested pair type of the required structure.

    ```k
    rule #CDAR(S) => CAR .AnnotationList ; #CDAR( #Advance(1,S) )
      requires    lengthString(S) >Int 0
      andThenBool #CharAt(0, S) ==String "A"

    rule #CDAR(S) => CDR .AnnotationList ; #CDAR( #Advance(1,S) )
      requires    lengthString(S) >Int 0
      andThenBool #CharAt(0, S) ==String "D"

    rule #CDAR("") => { }
    ```

3.  The `SET_C[AD]+R` macro sets a designated leaf element in a nested `pair`
    type with depth `n` which depends on the sequence of `A`s and `D`s in the
    macro. Thus, we can infer a typing rule of the form:

    `ptype(...x...) x S => ptype(...x...) S`.

    ```k
    rule #SET_CDAR(S)
      => { DUP .AnnotationList ;
           DIP .AnnotationList
               { CAR .AnnotationList ;
             { #SET_CDAR( #Advance(1,S) ) }
               } ;
           CDR .AnnotationList ;
           SWAP .AnnotationList ;
           PAIR .AnnotationList
         }
      requires    lengthString(S) >=Int 2
      andThenBool #CharAt(0, S) ==String "A"

    rule #SET_CDAR(S)
      => { DUP .AnnotationList ;
           DIP .AnnotationList
               { CDR .AnnotationList ;
             { #SET_CDAR( #Advance(1,S) ) }
               } ;
           CAR .AnnotationList ;
           PAIR .AnnotationList
         }
      requires    lengthString(S) >=Int 2
      andThenBool #CharAt(0, S) ==String "D"

    rule #SET_CDAR("A") => SET_CAR .AnnotationList
    rule #SET_CDAR("D") => SET_CDR .AnnotationList
    ```

4.  The `MAP_C[AD]+R code` assumes the following conditions:

    - the macro is applied to a stack of the for `ptype S` where `ptype` is a
      nested `pair` type with a designated leaf of depth `n` which depends on
      the sequence of `A`s and `D`s in the macro;
    - the designated leaf value is `v` with type `x`;
    - for the given `code`, we can infer a typing rule of the form `x S => x S`

    Then `MAP_C[AD]+R` macro transforms the pair by applying `code` to leaf
    value `v`. Thus, we can infer a typing rule of the form:

    `ptype(...x...) S => ptype(...x...) S`.

    ```k
    rule #MAP_CDAR(S, Body)
      => { DUP .AnnotationList ;
           DIP .AnnotationList
               { CAR .AnnotationList ;
                 { #MAP_CDAR( #Advance(1,S), Body ) }
               } ;
           CDR .AnnotationList ;
           SWAP .AnnotationList ;
           PAIR .AnnotationList
         }
      requires    lengthString(S) >=Int 2
      andThenBool #CharAt(0, S) ==String "A"

    rule #MAP_CDAR(S, Body)
      => { DUP .AnnotationList ;
           DIP .AnnotationList
               { CDR .AnnotationList ;
                 { #MAP_CDAR( #Advance(1,S), Body ) }
               } ;
           CAR .AnnotationList ;
           PAIR .AnnotationList
         }
      requires    lengthString(S) >=Int 2
      andThenBool #CharAt(0, S) ==String "D"

    rule #MAP_CDAR("A", Body) => MAP_CAR .AnnotationList Body
    rule #MAP_CDAR("D", Body) => MAP_CDR .AnnotationList Body
    ```

5.  The `DII+P` macro evaluates to `DIP n` where `n` is the number of `I`s in
    the string. Technically, this macro accepts a variant of roman numerals with
    characters `MDCLXVI` where different letters increase the value of `n` by
    more than 1, but, for simplicity, we only accept the consecutive `I`s.
    Its typing rule is equivalent to the typing rule for `DIP n code`

    ```k
    rule #DIP(S,Body) => DIP .AnnotationList lengthString(S) Body
    ```

#### Unimplemented Macros

The following two macros are left for future implementation work.

```disabled
  rule <k> M:PairMacro    _ => #Eval(#Trim(1,1,#PairMacroToString(M)),    M) ... </k>
  rule <k> M:UnpairMacro  _ => #Eval(#Trim(2,1,#UnpairMacroToString(M)),  M) ... </k>
```

### Macro Evaluation Auxiliary Functions

```k
  syntax String ::= #Trim(Int, Int, String) [function]
  // -------------------------------------------------
  rule #Trim(B, E, S) => substrString(S, B, lengthString(S) -Int E)

  syntax String ::= #CharAt(Int, String) [function]
  // ----------------------------------------------
  rule #CharAt(N, S) => substrString(S, N, N +Int 1)

  syntax String ::= #Advance(Int, String)  [function]
  // ------------------------------------------------
  rule #Advance(N, S) => substrString(S, N, lengthString(S))
```

Symbolic Value Processing
-------------------------

### Extending functions to `SymbolicData`

```k
  syntax TypedSymbol ::= #TypedSymbol(TypeName, Data)
```

```internalized-rl
  rule [[ #MichelineToNative(S:SymbolicData, T, _, _) => D ]]
       <symbols> S |-> #TypedSymbol(TN, D) ... </symbols>
    requires TN ==K #Name(T)

  rule [[ #MichelineToNative(S:SymbolicData, T, _, _) => S ]]
       <symbols> Syms:Map </symbols>
    requires notBool (S in_keys(Syms))
```

### `#CreateSymbol`

`#CreateSymbol` is responsible for setting up the initial symbol table.

```internalized-rl
  syntax KItem ::= #CreateSymbol(SymbolicData, Type)
  // -----------------------------------------------
  rule <k> (.K => #MakeFresh(T)) ~>  #CreateSymbol(_, T) ... </k>
  rule <k> (V ~> #CreateSymbol(N, T)) => . ... </k>
       <symbols> M => M[N <- #TypedSymbol(#Name(T), V)] </symbols>
    requires isValue(#Name(T), V)
```

### "Evaluating" Data

The `isValue` predicate indicates if a `Data` has been fully evaluated.
It has an untyped and typed variant.

```k
  syntax Bool ::= isValue(Data) [function, functional]
  // -------------------------------------------------
  rule isValue(_:SimpleData) => true
  rule isValue(None) => true
  rule isValue(Some V) => isValue(V)
  rule isValue(Left V) => isValue(V)
  rule isValue(Right V) => isValue(V)
  rule isValue(Pair L R) => isValue(L) andBool isValue(R)
  rule isValue(_) => false [owise]

  syntax Bool ::= isValue(TypeName, Data) [function, functional]
  // -----------------------------------------------------------
  rule isValue(nat,       V:Int)                 => true requires V >=Int 0
  rule isValue(int,       _:Int)                 => true
  rule isValue(mutez,     #Mutez(V:Int))         => true requires #IsLegalMutezValue(V)
  rule isValue(bool,      _:Bool)                => true
  rule isValue(bytes,     _:Bytes)               => true
  rule isValue(string,    _:String)              => true
  rule isValue(unit,      Unit)                  => true
  rule isValue(key,       #Key(_:String))        => true
  rule isValue(key_hash,  #KeyHash(_:String))    => true
  rule isValue(signature, #Signature(_:String))  => true
  rule isValue(timestamp, #Timestamp(_:Int))     => true
  rule isValue(address,   #Address(_:String))    => true
  rule isValue(chain_id,  #ChainId(_:Bytes))     => true
  rule isValue(operation, _:BlockchainOperation) => true

  rule isValue(contract T, #Contract(#Address(_:String),TY)) => true requires T ==K #Name(TY)
  rule isValue(option _, None)                               => true
  rule isValue(option T, Some V)                             => isValue(T, V)

  rule isValue(pair T1 T2, Pair LV RV)                => isValue(T1, LV) andBool isValue(T2, RV)
  rule isValue(or  T1 _T2, Left  V)                   => isValue(T1, V)
  rule isValue(or _T1  T2, Right V)                   => isValue(T2, V)
  rule isValue(lambda T1 T2, #Lambda(T1,T2, _:Block)) => true

  rule isValue(list _, _:InternalList)   => true
  rule isValue(set _,  _:Set)            => true
  rule isValue(_:MapTypeName _ _, _:Map) => true

  rule isValue(_,_) => false [owise]
```

```k
    syntax Data ::= "#hole"

    rule <k> Pair V1 V2 => (V1 ~> Pair #hole V2) ... </k> requires notBool isValue(V1)
    rule <k> Pair V1 V2 => (V2 ~> Pair V1 #hole) ... </k> requires isValue(V1) andBool notBool isValue(V2)
    rule <k> (V1 ~> Pair #hole V2) => Pair V1 V2 ... </k> requires isValue(V1)
    rule <k> (V2 ~> Pair V1 #hole) => Pair V1 V2 ... </k> requires isValue(V2)

    rule <k> Some V => (V ~> Some #hole) ... </k> requires notBool isValue(V)
    rule <k> (V ~> Some #hole) => Some V ... </k> requires isValue(V)
    rule <k> Left V => (V ~> Left #hole) ... </k> requires notBool isValue(V)
    rule <k> (V ~> Left #hole) => Left V ... </k> requires isValue(V)
    rule <k> Right V => (V ~> Right #hole) ... </k> requires notBool isValue(V)
    rule <k> (V ~> Right #hole) => Right V ... </k> requires isValue(V)
```

### `#MakeFresh`

`#MakeFresh` is responsible for generating a fresh value of a given type.

```internalized-rl
  syntax Data ::= #MakeFresh(Type)

  rule <k> #MakeFresh(nat       _:AnnotationList) => #Assume(?V >=Int 0)             ~> ?V:Int         ... </k>
  rule <k> #MakeFresh(mutez     _:AnnotationList) => #Assume(#IsLegalMutezValue(?V)) ~> #Mutez(?V:Int) ... </k>

  rule <k> #MakeFresh(bool      _:AnnotationList) => ?_:Bool                ... </k>
  rule <k> #MakeFresh(int       _:AnnotationList) => ?_:Int                 ... </k>
  rule <k> #MakeFresh(bytes     _:AnnotationList) => ?_:Bytes               ... </k>
  rule <k> #MakeFresh(string    _:AnnotationList) => ?_:String              ... </k>
  rule <k> #MakeFresh(unit      _:AnnotationList) => Unit                   ... </k>
  rule <k> #MakeFresh(key       _:AnnotationList) => #Key(?_:String)        ... </k>
  rule <k> #MakeFresh(key_hash  _:AnnotationList) => #KeyHash(?_:String)    ... </k>
  rule <k> #MakeFresh(signature _:AnnotationList) => #Signature(?_:String)  ... </k>
  rule <k> #MakeFresh(timestamp _:AnnotationList) => #Timestamp(?_:Int)     ... </k>
  rule <k> #MakeFresh(address   _:AnnotationList) => #Address(?_:String)    ... </k>
  rule <k> #MakeFresh(chain_id  _:AnnotationList) => #ChainId(?_:Bytes)     ... </k>
  // TODO: should we expand into the three separate kinds of Blockchain operations?
  rule <k> #MakeFresh(operation _:AnnotationList) => ?_:BlockchainOperation ... </k>

  rule <k> #MakeFresh(list      _:AnnotationList _:Type)        => ?_:InternalList                       ... </k>
  rule <k> #MakeFresh(set       _:AnnotationList _:Type)        => ?_:Set                                ... </k>
  rule <k> #MakeFresh(map       _:AnnotationList _:Type _:Type) => ?_:Map                                ... </k>
  rule <k> #MakeFresh(big_map   _:AnnotationList _:Type _:Type) => ?_:Map                                ... </k>
  rule <k> #MakeFresh(contract  _:AnnotationList T)             => #Contract(#Address(?_:String),T)      ... </k>

  rule <k> #MakeFresh(pair _:AnnotationList T1 T2)
        => (Pair #MakeFresh(T1) #MakeFresh(T2))
           ...
       </k>

  rule <k> #MakeFresh(option _:AnnotationList T) => None               ... </k>
  rule <k> #MakeFresh(option _:AnnotationList T) => Some #MakeFresh(T) ... </k>

  rule <k> #MakeFresh(or _:AnnotationList T1 T2) => Left  #MakeFresh(T1) ... </k>
  rule <k> #MakeFresh(or _:AnnotationList T1 T2) => Right #MakeFresh(T2) ... </k>
```

We implement fresh lambdas as fresh uninterpreted functions.

```internalized-rl
  rule <k> #MakeFresh(lambda    _:AnnotationList T1 T2)
        => #Lambda(#Name(T1), #Name(T2), { #Uninterpreted(!Id, #Name(T1), #Name(T2)) })
           ...
       </k>

  syntax Instruction ::= #Uninterpreted(id: Int, arg: TypeName, return: TypeName)
  syntax Data ::= uninterpreted(id: Int, arg: Data) [function, functional, no-evaluators]
  // ------------------------------------------------------------------------------------
  rule <k> #Uninterpreted(Id, ArgT, RetT)
        => #Assume(uninterpreted(Id, Arg) == #MakeFresh(#Type(RetT)))
           ...
       </k>
       <stack> [ArgT Arg] ; SS => [RetT uninterpreted(Id, Arg):Data] ; SS </stack>
    requires isValue(ArgT, Arg)
```

```k
endmodule
```

This function implements a relaxed equality check between two data elements.
In particular, it handles the wildcard match behavior described in the `.tzt`
format proposal and discards list type information as discussed earlier.

```k
module MATCHER
  imports MICHELSON-COMMON
  imports UNIT-TEST-COMMON-SYNTAX

  syntax Bool ::= #Matches(Data, Data) [function] // Expected, Actual

  // This covers any structurally different data. (e.g. (Left 1) vs (Right 1))
  rule #Matches(D1, D2) => D1 ==K D2 [owise]

  rule #Matches(_:Wildcard, _) => true

  rule #Matches(.InternalList, .InternalList) => true
  rule #Matches(L1 ;; Ls1, L2 ;; Ls2)
    => #Matches(L1, L2) andBool #Matches(Ls1, Ls2)

  rule #Matches(.Set, .Set) => true
  rule #Matches(SetItem(S1) Ss1, SetItem(S2) Ss2)
    => #Matches(S1, S2) andBool #Matches(Ss1, Ss2)

  rule #Matches(.Map, .Map) => true
  rule #Matches((K |-> V1) M1, (K |-> V2) M2)
    => #Matches(V1, V2) andBool #Matches(M1, M2)

  syntax Data ::= FailedStack

  rule #Matches(Create_contract { C } O1 M1 D1 I1,
                Create_contract { C } O2 M2 D2 I2)
    => #Matches(I1, I2) andBool
       #Matches(O1, O2) andBool
       #Matches(M1, M2) andBool
       #Matches(D1, D2)

  rule #Matches(Transfer_tokens D1 M1 A1 I1,
                Transfer_tokens D2 M2 A2 I2)
    => #Matches(I1, I2) andBool
       #Matches(D1, D2) andBool
       #Matches(M1, M2) andBool
       #Matches(A1, A2)

  rule #Matches(Set_delegate O1 I1,
                Set_delegate O2 I2)
    => #Matches(I1, I2) andBool #Matches(O1, O2)

  rule #Matches(Pair L1 R1, Pair L2 R2)
    => #Matches(L1, L2) andBool #Matches(R1, R2)

  rule #Matches(Some D1, Some D2) => #Matches(D1, D2)

  rule #Matches(Left D1, Left D2) => #Matches(D1, D2)
  rule #Matches(Right D1, Right D2) => #Matches(D1, D2)
endmodule
```
