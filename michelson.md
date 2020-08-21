```k
requires "common.md"
requires "types.md"
```

Michelson Interpreter State
===========================

K-Michelson: an intra-contract semantics
----------------------------------------

```k
module MICHELSON-CONFIG
  imports MICHELSON-COMMON
  imports DOMAINS
```

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
                  <inputstack> .K </inputstack>
    ```

2. The `<expected>` cell contains the expected output stack for the given
   Michelson execution as prescribed by the Michelson unit test format. It is
   used for test validation, type-checking, and debugging purposes.

    ```k
                  <expected> .K </expected>
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

    ```symbolic
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
        => #CreateSymbols
        ~> #ConvertBigMapsToNative
        ~> #ConvertParamToNative
        ~> #ConvertStorageToNative
        ~> #ExecutePreConditions
        ~> #LoadInputStack
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

  syntax KItem ::= "#BigMap" "(" SequenceData "," Type ")"
  rule #BigMapsEntryToKMap(Big_map I T1 T2 { }          ) => I |-> #BigMap({ }, big_map .AnnotationList T1 T2)
  rule #BigMapsEntryToKMap(Big_map I T1 T2 ML:MapLiteral) => I |-> #BigMap(ML,  big_map .AnnotationList T1 T2)

  rule <k> invariant Annot { Stack } { Blocks } => . ... </k>
       <invs> .Map
           => (Annot |-> { Stack } { Blocks })
              ...
       </invs>

  rule <k> input LS => .K ... </k>
       <inputstack> .K => LS </inputstack>

  rule <k> output Os => .K ... </k>
       <expected> .K => Os </expected>

  rule <k> precondition { Bs } => .K ... </k>
       <pre> .BlockList => Bs </pre>

  rule <k> postcondition { Bs } => .K ... </k>
       <post> .BlockList => Bs </post>
```

### Symbol Creation

Load symbolic variables into the `<symbols>` map.

```k
  syntax KItem ::= "#CreateSymbols"
```

```concrete
  rule <k> #CreateSymbols => . ... </k>
```

```symbolic
  rule <k> #CreateSymbols
        => #CreateSymbols(#UnifiedSetToList(#UnifyTypes( #FindSymbolsS(Stack)
                                                    |Set #FindSymbolsBL(Pre)
                                                    |Set #FindSymbolsB({ Script })
                         )                )           )
           ...
       </k>
       <inputstack> { Stack }:LiteralStack </inputstack>
       <pre> Pre:BlockList </pre>
       <script> Script:Data </script>
```

### Micheline to Native Conversion

```k
  syntax KItem ::= "#ConvertBigMapsToNative"
  rule <k> #ConvertBigMapsToNative => .K ... </k>
       <knownaddrs> KnownAddrs </knownaddrs>
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
checking at a later time.

### Stack Loading

```k
  syntax KItem ::= "#LoadInputStack"
  rule <k> #LoadInputStack => .K ... </k>
       <stack> _ => #StackToNative(Actual, Addrs, BigMaps) </stack>
       <inputstack> Actual </inputstack>
       <knownaddrs> Addrs </knownaddrs>
       <bigmaps> BigMaps </bigmaps>

  syntax InternalStack ::= #StackToNative(OutputStack, Map, Map) [function]
  syntax Stack ::= #StackToNativeAux(StackElementList, Map, Map) [function]
  // ----------------------------------------------------------------------
  rule #StackToNative( { Ls }, Addrs, BigMaps )
    => #StackToNativeAux( Ls, Addrs, BigMaps )
  rule #StackToNative( FS:FailedStack, _, _ ) => FS

  rule #StackToNativeAux(.StackElementList, _Addrs, _BigMaps) => .Stack
  rule #StackToNativeAux(Stack_elt T D ; Gs, Addrs, BigMaps)
    => [ #Name(T) #MichelineToNative(D, T, Addrs, BigMaps) ] ;
       #StackToNativeAux(Gs, Addrs, BigMaps)
```

```k
  syntax KItem ::= "#LoadDefaultContractStack"
  rule <k> #LoadDefaultContractStack ... </k>
       <stack> _:Stack
            => [ (pair #Name(PT) #Name(ST)) Pair P S ]
       </stack>
       <paramvalue> P </paramvalue>
       <paramtype> PT </paramtype>
       <storagevalue> S </storagevalue>
       <storagetype> ST </storagetype>
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

When the `<k>` cell is empty, we consider execution successful.

```k
  rule <k> . </k>
       <returncode> 1 => 0 </returncode>
```

We handle typed instruction wrappers and blocks here.

```k
  rule I:Instruction ; Is => I ~> Is [structural]
  rule {}                 => .K      [structrual]
  rule { Is:DataList }    => Is      [structural]
```

For now, annotations are ignored.

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
       <stack> [ T D:Data ] ; Rs => ( Failed D ) </stack>
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
  rule <k> IF A BT BF => #HandleAnnotations(A) ~> BT ... </k>
       <stack> [ bool true  ] ; SS => SS </stack>

  rule <k> IF A BT BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> [ bool false ] ; SS => SS </stack>
```

### Loops

Here we handle concrete loop semantics.

```k
  rule <k> LOOP .AnnotationList B
        => B ~> LOOP .AnnotationList B
           ...
       </k>
       <stack> [ bool true  ] ; SS => SS </stack>
  rule <k> LOOP .AnnotationList B => .K ... </k>
       <stack> [ bool false ] ; SS => SS </stack>
```

```k
  rule <k> LOOP_LEFT .AnnotationList B
        => B
        ~> LOOP_LEFT .AnnotationList B
           ...
        </k>
       <stack> [ (or LX RX) Left D ] ; SS
           =>  [ LX D ] ; SS
       </stack>
  rule <k> LOOP_LEFT .AnnotationList B => .K ... </k>
       <stack> [ (or LX RX) Right D ] ; SS
            => [ RX D ] ; SS
       </stack>
```

Here we handle symbolic loop semantics.

```symbolic
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

This pseudo-instruction implements the behavior of restoring the previous stack
when a lambda completes execution.

```k
  syntax Instruction ::= #ReturnStack(Stack)

  rule <k> #ReturnStack(SS) => . ... </k>
       <stack> E ; _ => E ; SS </stack>
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
       <stack> SS
            => [ #Name(T) #MichelineToNative(X, T, .Map, .Map) ] ; SS
       </stack>
    requires isValue(X)
```

If it is not a `Value`, `PUSH` converts its argument to a `Value`, either by
converting the parse-time representation to an internal one or else by looking
up/creating a new symbol in the symbol table.

```k
  rule <k> PUSH A T (X => #MichelineToNative(X, T, .Map, .Map)) ... </k>
    requires notBool isValue(X)
     andBool notBool isSymbolicData(X)
```

```symbolic
  rule <k> PUSH A T (X:SymbolicData => D)  ... </k>
       <symbols> X |-> #TypedSymbol(#Name(T), D) ... </symbols>
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
  rule <k> EXEC B => #HandleAnnotations(B) ~> C ~> #ReturnStack(SS) ... </k>
       <stack> [ T1 D ]
             ; [ (lambda T1 T2) #Lambda(T1, T2, C) ]
             ; SS
            => [ T1 D ]
       </stack>
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

```k
    rule A  >Int B => notBool( A <=Int B ) [simplification]
    rule A >=Int B => notBool( A  <Int B ) [simplification]
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

### Integer and Natural Operations

Michelson `int` and `nat` datatypes are both represnted using the K `Int` type.
These operations map directly to their K equivalents.

```k
  rule <k> NEG A => #HandleAnnotations(A) ... </k>
       <stack> [ N:NumTypeName I ] ; SS => [ int 0 -Int I ] ; SS </stack>

  rule <k> ABS A => #HandleAnnotations(A) ... </k>
       <stack> [ int I ] ; SS => [ nat absInt(I) ] ; SS </stack>

  rule <k> ISNAT A => #HandleAnnotations(A) ... </k>
       <stack> [ int I ] ; SS => [ (option nat) Some I ] ; SS </stack>
       requires I >=Int 0

  rule <k> ISNAT A => #HandleAnnotations(A) ... </k>
       <stack> [ int I ] ; SS => [ (option nat) None ] ; SS </stack>
       requires I <Int 0

  rule <k> INT A => #HandleAnnotations(A) ... </k>
       <stack> [ (nat => int) I:Int ] ; SS </stack>

  rule <k> ADD A => #HandleAnnotations(A) ... </k>
       <stack> [ T1:NumTypeName I1 ] ;
               [ T2:NumTypeName I2 ] ;
               SS
            => [ BinOpNumType(T1,T2) I1 +Int I2 ] ;
               SS
       </stack>

  rule <k> SUB A => #HandleAnnotations(A) ... </k>
       <stack> [ T1:NumTypeName I1 ] ;
               [ T2:NumTypeName I2 ] ;
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
       <stack> [ T1:NumTypeName I1:Int ] ;
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
  rule BinOpNumType(N:NumTypeName, int) => int
  rule BinOpNumType(int, N:NumTypeName) => int
  rule BinOpNumType(nat, nat) => nat
```

Bitwise operations on Michelson `int`s map directly onto K `Int` functions.
The `LSL` and `LSR` operations produce exceptions when their shift arugment
overflows.

```k
  rule <k> OR A => #HandleAnnotations(A)  ... </k>
       <stack> [ nat I1 ] ; [ nat I2 ] ; SS => [ nat I1 |Int I2 ] ; SS </stack>

  rule <k> AND A => #HandleAnnotations(A) ... </k>
       <stack> [ T1:NumTypeName I1 ] ; [ nat I2 ] ; SS => [ nat I1 &Int I2 ] ; SS </stack>

  rule <k> XOR A => #HandleAnnotations(A) ... </k>
       <stack> [ nat I1 ] ; [ nat I2 ] ; SS => [ nat I1 xorInt I2 ] ; SS </stack>

  rule <k> NOT A => #HandleAnnotations(A) ... </k>
       <stack> [ T1:NumTypeName I ] ; SS => [ int ~Int I ] ; SS </stack>

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
  rule #IsComparable(NumberType) => true
  rule #IsComparable(string) => true
  rule #IsComparable(bytes) => true
  rule #IsComparable(mutez) => true
  rule #IsComparable(bool) => true
  rule #IsComparable(key_hash) => true
  rule #IsComparable(timestamp) => true
  rule #IsComparable(address) => true
  rule #IsComparable(pair T1 T2) => #IsComparable(T1) andBool #IsComparable(T2)

  // Nullary Incomparables
  rule #IsComparable(key) => false
  rule #IsComparable(unit) => false
  rule #IsComparable(signature) => false
  rule #IsComparable(operation) => false
  rule #IsComparable(chain_id) => false

  // Unary Incomparables
  rule #IsComparable(option _) => false
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
  syntax Int ::= #DoCompare(Data, Data) [function, functional]

  rule #DoCompare(true, true) => 0
  rule #DoCompare(false, false) => 0
  rule #DoCompare(false, true) => -1
  rule #DoCompare(true, false) => 1

  rule #DoCompare(I1:Int, I2:Int) => -1 requires I1 <Int I2
  rule #DoCompare(I1:Int, I2:Int) => 0 requires I1 ==Int I2
  rule #DoCompare(I1:Int, I2:Int) => 1 requires I1 >Int I2

  rule #DoCompare(S1:String, S2:String) => -1 requires S1 <String S2
  rule #DoCompare(S1:String, S2:String) => 0 requires S1 ==String S2
  rule #DoCompare(S1:String, S2:String) => 1 requires S1 >String S2

  rule #DoCompare((Pair A1 A2), (Pair B1 B2)) => -1                 requires #DoCompare(A1, B1) ==Int -1
  rule #DoCompare((Pair A1 A2), (Pair B1 B2)) => #DoCompare(A2, B2) requires #DoCompare(A1, B1) ==Int 0
  rule #DoCompare((Pair A1 A2), (Pair B1 B2)) => 1                  requires #DoCompare(A1, B1) ==Int 1

  rule #DoCompare(B1:Bytes, B2:Bytes) => #DoCompare(Bytes2Int(B1, BE, Unsigned), Bytes2Int(B2, BE, Unsigned))
  rule #DoCompare(#KeyHash(S1), #KeyHash(S2)) => #DoCompare(S1, S2)
  rule #DoCompare(#Mutez(I1), #Mutez(I2)) => #DoCompare(I1, I2)
  rule #DoCompare(#Timestamp(I1), #Timestamp(I2)) => #DoCompare(I1, I2)
  rule #DoCompare(#Address(S1), #Address(S2)) => #DoCompare(S1, S2)
```

The `#DoCompare` function requires additional lemmas for symbolic execution.

```symbolic
  rule #DoCompare(I1:Bool, I2:Bool) <Int 0  => (I1 ==Bool false) andBool (I2 ==Bool true)                       [simplification]
  rule #DoCompare(I1:Bool, I2:Bool) <=Int 0 => ((I1 ==Bool false) andBool (I2 ==Bool true)) orBool I1 ==Bool I2 [simplification]
  rule #DoCompare(I1:Bool, I2:Bool) ==Int 0 => I1 ==Bool I2                                                     [simplification]
  rule #DoCompare(I1:Bool, I2:Bool) >=Int 0 => ((I1 ==Bool true) andBool (I2 ==Bool false)) orBool I1 ==Bool I2 [simplification]
  rule #DoCompare(I1:Bool, I2:Bool) >Int 0  => (I1 ==Bool true) andBool (I2 ==Bool false)                       [simplification]

  rule #DoCompare(I1:Int, I2:Int) <Int 0  => I1 <Int I2  [simplification]
  rule #DoCompare(I1:Int, I2:Int) <=Int 0 => I1 <=Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) ==Int 0 => I1 ==Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) >=Int 0 => I1 >=Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) >Int 0  => I1 >Int I2  [simplification]

  rule #DoCompare(I1:String, I2:String) <Int 0  => I1 <String I2  [simplification]
  rule #DoCompare(I1:String, I2:String) <=Int 0 => I1 <=String I2 [simplification]
  rule #DoCompare(I1:String, I2:String) ==Int 0 => I1 ==String I2 [simplification]
  rule #DoCompare(I1:String, I2:String) >=Int 0 => I1 >=String I2 [simplification]
  rule #DoCompare(I1:String, I2:String) >Int 0  => I1 >String I2  [simplification]

  // TODO: at some point this rule should be builtin
  rule X ==String X => true [simplification]
```

### String Operations

```k
  syntax String ::= #ConcatStrings(List, String) [function]
  rule #ConcatStrings(.List, A) => A
  rule #ConcatStrings(ListItem(S1) DL, A) => #ConcatStrings(DL, A +String S1)

  rule <k> CONCAT A => #HandleAnnotations(A) ... </k>
       <stack> [string S1] ; [string S2] ; SS => [string S1 +String S2] ; SS </stack>

  rule <k> CONCAT A => #HandleAnnotations(A) ... </k>
       <stack> [(list string) L] ; SS => [string #ConcatStrings(L, "")] ; SS </stack>

  rule <k> SIZE A => #HandleAnnotations(A) ... </k>
       <stack> [string S] ; SS => [nat lengthString(S)] ; SS </stack>
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

  rule #SliceString(S, O, L) => None [owise]
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

  syntax Bytes ::= #ConcatBytes(List, Bytes) [function]
  rule #ConcatBytes(.List, A) => A
  rule #ConcatBytes(ListItem(B) DL, A) => #ConcatBytes(DL, A +Bytes B)
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

  rule #SliceBytes(S, O, L) => None [owise]
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

  rule <k> ITER A B
        => #HandleAnnotations(A)
        ~> B
        ~> #Push(set T,S -Set SetItem(#MinimalElement(Set2List(S))))
        ~> ITER .AnnotationList B
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

### Map Operations

```k
  rule <k> GET A => #HandleAnnotations(A) ... </k>
       <stack> [KT X] ; [MT:MapTypeName KT VT M] ; SS => [option VT None] ; SS </stack>
    requires isValue(X)
     andBool notBool(X in_keys(M))
```

```concrete
   rule <k> GET A => #HandleAnnotations(A) ... </k>
        <stack> [KT X] ; [MT:MapTypeName KT VT M] ; SS => [option VT Some {M[X]}:>Data] ; SS </stack>
     requires isValue(X)
      andBool X in_keys(M)
```

```symbolic
  rule <k> GET A
        => #HandleAnnotations(A)
        ~> #Assume(?Val == #MakeFresh(#Type(VT)))
        ~> #Assume(M[X] == ?Val)
           ...
       </k>
       <stack> [KT X] ; [MT:MapTypeName KT VT M] ; SS => [option VT Some ?Val] ; SS </stack>
    requires X in_keys(M)

  rule K1 in_keys(M:Map[ K2 <- _ ]) => K1 ==K K2 orBool K1 in_keys(M) [simplification]
```

```k
  rule <k> EMPTY_MAP A KT VT => #HandleAnnotations(A) ... </k>
       <stack> SS => [#Name(map A KT VT) .Map] ; SS </stack>

  rule <k> MEM A => #HandleAnnotations(A) ~> . ... </k>
       <stack> [KT X] ; [MT:MapTypeName KT VT M] ; SS => [bool X in_keys(M)] ; SS </stack>

  rule <k> UPDATE A => #HandleAnnotations(A)  ... </k>
       <stack> [KT K] ; [option VT Some V] ; [MT:MapTypeName KT VT M:Map] ; SS => [MT KT VT M[K <- V]] ; SS </stack>

  rule <k> UPDATE A => #HandleAnnotations(A)  ... </k>
       <stack> [KT K] ; [option VT None] ; [MT:MapTypeName KT VT M:Map] ; SS => [MT KT VT M[K <- undef]] ; SS </stack>

  rule <k> SIZE A => #HandleAnnotations(A)  ... </k>
       <stack> [MT:MapTypeName KT VT M:Map] ; SS => [nat size(M)] ; SS </stack>
```

The `MAP` operation, over maps, is somewhat more involved. We need to set up a
stack without the actual map to execute the block on, and we need to keep track
of the updated map as we do. We implement this by splitting the operation into
multiple K items.

```k
  rule <k> MAP A B => #HandleAnnotations(A) ~> #PerformMap(MT, KT, VT, NoneType, M, .Map, B) ... </k>
       <stack> [MT:MapTypeName KT VT M] ; SS => SS </stack>
```

`#PerformMap` holds the old map, the new map, and the block to execute.
It sets up the new stack and queues up a `#PopNewVal` which removes
the value produced by the MAP block and adds it to the second map argument.
Like Sets, iteration order is actually defined, and we implement it by
repeatedly selecting the minimal element in the list of keys in the map.

```k
  syntax MaybeTypeName ::= TypeName
                         | "NoneType"

  syntax Instruction ::= #PerformMap(MapTypeName, TypeName, TypeName, MaybeTypeName, Map, Map, Block)
  // ------------------------------------------------------------------------------------------------
  rule <k> #PerformMap(MT, KT, VT, NVT, M1, M2, B)
        => B
        ~> #PopNewVal(#MinimalKey(M1))
        ~> #PerformMap(MT, KT, VT, NVT, M1[#MinimalKey(M1) <- undef], M2, B)
           ...
       </k>
       <stack> SS => [pair KT VT Pair #MinimalKey(M1) {M1[#MinimalKey(M1)]}:>Data] ; SS
       </stack>
    requires size(M1) >Int 0

  rule <k> #PerformMap(MT, KT, VT, NVT, .Map, M, _) => . ... </k>
       <stack> SS => [MT KT NVT M] ; SS </stack>

  syntax Instruction ::= #PopNewVal(Data)
  // ------------------------------------
  rule <k> #PopNewVal(K) ~> #PerformMap(MT, KT, VT, NVT, M1, M2, B)
        => #PerformMap(MT, KT, VT, NVT', M1, M2[K <- V], B)
        ...
       </k>
       <stack> [NVT' V] ; SS => SS </stack>
    requires #CompatibleTypes(NVT,NVT')

  syntax Data ::= #MinimalKey(Map) [function]
  // ----------------------------------------
  rule #MinimalKey(M) => #MinimalElement(keys_list(M))

  syntax Bool ::= #CompatibleTypes(MaybeTypeName, TypeName) [function]
  rule #CompatibleTypes(NoneType,T) => true
  rule #CompatibleTypes(T1:TypeName, T2) => T1 ==K T2
```

`ITER` is relatively easy to implement using a straightforward recursive style,
since it does not need to track the new map while keeping it off the stack.

```k
  rule <k> ITER A B => #HandleAnnotations(A)  ... </k>
       <stack> [map KT VT .Map] ; SS => SS </stack>

  rule <k> ITER A B
        => #HandleAnnotations(A)
        ~> B
        ~> #Push(map KT VT, M[#MinimalKey(M) <- undef])
        ~> ITER .AnnotationList B
           ...
       </k>
       <stack> [map KT VT M:Map] ; SS
            => [pair KT VT Pair #MinimalKey(M) {M[#MinimalKey(M)]}:>Data] ; SS
       </stack>
    requires size(M) >Int 0
```

### Big Map Operations

For the purposes of this semantics, `big_map`s are represented in the same way
as maps, so they can reuse the same execution rules.

```k
  rule <k> EMPTY_BIG_MAP A KT VT => #HandleAnnotations(A) ... </k>
       <stack> SS => [#Name(big_map A KT VT) .Map] ; SS </stack>
```

The other operations are identical.

### Option Operations

```k
  rule <k> SOME A => #HandleAnnotations(A)  ... </k>
       <stack> [T X] ; SS => [option T Some X] ; SS </stack>

  rule <k> NONE A T:Type => #HandleAnnotations(A)  ... </k>
       <stack> SS => [option #Name(T) None] ; SS </stack>

  rule <k> IF_NONE A BT BF => #HandleAnnotations(A) ~> BT ... </k>
       <stack> [option T None] ; SS => SS </stack>

  rule <k> IF_NONE A BT BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> [option T Some V] ; SS => [T V] ; SS </stack>
```

### Union Operations

```k
  rule <k> LEFT A RTy:Type => #HandleAnnotations(A)  ... </k>
       <stack> [LTy X:Data] ; SS => [or LTy #Name(RTy) Left X] ; SS </stack>

  rule <k> RIGHT A LTy:Type => #HandleAnnotations(A) ... </k>
       <stack> [RTy X:Data] ; SS => [or #Name(LTy) RTy Right X] ; SS </stack>

  rule <k> IF_LEFT A BT BF => #HandleAnnotations(A) ~> BT ... </k>
       <stack> [or LTy RTy Left V] ; SS => [LTy V] ; SS </stack>

  rule <k> IF_LEFT A BT BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> [or LTy RTy Right V] ; SS => [RTy V] ; SS </stack>
```

### List Operations

```k
  rule <k> CONS A => #HandleAnnotations(A)  ... </k>
       <stack> [T V] ; [list T L:List] ; SS => [list T ListItem(V) L] ; SS </stack>

  rule <k> NIL A T => #HandleAnnotations(A)  ... </k>
       <stack> SS => [list #Name(T) .List] ; SS </stack>

  rule <k> IF_CONS A BT BF => #HandleAnnotations(A) ~> BT ... </k>
       <stack> [list T ListItem(L1) Ls] ; SS => [T L1] ; [list T Ls] ; SS </stack>

  rule <k> IF_CONS A BT BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> [list T .List ] ; SS => SS </stack>

  rule <k> SIZE A => #HandleAnnotations(A)  ... </k>
       <stack> [list T L:List] ; SS => [nat size(L)] ; SS </stack>

  rule <k> ITER A B =>  #HandleAnnotations(A) ~>. ... </k>
       <stack> [list T .List] ; SS => SS </stack>

  rule <k> ITER A B
        => #HandleAnnotations(A)
        ~> B
        ~> #Push(list T,Ls)
        ~> ITER .AnnotationList B
           ...
       </k>
       <stack> [list T ListItem(E) Ls] ; SS => [T E] ; SS </stack>
```

The `MAP` operation over `list`s is defined in terms of a helper function.

```k
  rule <k> MAP A B
        => #HandleAnnotations(A)
        ~> #PerformMapList(T, Ls, .List, B)
           ...
       </k>
       <stack> [list T Ls] ; SS => SS </stack>

  syntax Instruction ::= #PerformMapList(TypeName, List, List, Block)
                       | #AddToList(List, List, Block)
  // ------------------------------------------------------
  rule <k> #PerformMapList(T, .List, Acc, B) => . ... </k>
       <stack> SS => [list T #ReverseList(Acc)] ; SS </stack>

  rule <k> #PerformMapList(T, ListItem(E) Ls, Acc, B)
        => B
        ~> #AddToList(Ls, Acc, B)
           ...
       </k>
       <stack> SS => [T E] ; SS </stack>

  rule <k> #AddToList(Ls, Acc, B)
        => #PerformMapList(T, Ls, ListItem(E) Acc, B)
           ...
       </k>
       <stack> [T E] ; SS => SS </stack>

  syntax List ::= #ReverseList(List) [function]
  syntax List ::= #ReverseListAux(List, List) [function]
  // ---------------------------------------------------
  rule #ReverseList(L) => #ReverseListAux(L, .List)
  rule #ReverseListAux(ListItem(L1) Ls, Acc)
    => #ReverseListAux(Ls, ListItem(L1) Acc)
  rule #ReverseListAux(.List, Acc) => Acc
```

Domain Specific operations
--------------------------

### Timestamp Operations

Timestamps are simply wrapped ints in Michelson, so the implementation of
simple arithmetic over them is straightforward. The differing argument types
however forces us to use two rules for each operation.

```k
  rule <k> ADD A => . ... </k>
       <stack> [timestamp #Timestamp(I1)] ; [int I2] ; SS => [timestamp #Timestamp(I1 +Int I2)] ; SS </stack>

  rule <k> ADD A => . ... </k>
       <stack> [int I1] ; [timestamp #Timestamp(I2)] ; SS => [timestamp #Timestamp(I1 +Int I2)] ; SS </stack>

  rule <k> SUB A => . ... </k>
       <stack> [timestamp #Timestamp(I1)] ; [int I2] ; SS => [timestamp #Timestamp(I1 -Int I2)] ; SS </stack>

  rule <k> SUB A => . ... </k>
       <stack> [timestamp #Timestamp(I1)] ; [timestamp #Timestamp(I2)] ; SS => [int I1 -Int I2] ; SS </stack>
```

### Blockchain Operations

```k
  rule <k> CREATE_CONTRACT A:AnnotationList { C } => . ... </k>
       <stack> [option key_hash Delegate:OptionData] ;
               [mutez Initial:Mutez] ;
               [T Storage:Data] ;
               SS
            => [operation Create_contract(O, C, Delegate, Initial, Storage)] ;
               [address #Address("@Address(" +String Int2String(!_:Int) +String ")")] ;
               SS
       </stack>
       <nonce> #Nonce(O) => #NextNonce(#Nonce(O)) </nonce>

  rule <k> TRANSFER_TOKENS _ => . ... </k>
       <stack> [T D] ; [mutez M] ; [contract T #Contract(A, _)] ; SS
            => [operation Transfer_tokens(O, D, M, A)] ; SS
       </stack>
       <nonce> #Nonce(O) => #NextNonce(#Nonce(O)) </nonce>

  rule <k> SET_DELEGATE A => . ... </k>
       <stack> [option key_hash D] ; SS => [operation Set_delegate(O, D)] ; SS </stack>
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
  rule <k> CONTRACT _ T => . ... </k>
       <stack> [address A] ; SS => [option contract #Name(T) Some {M[A]}:>Data] ; SS </stack>
       <knownaddrs> M </knownaddrs>
    requires A in_keys(M)
     andBool #TypeFromContractStruct({M[A]}:>Data) ==K T

  rule <k> CONTRACT _ T => . ... </k>
       <stack> [address A:Address] ; SS => [option contract #Name(T) None] ; SS </stack>
       <knownaddrs> M </knownaddrs> [owise]

  rule <k> IMPLICIT_ACCOUNT Ann => . ... </k>
       <stack> [key_hash #KeyHash(A)] ; SS
            => [contract unit #Contract(#Address(A), unit .AnnotationList)] ; SS
       </stack>

  syntax Type ::= #TypeFromContractStruct(Data) [function]
  rule #TypeFromContractStruct(#Contract(_, T)) => T
```

These instructions push blockchain state on the stack.

```k
  rule <k> BALANCE A => . ... </k>
       <stack> SS => [mutez B] ; SS </stack>
       <mybalance> B </mybalance>

  rule <k> ADDRESS Ann => . ... </k>
       <stack> [contract T #Contract(A, _)] ; SS => [address A] ; SS </stack>

  rule <k> SOURCE Ann => . ... </k>
       <stack> SS => [address A] ; SS </stack>
       <sourceaddr> A </sourceaddr>

  rule <k> SENDER Ann => . ... </k>
       <stack> SS => [address A] ; SS </stack>
       <senderaddr> A </senderaddr>

  rule <k> SELF Ann => . ... </k>
       <stack> SS => [contract #Name(T) #Contract(A, T)] ; SS </stack>
       <paramtype> T </paramtype>
       <myaddr> A </myaddr>

  rule <k> AMOUNT Ann => . ... </k>
       <stack> SS => [mutez M] ; SS </stack>
       <myamount> M </myamount>

  rule <k> CHAIN_ID A => . ... </k>
       <stack> SS => [chain_id C] ; SS </stack>
       <mychainid> C </mychainid>

  rule <k> NOW A => . ... </k>
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
       <stack> [bytes B:MBytes] ; SS => [bytes #Blake2B(B)] ; SS </stack>

  rule <k> SHA256 A => #HandleAnnotations(A) ... </k>
       <stack> [bytes B:MBytes] ; SS => [bytes #SHA256(B)] ; SS </stack>

  rule <k> SHA512 A => #HandleAnnotations(A) ... </k>
       <stack> [bytes B:MBytes] ; SS => [bytes #SHA512(B)] ; SS </stack>

  syntax MBytes ::= #SignedMBytes(Key, Signature, MBytes)

  /*
  // FIXME: The haskell backend does not support distinguishing these rules.
  rule <k> CHECK_SIGNATURE A => #HandleAnnotations(A) ... </k>
       <stack> #Key(K)
            ~> #Signature(S)
            ~> #SignedMBytes(#Key(K), #Signature(S), _)
            => true
               ...
       </stack>

  rule <k> CHECK_SIGNATURE A => #HandleAnnotations(A) ... </k>
       <stack> #Key(_)
            ~> #Signature(_)
            ~> _:MBytes
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
       <stack> [mutez #Mutez(I1)] ; [mutez #Mutez(0)] ; SS => [option pair nat mutez None] ; SS </stack>

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> [mutez #Mutez(I1)] ; [nat 0] ; SS => [option pair mutez mutez None] ; SS </stack>

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

```symbolic
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

```symbolic
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
    requires isValue(V)
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
       <knownaddrs> Addrs </knownaddrs>
       <bigmaps> BigMaps </bigmaps>
       <symbols> Symbols </symbols>
       <stack> Stack => .Stack </stack>
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
       <paramtype> PT </paramtype>
       <symbols> Syms => S |-> #TypedSymbol(T, D) Syms </symbols>
    requires notBool S in_keys(Syms)

  rule <k> #Bind( [ T S:SymbolicData ] ; SS  => SS
                , [ T D1 ]             ; SS' => SS'
                )
           ...
       </k>
       <paramtype> PT </paramtype>
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
  rule #ConcreteMatch(S:SymbolicData, _, _, _, _) => false
  rule #ConcreteMatch(ED, T, Addrs, BigMaps, AD) => #Matches(#MichelineToNative(ED,T,Addrs,BigMaps),AD)
    requires notBool isSymbolicData(ED)
```

```k
  syntax KItem ::= #RestoreSymbols(Map)
  rule <k> #RestoreSymbols(Symbols) => .K ... </k>
       <symbols> _ => Symbols </symbols>
```

Symbolic Value Processing
-------------------------

### Extending functions to `SymbolicData`

```k
  syntax TypedSymbol ::= #TypedSymbol(TypeName, Data)
```

```symbolic
  rule [[ #MichelineToNative(S:SymbolicData, T, _, _) => D ]]
       <symbols> S |-> #TypedSymbol(#Name(T), D) ... </symbols>

  rule [[ #MichelineToNative(S:SymbolicData, T, _, _) => S ]]
       <symbols> Syms:Map </symbols>
    requires notBool (S in_keys(Syms))
```

### `#CreateSymbol`

`#CreateSymbol` is responsible for setting up the initial symbol table.

```k
  syntax Type ::= "#UnknownType"

  syntax KItem ::= SymbolicElement

  syntax SymbolicElement ::= #SymbolicElement(SymbolicData, Type)

  syntax Set ::= #FindSymbolsBL(BlockList) [function, functional]
  rule #FindSymbolsBL(.BlockList) => .Set
  rule #FindSymbolsBL(B:Block ; Rs:BlockList)
    => #FindSymbolsB(B) #FindSymbolsBL(Rs)

  syntax Set ::= #FindSymbolsB(Block) [function, functional]
  rule #FindSymbolsB({ }) => .Set
  rule #FindSymbolsB({ I:Instruction }) => #FindSymbolsI(I)
  rule #FindSymbolsB({ I:Instruction ; Is:DataList }:Block)
    => #FindSymbolsI(I) |Set #FindSymbolsB({ Is })

  syntax Set ::= #FindSymbolsI(Instruction) [function, functional]
  rule #FindSymbolsI(PUSH _ T D) => #FindSymbolsIn(D, T)
  rule #FindSymbolsI(_)          => .Set [owise]

  syntax Set ::= #FindSymbolsS(StackElementList) [function, functional]
  rule #FindSymbolsS(.StackElementList) => .Set
  rule #FindSymbolsS((Stack_elt T D ); Ss:StackElementList)
    => #FindSymbolsIn(D, T) |Set #FindSymbolsS(Ss)
```

```k
  syntax Set ::= #FindSymbolsIn(Data, Type) [function, functional]
  rule #FindSymbolsIn(S:SymbolicData, T)
    => SetItem(#SymbolicElement(S, T)) // ???

  rule #FindSymbolsIn(Pair V1 V2, pair _ T1 T2)
    => #FindSymbolsIn(V1, T1) |Set #FindSymbolsIn(V2, T2)
  rule #FindSymbolsIn(Some V, option _ T) => #FindSymbolsIn(V, T)
  rule #FindSymbolsIn(Left V, or _ T _) => #FindSymbolsIn(V, T)
  rule #FindSymbolsIn(Right V, or _ _ T) => #FindSymbolsIn(V, T)
  rule #FindSymbolsIn(B:Block, lambda _ _ _) => #FindSymbolsB(B)

  rule #FindSymbolsIn({ }, list _ _) => .Set
  rule #FindSymbolsIn({ D:Data }, list _ T) => #FindSymbolsIn(D, T)
  rule #FindSymbolsIn({ D:Data ; DL }, list _ T)
    => #FindSymbolsIn(D, T) |Set #FindSymbolsIn({ DL }, T)

  rule #FindSymbolsIn({ }, set _ _) => .Set
  rule #FindSymbolsIn({ D:Data }, set _ T) => #FindSymbolsIn(D, T)
  rule #FindSymbolsIn({ D:Data ; DL }, set _ T)
    => #FindSymbolsIn(D, T) |Set #FindSymbolsIn({ DL }, T)

  rule #FindSymbolsIn({ }, map _ _ _) => .Set
  rule #FindSymbolsIn({ Elt K V }, map _ KT VT)
    => #FindSymbolsIn(K, KT) |Set #FindSymbolsIn(V, VT)
  rule #FindSymbolsIn({ M:MapEntry ; ML:MapEntryList }, (map _ _ _) #as MT)
    => #FindSymbolsIn({ M }, MT) |Set #FindSymbolsIn({ ML }, MT)

  rule #FindSymbolsIn(M:MapLiteral, big_map A KT VT)
    => #FindSymbolsIn(M, map A KT VT)

  rule #FindSymbolsIn(_, _) => .Set [owise]
```

```k
  syntax Bool ::= #AllTypesKnown(Set) [function, functional]
  rule #AllTypesKnown(SetItem(#SymbolicElement(_, #UnknownType)) _) => false
  rule #AllTypesKnown(_) => true [owise]

  syntax UnificationFailure ::= "#UnificationFailure"

  syntax UnifiedSet ::= Set | UnificationFailure

  syntax UnifiedSet ::= #UnifyTypes(Set) [function, functional]

  rule #UnifyTypes(SetItem(#SymbolicElement(S, #UnknownType))
                   SetItem(#SymbolicElement(S, T)) Ss)
    => #UnifyTypes(SetItem(#SymbolicElement(S, T)) Ss)

  rule #UnifyTypes(SetItem(#SymbolicElement(S, T1))
                   SetItem(#SymbolicElement(S, T2)) _)
    => #UnificationFailure
    requires T1 =/=K T2
     andBool T1 =/=K #UnknownType
     andBool T2 =/=K #UnknownType

  rule #UnifyTypes(S) => S
    requires #AllTypesKnown(S) [owise]

  rule #UnifyTypes(S) => #UnificationFailure
    requires notBool(#AllTypesKnown(S)) [owise]

  syntax UnifiedList ::= List | UnificationFailure
  syntax UnifiedList ::= #UnifiedSetToList(UnifiedSet) [function, functional]

  rule #UnifiedSetToList(S:Set) => Set2List(S)
  rule #UnifiedSetToList(#UnificationFailure) => #UnificationFailure
```

```symbolic
  syntax KItem ::= #CreateSymbols(UnifiedList)
  rule <k> #CreateSymbols(.List) => . ... </k>
  rule <k> #CreateSymbols(ListItem(#SymbolicElement(D, T)) S)
        => #CreateSymbol(D, T)
        ~> #CreateSymbols(S)
           ...
       </k>
```

```symbolic
  syntax KItem ::= #CreateSymbol(SymbolicData, Type)
  rule <k> (.K => #MakeFresh(T)) ~>  #CreateSymbol(_, T) ... </k>
  rule <k> (V ~> #CreateSymbol(N, T)) => . ... </k>
       <symbols> M => M[N <- #TypedSymbol(#Name(T), V)] </symbols>
    requires isValue(V)
```

### "Evaluating" Data

The `isValue` predicate indicates if a `Data` has been fully evaluated.

```k
    syntax Bool ::= isValue(Data) [function, functional]
    rule isValue(D:SimpleData) => true
    rule isValue(None) => true
    rule isValue(Some V) => isValue(V)
    rule isValue(Left V) => isValue(V)
    rule isValue(Right V) => isValue(V)
    rule isValue(Pair L R) => isValue(L) andBool isValue(R)
    rule isValue(_) => false [owise]
```

```symbolic
    rule isValue(D:SimpleData) => true [simplification]
    rule isValue(None) => true [simplification]
    rule isValue(Some V) => isValue(V) [simplification]
    rule isValue(Left V) => isValue(V) [simplification]
    rule isValue(Right V) => isValue(V) [simplification]
    rule isValue(Pair L R) => isValue(L) andBool isValue(R) [simplification]
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

```symbolic
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

  rule <k> #MakeFresh(list      _:AnnotationList _:Type)        => ?_:List                               ... </k>
  rule <k> #MakeFresh(set       _:AnnotationList _:Type)        => ?_:Set                                ... </k>
  rule <k> #MakeFresh(map       _:AnnotationList _:Type _:Type) => ?_:Map                                ... </k>
  rule <k> #MakeFresh(big_map   _:AnnotationList _:Type _:Type) => ?_:Map                                ... </k>
  rule <k> #MakeFresh(lambda    _:AnnotationList T1 T2)         => #Lambda(#Name(T1),#Name(T2),?_:Block) ... </k>
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

  rule #Matches(#Any, _) => true

  rule #Matches(.List, .List) => true
  rule #Matches(ListItem(L1) Ls1:List, ListItem(L2) Ls2:List)
    => #Matches(L1, L2) andBool #Matches(Ls1, Ls2)

  rule #Matches(.Set, .Set) => true
  rule #Matches(SetItem(S1) Ss1, SetItem(S2) Ss2)
    => #Matches(S1, S2) andBool #Matches(Ss1, Ss2)

  rule #Matches(.Map, .Map) => true
  rule #Matches((K |-> V1) M1, (K |-> V2) M2)
    => #Matches(V1, V2) andBool #Matches(M1, M2)

  syntax Data ::= FailedStack

  rule #Matches(Create_contract(I1, C, O1, M1, D1),
                Create_contract(I2, C, O2, M2, D2))
    => #Matches(I1, I2) andBool
       #Matches(O1, O2) andBool
       #Matches(M1, M2) andBool
      #Matches(D1, D2)

  rule #Matches(Transfer_tokens(I1, D1, M1, A1),
                Transfer_tokens(I2, D2, M2, A2))
    => #Matches(I1, I2) andBool
       #Matches(D1, D2) andBool
       #Matches(M1, M2) andBool
       #Matches(A1, A2)

  rule #Matches(Set_delegate(I1, O1),
                Set_delegate(I2, O2))
    => #Matches(I1, I2) andBool #Matches(O1, O2)

  rule #Matches(Pair L1 R1, Pair L2 R2)
    => #Matches(L1, L2) andBool #Matches(R1, R2)

  rule #Matches(Some D1, Some D2) => #Matches(D1, D2)

  rule #Matches(Left D1, Left D2) => #Matches(D1, D2)
  rule #Matches(Right D1, Right D2) => #Matches(D1, D2)
endmodule
```
