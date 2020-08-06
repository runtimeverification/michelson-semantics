```k
requires "michelson/common.md"
requires "michelson/types.md"
```

Michelson Interpreter State
===========================

K-Michelson: an intra-contract semantics
----------------------------------------

```k
module MICHELSON-CONFIG
  imports MICHELSON-COMMON
  imports MICHELSON-TYPES
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

Michelson Core Semantics
========================

This is the main execution semantics for Michelson. It contains the rewrite
rule semantics for the all Michelson instructions, as well as logic for
transforming values from their Micheline representations to K internal
representations. It also contains the .tzt file loading and contract
initialization logic.

```k
module MICHELSON
  imports MICHELSON-CONFIG
  imports MICHELSON-INTERNAL-SYNTAX
  imports DOMAINS
  imports BYTES
```


The `#BaseInit` takes care of initialization common to the different semantics.
This can be invoked by a rule similar to `rule <k> #Init => #Preprocess ~>
#BaseInit ~> #Postprocess ... </k>`.

```k
  syntax KItem ::= "#BaseInit"
  rule <k> #BaseInit
        => #ConvertBigMapsToNative
        ~> #ConvertParamToNative
        ~> #ConvertStorageToNative
           ...
       </k>
```

Loading groups into the K configuration
---------------------------------------

Below are the rules for loading specific groups.

Loading a `now` group simply involves setting the contents of the now timestamp
to the contained integer. Similarly simple logic applies to sender, source,
chain\_id and self.

```k
  rule <k> G ; Gs:Groups => G ~> Gs ... </k>
  rule <k> G ; =>           G       ... </k>

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
```

Amount and balance require slightly more logic to verify that the value they're
being set to is actually a legal mutez value, but are otherwise relatively
simple.

```k
  rule <k> amount I => .K ... </k>
       <myamount> #Mutez(0 => I) </myamount>
    requires #IsLegalMutezValue(I)

  rule <k> balance I => .K ... </k>
       <mybalance> #Mutez(0 => I) </mybalance>
    requires #IsLegalMutezValue(I)
```

Loading the other contracts map involves transforming its map entry list style
concrete representation to a K-Michelson map.

```k
  rule <k> other_contracts { M } => .K ... </k>
       <knownaddrs> .Map => #OtherContractsMapEntryListToKMap(M) </knownaddrs>
```

These two groups contain information from the contract itself, but they are
promoted to loading groups due to data dependency orders. Specifically, if we
left these as part of the contract, we would need to load the `parameter_value`
and `storage_value` groups after the contract so we know what types we're working
with, but we need to load those groups before the contract so the contract can
execute with the appropriate starting state. Extracting these groups solves the
cyclical ordering problem.

```k
  rule <k> parameter T => .K ... </k>
       <paramtype> #NotSet => T </paramtype>

  rule <k> storage T => .K ... </k>
       <storagetype> #NotSet => T </storagetype>
```

Similar to the `other_contracts` rule, we need to transform BigMaps into the
appropriate K-Michelson type.

```k
  rule <k> big_maps { M } => .K ... </k>
       <bigmaps> .Map => #BigMapsEntryListToKMap(M) </bigmaps>

  syntax Map ::= #BigMapsEntryListToKMap(BigMapEntryList) [function]
  syntax Map ::= #BigMapsEntryToKMap(BigMapEntry) [function]

  rule #BigMapsEntryListToKMap(.BigMapEntryList) => .Map
  rule #BigMapsEntryListToKMap(E ; Es) => #BigMapsEntryToKMap(E) #BigMapsEntryListToKMap(Es)

  syntax KItem ::= "#BigMap" "(" SequenceData "," Type ")"
  rule #BigMapsEntryToKMap(Big_map I T1 T2 { }          ) => I |-> #BigMap({ }, big_map .AnnotationList T1 T2)
  rule #BigMapsEntryToKMap(Big_map I T1 T2 ML:MapLiteral) => I |-> #BigMap(ML,  big_map .AnnotationList T1 T2)
```

The final loading group in this file is the contract group. The storage and
parameter values are combined and the stack is initialized, and then the code is
extracted so that we can move on to the execution semantics.

```k
  rule <k> contract { code C ; storage S ; parameter P ; } ; Cs => .K ... </k>
       <script> #NoData => C </script>
       <paramtype> #NotSet => P </paramtype>
       <storagetype> #NotSet => S </storagetype>
```

From Micheline to K-Michelson Internal Representation
-----------------------------------------------------

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

Execution Semantics
===================

```k
  syntax KItem ::= "#LoadStack"
  rule <k> #LoadStack ... </k>
       <stack> .K => Pair P S </stack>
       <paramvalue> P </paramvalue>
       <storagevalue> S </storagevalue>

  syntax KItem ::= "#ExecuteScript"
  rule <k> #ExecuteScript => Script ... </k>
       <script> Script </script>
```

These rules split apart blocks into KItems so that the main semantic rules can
use idiomatic K.

```k
  rule #Exec(Is) => Is

  rule TI:TypedInstruction ; TIS => TI ~> TIS

  rule <k> #TI(I, T1 -> T2) => I ... </k>
       <stacktypes> _ => T1 </stacktypes>

  rule I:Instruction ; Is => I ~> Is
  rule {} => .K [structrual]
  rule { Is:DataList } => Is
//  rule I:Data ; => I [anywhere]
```

For now, annotations are simply ignored.

```k
  syntax Instruction ::= #HandleAnnotations(AnnotationList)
  rule #HandleAnnotations(_) => .
```

This production contains error information when a contract fails at runtime. Its
arguments are:

1. An error message.
2. The top element of the stack (the argument to `FAILWITH`)
3. The remainder of the stack.
4. The remainder of the K cell.

```k
  syntax Error ::= Aborted(String, KItem, K, K)

  // Core Instructioons
  //// Control Structures
  rule <k> FAILWITH A ~> Rk => #HandleAnnotations(A) ~> Aborted("FAILWITH instruction reached", D, Rk, Rs) ~> Rk </k>
       <stack> D ~> Rs => ( Failed D ) </stack>
```

It then consumes the rest of the program:

```k
  rule <k> Aborted(_, _, _, _) ~> (_:TypedInstruction => .K) ... </k>
  rule <k> Aborted(_, _, _, _) ~> (_:DataList => .K) ... </k>
  rule <k> Aborted(_, _, _, _) ~> (_:Data => .K) ... </k>
```

Conditionals
------------

The control flow instruction's implementations in K should look extremely
similar to their formal description in the [Michelson
documentation](https://tezos.gitlab.io/whitedoc/michelson.html#control-structures).
Keeping this similarity, unless absolutely prevented for performance or K style
reasons, was a major design goal of the semantics.

```k
  rule <k> IF A BT BF => #HandleAnnotations(A) ~> BT ... </k>
       <stack> true => . ... </stack>

  rule <k> IF A BT BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> false => . ... </stack>
```

Loops
-----

```k
  rule <k> LOOP .AnnotationList B
        => B ~> LOOP .AnnotationList B
           ...
       </k>
       <stack> true => . ... </stack>
  rule <k> LOOP .AnnotationList B => .K ... </k>
       <stack> false => . ... </stack>
```

```k
  rule <k> LOOP_LEFT A B => #HandleAnnotations(A) ~> B ~> LOOP_LEFT .AnnotationList B ... </k>
       <stack> Left D => D ... </stack>
  rule <k> LOOP_LEFT A B => #HandleAnnotations(A) ... </k>
       <stack> Right D => D ... </stack>
```

It is sometimes useful to create "pseudo-instructions" like this to schedule
operations to happen in the future.

```k
  syntax Instruction ::= #Push(Data)
  rule <k> #Push(D) => . ... </k>
       <stack> . => D ... </stack>
```

The DIP instruction uses the `#Push` pseudo-instruction to replace the element
it pops off for its block.

```k
  rule <k> DIP A B => #HandleAnnotations(A) ~> B ~> #Push(D) ... </k>
       <stack> D:Data => . ... </stack>
```

The multiple DIP instruction is defined recursively

```k
  rule <k> DIP A 0 B => #HandleAnnotations(A) ~> B ... </k>
  rule <k> DIP A I B => #HandleAnnotations(A) ~> DIP .AnnotationList { DIP .AnnotationList  I -Int 1 B } ... </k>
       requires I >Int 0
```

This pseudo-instruction implements the behavior of restoring the previous stack
when a lambda completes execution.

```k
  syntax Instruction ::= #ReturnStack(K)

  rule <k> #ReturnStack(Ls) => . ... </k>
       <stack> R:Data => R ~> Ls </stack>
```

An EXEC instruction replaces the stack and schedules the restoration of the old
stack after the completion of the lambda code.

```k
  rule <k> EXEC B => #HandleAnnotations(B) ~> C ~> #ReturnStack(Rs) ... </k>
       <stack> A:Data ~> #Lambda(_, _, C):Data ~> Rs:K => A </stack>
```

APPLY demonstrates why lambdas have their type information preserved, as
otherwise we would be unable to produce an appropriate `PUSH` instruction for
the expanded lambda.

```k
  rule <k> APPLY A => #HandleAnnotations(A) ... </k>
       <stack> D:Data ~> #Lambda(pair _:AnnotationList T0 T1, T2, { C } ) => #Lambda(T1, T2, { PUSH .AnnotationList T0 D ; PAIR .AnnotationList ; { C } } ) ... </stack>
```

`DROP n` is implemented in a recursive style, like in the Michelson
documentation.

```k
  ////Stack operations

  rule <k> DROP A =>  #HandleAnnotations(A) ... </k>
       <stack> _:Data => . ... </stack>

  rule <k> DROP A I => #HandleAnnotations(A) ~> DROP .AnnotationList ~> DROP .AnnotationList I -Int 1 ... </k>
       requires I >Int 0

  rule <k> DROP A 0 => #HandleAnnotations(A) ... </k>
```

DUP and SWAP are essentially lifted directly from the docs.

```k
  rule <k> DUP A => #HandleAnnotations(A) ... </k>
       <stack> X:Data => X ~> X ... </stack>

  rule <k> SWAP A => #HandleAnnotations(A) ... </k>
       <stack> X:Data ~> Y:Data => Y ~> X ... </stack>
```

Dig is implemented in 2 phases, digging down and building back up. This is
implemented with the following production, which functions essentially like a
FSM. When `I > 0`, we push elements into the internal stack after popping them
from the main stack. When `I = 0`, we have found the element to move to the top
and can save it. When `I = -1`, we need to start unwinding the inner stack and
restoring the elements under the selected one.

```k
  syntax Instruction ::= #DoDig(Int, K, OptionData)

  rule <k> DIG A I => #HandleAnnotations(A) ~> #DoDig(I, .K, None) ... </k>
       <stack> S </stack>

  rule <k> #DoDig(I, A, None) => #DoDig(I -Int 1, F ~> A, None) ... </k>
       <stack> F:Data => . ... </stack>
       requires I >Int 0

  rule <k> #DoDig(0, A, None) => #DoDig(-1, A, Some F) ... </k>
       <stack> F:Data => . ... </stack>

  rule <k> #DoDig(-1, F:Data ~> A, Some T) => #DoDig(-1, A, Some T) ... </k>
       <stack> . => F ... </stack>

  rule <k> #DoDig(-1, .K, Some T) => . ... </k>
       <stack> . => T ... </stack>
```

Dug is implemented similar to Dig, except the element to move is saved
immediately rather than waiting for `I = 0`. Instead it is placed when `I = 0`.

```k
  syntax Instruction ::= #DoDug(Int, K, Data)

  rule <k> DUG A I => #HandleAnnotations(A) ~> #DoDug(I, .K, T) ... </k>
       <stack> T => .K ... </stack>

  rule <k> #DoDug(I, S, R) => #DoDug(I -Int 1, T ~> S, R) ... </k>
       <stack> T:Data => .K ... </stack>
       requires I >Int 0

  rule <k> #DoDug(0, S, R) => #DoDug(-1, S, R) ... </k>
       <stack> .K => R ... </stack>

  rule <k> #DoDug(-1, T:Data ~> S, R) => #DoDug(-1, S, R) ... </k>
       <stack> .K => T ... </stack>

  rule <k> #DoDug(-1, .K, _) => .K ... </k>
```

PUSH needs to convert its argument to semantics form, but otherwise matches the
documentation directly.

```k
  rule <k> PUSH A T X => #HandleAnnotations(A) ... </k>
       <stack> . => #MichelineToNative(X, T, .Map, .Map) ... </stack>
```

UNIT and LAMBDA are implemented almost exactly as specified in the documentation.

```k
  rule <k> UNIT A => #HandleAnnotations(A) ... </k>
       <stack> . => Unit ... </stack>

  rule <k> LAMBDA A T1 T2 C => #HandleAnnotations(A) ... </k>
       <stack> . => #Lambda(T1, T2, C) ... </stack>
```

Comparisons map directly onto K Int functions.

```k
  //// Generic Comparisons

  rule <k> EQ A => #HandleAnnotations(A) ... </k>
       <stack> I => I ==Int 0 ... </stack>

  rule <k> NEQ A => #HandleAnnotations(A) ... </k>
       <stack> I => I =/=Int 0 ... </stack>

  rule <k> LT A => #HandleAnnotations(A) ... </k>
       <stack> I => I <Int 0 ... </stack>

  rule <k> GT A => #HandleAnnotations(A) ... </k>
       <stack> I => I >Int 0 ... </stack>

  rule <k> LE A => #HandleAnnotations(A) ... </k>
       <stack> I => I <=Int 0 ... </stack>

  rule <k> GE A => #HandleAnnotations(A) ... </k>
       <stack> I => I >=Int 0 ... </stack>
```

```k
    rule A  >Int B => notBool( A <=Int B ) [simplification]
    rule A >=Int B => notBool( A  <Int B ) [simplification]
```

As do basic boolean functions.

```k
  // Operations
  //// Operations on booleans
  rule <k> OR A => #HandleAnnotations(A) ... </k>
       <stack> B1 ~> B2 => B1 orBool B2 ...  </stack>

  rule <k> AND A => #HandleAnnotations(A) ... </k>
       <stack> B1 ~> B2 => B1 andBool B2 ... </stack>

  rule <k> XOR A => #HandleAnnotations(A) ... </k>
       <stack> B1 ~> B2 => B1 xorBool B2 ... </stack>

  rule <k> NOT A => #HandleAnnotations(A) ... </k>
       <stack> B => notBool B ... </stack>
```

Negation and taking absolute value are similarly trivial.

```k
  //// Operations on integers and natural numbers
  rule <k> NEG A => #HandleAnnotations(A) ... </k>
       <stack> I => 0 -Int I ... </stack>

  rule <k> ABS A => #HandleAnnotations(A) ... </k>
       <stack> I => absInt(I) ... </stack>
```

ISNAT could be implemented in a single rule with the `#if _ #then _ #else _ #fi`
structure, but I think this is easier to read.

```k
  rule <k> ISNAT A => #HandleAnnotations(A) ... </k>
       <stack> I => Some I ... </stack>
       requires I >=Int 0

  rule <k> ISNAT A => #HandleAnnotations(A) ... </k>
       <stack> I => None ... </stack>
       requires I <Int 0
```

Since Ints and Nats are both represented by the Int sort in K, INT is a noop.

```k
  rule <k> INT A => #HandleAnnotations(A) ... </k>
       <stack> I:Int ... </stack>
```

Basic arithmetic operations map directly.

```k
  rule <k> ADD A => #HandleAnnotations(A) ... </k>
       <stack> I1 ~> I2 => I1 +Int I2 ... </stack>

  rule <k> SUB A => #HandleAnnotations(A) ... </k>
       <stack> I1 ~> I2 => I1 -Int I2 ... </stack>

  rule <k> MUL A => #HandleAnnotations(A) ... </k>
       <stack> I1 ~> I2 => I1 *Int I2 ... </stack>
```

EDIV could, like ISNAT, probably be written in one rule, but this is probably
easier to interpret.

```k
  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> I1:Int ~> 0 => None ... </stack>
       // Could combine this rule with the Mutez one but probably a disadvantage in readability.

  rule <k> EDIV A  => #HandleAnnotations(A) ... </k>
       <stack> I1 ~> I2 => Some (Pair (I1 /Int I2) (I1 %Int I2)) ... </stack>
       requires I2 =/=Int 0
```

Bitwise operations on ints map directly onto K functions over ints.

```k
  rule <k> OR A => #HandleAnnotations(A)  ... </k>
       <stack> I1 ~> I2 => I1 |Int I2 ... </stack>

  rule <k> AND A => #HandleAnnotations(A) ... </k>
       <stack> I1 ~> I2 => I1 &Int I2 ... </stack>

  rule <k> XOR A => #HandleAnnotations(A) ... </k>
       <stack> I1 ~> I2 => I1 xorInt I2 ... </stack>

  rule <k> NOT A => #HandleAnnotations(A) ... </k>
       <stack> I => ~Int I ... </stack>
```

These rules are interesting mainly for their failure cases, which rewrite the k
cell to an Aborted production.

```k
  rule <k> LSL A => #HandleAnnotations(A) ... </k>
       <stack> X ~> S => X <<Int S ... </stack>
       requires S <=Int 256

  rule <k> LSL A ~> Rk => #HandleAnnotations(A) ~> Aborted("LSL out of range", S, Rk, Rs) ~> Rk </k>
       <stack> C:Int ~> S:Int ~> Rs => ( GeneralOverflow C S )  </stack>
       requires S >Int 256

  rule <k> LSR A => #HandleAnnotations(A) ... </k>
       <stack> X ~> S => X >>Int S ... </stack>
       requires S <=Int 256

  rule <k> LSR A ~> Rk => #HandleAnnotations(A) ~> Aborted("LSR out of range", S, Rk, Rs) ~> Rk </k>
       <stack> X ~> S ~> Rs => ( GeneralOverflow X S ) </stack>
       requires S >Int 256
```

We lift the COMPARE operation to a function over Data, allowing many different
instantiations of the COMPARE operation to be implemented in fewer rules.

```k
  syntax Int ::= #DoCompare(Data, Data) [function, functional]

  rule #DoCompare(true, true) => 0
  rule #DoCompare(false, false) => 0
  rule #DoCompare(false, true) => -1
  rule #DoCompare(true, false) => 1

  rule #DoCompare(I1:Int, I2:Int) => -1 requires I1 <Int I2 [concrete(I1,I2)]
  rule #DoCompare(I1:Int, I2:Int) => 0 requires I1 ==Int I2 [concrete(I1,I2)]
  rule #DoCompare(I1:Int, I2:Int) => 1 requires I1 >Int I2  [concrete(I1,I2)]

  rule #DoCompare(S1:String, S2:String) => -1 requires S1 <String S2 [concrete]
  rule #DoCompare(S1:String, S2:String) => 0 requires S1 ==String S2 [concrete]
  rule #DoCompare(S1:String, S2:String) => 1 requires S1 >String S2  [concrete]

  rule #DoCompare((Pair A1 A2), (Pair B1 B2)) => -1                 requires #DoCompare(A1, B1) ==Int -1
  rule #DoCompare((Pair A1 A2), (Pair B1 B2)) => #DoCompare(A2, B2) requires #DoCompare(A1, B1) ==Int 0
  rule #DoCompare((Pair A1 A2), (Pair B1 B2)) => 1                  requires #DoCompare(A1, B1) ==Int 1

  rule <k> COMPARE A => #HandleAnnotations(A) ... </k>
       <stack> V1 ~> V2 => #DoCompare(V1, V2) ... </stack>

  rule #DoCompare(B1:Bytes, B2:Bytes) => #DoCompare(Bytes2Int(B1, BE, Unsigned), Bytes2Int(B2, BE, Unsigned))
  rule #DoCompare(#KeyHash(S1), #KeyHash(S2)) => #DoCompare(S1, S2)
  rule #DoCompare(#Mutez(I1), #Mutez(I2)) => #DoCompare(I1, I2)
  rule #DoCompare(#Timestamp(I1), #Timestamp(I2)) => #DoCompare(I1, I2)
  rule #DoCompare(#Address(S1), #Address(S2)) => #DoCompare(S1, S2)
```

TODO: If we define `DoCompare` as a macro for `#ite` we can avoid this.

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

CONCAT is complicated by the fact that it is defined differently over strings
and bytes, and so we need type information to select the correct implementation.
This is no problem for non-empty lists, but CONCATing an empty list of strings
should produce `""`, whereas CONCATing an empty list of bytes should produce 0x.
We use the type information stored in `#List` to try to determine this, but the
case of lists produced with MAP cannot be solved without a full type system,
which is currently out of scope.

```k
  syntax String ::= #ConcatStrings(List, String) [function]
  rule #ConcatStrings(.List, A) => A
  rule #ConcatStrings(ListItem(S1) DL, A) => #ConcatStrings(DL, A +String S1)

  //// Operations on strings
  rule <k> CONCAT A => #HandleAnnotations(A) ... </k>
       <stack> S1 ~> S2 => S1 +String S2 ... </stack>

  rule <k> CONCAT A => #HandleAnnotations(A) ... </k>
       <stack> L => #ConcatStrings(L, "") ... </stack>
       <stacktypes> list _ string _ ; _ </stacktypes>

  rule <k> SIZE A => #HandleAnnotations(A) ... </k>
       <stack> S => lengthString(S) ... </stack>
```

The actual out of bounds conditions here are determined by experimentation.
Earlier versions of the semantics didn't check if O was in bounds, resulting in
`Slice("", 0, 0) => Some ""` rather than the correct
`#SliceString("", 0, 0) => None`

```k
  syntax OptionData ::= #SliceString(String, Int, Int) [function]

  rule #SliceString(S, O, L) => Some substrString(S, O, O +Int L)
  requires O >=Int 0 andBool L >=Int 0 andBool O <Int lengthString(S) andBool (O +Int L) <=Int lengthString(S)

  rule #SliceString(S, O, L) => None [owise]

  rule <k> SLICE A => #HandleAnnotations(A) ... </k>
       <stack> O ~> L ~> S => #SliceString(S, O, L)  ... </stack>
```

Pair operations lift directly from the documentation.

```k
  //// Operations on pairs
  rule <k> PAIR A => #HandleAnnotations(A) ... </k>
       <stack> L ~> R => Pair L R ... </stack>

  rule <k> UNPAIR A => #HandleAnnotations(A) ... </k>
       <stack> Pair L R => L ~> R ... </stack>

  rule <k> CAR A => #HandleAnnotations(A) ... </k>
       <stack> Pair L _ => L ... </stack>

  rule <k> CDR A => #HandleAnnotations(A) ... </k>
       <stack> Pair _ R => R ... </stack>
```

Sets in Michelson are implemented using the K hooked set implementation. This
allows many Michelson operation, like MEM, to lift directly to Set functions.

```k
  //// Operations on sets
  rule <k> EMPTY_SET A _ => #HandleAnnotations(A) ... </k>
       <stack> . => .Set ... </stack>

  rule <k> MEM A => #HandleAnnotations(A) ... </k>
       <stack> X ~> S:Set => X in S ... </stack>
```

Built-in support for sets allows clean rules like the ones below for adding and
removing elements from the set respectively.

```k
  // True to insert, False to remove.

  rule <k> UPDATE A => #HandleAnnotations(A) ... </k>
       <stack> D ~> true ~> S => SetItem(D) S ... </stack>

  rule <k> UPDATE A => #HandleAnnotations(A) ... </k>
       <stack> D ~> false ~> SetItem(D) S => S ... </stack>

  rule <k> UPDATE A => #HandleAnnotations(A) ... </k>
       <stack> (D ~> false => .) ~> S:Set ... </stack>
       requires notBool(D in S)
```

Note that, according to the Michelson documentation, set iteration order is
actually defined (the set is iterated over in ascending order)! For simplicity
we implement this by repeatedly selecting the minimal element.

```k
  syntax Data ::= #MinimalElement(List) [function]
  syntax Data ::= #MinimalElementAux(List, Data) [function]

  rule #MinimalElement(ListItem(H) L) => #MinimalElementAux(L, H)
  rule #MinimalElementAux(.List, M) => M
  rule #MinimalElementAux(ListItem(H) L, M) => #MinimalElementAux(L, M) requires #DoCompare(M, H) <=Int 0
  rule #MinimalElementAux(ListItem(H) L, M) => #MinimalElementAux(L, H) requires #DoCompare(M, H) ==Int 1

  rule <k> ITER A _ => #HandleAnnotations(A) ... </k>
       <stack> .Set => . ... </stack>

  rule <k> ITER A B => #HandleAnnotations(A) ~> B ~> #Push(S -Set SetItem(#MinimalElement(Set2List(S)))) ~> ITER .AnnotationList B ... </k>
       <stack> S => #MinimalElement(Set2List(S)) ... </stack>
       requires size(S) >Int 0
```

Set size is lifts directly into Michelson.

```k
  rule <k> SIZE A => #HandleAnnotations(A) ... </k>
       <stack> S:Set => size(S) ... </stack>
```

Much like Sets, MAP operations lift reasonably easily into K.

```k
  //// Operations on maps
  rule <k> EMPTY_MAP A _ _ => #HandleAnnotations(A) ... </k>
       <stack> . => .Map ... </stack>

  rule <k> GET A => #HandleAnnotations(A) ... </k>
       <stack> X ~> M => Some {M[X]}:>Data ... </stack>
       requires X in_keys(M)

  rule <k> GET A => #HandleAnnotations(A) ... </k>
       <stack> X ~> M => None ... </stack>
       requires notBool(X in_keys(M))

  rule <k> MEM A => #HandleAnnotations(A) ~> . ... </k>
       <stack> X ~> M => X in_keys(M) ... </stack>

  rule <k> UPDATE A => #HandleAnnotations(A)  ... </k>
       <stack> K ~> Some V ~> M:Map => M[K <- V] ... </stack>

  rule <k> UPDATE A => #HandleAnnotations(A)  ... </k>
       <stack> K ~> None ~> M:Map => M[K <- undef] ... </stack>
```

The MAP operation, over maps, is somewhat more involved. We need to set up a
stack without the actual map to execute the block on, and we need to keep track
of the updated map as we do. We implement this by splitting the operation into
multiple K items. `#PerformMap` holds the old map, the new map, and the block to
execute. When it rewrites, it sets up the new stack and queues up a `#PopNewVal`
which removes the value produced by the MAP block and adds it to the second map
argument. Like Sets, iteration order is actually defined, and we implement it by
repeatedly selecting the minimal element in the list of keys in the map.

```k
  syntax Instruction ::= #PerformMap(Map, Map, Block)

  rule <k> MAP A B => #HandleAnnotations(A) ~> #PerformMap(M, .Map, B) ... </k>
       <stack> M => . ... </stack>

  syntax Instruction ::= #PopNewVal(Data)

  rule <k> #PopNewVal(K) ~> #PerformMap(M1, M2, B) => #PerformMap(M1, M2[K <- V], B) ... </k>
       <stack> V => . ... </stack>

  syntax Data ::= #MinimalKey(Map) [function]
  rule #MinimalKey(M) => #MinimalElement(keys_list(M))

  rule <k> #PerformMap(M1, M2, B) => B ~> #PopNewVal(#MinimalKey(M1))
        ~> #PerformMap(M1[#MinimalKey(M1) <- undef], M2, B) ... </k>
       <stack> . => Pair #MinimalKey(M1) {M1[#MinimalKey(M1)]}:>Data ... </stack>
       requires size(M1) >Int 0

  rule <k> #PerformMap(.Map, M, _) => . ... </k>
       <stack> . => M ... </stack>
```

Iter is relatively easy to implement using a straightforward recursive style,
since it does not need to track the new map while keeping it off the stack.

```k
  rule <k> ITER A B => #HandleAnnotations(A)  ... </k>
       <stack> .Map => . ... </stack>

  rule <k> ITER A B => #HandleAnnotations(A) ~> B ~> #Push(M[#MinimalKey(M) <- undef]) ~> ITER .AnnotationList B ... </k>
       <stack> M:Map => Pair #MinimalKey(M) {M[#MinimalKey(M)]}:>Data ... </stack>
       requires size(M) >Int 0
```

SIZE lifts direclty into K.

```k
  rule <k> SIZE A => #HandleAnnotations(A)  ... </k>
       <stack> M:Map => size(M) ... </stack>
```

For the purposes of this semantics, `big_map`s are represented in the same way
as maps, so they can reuse the same execution rules.

```k
  //// Operations on big maps

  rule <k> EMPTY_BIG_MAP A _ _ => #HandleAnnotations(A)  ... </k>
       <stack> . => .Map ... </stack>

  // Same as maps
```

Option operations are relatively straightforward translations of the Michelson
documentation into rewrite rules.

```k
  //// Operations on optional values

  rule <k> SOME A => #HandleAnnotations(A)  ... </k>
       <stack> X => Some X ... </stack>

  rule <k> NONE A _ => #HandleAnnotations(A)  ... </k>
       <stack> . => None ... </stack>

  rule <k> IF_NONE A BT BF => #HandleAnnotations(A) ~> BT ... </k>
       <stack> None => . ... </stack>

  rule <k> IF_NONE A BT BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> Some V => V ... </stack>
```

Sum types are similar to options.

```k
  //// Operations on unions
  rule <k> LEFT A _ => #HandleAnnotations(A)  ... </k>
       <stack> X:Data => Left X ... </stack>

  rule <k> RIGHT A _:Type => #HandleAnnotations(A) ... </k>
       <stack> X:Data => Right X ... </stack>

  rule <k> IF_LEFT A BT BF => #HandleAnnotations(A) ~> BT ... </k>
       <stack> Left V => V ... </stack>

  rule <k> IF_LEFT A BT BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> Right V => V ... </stack>
```

Lists are somewhat nontrivial in that we need to keep track of typing
information and hence we have the `#List` nonterminal. Aside from that, the
rules are a direct translation of the documentation into K.

```k
  //// Operations on lists
  rule <k> CONS A => #HandleAnnotations(A)  ... </k>
       <stack> V ~> L:List => ListItem(V) L ... </stack>

  rule <k> NIL A _ => #HandleAnnotations(A)  ... </k>
       <stack> . => .List ... </stack>

  rule <k> IF_CONS A BT BF => #HandleAnnotations(A) ~> BT ... </k>
       <stack> ListItem(L1) Ls => L1 ~> Ls ... </stack>

  rule <k> IF_CONS A BT BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> .List => . ... </stack>
```

Note that, like Maps, Lists must keep track of the updated list on the fly
during a `MAP` operation. We cannot currently determine the type of the result
list as we do not have a static type system.

```k
  syntax Instruction ::= #PerformMapList(List, List, Block)

  rule <k> MAP A B => #HandleAnnotations(A) ~> #PerformMapList(Ls, .List, B) ... </k>
       <stack> Ls => . ... </stack>
```

The accumulator list in `#PerformMapList` is actually backwards, so we need to
reverse it before placing it back onto the stack.

```k
  syntax List ::= #ReverseList(List) [function]
  syntax List ::= #ReverseListAux(List, List) [function]
  rule #ReverseList(L) => #ReverseListAux(L, .List)
  rule #ReverseListAux(ListItem(L1) Ls, Acc) => #ReverseListAux(Ls, ListItem(L1) Acc)
  rule #ReverseListAux(.List, Acc) => Acc

  rule <k> #PerformMapList(.List, Acc, B) => . ... </k>
       <stack> . => #ReverseList(Acc) ... </stack>
```

As with maps, before we execute a `MAP` block we must add the first element in
the input list to the stack and schedule an `#AddToList` to pop the result off
the stack.

```k
  syntax Instruction ::= #AddToList(List, List, Block)
  rule <k> #PerformMapList(ListItem(L) Ls, Acc, B) => B ~> #AddToList(Ls, Acc, B) ... </k>
       <stack> . => L ... </stack>

  rule <k> #AddToList(Ls, Acc, B) => #PerformMapList(Ls, ListItem(L) Acc, B) ... </k>
       <stack> L => . ... </stack>
```

Size and iter have relatively simple implementations.

```k
  rule <k> SIZE A => #HandleAnnotations(A)  ... </k>
       <stack> L:List => size(L) ... </stack>

  rule <k> ITER A B =>  #HandleAnnotations(A) ~>. ... </k>
       <stack> .List => . ... </stack>

  rule <k> ITER A B => #HandleAnnotations(A) ~> B ~> #Push(Ls) ~> ITER .AnnotationList B ... </k>
       <stack> ListItem(L) Ls => L ... </stack>
```

Timestamps are simply wrapped ints in K-Michelson, so the implementation of
simple arithmetic over them is straightforward. The differing argument types
however forces us to use two rules for each operation.

```k
  // Domain Specific operations
  //// Operations on timestamps
  rule <k> ADD A => . ... </k>
       <stack> #Timestamp(I1) ~> I2 => #Timestamp(I1 +Int I2) ... </stack>

  rule <k> ADD A => . ... </k>
       <stack> I1 ~> #Timestamp(I2) => #Timestamp(I1 +Int I2) ... </stack>

  rule <k> SUB A => . ... </k>
       <stack> #Timestamp(I1) ~> I2 => #Timestamp(I1 -Int I2) ... </stack>

  rule <k> SUB A => . ... </k>
       <stack> #Timestamp(I1) ~> #Timestamp(I2) => I1 -Int I2 ... </stack>

```

Operations instructions mostly simply sanity check their arguments and then
package them into the appropriate operation structure from
michelson-internal-syntax.md. Of interest in the `CREATE_CONTRACT` instructon is
the `!_:Int` syntax, which simply generates a fresh integer that has not been
used by this rule during this execution. This ensures that two different
`CREATE_CONTRACT` executions will produce different addresses.

```k
  rule <k> CREATE_CONTRACT A:AnnotationList { C } => . ... </k>
       <stack> Delegate:OptionData ~> Initial:Mutez ~> Stor:Data => Create_contract(O, C, Delegate, Initial, Stor) ~> #Address("@Address(" +String Int2String(!_:Int) +String ")") ... </stack>
       <nonce> #Nonce(O) => #NextNonce(#Nonce(O)) </nonce>

  rule <k> TRANSFER_TOKENS _ => . ... </k>
       <stack> D ~> M ~> #Contract(A, _) => Transfer_tokens(O, D, M, A) ... </stack>
       <nonce> #Nonce(O) => #NextNonce(#Nonce(O)) </nonce>

  rule <k> SET_DELEGATE A => . ... </k>
       <stack> D => Set_delegate(O, D) ... </stack>
       <nonce> #Nonce(O) => #NextNonce(#Nonce(O)) </nonce>
```

In principle the way an operation's nonce should be generated by the semantics
is not documented in Michelson. The only true requirement is for different
operation producing instruction executions to produce different nonces. Thus,
for the time being we use the simplest possible representation.

```k
  syntax OperationNonce ::= #NextNonce(OperationNonce) [function]
  rule #NextNonce(#Nonce(I)) => #Nonce(I +Int 1)
```

The Balance instruction simply pushes the value stored in the mybalance cell.

```k
  rule <k> BALANCE A => . ... </k>
       <stack> . => B ... </stack>
       <mybalance> B </mybalance>
```

The ADDRESS instruction simply takes the address field of the contract structure
and discards the rest.

```k
  rule <k> ADDRESS Ann => . ... </k>
       <stack> #Contract(A, _) => A ... </stack>
```

We need to perform a type check in order to correctly implement the CONTRACT
instruction since it should return None if a contract with a different parameter
type exists at the given address. This requires us to extract the type from the
stored contract. This is possible without another rule through more advanced K
syntax but using a simple helper function seems more readable.

```k
  syntax Type ::= #TypeFromContractStruct(Data) [function]
  rule #TypeFromContractStruct(#Contract(_, T)) => T

  rule <k> CONTRACT _ T => . ... </k>
       <stack> A => Some {M[A]}:>Data ... </stack>
       <knownaddrs> M </knownaddrs>
       requires A in_keys(M) andBool #TypeFromContractStruct({M[A]}:>Data) ==K T

  rule <k> CONTRACT _ T => . ... </k>
       <stack> A:Address => None ... </stack>
       <knownaddrs> M </knownaddrs> [owise]
```

Like Balance, these instructions simply push the contents of the corresponding
cells.

```k
  rule <k> SOURCE Ann => . ... </k>
       <stack> . => A ... </stack>
       <sourceaddr> A </sourceaddr>

  rule <k> SENDER Ann => . ... </k>
       <stack> . => A ... </stack>
       <senderaddr> A </senderaddr>

  rule <k> SELF Ann => . ... </k>
       <stack> . => #Contract(A, T) ... </stack>
       <paramtype> T </paramtype>
       <myaddr> A </myaddr>

  rule <k> AMOUNT Ann => . ... </k>
       <stack> . => M ... </stack>
       <myamount> M </myamount>
```

`IMPLICIT_ACCOUNT` adds the additional information that the type of any such
account is unit.

```k
  rule <k> IMPLICIT_ACCOUNT Ann => . ... </k>
       <stack> #KeyHash(A) => #Contract(#Address(A), unit .AnnotationList) ... </stack>

  //// Special Operations
  rule <k> CHAIN_ID A => . ... </k>
       <stack> . => C ... </stack>
       <mychainid> C </mychainid>

  rule <k> NOW A => . ... </k>
       <stack> . => N ... </stack>
       <mynow> N </mynow>
```

The bytes instructions have a stubbed implementation for the time being, since
the actual serialization format is unspecified.

```k
  //// Operations on MBytes, stubbed for now because of the lack of a documented bytes format.
  rule <k> PACK A => #HandleAnnotations(A) ... </k>
       <stack> T => #Packed(T) ... </stack>

  rule <k> UNPACK A _ => #HandleAnnotations(A) ... </k>
       <stack> #Packed(T) => Some T ... </stack>
```

The concat operation over two bytes is relatively straightforward since we
already have helper functions to extract bytes content.

```k
  rule <k> CONCAT A => #HandleAnnotations(A) ... </k>
       <stack> B1:Bytes ~> B2:Bytes => B1 +Bytes B2 ... </stack>
```

Concatenating lists of bytes is somewhat more involved, since we need to
distinguish this case from lists of strings.

```k
  syntax Bytes ::= #ConcatBytes(List, Bytes) [function]
  rule #ConcatBytes(.List, A) => A
  rule #ConcatBytes(ListItem(B) DL, A) => #ConcatBytes(DL, A +Bytes B)

  rule <k> CONCAT A => #HandleAnnotations(A) ... </k>
       <stack> L => #ConcatBytes(L, .Bytes) ... </stack>
       <stacktypes> list _ bytes _ ; _ </stacktypes>
```

Size is relatively simple, except that we must remember to divide by two, since
bytes length is measured in terms of number of bytes, not characters in the hex
string.

```k
  rule <k> SIZE A => #HandleAnnotations(A) ... </k>
       <stack> B => lengthBytes(B) ... </stack>
```

The remaining operations are defined in terms of the same operations on strings,
allowing for code reuse.

```k
  syntax OptionData ::= #SliceBytes(Bytes, Int, Int) [function]

  rule #SliceBytes(S, O, L) => Some substrBytes(S, O, O +Int L)
  requires O >=Int 0 andBool L >=Int 0 andBool O <Int lengthBytes(S) andBool (O +Int L) <=Int lengthBytes(S)

  rule #SliceBytes(S, O, L) => None [owise]

  rule <k> SLICE A => #HandleAnnotations(A) ... </k>
       <stack> O:Int ~> L:Int ~> B:Bytes => #SliceBytes(B, O, L)  ... </stack>
```

The cryptographic operations are simply stubbed for now.

```k
  //// Cryptographic primitives

  syntax String ::= #Blake2BKeyHash(String) [function] // TODO: Blake2B crypto hook.
  rule #Blake2BKeyHash(S) => S

  rule <k> HASH_KEY A => #HandleAnnotations(A) ... </k>
       <stack> #Key(S) => #KeyHash(#Blake2BKeyHash(S)) ... </stack>

  rule <k> BLAKE2B A => #HandleAnnotations(A) ... </k>
       <stack> B:MBytes => #Blake2B(B) ... </stack>

  rule <k> SHA256 A => #HandleAnnotations(A) ... </k>
       <stack> B:MBytes => #SHA256(B) ... </stack>

  rule <k> SHA512 A => #HandleAnnotations(A) ... </k>
       <stack> B:MBytes => #SHA512(B) ... </stack>

  syntax MBytes ::= #SignedMBytes(Key, Signature, MBytes)

/*  rule <k> CHECK_SIGNATURE A => #HandleAnnotations(A) ... </k>
       <stack> #Key(K) ~> #Signature(S) ~> #SignedMBytes(#Key(K), #Signature(S), _) => true ... </stack>

  rule <k> CHECK_SIGNATURE A => #HandleAnnotations(A) ... </k>
       <stack> #Key(_) ~> #Signature(_) ~> _:MBytes => false ... </stack> [owise] // TODO: Bug - The haskell backend does not support distinguishing these rules.*/
```

Mutez operations need to check their results since Mutez is not an unlimited
precision type. This internal instruction checks and produces the appropriate error case if the
value is invalid.

```k
  //// Operations on Mutez
  syntax Instruction ::= #ValidateMutezAndPush(Mutez, Int, Int)

  syntax FailedStack ::= #FailureFromMutezValue(Mutez, Int, Int) [function]
  rule #FailureFromMutezValue(#Mutez(I), I1, I2) => ( MutezOverflow I1 I2 ) requires I >=Int #MutezOverflowLimit
  rule #FailureFromMutezValue(#Mutez(I), I1, I2) => ( MutezUnderflow I1 I2 ) requires I <Int 0


  rule <k> #ValidateMutezAndPush(#Mutez(I), _, _) => . ... </k>
       <stack> . => #Mutez(I) ... </stack>
       requires #IsLegalMutezValue(I)

  rule <k> #ValidateMutezAndPush(#Mutez(I), I1, I2) ~> Rk => Aborted("Mutez out of bounds", I, Rk, Rs) ~> Rk </k>
       <stack> Rs => #FailureFromMutezValue(#Mutez(I), I1, I2) </stack>
       requires notBool #IsLegalMutezValue(I)
```

Other than the mutez validation step, these arithmetic rules are essentially
identical to those defined over integers.

```k
  rule <k> ADD A => #ValidateMutezAndPush(#Mutez(I1 +Int I2), I1, I2) ~> #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> #Mutez(I2) => . ... </stack>

  rule <k> SUB A => #ValidateMutezAndPush(#Mutez(I1 -Int I2), I1, I2) ~> #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> #Mutez(I2) => . ... </stack>

  rule <k> MUL A => #ValidateMutezAndPush(#Mutez(I1 *Int I2), I1, I2) ~> #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> I2 => . ... </stack>

  rule <k> MUL A => #ValidateMutezAndPush(#Mutez(I1 *Int I2), I1, I2) ~> #HandleAnnotations(A) ... </k>
       <stack> I1 ~> #Mutez(I2) => . ... </stack>

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> #Mutez(0) => None ... </stack>

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> 0 => None ... </stack>

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> #Mutez(I2) => Some (Pair (I1 /Int I2) #Mutez(I1 %Int I2)) ... </stack>
       requires I2 >Int 0

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> I2 => Some (Pair #Mutez(I1 /Int I2) #Mutez(I1 %Int I2)) </stack>
       requires I2 >Int 0

```

We introduce several pseudo-instructions that are used for debugging:

-   `TRACE` appends its string content to the `<trace>` cell as a debugging aid
    for complex programs.
-   `STOP` is an instruction that cannot be evaluated and causes the program to
    get stuck
-   `PAUSE` non-determinstically chooses to either do nothing or else `STOP`;
    it optionally traces at its pause point.

```k
  rule <k> TRACE(S) => .K ... </k>
       <trace> K:K => (K ~> S) </trace>

  rule <k> PAUSE    => .K                ... </k>
  rule <k> PAUSE    => STOP              ... </k>
  rule <k> PAUSE(S) => TRACE(S) ~> PAUSE ... </k>
```

When the `<k>` cell is empty, we consider execution successful

```k
  rule <k> . </k>
       <returncode> 1 => 0 </returncode>
```

```k
endmodule
```

Unit Test Semantics
===================

We implement the unit test section of the .tzt format described by the
Tezos foundation
[here](https://gitlab.com/tezos/tezos/-/merge_requests/1487/diffs). This file
implements the behavior of the 'code,' 'input,' and 'output' applications
discussed in that document.

```k
module UNIT-TEST-DRIVER
  imports UNIT-TEST
  imports SYMBOLIC-UNIT-TEST-SYNTAX

  rule <k> #Init
        => #CreateSymbols
        ~> #BaseInit
        ~> #ExecutePreConditions
        ~> #TypeCheck
        ~> #LoadInputStack
        ~> #ExecuteScript
        ~> #ExecutePostConditions
           ...
       </k>
endmodule
```

```k
module UNIT-TEST
  imports SYMBOLIC-UNIT-TEST-COMMON-SYNTAX
  imports MICHELSON
  imports MICHELSON-TYPES
  imports MATCHER
```

`#CreateSymbol`
--------------

```k
  syntax Type ::= "#UnknownType"

  syntax KItem ::= SymbolicElement

  syntax SymbolicElement ::= #SymbolicElement(SymbolicData, Type)

  syntax Set ::= #FindSymbolsBL(BlockList) [function, functional]
  rule #FindSymbolsBL(.BlockList) => .Set
  rule #FindSymbolsBL(B:Block ; Rs:BlockList) => #FindSymbolsB(B) #FindSymbolsBL(Rs)

  syntax Set ::= #FindSymbolsB(Block) [function, functional]
  rule #FindSymbolsB({ }) => .Set
  rule #FindSymbolsB({ I:Instruction }) => #FindSymbolsI(I)
  rule #FindSymbolsB({ I:Instruction ; Is:DataList }:Block) => #FindSymbolsI(I) |Set #FindSymbolsB({ Is })

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
  rule #FindSymbolsIn(S:SymbolicData, T) => SetItem(#SymbolicElement(S, T)) // ???
  rule #FindSymbolsIn(Pair V1 V2, pair _ T1 T2) => #FindSymbolsIn(V1, T1) |Set #FindSymbolsIn(V2, T2)
  rule #FindSymbolsIn(Some V, option _ T) => #FindSymbolsIn(V, T)
  rule #FindSymbolsIn(Left V, or _ T _) => #FindSymbolsIn(V, T)
  rule #FindSymbolsIn(Right V, or _ _ T) => #FindSymbolsIn(V, T)
  rule #FindSymbolsIn(B:Block, lambda _ _ _) => #FindSymbolsB(B)

  rule #FindSymbolsIn({ }, list _ _) => .Set
  rule #FindSymbolsIn({ D:Data }, list _ T) => #FindSymbolsIn(D, T)
  rule #FindSymbolsIn({ D:Data ; DL }, list _ T) => #FindSymbolsIn(D, T) |Set #FindSymbolsIn({ DL }, T)

  rule #FindSymbolsIn({ }, set _ _) => .Set
  rule #FindSymbolsIn({ D:Data }, set _ T) => #FindSymbolsIn(D, T)
  rule #FindSymbolsIn({ D:Data ; DL }, set _ T) => #FindSymbolsIn(D, T) |Set #FindSymbolsIn({ DL }, T)

  rule #FindSymbolsIn({ }, map _ _ _) => .Set
  rule #FindSymbolsIn({ Elt K V }, map _ KT VT) => #FindSymbolsIn(K, KT) |Set #FindSymbolsIn(V, VT)
  rule #FindSymbolsIn({ M:MapEntry ; ML:MapEntryList }, (map _ _ _) #as MT) =>
       #FindSymbolsIn({ M }, MT) |Set #FindSymbolsIn({ ML }, MT)

  rule #FindSymbolsIn(M:MapLiteral, big_map A KT VT) => #FindSymbolsIn(M, map A KT VT)

  rule #FindSymbolsIn(_, _) => .Set [owise]
```

```k
  syntax Bool ::= #AllTypesKnown(Set) [function, functional]
  rule #AllTypesKnown(SetItem(#SymbolicElement(_, #UnknownType)) _) => false
  rule #AllTypesKnown(_) => true [owise]

  syntax UnificationFailure ::= "#UnificationFailure"

  syntax UnifiedSet ::= Set | UnificationFailure

  syntax UnifiedSet ::= #UnifyTypes(Set) [function, functional]

  rule #UnifyTypes(SetItem(#SymbolicElement(S, #UnknownType)) SetItem(#SymbolicElement(S, T)) Ss) => #UnifyTypes(SetItem(#SymbolicElement(S, T)) Ss)

  rule #UnifyTypes(SetItem(#SymbolicElement(S, T1)) SetItem(#SymbolicElement(S, T2)) _) => #UnificationFailure
       requires T1 =/=K T2 andBool T1 =/=K #UnknownType andBool T2 =/=K #UnknownType

  rule #UnifyTypes(S) => S requires #AllTypesKnown(S) [owise]
  rule #UnifyTypes(S) => #UnificationFailure requires notBool(#AllTypesKnown(S)) [owise]

  syntax UnifiedList ::= List | UnificationFailure
  syntax UnifiedList ::= #UnifiedSetToList(UnifiedSet) [function, functional]

  rule #UnifiedSetToList(S:Set) => Set2List(S)
  rule #UnifiedSetToList(#UnificationFailure) => #UnificationFailure
```

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
  rule <k> (V:SimpleData ~> #CreateSymbol(N, T)) => . ... </k>
       <symbols> M => M[N <- #TypedSymbol(T, V)] </symbols>
```

The representation of \#Any is the same in the semantics and the concrete
syntax.

```k
  rule #MichelineToNative(#Any, _, _, _) => #Any
  rule #TypeData(_, #Any, T) => #Typed(#Any, T)
```


This function transforms a LiteralStack (e.g. a sequence of `Stack_elt`
productions) into a KSequence (the same format as the execution stack).

```k
  syntax K ::= #LiteralStackToSemantics(LiteralStack, Map, Map) [function]
  rule #LiteralStackToSemantics({ .StackElementList }, _KnownAddrs, _BigMaps) => .
  rule #LiteralStackToSemantics({ Stack_elt T D ; Gs:StackElementList }, KnownAddrs, BigMaps)
    => #MichelineToNative(D, T, KnownAddrs, BigMaps)
    ~> #LiteralStackToSemantics({ Gs }, KnownAddrs, BigMaps)
```

This function transforms an expected output stack to its internal representation
(failed stacks are already in their internal representation, literals must be
transformed as in the input group).

```k
  syntax K ::= #OutputStackToSemantics(OutputStack, Map, Map) [function]
  rule #OutputStackToSemantics(L, KnownAddrs, BigMaps)
    => #LiteralStackToSemantics(L, KnownAddrs, BigMaps)
  rule #OutputStackToSemantics(X:FailedStack, _, _) => X
```

Loading the input or expected output stack involves simply converting it to a
KSeq whose elements are Data in their internal representations, and then
placing that KSeq in the main execution stack configuration cell.

```k
  rule <k> input LS => .K ... </k>
       <inputstack> .K => LS </inputstack>

  rule <k> output Os => .K ... </k>
       <expected> .K => Os </expected>
```

```k
  syntax KItem ::= "#LoadInputStack"
  rule <k> #LoadInputStack => .K ... </k>
       <stack> _ => #LiteralStackToSemantics(Actual, KnownAddrs, BigMaps) </stack>
       <stacktypes> _ => #LiteralStackToTypes(Actual,PT) </stacktypes>
       <inputstack> Actual </inputstack>
       <paramtype> PT </paramtype>
       <knownaddrs> KnownAddrs </knownaddrs>
       <bigmaps> BigMaps </bigmaps>
```

As in the case of the contract group, loading the code group is trivial --
simply extract the block and let the main semantics handle the rest.

```k
  rule <k> code C => .K ... </k>
       <script> #NoData => C </script>
```

Type Checking Extension
-----------------------

For type-checking purposes, given an input or expected output stack, we need to
know what types are on the stack.

```k
  syntax TypeSeq ::= #LiteralStackToTypes(LiteralStack, Type) [function]
  rule #LiteralStackToTypes( { .StackElementList }, _) => .TypeSeq
  rule #LiteralStackToTypes( { Stack_elt T D ; Gs:StackElementList }, PT)
    => T ; #LiteralStackToTypes({ Gs }, PT)
    requires #Typed(D, T) :=K #TypeData(PT, D, T)
  rule #LiteralStackToTypes({ Stack_elt T _:SymbolicData ; Gs:StackElementList }, PT)
    => T ; #LiteralStackToTypes({ Gs }, PT)
```

### `#TypeCheck` function

Executing Michelson code without type information leads to non-determinism.
For example, the `CONCAT` instruction, when applied to an empty list, produces
either an empty `string` or empty `bytes`. Without knowing the type of the list,
the resulting type of value is unknown.

The K-Michelson semantics was originally written without a type system/checker.
Later, a type system was added to resolve various issues, including the one
mentioned above.

The result of type-checking a block of code produces an equivalent block where
each instruction has been wrapped in its corresponding type. These types are
unwrapped and stored in a fresh configuration cell `<stacktypes>` during
execution. This allows the oringal "type-free" semantics can be used for all
unambiguous cases while any type-dependent instructions can reference the
`<stacktypes>` cell to determine which execution path is needed.

To correctly check the typing of a unit test, we need the following info:

1. the contract parameter type --- only used in typing the `SELF` instruction
2. the input stack types --- which depend on (1) because `lambda`
3. the output stack types --- which depend on (1) for the same reason
4. a Michelson script

The `#TypeCheck` takes parameters 1-4, performs the type-check, and then
replaces the code in the script cell with typed version.

TODO: `#TypeCheck` currently is a no-op when the expected output stack is
a failed stack --- but this means that we cannot execute tests fully when we
expect failure. See note below.

TODO: Consider best way to introduce type-checks to pre/post conditions

```k
  syntax KItem ::= #TypeCheck(Block, Type, LiteralStack, OutputStack)
  syntax KItem ::= #TypeCheckAux(LiteralStack, LiteralStack, TypeSeq, TypedInstruction)

  rule <k> #TypeCheck(B, P, IS, OS:LiteralStack)
        => #TypeCheckAux(
             IS,
             OS,
             #LiteralStackToTypes(OS, P),
             #TypeInstruction(P, B, #LiteralStackToTypes(IS,P))
           )
           ...
       </k>

  // TODO: Implement a "partial" type check case
  rule <k> #TypeCheck(B, _P, _IS, _OS:FailedStack) => . ... </k>
       <script> B </script>

  rule <k> #TypeCheckAux(_IS, _OS, OSTypes, #TI(B, ISTypes -> OSTypes))
        => .
           ...
       </k>
       <script> _ => { #Exec(#TI(B, ISTypes -> OSTypes)) } </script>
```

This directive supplies all of the arguments to the `#TypeCheck` rule.

```k
  syntax KItem ::= "#TypeCheck"
  rule <k> #TypeCheck
        => #TypeCheck(B,PT,IS,OS)
        ...
       </k>
       <script> B </script>
       <paramtype> PT </paramtype>
       <inputstack> IS </inputstack>
       <expected> OS </expected>
```

`#Assume`/`#Assert` instructions
--------------------------------

```k
  syntax Instruction ::= "ASSERT" "{" BlockList "}"
                       | "ASSUME" "{" BlockList "}"
```

```k
  rule <k> ASSERT { .BlockList } => .K ... </k>
  rule <k> ASSERT { B; Bs }
        => B ~> #AssertTrue ~> ASSERT { Bs } ~> #RestoreStack(Stack)
           ...
       </k>
       <stack> Stack => .K </stack>
```

```k
  rule <k> ASSUME { .BlockList } => .K ... </k>
  rule <k> ASSUME { B; Bs }
        => B ~> #AssumeTrue ~> ASSUME { Bs } ~> #RestoreStack(Stack)
           ...
       </k>
       <stack> Stack => .K </stack>
```

```k
  syntax Instruction ::= #RestoreStack(K)
  rule <k> #RestoreStack(Stack) => .K ... </k>
       <stack> _ => Stack </stack>
```

```k
  syntax Instruction ::= "#AssertTrue"
  rule <k> #AssertTrue => #Assert(B) ... </k>
       <stack> B:Bool => . </stack>
```

```k
  syntax Instruction ::= "#AssumeTrue"
  rule <k> #AssumeTrue => #Assume(B) ... </k>
       <stack> B:Bool => . </stack>
```

```k
  syntax KItem ::= #Assert(BoolExp) [strict, result(Data)]
  rule <k> #Assert(true)  => .             ... </k>
  rule <k> #Assert(false) => #AssertFailed ... </k>
  syntax KItem ::= "#AssertFailed" [klabel(#AssertFailed), symbol]
```

```k
  syntax KItem ::= #Assume(BoolExp) [strict, result(Data)]
  rule <k> #Assume(true)  => .             ... </k>
  rule <k> #Assume(false) ~> _:K => . </k>
       <assumeFailed> _ => true </assumeFailed> [transition]

  syntax BoolExp ::= Bool
                   | Data "==" Data [seqstrict]
  rule <k> D1:Data == D2:Data => D1 ==K D2 ... </k>
```

`precondition` Groups
---------------------

```k
  rule <k> precondition { Bs } => .K ... </k>
       <pre> .BlockList => Bs </pre>
```

```k
  syntax KItem ::= "#ExecutePreConditions"
  rule <k> #ExecutePreConditions => ASSUME { Preconditions } ... </k>
       <pre> Preconditions </pre>
```

`postcondition` group
---------------------

```k
  rule <k> postcondition { Bs } => .K ... </k>
       <post> .BlockList => Bs </post>
```

```k
  syntax KItem ::= "#ExecutePostConditions"
  rule <k> #ExecutePostConditions
        => BIND Expected { ASSERT { Postconditions } }
           ...
       </k>
       <expected> Expected </expected>
       <post> Postconditions </post>
```

`invariants` group
---------------------

```k
  rule <k> invariant Annot { Stack } { Blocks } => . ... </k>
       <invs> .Map
           => (Annot |-> { Stack } { Blocks })
              ...
       </invs>
```

We need stack concatentation for invariant preprocessing.
Note that `#AnyStack` on the lefthand side is currently unhandled.

```k
  syntax StackElementList ::= StackElementList "++StackElementList" StackElementList [function, left, avoid]
  rule .StackElementList ++StackElementList S2 => S2
  rule (E1 ; S1)         ++StackElementList S2 => E1 ; (S1 ++StackElementList S2)
```

```symbolic
  syntax Instruction ::= CUTPOINT( id: Int, invariant: Invariant)
  rule <k> LOOP A .AnnotationList Body
        => CUTPOINT(!Id, Invariant) ;
           LOOP .AnnotationList {
             Body ;
             CUTPOINT(!Id, Invariant)
           }
           ...
       </k>
       <invs> A |-> Invariant ... </invs>
```

### `CUTPOINT`s and stack generalization

A cutpoint is a semantic construct that internalizes the notion of a
reachability logic circularity (or claim).
When we reach a cutpoint, we need to generalize our current state into one which
corresponds to the reachability logic circularity that we wish to use.

```symbolic
  rule <k> CUTPOINT(I, { Shape } { Predicates })
        => BIND { Shape } { ASSERT { Predicates }}
        ~> #GeneralizeStack(Shape, .K)
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
  syntax KItem ::= #GeneralizeStack(StackElementList, K)
  rule <k> #GeneralizeStack(.StackElementList, Stack) => . ... </k>
       <stack> .K => Stack </stack>

  rule <k> #GeneralizeStack(Stack_elt T D ; Stack, KSeq:K)
        => #GeneralizeStack(Stack, KSeq ~> D)
           ...
       </k>
       <stack> _:Data => . ... </stack>
    requires notBool isSymbolicData(D)

  rule <k> (.K => #MakeFresh(T))
        ~> #GeneralizeStack(Stack_elt T D:SymbolicData ; Stack, KSeq)
           ...
       </k>

  rule <k> ( V:SimpleData
          ~> #GeneralizeStack(Stack_elt T D:SymbolicData ; Stack, KSeq)
           )
        =>   #GeneralizeStack(Stack_elt T V ; Stack, KSeq)
           ...
       </k>
```

Here `#MakeFresh` is responsible for generating a fresh value of a given type.

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

  rule <k> #MakeFresh(list      _:AnnotationList _:Type)        => ?_:List                          ... </k>
  rule <k> #MakeFresh(set       _:AnnotationList _:Type)        => ?_:Set                           ... </k>
  rule <k> #MakeFresh(map       _:AnnotationList _:Type _:Type) => ?_:Map                           ... </k>
  rule <k> #MakeFresh(big_map   _:AnnotationList _:Type _:Type) => ?_:Map                           ... </k>
  rule <k> #MakeFresh(lambda    _:AnnotationList T1 T2)         => #Lambda(T1,T2,?_:Block)          ... </k>
  rule <k> #MakeFresh(contract  _:AnnotationList T)             => #Contract(#Address(?_:String),T) ... </k>

  rule <k> #MakeFresh(pair _:AnnotationList T1 T2)
        => (Pair #MakeFresh(T1) #MakeFresh(T2))
           ...
       </k>

  rule <k> #MakeFresh(option _:AnnotationList T) => None               ... </k>
  rule <k> #MakeFresh(option _:AnnotationList T) => Some #MakeFresh(T) ... </k>

  rule <k> #MakeFresh(or _:AnnotationList T1 T2) => Left  #MakeFresh(T1) ... </k>
  rule <k> #MakeFresh(or _:AnnotationList T1 T2) => Right #MakeFresh(T2) ... </k>
```

Handle `Aborted`
----------------

```k
  syntax TypedSymbol ::= #TypedSymbol(Type, Data)
```

If a program aborts due to the FAILWITH instruction, we throw away the abortion debug info:

```k
  rule <k> (Aborted(_, _, _, _) => .K) ~> #ExecutePostConditions ... </k>
```

The `BIND` instruction
----------------------

```k
  syntax Instruction ::= "BIND" OutputStack Block
  rule <k> BIND Shape Block
        => #Bind(Shape, Stack)
        ~> Block
        ~> #RestoreSymbols(Symbols)
           ...
       </k>
       <symbols> Symbols </symbols>
       <stack> Stack </stack>
```

```k
  syntax KItem ::= #Bind(OutputStack, K)

  rule <k> #Bind({ .StackElementList }, .K) => .K ... </k>

  rule <k> #Bind(S1:FailedStack, S2:FailedStack) => .K ... </k>
    requires #Matches(S1, S2)

  rule <k> #Bind( { Stack_elt T S:SymbolicData ; Ss } => { Ss }
                , ( (D ~> K:K)                        => K )
                )
           ...
       </k>
       <paramtype> PT </paramtype>
       <symbols> .Map => S |-> #TypedSymbol(T, D) ... </symbols>

  rule <k> #Bind( { Stack_elt T S:SymbolicData ; Ss } => { Ss }
                , ( (D ~> K:K)                        => K )
                )
           ...
       </k>
       <paramtype> PT </paramtype>
       <symbols> S |-> #TypedSymbol(T, D) ... </symbols>

  rule <k> #Bind( { Stack_elt T ED ; Ss } => { Ss }
                , ( (AD ~> K:K)             => K )
                )
           ...
       </k>
       <knownaddrs> KnownAddrs </knownaddrs>
       <bigmaps> BigMaps </bigmaps>
       <stack> AD => .K ... </stack>
    requires #Matches(#MichelineToNative(ED,T,KnownAddrs,BigMaps),AD)
     andBool notBool isSymbolicData(ED)
```

```k
  syntax KItem ::= #RestoreSymbols(Map)
  rule <k> #RestoreSymbols(Symbols) => .K ... </k>
       <symbols> _ => Symbols </symbols>
```

Extending functions to `SymbolicData`
-------------------------------------

```symbolic
  rule [[ #MichelineToNative(S:SymbolicData, T, _, _) => D ]]
       <symbols> S |-> #TypedSymbol(T, D) ... </symbols>

  rule [[ #MichelineToNative(S:SymbolicData, T, _, _) => S ]]
       <symbols> Syms:Map </symbols>
    requires notBool (S in_keys(Syms))

  rule [[ #TypeData(_, S:SymbolicData, T) => #Typed(S, T) ]]
       <symbols> ... S |-> #TypedSymbol(T, _) ... </symbols>
```

```k
endmodule
```

This function implements a relaxed equality check between two data elements. In
particular, it handles the wildcard matching behavior described in the .tzt
format proposal and discards list type information as discussed earlier.

```k
module MATCHER
  imports MICHELSON-COMMON
  imports UNIT-TEST-COMMON-SYNTAX

  syntax Bool ::= #Matches(Data, Data) [function] // Expected, Actual

  rule #Matches(#Any, _) => true

  rule #Matches(D1, D2) => D1 ==K D2 [owise]
  // This also covers any structurally different data. (e.g. (Left 1) vs (Right 1))

  rule #Matches(.List, .List) => true
  rule #Matches(ListItem(L1) Ls1:List, ListItem(L2) Ls2:List) => #Matches(L1, L2) andBool #Matches(Ls1, Ls2)

  rule #Matches(.Set, .Set) => true
  rule #Matches(SetItem(S1) Ss1, SetItem(S2) Ss2) => #Matches(S1, S2) andBool #Matches(Ss1, Ss2)

  rule #Matches(.Map, .Map) => true
  rule #Matches((K |-> V1) M1, (K |-> V2) M2) => #Matches(V1, V2) andBool #Matches(M1, M2)

  syntax Data ::= FailedStack

  rule #Matches(Create_contract(I1, C, O1, M1, D1), Create_contract(I2, C, O2, M2, D2)) =>
    #Matches(I1, I2) andBool
    #Matches(O1, O2) andBool
    #Matches(M1, M2) andBool
    #Matches(D1, D2)

  rule #Matches(Transfer_tokens(I1, D1, M1, A1), Transfer_tokens(I2, D2, M2, A2)) =>
    #Matches(I1, I2) andBool
    #Matches(D1, D2) andBool
    #Matches(M1, M2) andBool
    #Matches(A1, A2)

  rule #Matches(Set_delegate(I1, O1), Set_delegate(I2, O2)) =>
    #Matches(I1, I2) andBool #Matches(O1, O2)

  rule #Matches(Pair L1 R1, Pair L2 R2) => #Matches(L1, L2) andBool #Matches(R1, R2)

  rule #Matches(Some D1, Some D2) => #Matches(D1, D2)

  rule #Matches(Left D1, Left D2) => #Matches(D1, D2)
  rule #Matches(Right D1, Right D2) => #Matches(D1, D2)

endmodule
```
