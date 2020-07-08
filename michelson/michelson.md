This is the main execution semantics file for Michelson. It contains the rewrite
rule semantics for the all Michelson instructions, as well as logic for
transforming values from their Micheline representations to K internal
representations. It also contains the .tzt file loading and contract
initialization logic.

```k
requires "michelson/syntax.md"
requires "michelson/configuration.md"
requires "michelson/internal-syntax.md"
requires "michelson/types.md"

module MICHELSON
  imports MICHELSON-SYNTAX
  imports MICHELSON-CONFIG
  imports MICHELSON-INTERNAL-SYNTAX
  imports DOMAINS
  imports BYTES
```

Michelson Semantics Initialization
==================================

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
  rule <k> other_contracts M => .K ... </k>
       <knownaddrs> .Map => #OtherContractsMapToKMap(M) </knownaddrs>
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
  rule <k> big_maps M => .K ... </k>
       <bigmaps> .Map => #BigMapsToKMap(M) </bigmaps>

  syntax Map ::= #BigMapsToKMap(BigMapMap) [function]
  syntax Map ::= #BigMapsEntryListToKMap(BigMapEntryList) [function]
  syntax Map ::= #BigMapsEntryToKMap(BigMapEntry) [function]

  rule #BigMapsToKMap({ }) => .Map
  rule #BigMapsToKMap({ EL }) => #BigMapsEntryListToKMap(EL)

  rule #BigMapsEntryListToKMap(E) => #BigMapsEntryToKMap(E)
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

  rule #DoCompare(S1:String, S2:String) => -1 requires S1 <String S2
  rule #DoCompare(S1:String, S2:String) => 0 requires S1 ==String S2
  rule #DoCompare(S1:String, S2:String) => 1 requires S1 >String S2

  rule #DoCompare((Pair A1 A2), (Pair B1 B2)) => -1                 requires #DoCompare(A1, B1) ==Int -1
  rule #DoCompare((Pair A1 A2), (Pair B1 B2)) => #DoCompare(A2, B2) requires #DoCompare(A1, B1) ==Int 0
  rule #DoCompare((Pair A1 A2), (Pair B1 B2)) => 1                  requires #DoCompare(A1, B1) ==Int 1

  rule <k> COMPARE A => #HandleAnnotations(A) ... </k>
       <stack> V1 ~> V2 => #DoCompare(V1, V2) ... </stack>
```

TODO: If we define `DoCompare` as a macro for `#ite` we can avoid this.

```symbolic
  rule #DoCompare(B1:Bool, B2:Bool) ==Int 0 => B1 ==Bool B2 [simplification]

  rule #DoCompare(I1:Int, I2:Int) <Int 0 => I1 <Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) <=Int 0 => I1 <=Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) ==Int 0 => I1 ==Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) >=Int 0 => I1 >=Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) >Int 0 => I1 >Int I2 [simplification]
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

  rule <k> IF_RIGHT A BT BF => #HandleAnnotations(A) ~> BT ... </k>
       <stack> Right V => V ... </stack>

  rule <k> IF_RIGHT A BT BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> Left V => V ... </stack>
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

  rule #DoCompare(#Timestamp(I1), #Timestamp(I2)) => #DoCompare(I1, I2)
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

  rule #DoCompare(B1:Bytes, B2:Bytes) => #DoCompare(Bytes2Int(B1, BE, Unsigned), Bytes2Int(B2, BE, Unsigned))
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

  rule #DoCompare(#KeyHash(S1), #KeyHash(S2)) => #DoCompare(S1, S2)
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

  rule #DoCompare(#Mutez(I1), #Mutez(I2)) => #DoCompare(I1, I2)
```

When the `<k>` cell is empty, we consider execution successful

```k
  rule <k> . </k>
       <returncode> 1 => 0 </returncode>
```

```k
endmodule
```
