This file implements the unit test section of the .tzt format described by the
Tezos foundation
[here](https://gitlab.com/tezos/tezos/-/merge_requests/1487/diffs). This file
implements the behavior of the 'code,' 'input,' and 'output' applications
discussed in that document.

```k
requires "michelson/michelson.md"
requires "michelson/types.md"
requires "unit-test/syntax.md"
```

The unit-test semantics does not need any processing in addition to the base initialization.

```k
module UNIT-TEST-DRIVER
  imports UNIT-TEST

  rule <k> #Init
        => #CreateSymbols
        ~> #BaseInit
        ~> #ExecutePreConditions
        ~> #TypeCheck
        ~> #LoadInputStack
        ~> #ExecuteScript
        ~> #CheckOutput
        ~> #ExecutePostConditions
           ...
       </k>
endmodule
```

```k
module UNIT-TEST
  imports SYMBOLIC-UNIT-TEST-SYNTAX
  imports MICHELSON
  imports MICHELSON-TYPES
  imports MATCHER
```

`#CreateSymbol`
--------------

```k
  syntax Set ::= Set "|Set" Set [function, functional, left]
  rule S1 |Set S2 => S1 (S2 -Set S1)

  syntax Type ::= "#UnknownType"

  syntax KItem ::= SymbolicElement

  syntax SymbolicElement ::= #SymbolicElement(SymbolicData, Type)
  syntax SymbolicElement ::= "#DummyElement"

  syntax Set ::= #FindSymbolsIn(Data, Type) [function, functional]
  syntax Set ::= #FindSymbols(KItem) [function, functional]

  rule #FindSymbols({ B:BlockList }) => #FindSymbols(B)

  rule #FindSymbols(B:Block ; Rs:BlockList) => #FindSymbols(B) #FindSymbols(Rs)

  rule #FindSymbols({ }) => .Set
  rule #FindSymbols( { I:Instruction }) => #FindSymbols(I)
  rule #FindSymbols({ I:Instruction ; Is:DataList }) => #FindSymbols(I) |Set #FindSymbols(Is)

  rule #FindSymbols(PUSH _ T D) => #FindSymbolsIn(D, T)


  rule #FindSymbols( ( Failed S:SymbolicData ) ) => SetItem(#SymbolicElement(S, #UnknownType))

  rule #FindSymbols( { S:StackElementList } ) => #FindSymbols(S)
  rule #FindSymbols( S:StackElement ; Ss:StackElementList) => #FindSymbols(S) |Set #FindSymbols(Ss)

  rule #FindSymbols( Stack_elt T D ) => #FindSymbolsIn(D, T)

  rule #FindSymbols(_) => .Set [owise]

  rule #FindSymbolsIn(S:SymbolicData, T) => SetItem(#SymbolicElement(S, T)) // ???

  rule #FindSymbolsIn(Pair V1 V2, pair _ T1 T2) => #FindSymbolsIn(V1, T1) |Set #FindSymbolsIn(V2, T2)
  rule #FindSymbolsIn(Some V, option _ T) => #FindSymbolsIn(V, T)
  rule #FindSymbolsIn(Left V, or _ T _) => #FindSymbolsIn(V, T)
  rule #FindSymbolsIn(Right V, or _ _ T) => #FindSymbolsIn(V, T)

  rule #FindSymbolsIn(B:Block, lambda _ _ _) => #FindSymbols(B)

  rule #FindSymbolsIn({ }, list _ _) => .Set
  rule #FindSymbolsIn({ D:Data }, list _ T) => #FindSymbolsIn(D, T)
  rule #FindSymbolsIn({ D:Data ; DL }, list _ T) => #FindSymbolsIn(D, T) |Set #FindSymbolsIn({ DL }, T)

  rule #FindSymbolsIn({ }, set _ _) => .Set
  rule #FindSymbolsIn({ D:Data }, set _ T) => #FindSymbolsIn(D, T)
  rule #FindSymbolsIn({ D:Data ; DL }, set _ T) => #FindSymbolsIn(D, T) |Set #FindSymbolsIn({ DL }, T)

  rule #FindSymbolsIn({ }, map _ _ _) => .Set
  rule #FindSymbolsIn({ Elt K V }, map _ KT VT) => #FindSymbolsIn(K, KT) |Set #FindSymbolsIn(V, VT)
  rule #FindSymbolsIn({ M:MapEntry ; ML:MapEntryList }, (map _ K V) #as MT) =>
       #FindSymbolsIn({ M }, MT) |Set #FindSymbolsIn({ ML }, MT)

  rule #FindSymbolsIn(M:MapLiteral, big_map A KT VT) => #FindSymbolsIn(M, map A KT VT)

  rule #FindSymbolsIn(_, _) => .Set [owise]

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
  rule <k> #CreateSymbols
        => #CreateSymbols(#UnifiedSetToList(#UnifyTypes( #FindSymbols(Stack)
                                                    |Set #FindSymbols(Pre)
                                                    |Set #FindSymbols(Script)
                         )                )           )
           ...
       </k>
       <inputstack> Stack </inputstack>
       <pre> Pre </pre>
       <script> Script </script>
```

```k
  syntax KItem ::= #CreateSymbols(UnifiedList)
  rule <k> #CreateSymbols(.List) => . ... </k>
  rule <k> #CreateSymbols(ListItem(#SymbolicElement(D, T)) S)
        => #CreateSymbol(D, T)
        ~> #CreateSymbols(S)
           ...
       </k>
```

```k
  syntax KItem ::= #CreateSymbol(SymbolicData, Type)
```

```symbolic
  rule <k> #CreateSymbol(N, (nat _) #as T) => . ... </k>
       <symbols> M => M[N <- #TypedSymbol(T, ?V:Int)] </symbols>
    ensures ?V >=Int 0

  rule <k> #CreateSymbol(N, (int _) #as T) => . ... </k>
       <symbols> M => M[N <- #TypedSymbol(T, ?V:Int)] </symbols>

  rule <k> #CreateSymbol(N, (string _) #as T) => . ... </k>
       <symbols> M => M[N <- #TypedSymbol(T, ?V:String)] </symbols>
```

During the final output comparison step we discard the type information retained
in lists. This allows us to compare lists which result from the 'MAP'
instruction correctly, since we do not presently determine a type for those
lists.

```k
  syntax Data ::= List
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
  rule #LiteralStackToSemantics({ .StackElementList }, KnownAddrs, BigMaps) => .
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

  rule <k> #TypeCheck(B,P,IS,OS:LiteralStack)
        => #TypeCheckAux(
             IS,
             OS,
             #LiteralStackToTypes(OS, P),
             #TypeInstruction(P, B, #LiteralStackToTypes(IS,P))
           )
           ...
       </k>

  // TODO: Implement a "partial" type check case
  rule <k> #TypeCheck(B,P,IS,OS:FailedStack) => . ... </k>
       <script> B </script>

  rule <k> #TypeCheckAux(IS, OS, OSTypes, #TI(B, ISTypes -> OSTypes))
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
  syntax Instruction ::= "ASSERT" Blocks
                       | "ASSUME" Blocks
```

```k
  rule <k> ASSERT { }:EmptyBlock => .K ... </k>
  rule <k> ASSERT { { } } => .K ... </k>
  rule <k> ASSERT { B } => ASSERT { B ; { } } ... </k> requires B =/=K { }
  rule <k> ASSERT { B; Bs }
        => B ~> #AssertTrue ~> ASSERT { Bs } ~> #RestoreStack(Stack)
           ...
       </k>
       <stack> Stack => .K </stack>
```

```k
  rule <k> ASSUME { }:EmptyBlock => .K ... </k>
  rule <k> ASSUME { { } } => .K ... </k>
  rule <k> ASSUME { B } => ASSUME { B ; { } } ... </k> requires B =/=K { }
  rule <k> ASSUME { B; Bs }
        => B ~> #AssumeTrue ~> ASSUME { Bs } ~> #RestoreStack(Stack)
           ...
       </k>
       <stack> Stack => .K </stack>
```

```k
  syntax KItem ::= #RestoreStack(K)
  rule <k> #RestoreStack(Stack) => .K ... </k>
       <stack> _ => Stack </stack>
```

```k
  syntax InternalInstruction ::= "#AssertTrue"
  rule <k> #AssertTrue => #Assert(B) ... </k>
       <stack> B:Bool => . </stack>
```

```k
  syntax KItem ::= "#AssumeTrue"
  rule <k> #AssumeTrue => #Assume(B) ... </k>
       <stack> B:Bool => . </stack>
```

```k
  syntax KItem ::= #Assert(Bool)
  rule <k> #Assert(true)  => .             ... </k>
  rule <k> #Assert(false) => #AssertFailed ... </k>
  syntax KItem ::= "#AssertFailed" [klabel(#AssertFailed), symbol]
```

```k
  syntax KItem ::= #Assume(Bool)
  rule <k> #Assume(true)  => .             ... </k>
  rule <k> #Assume(false) ~> _:K => . </k>
       <assumeFailed> _ => true </assumeFailed> [transition]
```

`precondition` Groups
---------------------

```k
  rule <k> precondition Bs => .K ... </k>
       <pre>  { } => Bs </pre>
```

```k
  syntax KItem ::= "#ExecutePreConditions"
  rule <k> #ExecutePreConditions => ASSUME Preconditions ... </k>
       <pre> Preconditions </pre>
```

`postcondition` group
---------------------

```k
  rule <k> postcondition Bs => .K ... </k>
       <post>  { } => Bs </post>
```

```k
  syntax KItem ::= "#ExecutePostConditions"
  rule <k> #ExecutePostConditions => ASSERT Postconditions ... </k>
       <post> Postconditions </post>
```

`invariants` group
---------------------

```k
  rule <k> invariants { } => .K ... </k>
  rule <k> invariants { I1 ; Is }
        => invariants( { I1 } ) ~> invariants ({ Is })
           ...
       </k>
  rule <k> invariants ({ Annotation:VariableAnnotation Blocks:Blocks }) => . ... </k>
       <invs> .Map => (Annotation |-> Blocks)  ... </invs>
```

Annotated in`LOOP`s may be annotated with invariants.

```symbolic
  syntax Instruction ::= CUTPOINT( id: Int, localStackDepth: Int )
  rule <k> LOOP A .AnnotationList { #Exec ( B ) }
        => LOOP .AnnotationList {
             ASSERT Invariant  ;  // Subsumption check
             CUTPOINT(!Int, #GetLocalStackDepth(B)) ;
             ASSUME Invariant  ;
             { #Exec ( B ) }
           }
           ...
       </k>
       <invs> A:VariableAnnotation |-> Invariant:Blocks ... </invs>

  rule <k> CUTPOINT(I, LocalStackDepth) => #GeneralizeStack(LocalStackDepth) ... </k>
       <cutpoints> (.Set => SetItem(I)) VisitedCutpoints </cutpoints>
    requires notBool I in VisitedCutpoints
  rule <k> CUTPOINT(I, _) => #Assume(false) ... </k>
       <cutpoints> VisitedCutpoints </cutpoints>
    requires I in VisitedCutpoints

  syntax KItem ::= #GeneralizeStack(Int)
  rule <k> #GeneralizeStack(I) => #GeneralizeStackAux(I, #ReverseKSeq(Ds), #ReverseTypeSeq(Ts), .K, .TypeSeq) ... </k>
       <stack> Ds => . </stack>
       <stacktypes> Ts => .TypeSeq </stacktypes>

  syntax KItem ::= #GeneralizeStackAux(Int, K, TypeSeq, K, TypeSeq)
  rule <k> #GeneralizeStackAux(I, V:KItem ~> Vs1, T ; Ts1, Vs2, Ts2) => #GeneralizeStackAux(I -Int 1, Vs1, Ts1, V ~> Vs2, T ; Ts2) ... </k> requires I >Int 0
  rule <k> #GeneralizeStackAux(0, _:Int ~> Vs1, int A ; Ts1, Vs2, Ts2) => #GeneralizeStackAux(0, Vs1, Ts1, ?_:Int ~> Vs2, int A ; Ts2) ... </k>
  rule <k> #GeneralizeStackAux(0, _:Int ~> Vs1, nat A ; Ts1, Vs2, Ts2) => #GeneralizeStackAux(0, Vs1, Ts1, ?V:Int ~> Vs2, nat A ; Ts2) ... </k> requires ?V:Int >=Int 0
  rule <k> #GeneralizeStackAux(0, _:String ~> Vs1, string A ; Ts1, Vs2, Ts2) => #GeneralizeStackAux(0, Vs1, Ts1, ?V:String ~> Vs2, string A ; Ts2) ... </k>
  rule <k> #GeneralizeStackAux(0, .K, _, Vs2, Ts2) => . ... </k>
       <stack> _ => Vs2 </stack>
       <stacktypes> _ => Ts2 </stacktypes>

  syntax K ::= #ReverseKSeq(K) [function, functional]
  syntax K ::= #ReverseKSeqAux(K, K) [function, functional]
  rule #ReverseKSeq(V) => #ReverseKSeqAux(V, .K)
  rule #ReverseKSeqAux(V:KItem ~> Vs1, Vs2) => #ReverseKSeqAux(Vs1, V ~> Vs2)
  rule #ReverseKSeqAux(.K, Vs) => Vs
```

`#CheckOutput`
--------------

```k
  syntax TypedSymbol ::= #TypedSymbol(Type, Data)
```

If a program aborts due to the FAILWITH instruction, we throw away the abortion debug info:

```k
  rule <k> (Aborted(_, _, _, _) => .K) ~> #CheckOutput ... </k>
```

```k
  syntax KItem ::= "#CheckOutput"
  rule <k> #CheckOutput => #Bind(ExpectedStack) ... </k>
       <expected> ExpectedStack </expected>
```

```k
  syntax KItem ::= #Bind(OutputStack)
  syntax KItem ::= #BindSingle(StackElement)
  rule <k> #Bind({ .StackElementList }) => .K ... </k>
       <stack> .K </stack>
  rule <k> #Bind({ S ; Ss }) => #BindSingle(S) ~> #Bind({ Ss }) ... </k>

  rule <k> #Bind(S1:FailedStack) => .K ... </k>
       <stack> S2:FailedStack => .K ... </stack>
	requires #Matches(S1, S2)

  rule <k> #BindSingle(Stack_elt T S:SymbolicData) => .K ...
	   </k>
	   <paramtype> PT </paramtype>
       <stack> D => .K ... </stack>
       <symbols> M => M[ S <- #TypedSymbol(T, D) ] </symbols>
    requires isTypedData(#TypeData(PT,D,T))

  rule <k> #BindSingle(Stack_elt T ED) => .K ... </k>
	   <knownaddrs> KnownAddrs </knownaddrs>
	   <bigmaps> BigMaps </bigmaps>
       <stack> AD => .K ... </stack>
    requires #Matches(#MichelineToNative(ED,T,KnownAddrs,BigMaps),AD)
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

```symbolic
  rule #LiteralStackToTypes({ Stack_elt T S:SymbolicData ; Gs:StackElementList }, PT)
    => T ; #LiteralStackToTypes({ Gs }, PT)
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
  imports UNIT-TEST-SYNTAX

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
