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
  rule <k> (.K => #MakeFresh(T)) ~>  #CreateSymbol(_, T) ... </k>
  rule <k> #Fresh(V) ~> #CreateSymbol(N, T) => . ... </k>
       <symbols> M => M[N <- #TypedSymbol(T, V)] </symbols>
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

  rule <k> ( #Fresh(V)
          ~> #GeneralizeStack(Stack_elt T D:SymbolicData ; Stack, KSeq)
           )
        =>   #GeneralizeStack(Stack_elt T V ; Stack, KSeq)
           ...
       </k>
```

Here `#MakeFresh` is responsible for generating a fresh value of a given type.

```symbolic
  syntax Data ::= #MakeFresh(Type) | #Fresh(Data) | "#hole"

  rule <k> #MakeFresh(bool   _:AnnotationList)                     =>                       #Fresh(?_:Bool)   ... </k>
  rule <k> #MakeFresh(int    _:AnnotationList)                     =>                       #Fresh(?_:Int)    ... </k>
  rule <k> #MakeFresh(nat    _:AnnotationList)                     => #Assume(?V >Int 0) ~> #Fresh(?V:Int)    ... </k>
  rule <k> #MakeFresh(string _:AnnotationList)                     =>                       #Fresh(?_:String) ... </k>
  rule <k> #MakeFresh(map    (_):AnnotationList (_):Type (_):Type) =>                       #Fresh(?_:Map)    ... </k>

  // TODO: Is there a neater way of doing this? Perhaps using K's contexts?
  rule <k> #MakeFresh(pair _:AnnotationList T1 T2)
        => #MakeFresh(T1)
        ~> #MakeFresh(T2)
        ~> #Fresh((Pair #hole #hole))
           ...
       </k>
  rule <k> (#Fresh(V1) => .K) ~> _:Data ~> #Fresh(Pair (#hole => V1) #hole) ... </k>
  rule <k> (#Fresh(V2) => .K) ~> #Fresh(Pair _ (#hole => V2)) ... </k>
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
    requires isTypedData(#TypeData(PT,D,T))

  rule <k> #Bind( { Stack_elt T S:SymbolicData ; Ss } => { Ss }
                , ( (D ~> K:K)                        => K )
                )
           ...
       </k>
       <paramtype> PT </paramtype>
       <symbols> S |-> #TypedSymbol(T, D) ... </symbols>
    requires isTypedData(#TypeData(PT,D,T))

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
