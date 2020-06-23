```k
requires "unit-test/unit-test.md"
requires "michelson/types.md"
requires "symbolic/configuration.md"
requires "symbolic/syntax.md"
```

```k
module SYMBOLIC-UNIT-TEST-DRIVER
  imports SYMBOLIC-UNIT-TEST-SYNTAX
  imports MICHELSON-TYPES
  imports SYMBOLIC-CONFIGURATION
  imports COLLECTIONS
  imports UNIT-TEST
  imports COLLECTIONS

  rule <k> #Init
        => #CreateSymbols
        ~> #UnitTestInit
        ~> #ExecutePreConditions
        ~> #ExecuteScript
        ~> #CheckSymbolicOutput
        ~> #ExecutePostConditions
           ...
       </k>

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

  rule [[ #MichelineToNative(S:SymbolicData, T) => D ]]
       <symbols> S |-> #TypedSymbol(T, D) ... </symbols>

  rule #MichelineToNative(S:SymbolicData, T) => S [owise]

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

`#CreateSymbol`
--------------

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
  rule <michelsonTop>
         <k> #CreateSymbol(N, (nat _) #as T) => . ... </k>
         ...
       </michelsonTop>
       <symbols> M => M[N <- #TypedSymbol(T, ?V:Int)] </symbols>
       ensures ?V >=Int 0
  rule <michelsonTop>
         <k> #CreateSymbol(N, (int _) #as T) => . ... </k>
         ...
       </michelsonTop>
       <symbols> M => M[N <- #TypedSymbol(T, ?V:Int)] </symbols>

  rule <michelsonTop>
         <k> #CreateSymbol(N, (string _) #as T) => . ... </k>
         ...
       </michelsonTop>
       <symbols> M => M[N <- #TypedSymbol(T, ?V:String)] </symbols>
```

```k
  syntax TypedSymbol ::= #TypedSymbol(Type, Data)
  rule [[ #TypeData(_, S:SymbolicData, T) => #Typed(S, T) ]]
       <symbols> ... S |-> #TypedSymbol(T, _) ... </symbols>

   // Complex types..._
```

`#LiteralStackToTypesAux` Extension
-----------------------------------

We extend this typing function to handle symbolic values.

```k
  rule #LiteralStackToTypesAux(Stack_elt T S:SymbolicData ; Gs:StackElementList, PT)
    => T ; #LiteralStackToTypesAux(Gs, PT)

  rule #LiteralStackToTypesAux(Stack_elt T S:SymbolicData, PT) => T
```

`#Assume`/`#Assert` instructions
--------------------------------

```k
  syntax KItem ::= "#AssumeTrue"
  rule <k> #AssumeTrue => . ... </k>
       <stack> true => . </stack> [transition]
  rule <k> #AssumeTrue ~> _:K => . </k>
       <stack> false => . </stack>
       <assumeFailed> _ => true </assumeFailed> [transition]
```

```k
  syntax KItem ::= "#AssertTrue"
  rule <k> #AssertTrue => . ... </k>
       <stack> true => . </stack>
  rule <k> #AssertTrue => #AssertFailed ... </k>
       <stack> false => . </stack>
  syntax KItem ::= "#AssertFailed" [klabel(#AssertFailed), symbol]
```

`#RestoreStack` utility function
--------------------------------

```k
  syntax KItem ::= "#RestoreStack" "(" K ")"
  rule <k> #RestoreStack(Stack) => .K ... </k>
       <stack> _ => Stack </stack>
```

`precondition` Groups
---------------------

```k
  rule <k> precondition Bs => .K ... </k>
       <pre>  { } => Bs </pre>
```

```k
  syntax KItem ::= "#ExecutePreConditions"
                 | "#ExecutePreConditions" "(" Block ")"
  rule <k> #ExecutePreConditions(B:Block)
        => B ~> #AssumeTrue ~> #ExecutePreConditions
           ...
       </k>
       <stack> Stack => .K </stack>

  rule <k> #ExecutePreConditions => #ExecutePreConditions(B) ... </k>
       <pre> { B ; Bs } => { Bs } </pre>
  rule <k> #ExecutePreConditions => #ExecutePreConditions(B) ... </k>
       <pre> { B } => { }  </pre>
  rule <k> #ExecutePreConditions => .K ... </k>
       <pre> { } </pre>
```

`#CheckSymbolicOutput`
----------------------

```k
  syntax KItem ::= "#CheckSymbolicOutput"
  rule <k> #CheckSymbolicOutput => #Bind(ExpectedStack) ... </k>
       <expected> ExpectedStack </expected>
```

```k
  syntax KItem ::= #Bind(LiteralStack)
  syntax KItem ::= #BindSingle(StackElement)
  rule <k> #Bind({ }) => . ... </k>
       <stack> . </stack>

  rule <k> #Bind({ S }) => #BindSingle(S) ... </k>
  rule <k> #Bind({ S ; Ss }) => #BindSingle(S) ~> #Bind({ Ss }) ... </k>

  rule <michelsonTop>
         <k> #BindSingle(Stack_elt T S:SymbolicData) => . ... </k>
         <stack> D => . ... </stack>
         ...
       </michelsonTop>
       <symbols> M => M[ S <- #TypedSymbol(T, D) ] </symbols>

  rule <k> #BindSingle(Stack_elt T D) => . ... </k>
       <stack> D => . ... </stack>
```

`postcondition` group
---------------------

```k
  rule <k> postcondition B => . ... </k>
       <post> { } => B </post>

  syntax KItem ::= "#ExecutePostConditions"
                 | #ExecutePostConditions(Block)
  rule <k> #ExecutePostConditions(B)
        => B ~> #AssertTrue ~>  #ExecutePostConditions
           ...
       </k>
       <stack> Stack => .K </stack>

  rule <k> #ExecutePostConditions => #ExecutePostConditions(B) ... </k>
       <post> { B ; Bs } => { Bs } </post>
  rule <k> #ExecutePostConditions => #ExecutePostConditions(B) ... </k>
       <post> { B } => { }  </post>
  rule <k> #ExecutePostConditions => .K ... </k>
       <post> { } </post>
```

`invariants` group
---------------------

```k
  syntax KItem ::= #LoadInvariants(Invariants) | #LoadInvariant(Invariant)

//  rule #LoadGroups(invariants Invs ; Gs) => #LoadInvariants(Invs) ~> #LoadGroups(Gs)

  rule #LoadInvariants({ }) => .
  rule #LoadInvariants({ I }) => #LoadInvariant(I)
  rule #LoadInvariants({ I1 ; Is }) => #LoadInvariant(I1) ~> #LoadInvariants({ Is })

  rule <k> #LoadInvariant(V Bs) => . ... </k>
       <invs> M => M[V <- Bs] </invs>
```

```k
  rule #Ceil(#DoCompare(@A:Int, @B:Int)) => #Ceil(@A) #And #Ceil(@B)  [anywhere, simplification]
  rule #DoCompare(I1:Int, I2:Int) <Int 0 => I1 <Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) <=Int 0 => I1 <=Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) ==Int 0 => I1 ==Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) >=Int 0 => I1 >=Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) >Int 0 => I1 >Int I2 [simplification]
```

```k
endmodule
```
