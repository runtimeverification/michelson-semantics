```k
requires "unit-test/unit-test.md"
requires "michelson/types.md"
requires "symbolic/configuration.md"
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
        ~> #BaseInit
        ~> #ExecutePreConditions
        ~> #LoadInputStack
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

  rule [[ #MichelineToNative(S:SymbolicData, T, _, _) => D ]]
       <symbols> S |-> #TypedSymbol(T, D) ... </symbols>

  rule [[ #MichelineToNative(S:SymbolicData, T, _, _) => S ]]
       <symbols> Syms:Map </symbols>
    requires notBool (S in_keys(Syms))

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

TODO: This is not excersized by any tests.

```k
  syntax KItem ::= #LoadInvariants(Invariants) | #LoadInvariant(Invariant)
  rule <k> invariants { } => .K ... </k>
  rule <k> invariants { I1 ; Is }
        => invariants( { I1 } ) ~> #LoadInvariants({ Is })
           ...
       </k>
  rule <k> invariants ({ I }) => .K ... </k>
  rule <k> invariants ({ Annotation Blocks }) => . ... </k>
       <invs> M => M[Annotation <- Blocks] </invs>
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
