```k
requires "unit-test.k"
requires "michelson-types.k"
requires "symbolic-unit-test-syntax.k"

module SYMBOLIC-UNIT-TEST
  imports SYMBOLIC-UNIT-TEST-SYNTAX
  imports MICHELSON-TYPES
  imports UNIT-TEST

  syntax Set ::= Set "|Set" Set [function, functional]
  rule S1 |Set S2 => S1 (S2 -Set S1)

  rule #GroupOrder(_:PreconditionGroup) => -1
  rule #GroupOrder(_:PostconditionGroup) => #GroupOrderMax +Int 2

  syntax Type ::= "#UnknownType"

  syntax KItem ::= SymbolicElement

  syntax SymbolicElement ::= #SymbolicElement(SymbolicData, Type)
  syntax SymbolicElement ::= "#DummyElement"

  syntax Set ::= #FindSymbolsIn(Data, Type) [function, functional]
  syntax Set ::= #FindSymbols(KItem) [function, functional]

  rule #FindSymbols(G:Group ; Gs:Groups) => #FindSymbols(G) |Set #FindSymbols(Gs)

  rule #FindSymbols(input S) => #FindSymbols(S)
  rule #FindSymbols(output S) => #FindSymbols(S)
  rule #FindSymbols(code B) => #FindSymbols(B)
  rule #FindSymbols(precondition B) => #FindSymbols(B)
  rule #FindSymbols(postcondition B) => #FindSymbols(B)

  rule #FindSymbols({ B:BlockList }) => #FindSymbols(B)

  rule #FindSymbols(B:Block ; Rs:BlockList) => #FindSymbols(B) #FindSymbols(Rs)

  rule #FindSymbols({ }) => .Set
  rule #FindSymbols( { I:Instruction }) => #FindSymbols(I)
  rule #FindSymbols({ I:Instruction ; Is:InstructionList }) => #FindSymbols(I) |Set #FindSymbols(Is)

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

  rule [[ #ConcreteArgToSemantics(S:SymbolicData, T) => D ]]
       <symbols> S |-> #TypedSymbol(T, D) ... </symbols>

  rule #ConcreteArgToSemantics(S:SymbolicData, T) => S [owise]

  syntax Bool ::= #AllTypesKnown(Set) [function, functional]
  rule #AllTypesKnown(SetItem(#SymbolicElement(_, #UnknownType)) _) => false
  rule #AllTypesKnown(_) => true [owise]

  syntax UnifiedSet ::= Set | "#UnificationFailure"

  syntax UnifiedSet ::= #UnifyTypes(Set) [function, functional]

  rule #UnifyTypes(SetItem(#SymbolicElement(S, #UnknownType)) SetItem(#SymbolicElement(S, T)) Ss) => #UnifyTypes(SetItem(#SymbolicElement(S, T)) Ss)

  rule #UnifyTypes(SetItem(#SymbolicElement(S, T1)) SetItem(#SymbolicElement(S, T2)) _) => #UnificationFailure
       requires T1 =/=K T2 andBool T1 =/=K #UnknownType andBool T2 =/=K #UnknownType

  rule #UnifyTypes(S) => S requires #AllTypesKnown(S) [owise]
  rule #UnifyTypes(S) => #UnificationFailure requires notBool(#AllTypesKnown(S)) [owise]

  syntax KItem ::= #CreateSymbols(UnifiedSet)

  syntax KItem ::= #CreateSymbol(SymbolicData, Type)

  rule <k> #CreateSymbols(.Set) => . ... </k>
  rule <k> #CreateSymbols(SetItem(#SymbolicElement(D, T)) S) => #CreateSymbol(D, T) ~> #CreateSymbols(S) ... </k>

  syntax TypedSymbol ::= #TypedSymbol(Type, Data)

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

   // Complex types..._


  syntax BinderGroup ::= "Binder" LiteralStack
  syntax Group ::= BinderGroup

  rule #GroupOrder(_:BinderGroup) => #GroupOrderMax +Int 1

  syntax Group ::= #DoReplace(Group) [function, functional]

  rule #DoReplace(output LS) => Binder LS
  rule #DoReplace(G) => G [owise]

  syntax Groups ::= #ReplaceOutputWithBinder(Groups) [function, functional]

  rule #ReplaceOutputWithBinder(G ; Gs) => #DoReplace(G) ; #ReplaceOutputWithBinder(Gs)
  rule #ReplaceOutputWithBinder(G:Group) => #DoReplace(G)
  rule #ReplaceOutputWithBinder(G:Group;) => #DoReplace(G)

  syntax KItem ::= Groups

  rule <michelsonTop>
         <k> Gs:Groups => #CreateSymbols(#UnifyTypes(#FindSymbols(Gs))) ~> #ReplaceOutputWithBinder(Gs) </k>
         ...
       </michelsonTop>
       <symbolsLoaded> false => true </symbolsLoaded>

  syntax KItem ::= "#AssumeTrue" | "#AssertTrue"

  rule <k> #AssumeTrue => . ... </k>
       <stack> true => . </stack>

  rule <k> #AssumeTrue ~> _:K => . </k>
       <stack> false => . </stack>

  rule <k> #LoadGroups(precondition { } ; Gs) => #LoadGroups(Gs) ... </k>
  rule <k> #LoadGroups(precondition { B:Block } ; Gs) => B ~> #AssumeTrue ~> #LoadGroups(Gs) ... </k>
  rule <k> #LoadGroups(precondition { B:Block ; Bs } ; Gs) =>
       B ~> #AssumeTrue ~> #LoadGroups(precondition { Bs } ; Gs) ... </k>

  syntax KItem ::= #Bind(LiteralStack)

  syntax KItem ::= #BindSingle(StackElement)

  rule <k> #LoadGroups(Binder LS ; Gs) => #LoadGroups(Gs) ... </k>

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

  syntax KItem ::= #DoPostConditions(BlockList)

  rule <k> #DoPostConditions(B ; Bs) => B ~> #AssertTrue ~> #DoPostConditions(Bs) ... </k>
  rule <k> #DoPostConditions(B) => B ~> #AssertTrue ... </k>

  rule <k> #AssertTrue => . ... </k>
       <stack> true => . </stack>

  rule <k> #LoadGroups(postcondition { }) => . ... </k>
  rule <k> #LoadGroups(postcondition { B }) => #DoPostConditions(B)  ... </k>

  rule #Ceil(#DoCompare(@A:Int, @B:Int)) => #Ceil(@A) #And #Ceil(@B)  [anywhere, simplification]

  rule #DoCompare(I1:Int, I2:Int) <Int 0 => I1 <Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) ==Int 0 => I1 ==Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) >Int 0 => I1 >Int I2 [simplification]

  configuration <michelsonTop/>
                <symbolsLoaded> false </symbolsLoaded>
                <symbols> .Map </symbols>
endmodule
```
