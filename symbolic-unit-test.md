```k
requires "unit-test.k"
requires "symbolic-configuration.k"
requires "michelson-types.k"
requires "symbolic-unit-test-syntax.k"


module SYMBOLIC-UNIT-TEST
  imports SYMBOLIC-UNIT-TEST-SYNTAX
  imports MICHELSON-TYPES
  imports SYMBOLIC-CONFIGURATION
  imports COLLECTIONS
  imports UNIT-TEST
  imports COLLECTIONS

  syntax Set ::= Set "|Set" Set [function, functional]
  rule S1 |Set S2 => S1 (S2 -Set S1)
  rule #GroupOrder(_:PreconditionGroup) => -1
  rule #GroupOrder(_:InvariantsGroup) => 0
  rule #GroupOrder(_:PostconditionGroup) => #GroupOrderMax +Int 2

  syntax BinderGroup ::= "Binder" LiteralStack
  syntax Group ::= BinderGroup

  rule #GroupOrder(_:BinderGroup) => #GroupOrderMax -Int 1

  syntax Type ::= "#UnknownType"

  syntax KItem ::= SymbolicElement

  syntax SymbolicElement ::= #SymbolicElement(SymbolicData, Type)
  syntax SymbolicElement ::= "#DummyElement"

  syntax Set ::= #FindSymbolsIn(Data, Type) [function, functional]
  syntax Set ::= #FindSymbols(KItem) [function, functional]

  rule #FindSymbols(G:Group ; Gs:Groups) => #FindSymbols(G) |Set #FindSymbols(Gs)

  rule #FindSymbols(input S) => #FindSymbols(S)
  rule #FindSymbols(output S) => .Set // #FindSymbols(S)
  rule #FindSymbols(code B) => #FindSymbols(B)
  rule #FindSymbols(precondition B) => #FindSymbols(B)
  rule #FindSymbols(postcondition B) => .Set // #FindSymbols(B)
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

  rule [[ #ConcreteArgToSemantics(S:SymbolicData, T) => D ]]
       <symbols> S |-> #TypedSymbol(T, D) ... </symbols>

  rule #ConcreteArgToSemantics(S:SymbolicData, T) => S [owise]

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

  syntax KItem ::= #CreateSymbols(UnifiedList)

  syntax KItem ::= #CreateSymbol(SymbolicData, Type)

  rule <k> #CreateSymbols(.List) => . ... </k>
  rule <k> #CreateSymbols(ListItem(#SymbolicElement(D, T)) S) => #CreateSymbol(D, T) ~> #CreateSymbols(S) ... </k>

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

  rule #TypeData(_, S:SymbolicData, T) => #Typed(S, T)

   // Complex types..._

  syntax Group ::= #DoReplace(Group) [function, functional]

  rule #DoReplace(output LS) => Binder LS
  rule #DoReplace(G) => G [owise]

  syntax Groups ::= #ReplaceOutputWithBinder(Groups) [function, functional]

  rule #ReplaceOutputWithBinder(G ; Gs) => #DoReplace(G) ; #ReplaceOutputWithBinder(Gs)
  rule #ReplaceOutputWithBinder(G:Group) => #DoReplace(G)
  rule #ReplaceOutputWithBinder(G:Group;) => #DoReplace(G)

  syntax KItem ::= Groups

  rule <michelsonTop>
         <k> Gs:Groups => #CreateSymbols(#UnifiedSetToList(#UnifyTypes(#FindSymbols(Gs)))) ~> #ReplaceOutputWithBinder(Gs) </k>
         ...
       </michelsonTop>
       <symbolsLoaded> false => true </symbolsLoaded>

  syntax KItem ::= #Assume(Bool) | #Assert(Bool)

  rule <k> #Assume(B) => . ... </k>
       <stack> B => . ... </stack> [transition]

  rule <k> #Assume(B1) ~> _:K => . </k>
       <stack> B2 => . ... </stack>
       <assumeFailed> _ => true </assumeFailed> 
       requires B1 =/=Bool B2
       [transition]


  rule <k> #LoadGroups(precondition { } ; Gs) => #LoadGroups(Gs) ... </k>
  rule <k> #LoadGroups(precondition { B:Block } ; Gs) => B ~> #Assume(true) ~> #LoadGroups(Gs) ... </k>
  rule <k> #LoadGroups(precondition { B:Block ; Bs } ; Gs) =>
       B ~> #Assume(true) ~> #LoadGroups(precondition { Bs } ; Gs) ... </k>

  syntax KItem ::= #Bind(LiteralStack)

  syntax KItem ::= #BindSingle(StackElement)

  rule <k> #LoadGroups(Binder LS ; (code B #as Gs)) => #CheckTypes(LS, B) ~> #LoadGroups(Gs) ~> #Bind(LS) ... </k>
  rule <k> #LoadGroups(Binder LS ; ((code B ; _) #as Gs)) => #CheckTypes(LS, B) ~> #LoadGroups(Gs) ~> #Bind(LS) ... </k>

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

  rule <k> #DoPostConditions(B ; Bs) => B ~> #Assert(true) ~> #DoPostConditions(Bs) ... </k>
  rule <k> #DoPostConditions(B) => B ~> #Assert(true) ... </k>

  rule <k> #Assert(B) => . ... </k>
       <stack> B => . ... </stack>

  syntax KItem ::= "#AssertFailed"

  rule <k> #Assert(B1) => #AssertFailed ... </k>
       <stack> B2 => . ... </stack>
       requires B1 =/=Bool B2

  rule <k> #LoadGroups(postcondition { }) ~> #Bind(_) => . ... </k>
  rule <k> #LoadGroups(postcondition { B }) ~> #Bind(Rs) => #Bind(Rs) ~> #DoPostConditions(B) ... </k>

  syntax KItem ::= #LoadInvariants(Invariants) | #LoadInvariant(Invariant)

  rule #LoadGroups(invariants Invs ; Gs) => #LoadInvariants(Invs) ~> #LoadGroups(Gs)

  rule #LoadInvariants({ }) => .
  rule #LoadInvariants({ I }) => #LoadInvariant(I)
  rule #LoadInvariants({ I1 ; Is }) => #LoadInvariant(I1) ~> #LoadInvariants({ Is })

  rule <k> #LoadInvariant(V Bs1 Bs2) => . ... </k>
       <invariants> M => M[V <- Bs1] </invariants>
       <guards> C => C[V <- Bs2] </guards>

  rule #Ceil(#DoCompare(@A:Int, @B:Int)) => #Ceil(@A) #And #Ceil(@B)  [anywhere, simplification]

  syntax MaybeInvariantId ::= "#NoInvariant" | VariableAnnotation

  syntax KItem ::= "#RecordHaltCondition"
  syntax KItem ::= #AssumeHaltCondition(Bool)
  syntax KItem ::= #ForgetAllModifiable(Block)
  syntax KItem ::= #AssertInvariant(MaybeInvariantId)
  syntax KItem ::= #AssumeInvariant(MaybeInvariantId)
  syntax KItem ::= #AssertGuard(MaybeInvariantId)
  syntax KItem ::= #AssumeNotGuard(MaybeInvariantId)
  syntax KItem ::= #AssertNotGuard(MaybeInvariantId)
  syntax KItem ::= #AssumeGuard(MaybeInvariantId)
  syntax KItem ::= #VerifyLoopEnd(MaybeInvariantId)

  syntax KItem ::= "#SaveStack"
  syntax KItem ::= #RestoreStack(K)

  syntax KItem ::= #AssertBlocks(Blocks, Bool) 

  rule #AssertBlocks({ }, _) => .
  rule #AssertBlocks({ B }, C) => #SaveStack ~> B ~> #Assert(C) ~> #RestoreStack(.K)
  rule #AssertBlocks({ B ; Bs }, C) => #SaveStack ~> B ~> #Assert(C) ~> #RestoreStack(.K)  ~> #AssertBlocks(Bs, C)

  syntax KItem ::= #AssumeBlocks(Blocks, Bool) 

  rule #AssumeBlocks({ }, _) => .
  rule #AssumeBlocks({ B }, C) => #SaveStack ~>  B ~> #Assume(C) ~> #RestoreStack(.K)
  rule #AssumeBlocks({ B ; Bs }, C) => #SaveStack ~> B ~> #Assume(C) ~> #RestoreStack(.K) ~> #AssumeBlocks(Bs, C)

  rule <k> #SaveStack ~> X:KItem ~> Y:KItem ~> #RestoreStack(_) => X ~> Y ~> #RestoreStack(S) ... </k>
       <stack> S </stack>

  rule <k> #RestoreStack(S) => . ... </k>
       <stack> _ => S </stack>

  rule <k> #AssumeInvariant(V:VariableAnnotation) => #AssumeBlocks(B, true) ... </k>
       <invariants> ... V |-> B ... </invariants>

  rule <k> #AssertInvariant(V:VariableAnnotation) => #AssertBlocks(B, true) ... </k>
       <invariants> ... V |-> B ... </invariants>

  rule <k> #AssumeGuard(V:VariableAnnotation) => #AssumeBlocks(B, true) ... </k>
       <guards> ... V |-> B ... </guards>

  rule <k> #AssertGuard(V:VariableAnnotation) => #AssertBlocks(B, true) ... </k>
       <guards> ... V |-> B ... </guards>

  rule <k> #AssumeNotGuard(V:VariableAnnotation) => #AssumeBlocks(B, false) ... </k>
       <guards> ... V |-> B ... </guards>

  rule <k> #AssertNotGuard(V:VariableAnnotation) => #AssertBlocks(B, false) ... </k>
       <guards> ... V |-> B ... </guards>

  rule <k> #VerifyLoopEnd(V) ~> _:K => #AssertGuard(V) ~> #AssertInvariant(V) </k>
       <stack> true => . ... </stack> [transition]

  rule <k> #VerifyLoopEnd(V) => #AssertNotGuard(V) ~> #AssertInvariant(V) ... </k>
       <stack> false => . ... </stack> [transition]

  rule <k> LOOP A B => #AssertInvariant(#FindInvariant(A)) ~> 
                       #AssertGuard(#FindInvariant(A)) ~> 
                       #ForgetAllModifiable(B) ~>
                       #AssumeInvariant(#FindInvariant(A)) ~>  // Split invariant in two?  One for before loop (incl. halt condition), one for after (we try to prove halt = false -> pre-condition)?
                       #AssumeGuard(#FindInvariant(A)) ~>
                       B ~>
                       #VerifyLoopEnd(#FindInvariant(A)) ~>
                       #ForgetAllModifiable(B) ~>
                       #AssumeInvariant(#FindInvariant(A)) ~>
                       #AssumeNotGuard(#FindInvariant(A)) 
                       ... </k>
       <stack> true => . ... </stack> 
       requires #HasInvariant(A)
       [simplification]

  syntax KItem ::= #ForgetAllAbove(Int)
  syntax KItem ::= #ForgetAllAboveAux(Int, K, TypeSeq, K, TypeSeq)

  rule <k> #ForgetAllModifiable({ #Exec(B) }) => #ForgetAllAbove(#GetCriticalPoint(B)) ... </k>

  syntax K ::= #ReverseKSeq(K) [function, functional]
  syntax K ::= #ReverseKSeqAux(K, K) [function, functional]
  rule #ReverseKSeq(V) => #ReverseKSeqAux(V, .K)
  rule #ReverseKSeqAux(V:KItem ~> Vs1, Vs2) => #ReverseKSeqAux(Vs1, V ~> Vs2)
  rule #ReverseKSeqAux(.K, Vs) => Vs

  rule <k> #ForgetAllAbove(I) => #ForgetAllAboveAux(I, #ReverseKSeq(Ds), #ReverseTypeSeq(Ts), .K, .TypeSeq) ... </k>
       <stack> Ds => . </stack>
       <stacktypes> Ts => .TypeSeq </stacktypes>

  rule <k> #ForgetAllAboveAux(I, V:KItem ~> Vs1, T ; Ts1, Vs2, Ts2) => #ForgetAllAboveAux(I -Int 1, Vs1, Ts1, V ~> Vs2, T ; Ts2) ... </k> requires I >Int 0

  rule <k> #ForgetAllAboveAux(0, _:Int ~> Vs1, int A ; Ts1, Vs2, Ts2) => #ForgetAllAboveAux(0, Vs1, Ts1, ?_:Int ~> Vs2, int A ; Ts2) ... </k>
  rule <k> #ForgetAllAboveAux(0, _:Int ~> Vs1, nat A ; Ts1, Vs2, Ts2) => #ForgetAllAboveAux(0, Vs1, Ts1, ?V:Int ~> Vs2, nat A ; Ts2) ... </k> requires ?V:Int >=Int 0
  rule <k> #ForgetAllAboveAux(0, _:String ~> Vs1, string A ; Ts1, Vs2, Ts2) => #ForgetAllAboveAux(0, Vs1, Ts1, ?V:String ~> Vs2, string A ; Ts2) ... </k>

  rule <k> #ForgetAllAboveAux(0, .K, _, Vs2, Ts2) => . ... </k>
       <stack> _ => Vs2 </stack>
       <stacktypes> _ => Ts2 </stacktypes>


  syntax Int ::= #GetCriticalPoint(TypedInstructionList) [function]

  rule #GetCriticalPoint(T ; Ts) => minInt(#GetCriticalPoint(T), #GetCriticalPoint(Ts))

  rule #GetCriticalPoint(#TI({ #Exec(Ts) }, _)) => #GetCriticalPoint(Ts)
  rule #GetCriticalPoint(#TI({ }, Ts1 -> Ts1)) => #LengthTypeSeq(Ts1)

  rule #GetCriticalPoint(#TI(DROP _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(DROP _ N, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int N
  rule #GetCriticalPoint(#TI(DIG _ N, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int N
  rule #GetCriticalPoint(#TI(DUG _ N, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int N
  rule #GetCriticalPoint(#TI(DUP _, Ts1 -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(SWAP _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 2
  rule #GetCriticalPoint(#TI(PUSH _ _ _, Ts1 -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(SOME _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(NONE _ _, Ts1 -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(UNIT _, Ts1 -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(IF_NONE _ { #Exec(B1) } { #Exec(B2) }, Ts1 -> _)) => minInt(#LengthTypeSeq(Ts1) -Int 1, minInt(#GetCriticalPoint(B1), #GetCriticalPoint(B2)))
  rule #GetCriticalPoint(#TI(PAIR _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 2
  rule #GetCriticalPoint(#TI(UNPAIR _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(CAR _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(CDR _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(LEFT _ _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(RIGHT _ _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(IF_LEFT _ { #Exec(B1) } { #Exec(B2) }, Ts1 -> _)) => minInt(#LengthTypeSeq(Ts1) -Int 1, minInt(#GetCriticalPoint(B1), #GetCriticalPoint(B2)))
  rule #GetCriticalPoint(#TI(IF_RIGHT _ { #Exec(B1) } { #Exec(B2) }, Ts1 -> _)) => minInt(#LengthTypeSeq(Ts1) -Int 1, minInt(#GetCriticalPoint(B1), #GetCriticalPoint(B2)))
  rule #GetCriticalPoint(#TI(NIL _ _, Ts1 -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(CONS _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 2
  rule #GetCriticalPoint(#TI(IF_CONS _ { #Exec(B1) } { #Exec(B2) }, Ts1 -> _)) => minInt(#LengthTypeSeq(Ts1) -Int 1, minInt(#GetCriticalPoint(B1), #GetCriticalPoint(B2)))
  rule #GetCriticalPoint(#TI(SIZE _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(EMPTY_SET _ _, Ts1 -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(EMPTY_MAP _ _ _, Ts1 -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(EMPTY_BIG_MAP _ _ _, Ts1 -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(MAP _ { #Exec(B1) }, Ts1 -> _)) => minInt(#LengthTypeSeq(Ts1) -Int 1, #GetCriticalPoint(B1))
  rule #GetCriticalPoint(#TI(ITER _ { #Exec(B1) }, Ts1 -> _)) => minInt(#LengthTypeSeq(Ts1) -Int 1, #GetCriticalPoint(B1))
  rule #GetCriticalPoint(#TI(MEM _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 2
  rule #GetCriticalPoint(#TI(GET _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 2
  rule #GetCriticalPoint(#TI(UPDATE _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 3
  rule #GetCriticalPoint(#TI(IF _ { #Exec(B1) } { #Exec(B2) }, Ts1 -> _)) => minInt(#LengthTypeSeq(Ts1) -Int 1, minInt(#GetCriticalPoint(B1), #GetCriticalPoint(B2)))
  rule #GetCriticalPoint(#TI(LOOP _ { #Exec(B1) }, Ts1 -> _)) => minInt(#LengthTypeSeq(Ts1) -Int 1, #GetCriticalPoint(B1))
  rule #GetCriticalPoint(#TI(LOOP_LEFT _ { #Exec(B1) }, Ts1 -> _)) => minInt(#LengthTypeSeq(Ts1) -Int 1, #GetCriticalPoint(B1))
  rule #GetCriticalPoint(#TI(LAMBDA _ _ _ _, Ts1 -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(EXEC _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 2
  rule #GetCriticalPoint(#TI(APPLY _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 2
  rule #GetCriticalPoint(#TI(DIP _ { #Exec(B1) }, Ts1 -> _)) => #GetCriticalPoint(B1)
  rule #GetCriticalPoint(#TI(DIP _ _:Int { #Exec(B1) }, Ts1 -> _)) => #GetCriticalPoint(B1)
  rule #GetCriticalPoint(#TI(FAILWITH _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(CAST _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(RENAME _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(CONCAT _, (string _ ; string _ ; Ts1) -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(CONCAT _, (bytes _ ; bytes _ ; Ts1) -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(CONCAT _, (list _ _ ; Ts1) -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(SLICE _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 3
  rule #GetCriticalPoint(#TI(PACK _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(UNPACK _ _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(ADD _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 2
  rule #GetCriticalPoint(#TI(SUB _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 2
  rule #GetCriticalPoint(#TI(MUL _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 2
  rule #GetCriticalPoint(#TI(EDIV _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 2
  rule #GetCriticalPoint(#TI(ABS _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(ISNAT _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(INT _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(NEG _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(LSL _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(LSR _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(OR _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 2
  rule #GetCriticalPoint(#TI(AND _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 2
  rule #GetCriticalPoint(#TI(XOR _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 2
  rule #GetCriticalPoint(#TI(NOT _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(COMPARE _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 2
  rule #GetCriticalPoint(#TI(EQ _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(NEQ _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(LT _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(GT _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(LE _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(GE _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(SELF _, Ts1 -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(CONTRACT _ _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(TRANSFER_TOKENS _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 3
  rule #GetCriticalPoint(#TI(SET_DELEGATE _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 2
  rule #GetCriticalPoint(#TI(NOW _, Ts1 -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(CHAIN_ID _, Ts1 -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(AMOUNT _, Ts1 -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(BALANCE _, Ts1 -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(CHECK_SIGNATURE _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 3
  rule #GetCriticalPoint(#TI(BLAKE2B _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(SHA256 _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(SHA512 _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(HASH_KEY _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1
  rule #GetCriticalPoint(#TI(SOURCE _, Ts1 -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(SENDER _, Ts1 -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(ADDRESS _, Ts1 -> _)) => #LengthTypeSeq(Ts1)
  rule #GetCriticalPoint(#TI(CREATE_CONTRACT _ { _ }, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 3
  rule #GetCriticalPoint(#TI(IMPLICIT_ACCOUNT _, Ts1 -> _)) => #LengthTypeSeq(Ts1) -Int 1

  syntax MaybeInvariantId ::= #FindInvariant(AnnotationList) [function, functional]

  rule #FindInvariant(V:VariableAnnotation _) => V
  rule #FindInvariant(_:Annotation Rs) => #FindInvariant(Rs) [owise]
  rule #FindInvariant(.AnnotationList) => #NoInvariant

  syntax Bool ::= #HasInvariant(AnnotationList) [function, functional]
  rule #HasInvariant(L) => #FindInvariant(L) =/=K #NoInvariant

  rule #DoCompare(I1:Int, I2:Int) <Int 0 => I1 <Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) <=Int 0 => I1 <=Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) ==Int 0 => I1 ==Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) >=Int 0 => I1 >=Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) >Int 0 => I1 >Int I2 [simplification]


  syntax KItem ::= "#Stop"
  rule <k> COMPARE _ => #Stop ... </k> 
       <stack> _:Bool ~> _:Int ... </stack> [simplification]

  rule I1 >=Int I2 andBool I1 <=Int I2 => I1 ==Int I2 [simplification]

  rule B:Bool ==Bool true => B [simplification]
  rule B:Bool ==Bool false => notBool(B) [simplification]
endmodule
```
