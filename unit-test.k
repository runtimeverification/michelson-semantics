requires "unit-test-syntax.k"
requires "michelson.k"

module UNIT-TEST
  imports UNIT-TEST-SYNTAX
  imports MICHELSON

  syntax Data ::= List

  syntax Data ::= #DropListTypesData(Data) [function]

  rule #DropListTypesData(#List(L, T)) => #DropListTypesData(L)

  rule #DropListTypesData(.List) => .List
  rule #DropListTypesData(ListItem(L1) L) => ListItem(#DropListTypesData(L1)) {#DropListTypesData(L)}:>List

  rule #DropListTypesData(.Set) => .Set
  rule #DropListTypesData(SetItem(S1) S) => SetItem(#DropListTypesData(S1)) {#DropListTypesData(S)}:>Set

  rule #DropListTypesData(.Map) => .Map
  rule #DropListTypesData((K |-> V) M) => (#DropListTypesData(K) |-> #DropListTypesData(V)) {#DropListTypesData(M)}:>Map

  rule #DropListTypesData(Create_contract(I, C, O, M, D)) => Create_contract(I, C, O, M, #DropListTypesData(D))
  rule #DropListTypesData(Transfer_tokens(I, D, M, A)) => Transfer_tokens(I, #DropListTypesData(D), M, A)

  rule #DropListTypesData(Pair L R) => Pair #DropListTypesData(L) #DropListTypesData(R)
  rule #DropListTypesData(Some D) => Some #DropListTypesData(D)
  rule #DropListTypesData(Left D) => Left #DropListTypesData(D)
  rule #DropListTypesData(Right D) => Right #DropListTypesData(D)

  rule #DropListTypesData(D) => D [owise] // Simple data.

  syntax KItem ::= #DropListTypes(KItem) [function]
 
  rule #DropListTypes(D:Data) => #DropListTypesData(D)
  rule #DropListTypes(O) => O [owise]

  rule #ConcreteArgToSemantics(#AnyAddress, address _) => #AnyAddress

  syntax K ::= #LiteralStackToSemantics(LiteralStack) [function]
  rule #LiteralStackToSemantics( { } ) => .
  rule #LiteralStackToSemantics( { L } ) => #LiteralStackToSemanticsAux(L, .List)

  syntax K ::= #LiteralStackToSemanticsAux(StackElementList, List) [function]

  rule #LiteralStackToSemanticsAux( Stack_elt T D ; Gs:StackElementList, L:List ) => 
       #LiteralStackToSemanticsAux( Gs, ListItem(#ConcreteArgToSemantics(D, T)) L)

  rule #LiteralStackToSemanticsAux( Stack_elt T D, L) =>
       #ListToKSeq(#ReverseList(ListItem(#ConcreteArgToSemantics(D, T)) L))

  syntax K ::= #OutputStackToSemantics(OutputStack) [function]
  rule #OutputStackToSemantics(L:LiteralStack) => #LiteralStackToSemantics(L)
  rule #OutputStackToSemantics(X:FailedStack) => X

  rule #GroupOrder(_:CodeGroup) => #GroupOrderMax
  rule #GroupOrder(_:OutputGroup) => #GroupOrderMax -Int 1
  rule #GroupOrder(_:InputGroup) => #GroupOrderMax -Int 2

  rule <k> #LoadGroups(input LS ; Gs => Gs) </k>
       <stack> . => #LiteralStackToSemantics(LS) </stack>

  syntax KItem ::= #VerifyOutput(K)

  rule <k> #LoadGroups(output Os ; Gs) => #LoadGroups(Gs) ~> #VerifyOutput(#OutputStackToSemantics(Os)) </k>

  rule <k> #LoadGroups(code C) => C ... </k>
  rule <k> #LoadGroups(code C ; _) => C ... </k>


  rule <k> #VerifyOutput(S1 ~> L => L) </k>
       <stack> S2 => . ... </stack>
       requires #DropListTypes(S1) ==K #DropListTypes(S2)

  rule <k> #VerifyOutput(#AnyAddress ~> L => L) </k>
       <stack> #Address(_) => . ... </stack>

  rule <k> #VerifyOutput(.) => . </k>
       <stack> . </stack>
       <returncode> _ => 0 </returncode> 

  syntax KItem ::= #FindVerifyOutput(K) [function]

  rule #FindVerifyOutput(#VerifyOutput(O) ~> _) => #VerifyOutput(O)
  rule #FindVerifyOutput(.) => #VerifyOutput(.)
  rule #FindVerifyOutput(_:KItem ~> Rs) => #FindVerifyOutput(Rs) [owise]

  rule <k> Aborted(_, _, Rk, _) => #FindVerifyOutput(Rk) ... </k>
endmodule
