```k
requires "unit-test-syntax.k"
requires "michelson.k"

module UNIT-TEST
  imports UNIT-TEST-SYNTAX
  imports MICHELSON

  syntax Data ::= List

  syntax Bool ::= #Matches(Data, Data) [function] // Expected, Actual

  rule #Matches(#Any, _) => true

  rule #Matches(D1, D2) => D1 ==K D2 [owise]

  rule #Matches(#List(L1, _), #List(L2, _)) => #Matches(L1, L2)

  rule #Matches(.List, .List) => true
  rule #Matches(ListItem(L1) Ls1, ListItem(L2) Ls2) => #Matches(L1, L2) andBool #Matches(Ls1, Ls2)

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

  rule #ConcreteArgToSemantics(#Any, _) => #Any

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

  rule <k> #VerifyOutput(S1 ~> L => L) </k>
       <stack> S2 => . ... </stack>
       requires #Matches(S1, S2)

  rule <k> #VerifyOutput(.) => . </k>
       <stack> . </stack>
       <returncode> _ => 0 </returncode> 

  syntax KItem ::= #FindVerifyOutput(K) [function]

  rule #FindVerifyOutput(#VerifyOutput(O) ~> _) => #VerifyOutput(O)
  rule #FindVerifyOutput(_:KItem ~> Rs) => #FindVerifyOutput(Rs) [owise]

  rule <k> Aborted(_, _, Rk, _) => #FindVerifyOutput(Rk) ... </k>
endmodule
```
