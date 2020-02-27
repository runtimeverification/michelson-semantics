requires "unit-test-syntax.k"
requires "michelson.k"

module UNIT-TEST
  imports UNIT-TEST-SYNTAX
  imports MICHELSON

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
  rule #OutputStackToSemantics( ( Failed D ) ) => #FailedStack(D)

  rule #GroupOrder(_:CodeGroup) => #GroupOrderMax
  rule #GroupOrder(_:OutputGroup) => #GroupOrderMax -Int 1
  rule #GroupOrder(_:InputGroup) => #GroupOrderMax -Int 2

  rule <k> #LoadGroups(input LS ; Gs => Gs) </k>
       <stack> . => #LiteralStackToSemantics(LS) </stack>

  syntax KItem ::= #VerifyOutput(K)

  rule <k> #LoadGroups(output Os ; Gs) => #LoadGroups(Gs) ~> #VerifyOutput(#OutputStackToSemantics(Os)) </k>

  rule <k> #LoadGroups(code C) => C ... </k>
       <returncode> _ => 1 </returncode>

  rule <k> #VerifyOutput(S:KItem ~> L => L) </k>
       <stack> S => . ... </stack>

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
