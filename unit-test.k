requires "unit-test-syntax.k"
requires "michelson.k"

module UNIT-TEST
  imports UNIT-TEST-SYNTAX
  imports MICHELSON

  syntax Groups ::= "#EmptyGroups"
  syntax Group ::= "#NoGroup"

  syntax KItem ::= #LoadGroups(Groups, Group, Group)

  rule <k> G:Groups => #LoadGroups(G, #NoGroup, #NoGroup) </k>
       <returncode> _ => 1 </returncode>

  rule <k> #LoadGroups(G ; Gs, C, O) => G ~> #LoadGroups(Gs, C, O) </k>
  rule <k> #LoadGroups(G:Group, C, O) => G ~> #LoadGroups(#EmptyGroups, C, O) </k>

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

  rule <k> input LS => . ... </k>
       <stack> . => #LiteralStackToSemantics(LS) </stack>

  rule <k> C:CodeGroup ~> #LoadGroups(Gs, #NoGroup, O) => #LoadGroups(Gs, C, O) ... </k>

  rule <k> O:OutputGroup ~> #LoadGroups(Gs, C, #NoGroup) => #LoadGroups(Gs, C, O) ... </k>

  rule <k> now I => . ... </k>
       <now> #Timestamp(0) => #Timestamp(I) </now> requires I >=Int 0

  rule <k> sender S => . ... </k> 
       <senderaddr> #Address("InvalidSenderAddr" => S) </senderaddr>

  rule <k> source S => . ... </k> 
       <sourceaddr> #Address("InvalidSourceAddr" => S) </sourceaddr>

  rule <k> chain_id M => . ... </k> 
       <chainid> #ChainId(0 => #MBytesToInt(M)) </chainid>

  rule <k> self S => . ... </k>
       <myaddr> #Address("InvalidMyAddr" => S) </myaddr>

  rule <k> parameter T => . ... </k>
       <paramtype> #NotSet => T </paramtype>

  rule <k> amount I => . ... </k>
       <amount> #Mutez(0 => I) </amount> 
       requires #IsLegalMutezValue(I)

  rule <k> balance I => . ... </k>
       <balance> #Mutez(0 => I) </balance> 
       requires #IsLegalMutezValue(I)

  rule <k> other_contracts { } => . ... </k> 

  rule <k> other_contracts { Elt A T } => .  ... </k>
       <knownaddrs> M => M[#Address(A) <- #Contract(#Address(A), T)] </knownaddrs>

  rule <k> other_contracts { Elt A T ; Rs } => other_contracts { Rs }  ... </k>
       <knownaddrs> M => M[#Address(A) <- #Contract(#Address(A), T)] </knownaddrs>

  syntax KItem ::= #VerifyOutput(K)

  rule <k> #LoadGroups(#EmptyGroups, code C, output O) => C ~> #VerifyOutput(#OutputStackToSemantics(O)) </k>

  rule <k> #VerifyOutput(S:KItem ~> L => L) </k>
       <stack> S => . ... </stack>

  rule <k> #VerifyOutput(.) => . </k>
       <stack> . </stack>
       <returncode> _ => 0 </returncode> 

  syntax KItem ::= #FindVerifyOutput(K) [function]

  rule #FindVerifyOutput(#VerifyOutput(O) ~> _) => #VerifyOutput(O)
  rule #FindVerifyOutput(.) => #VerifyOutput(.)
  rule #FindVerifyOutput(_:KItem ~> Rs) => #FindVerifyOutput(Rs) [owise]

  rule <k> Aborted(_, _, Rk, _) => #FindVerifyOutput(Rk) ... </k>
  
endmodule
