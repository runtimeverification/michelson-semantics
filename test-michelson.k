requires "test-michelson-syntax.k"
requires "michelson.k"
requires "unit-test.k"

module TEST-MICHELSON
  imports TEST-MICHELSON-SYNTAX
  imports UNIT-TEST
  imports MICHELSON

  rule <k> $Instructions { B } ; $Stack L ; => B </k>
     <stack> . => #ListToKSeq(L) </stack> [structural]

  rule <k> $Instructions { B } ; $Stack L ; OL ; => OL ~> B </k>
         <stack> . => #ListToKSeq(L) </stack> [structural]

  rule O:Optional ; OL => O ~> OL [structural]
  rule .OptionalGroup => . [structural]
  rule $Contracts CL => #ReadKnownAddrs(CL) [structural]
    
  rule <k> $ParamType T => . ... </k>
       <paramtype> _ => T </paramtype> [structural]

  syntax KItem ::= #TransformExpected(Data)
  syntax KItem ::= #VerifyStorage(Data)

  rule <k> P:Pgm $ExpectedOutput D ; => P ~> #TransformExpected(D) ... </k>

  rule <k> #TransformExpected(D) => #VerifyStorage(#ConcreteArgToSemantics(D, St)) ... </k>
       <storagetype> St </storagetype>

  rule <k> #VerifyStorage(D1) => . ... </k>
       <stack> Pair _ D2 </stack>
       requires D1 ==K D2
endmodule
