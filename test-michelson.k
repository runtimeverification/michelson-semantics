requires "test-michelson-syntax.k"
requires "michelson.k"

module TEST-MICHELSON
  imports TEST-MICHELSON-SYNTAX
  imports MICHELSON

  syntax List ::= ReverseList(List) [function]
  syntax List ::= ReverseListAux(List, List) [function]

  rule ReverseList(L) => ReverseListAux(L, .List)
  rule ReverseListAux(ListItem(L) L1, L2) => ReverseListAux(L1, ListItem(L) L2)
  rule ReverseListAux(.List, L) => L

  syntax K ::= ListToKSeq(List) [function]
  syntax K ::= ListToKSeqAux(List, K) [function]

  rule ListToKSeq(L) => ListToKSeqAux(ReverseList(L), .K)
  rule ListToKSeqAux(ListItem(O) L, S) => ListToKSeqAux(L, O ~> S)
  rule ListToKSeqAux(.List, S) => S

  rule <k> $Instructions B ; $Stack L ; => B </k>
     <stack> . => ListToKSeq(L) </stack> [structural]

  rule <k> $Instructions B ; $Stack L ; OL ; => OL ~> B </k>
         <stack> . => ListToKSeq(L) </stack> [structural]

  rule O:Optional ; OL => O ~> OL [structural]
  rule .OptionalGroup => . [structural]
  rule $Contracts CL => #ReadKnownAddrs(CL) [structural]
    
  rule <k> $ParamType T => . ... </k>
       <paramtype> _ => T </paramtype> [structural]
endmodule
