requires "michelson.k"

module SUM-SIMPLE
  imports MICHELSON

  rule <k>
       {  DUP ;
          DIP { CDR } ;
          CAR ;
          ADD ;
          NIL operation ; PAIR } => . </k>
        <stack> Pair A:Int B:Int => Pair .List (A +Int B)  </stack>
        <paramtype> nat </paramtype>
        <storagetype> nat </storagetype>
        <balance> #Mutez(0) </balance>
        <amount> #Mutez(0) </amount>
        <now> #Timestamp(0) </now>
        <myaddr> #Address(0) </myaddr>
        <knownaddrs> .Map </knownaddrs>
        <sourceaddr> #Address(0) </sourceaddr>
        <senderaddr> #Address(0) </senderaddr>
        <chainid> #ChainId(0) </chainid>
        requires A >=Int 0 andBool B >=Int 0
endmodule
