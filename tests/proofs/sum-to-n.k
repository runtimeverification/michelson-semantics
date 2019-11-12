requires "michelson.k"

module SUM-TO-N
  imports MICHELSON

  rule <k>
       { LEFT nat ;
       LOOP_LEFT { DUP ;
                   DIP { CDR } ;
                   CAR ;
                   DUP ; 
                   DIP { ADD } ;
                   PUSH nat 1 ;
                   SWAP ;
                   SUB ;
                   ISNAT ;
                   IF_NONE { RIGHT (pair nat nat) } { PAIR ; LEFT nat } } ; NIL operation ; PAIR } => . </k>
        <stack> Pair A:Int B:Int => Pair .List (B +Int (A *Int (A +Int 1)) /Int 2)  </stack>
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
