requires "michelson/michelson.md"

module VERIFICATION
  imports MICHELSON
endmodule

module SUM-TO-N-SPEC
  imports VERIFICATION

  rule <k> LEFT .AnnotationList nat => .K </k>
       <stack> Pair A:Int B:Int => Left Pair A B </stack>
       <paramtype> nat </paramtype>
       <paramvalue> A </paramvalue>
       <storagetype> nat </storagetype>
       <storagevalue> B </storagevalue>
       <mybalance> #Mutez(0) </mybalance>
       <myamount> #Mutez(0) </myamount>
       <mynow> #Timestamp(0) </mynow>
       <myaddr> #Address(0) </myaddr>
       <knownaddrs> .Map </knownaddrs>
       <sourceaddr> #Address(0) </sourceaddr>
       <senderaddr> #Address(0) </senderaddr>
       <mychainid> #ChainId(0) </mychainid>
       <nonce> #Nonce(0) </nonce>
       <returncode> 0 </returncode>
       requires A >=Int 0 andBool B >=Int 0

/*  rule <k>
       LOOP_LEFT { DUP ;
                   DIP { CDR } ;
                   CAR ;
                   DUP ; 
                   DIP { ADD } ;
                   PUSH nat 1 ;
                   SWAP ;
                   SUB ;
                   ISNAT ;
                   IF_NONE { RIGHT pair nat nat } { PAIR ; LEFT nat } }  => . </k>
        <stack> Left Pair A:Int B:Int => (B +Int ((A *Int (A +Int 1)) /Int 2)) </stack>
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

  rule <k> NIL operation ; PAIR => . ... </k>
       <stack> I:Int => Pair .List I </stack>
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
       requires A >=Int 0 andBool B >=Int 0*/
endmodule
