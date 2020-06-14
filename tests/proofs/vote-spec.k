requires "unit-test.md"
requires "lemmas.k"

module VERIFICATION
  imports UNIT-TEST
  imports LEMMAS
endmodule

module VOTE-SPEC
  imports VERIFICATION
  
  rule <k>
         storage_value ((M:Map)[S:String <- I:Int]) ;
         parameter_value S ;
         amount A:Int ;
         contract { 
           storage (map .AnnotationList (string .AnnotationList) (int .AnnotationList)) ;
           parameter (string .AnnotationList) ;
           code { 
             AMOUNT .AnnotationList ;
             PUSH .AnnotationList mutez .AnnotationList 5000000 ;
             COMPARE .AnnotationList ;
             GT .AnnotationList ;
             IF .AnnotationList { UNIT .AnnotationList ; FAILWITH .AnnotationList } { } ;
             DUP .AnnotationList ; DIP .AnnotationList { CDR .AnnotationList ; DUP .AnnotationList } ;
             CAR .AnnotationList ; DUP .AnnotationList ;
             DIP .AnnotationList {
                 GET .AnnotationList ;
                 IF_NONE .AnnotationList { 
                   UNIT .AnnotationList ; 
                   FAILWITH .AnnotationList } {} ; // ASSERT_SOME .AnnotationList ;
                 PUSH .AnnotationList int .AnnotationList 1 ;
                 ADD .AnnotationList ;
                 SOME .AnnotationList 
             } ; 
             UPDATE .AnnotationList ;
             NIL .AnnotationList operation .AnnotationList ;
             PAIR .AnnotationList } } => . </k>
        <stack> . => (Pair #List(.List, operation .AnnotationList) (M[S <- (1 +Int I)])) </stack>
        <paramtype> #NotSet => ?_ </paramtype>
        <paramvalue> #NoData => ?_ </paramvalue>
        <storagetype> #NotSet => ?_  </storagetype>
        <storagevalue> #NoData => ?_ </storagevalue>
        <mybalance> #Mutez(0) => ?_ </mybalance>
        <myamount> #Mutez(0) => #Mutez(A) </myamount>
        <mynow> #Timestamp(0) => ?_ </mynow>
        <myaddr> #Address("InvalidSelfAddr") => ?_ </myaddr>
        <knownaddrs> .Map => ?_ </knownaddrs>
        <sourceaddr> #Address("InvalidSourceAddr") => ?_ </sourceaddr>
        <senderaddr> #Address("InvalidSenderAddr") => ?_ </senderaddr>
        <mychainid> #ChainId(0x) => ?_ </mychainid>
        <nonce> #Nonce(0) => ?_ </nonce>
        <bigmaps> .Map => ?_ </bigmaps>
        <returncode> 1 => ?_ </returncode>
        requires A >Int 5000000 andBool A <Int 9223372036854775808

  rule <k>
         storage_value M:Map ;
         parameter_value S:String ;
         amount A:Int ;
         contract { 
           storage (map .AnnotationList (string .AnnotationList) (int .AnnotationList)) ;
           parameter (string .AnnotationList) ;
           code { 
             AMOUNT .AnnotationList ;
             PUSH .AnnotationList mutez .AnnotationList 5000000 ;
             COMPARE .AnnotationList ;
             GT .AnnotationList ;
             IF .AnnotationList { UNIT .AnnotationList ; FAILWITH .AnnotationList } { } ;
             DUP .AnnotationList ; DIP .AnnotationList { CDR .AnnotationList ; DUP .AnnotationList } ;
             CAR .AnnotationList ; DUP .AnnotationList ;
             DIP .AnnotationList {
                 GET .AnnotationList ;
                 IF_NONE .AnnotationList { 
                   UNIT .AnnotationList ; 
                   FAILWITH .AnnotationList } {} ; // ASSERT_SOME .AnnotationList ;
                 PUSH .AnnotationList int .AnnotationList 1 ;
                 ADD .AnnotationList ;
                 SOME .AnnotationList 
             } ; 
             UPDATE .AnnotationList ;
             NIL .AnnotationList operation .AnnotationList ;
             PAIR .AnnotationList } } => #FindVerifyOutput(., Aborted(?_, ?_, ?_, ?_)) </k>
        <stack> . => ?_ </stack>
        <paramtype> #NotSet => ?_ </paramtype>
        <paramvalue> #NoData => ?_ </paramvalue>
        <storagetype> #NotSet => ?_  </storagetype>
        <storagevalue> #NoData => ?_ </storagevalue>
        <mybalance> #Mutez(0) => ?_ </mybalance>
        <myamount> #Mutez(0) => #Mutez(A) </myamount>
        <mynow> #Timestamp(0) => ?_ </mynow>
        <myaddr> #Address("InvalidSelfAddr") => ?_ </myaddr>
        <knownaddrs> .Map => ?_ </knownaddrs>
        <sourceaddr> #Address("InvalidSourceAddr") => ?_ </sourceaddr>
        <senderaddr> #Address("InvalidSenderAddr") => ?_ </senderaddr>
        <mychainid> #ChainId(0x) => ?_ </mychainid>
        <nonce> #Nonce(0) => ?_ </nonce>
        <bigmaps> .Map => ?_ </bigmaps>
        <returncode> 1 => ?_ </returncode>
        requires 0 <=Int A andBool 5000000 >Int A

  rule <k>
         storage_value M:Map ;
         parameter_value S:String ;
         amount A:Int ;
         contract { 
           storage (map .AnnotationList (string .AnnotationList) (int .AnnotationList)) ;
           parameter (string .AnnotationList) ;
           code { 
             AMOUNT .AnnotationList ;
             PUSH .AnnotationList mutez .AnnotationList 5000000 ;
             COMPARE .AnnotationList ;
             GT .AnnotationList ;
             IF .AnnotationList { UNIT .AnnotationList ; FAILWITH .AnnotationList } { } ;
             DUP .AnnotationList ; DIP .AnnotationList { CDR .AnnotationList ; DUP .AnnotationList } ;
             CAR .AnnotationList ; DUP .AnnotationList ;
             DIP .AnnotationList {
                 GET .AnnotationList ;
                 IF_NONE .AnnotationList { 
                   UNIT .AnnotationList ; 
                   FAILWITH .AnnotationList } {} ; // ASSERT_SOME .AnnotationList ;
                 PUSH .AnnotationList int .AnnotationList 1 ;
                 ADD .AnnotationList ;
                 SOME .AnnotationList 
             } ; 
             UPDATE .AnnotationList ;
             NIL .AnnotationList operation .AnnotationList ;
             PAIR .AnnotationList } } => #FindVerifyOutput(., Aborted(?_, ?_, ?_, ?_)) </k>
        <stack> . => ?_ </stack>
        <paramtype> #NotSet => ?_ </paramtype>
        <paramvalue> #NoData => ?_ </paramvalue>
        <storagetype> #NotSet => ?_  </storagetype>
        <storagevalue> #NoData => ?_ </storagevalue>
        <mybalance> #Mutez(0) => ?_ </mybalance>
        <myamount> #Mutez(0) => #Mutez(A) </myamount>
        <mynow> #Timestamp(0) => ?_ </mynow>
        <myaddr> #Address("InvalidSelfAddr") => ?_ </myaddr>
        <knownaddrs> .Map => ?_ </knownaddrs>
        <sourceaddr> #Address("InvalidSourceAddr") => ?_ </sourceaddr>
        <senderaddr> #Address("InvalidSenderAddr") => ?_ </senderaddr>
        <mychainid> #ChainId(0x) => ?_ </mychainid>
        <nonce> #Nonce(0) => ?_ </nonce>
        <bigmaps> .Map => ?_ </bigmaps>
        <returncode> 1 => ?_ </returncode>
        requires A >Int 5000000 andBool A <Int 9223372036854775808 andBool notBool(S in_keys(M))
endmodule
