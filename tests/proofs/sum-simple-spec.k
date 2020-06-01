requires "michelson.k"
requires "lemmas.k"

module VERIFICATION
  imports MICHELSON
  imports LEMMAS
endmodule

module SUM-SIMPLE-SPEC
  imports VERIFICATION

  rule <k> { PUSH .AnnotationList (int .AnnotationList) 2 ; 
             PUSH .AnnotationList (int .AnnotationList) I1:Int ; 
             EDIV .AnnotationList ; 
             IF_NONE .AnnotationList { UNIT .AnnotationList ; 
                                       FAILWITH .AnnotationList } 
                                     { CDR .AnnotationList ; 
                                       PUSH .AnnotationList (nat .AnnotationList) 1 ; 
                                       COMPARE .AnnotationList ; 
                                       EQ .AnnotationList } } ; 
            #AssumeTrue ;
           { PUSH .AnnotationList (int .AnnotationList) 2 ; 
             PUSH .AnnotationList (int .AnnotationList) I2:Int ; 
             EDIV .AnnotationList ; 
             IF_NONE .AnnotationList { UNIT .AnnotationList ; 
                                       FAILWITH .AnnotationList } 
                                     { CDR .AnnotationList ; 
                                       PUSH .AnnotationList (nat .AnnotationList) 0 ; 
                                       COMPARE .AnnotationList ; 
                                       EQ .AnnotationList } } ;
             #AssumeTrue ; 
           { PUSH .AnnotationList (int .AnnotationList) 2 ;
             PUSH .AnnotationList (int .AnnotationList) I1:Int ;
             PUSH .AnnotationList (int .AnnotationList) I2:Int ;
             ADD .AnnotationList ;
             EDIV .AnnotationList ;
             IF_NONE .AnnotationList { UNIT .AnnotationList ; 
                                       FAILWITH .AnnotationList } 
                                     { CDR .AnnotationList ; 
                                       PUSH .AnnotationList (nat .AnnotationList) 1 ; 
                                       COMPARE .AnnotationList ; 
                                       EQ .AnnotationList } } ; 
              #AssertTrue => . </k>
        <stack> . </stack>
        <paramtype> #NotSet  </paramtype>
        <paramvalue> #NoData </paramvalue>
        <storagetype> #NotSet </storagetype>
        <storagevalue> #NoData </storagevalue>
        <mybalance> #Mutez(0) </mybalance>
        <myamount> #Mutez(0) </myamount>
        <mynow> #Timestamp(0) </mynow>
        <myaddr> #Address("") </myaddr>
        <knownaddrs> .Map </knownaddrs>
        <sourceaddr> #Address("") </sourceaddr>
        <senderaddr> #Address("") </senderaddr>
        <mychainid> #ChainId(0x) </mychainid>
        <nonce> #Nonce(0) </nonce>
        <bigmaps> .Map </bigmaps>
        <returncode> 0 </returncode>
endmodule
