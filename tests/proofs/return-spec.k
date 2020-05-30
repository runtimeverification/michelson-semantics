requires "michelson.k"

module VERIFICATION
  imports MICHELSON
endmodule

module RETURN-SPEC
  imports VERIFICATION

  rule <k>
       { 
         CDR NoAnnotations ;
         NIL NoAnnotations operation .TypeAnnotationList .FieldAnnotationList ;
         AMOUNT NoAnnotations ;
         PUSH NoAnnotations mutez .TypeAnnotationList .FieldAnnotationList 0 ; 
         IFCMPEQ NoAnnotations { }
         {
            SOURCE NoAnnotations ;
            CONTRACT NoAnnotations unit .TypeAnnotationList .FieldAnnotationList ;
            ASSERT_SOME NoAnnotations ;
            AMOUNT NoAnnotations ;
            UNIT NoAnnotations ;
            TRANSFER_TOKENS NoAnnotations ;
            CONS NoAnnotations 
         } ;
         PAIR NoAnnotations 
       } => . </k>
        <stack> Pair Unit Unit => Pair ListItem(#TransferTokens(Unit, #Mutez(A), #Contract(#Address("SourceAddr"), unit .TypeAnnotationList .FieldAnnotationList))) Unit </stack>
        <paramtype> unit .TypeAnnotationList .FieldAnnotationList </paramtype>
        <storagetype> unit .TypeAnnotationList .FieldAnnotationList </storagetype>
        <balance> #Mutez(0) </balance>
        <amount> #Mutez(A) </amount>
        <now> #Timestamp(0) </now>
        <myaddr> #Address("OwnAddr") </myaddr>
        <knownaddrs> #Address("SourceAddr") |-> #Contract(#Address("SourceAddr"), unit .TypeAnnotationList .FieldAnnotationList) </knownaddrs>
        <sourceaddr> #Address("SourceAddr") </sourceaddr>
        <senderaddr> #Address("SenderAddr") </senderaddr>
        <chainid> #ChainId(0) </chainid>
        requires A >Int 0 


  rule <k>
       { 
         CDR NoAnnotations ;
         NIL NoAnnotations operation .TypeAnnotationList .FieldAnnotationList ;
         AMOUNT NoAnnotations ;
         PUSH NoAnnotations mutez .TypeAnnotationList .FieldAnnotationList 0 ; 
         IFCMPEQ NoAnnotations { }
         {
            SOURCE NoAnnotations ;
            CONTRACT NoAnnotations unit .TypeAnnotationList .FieldAnnotationList ;
            ASSERT_SOME NoAnnotations ;
            AMOUNT NoAnnotations ;
            UNIT NoAnnotations ;
            TRANSFER_TOKENS NoAnnotations ;
            CONS NoAnnotations 
         } ;
         PAIR NoAnnotations 
       } => . </k>
        <stack> Pair Unit Unit => Pair .List Unit </stack>
        <paramtype> unit .TypeAnnotationList .FieldAnnotationList </paramtype>
        <storagetype> unit .TypeAnnotationList .FieldAnnotationList </storagetype>
        <balance> #Mutez(0) </balance>
        <amount> #Mutez(0) </amount>
        <now> #Timestamp(0) </now>
        <myaddr> #Address("OwnAddr") </myaddr>
        <knownaddrs> #Address("SourceAddr") |-> #Contract(#Address("SourceAddr"), unit .TypeAnnotationList .FieldAnnotationList) </knownaddrs>
        <sourceaddr> #Address("SourceAddr") </sourceaddr>
        <senderaddr> #Address("SenderAddr") </senderaddr>
        <chainid> #ChainId(0) </chainid>
endmodule
