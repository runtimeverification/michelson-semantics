requires "../../michelson.k"

module VOTE
  imports MICHELSON

  rule <k>
       { 
           AMOUNT NoAnnotations ;
           PUSH NoAnnotations mutez .TypeAnnotationList .FieldAnnotationList 5000000 ;
           COMPARE NoAnnotations ;
           GT NoAnnotations ;
           IF NoAnnotations { FAIL NoAnnotations } { } ;
           DUP NoAnnotations ; DIP NoAnnotations { CDR NoAnnotations ; DUP NoAnnotations } ;
           CAR NoAnnotations ; DUP NoAnnotations ;
           DIP NoAnnotations {
               GET NoAnnotations ;
               ASSERT_SOME NoAnnotations ;
               PUSH NoAnnotations int .TypeAnnotationList .FieldAnnotationList 1 ;
               ADD NoAnnotations ;
               SOME NoAnnotations 
           } ; 
           UPDATE NoAnnotations ;
           NIL NoAnnotations operation .TypeAnnotationList .FieldAnnotationList ;
           PAIR NoAnnotations 
       } => . </k>
        <stack> Pair S:String (M:Map)[S <- I:Int] => Pair .List _ </stack>
        <paramtype> string .TypeAnnotationList .FieldAnnotationList  </paramtype>
        <storagetype> map .TypeAnnotationList .FieldAnnotationList string .TypeAnnotationList .FieldAnnotationList int .TypeAnnotationList .FieldAnnotationList </storagetype>
        <balance> #Mutez(0) </balance>
        <amount> #Mutez(A) </amount>
        <now> #Timestamp(0) </now>
        <myaddr> #Address("OwnAddr") </myaddr>
        <knownaddrs> .Map </knownaddrs>
        <sourceaddr> #Address("SourceAddr") </sourceaddr>
        <senderaddr> #Address("SenderAddr") </senderaddr>
        <chainid> #ChainId(0) </chainid>
        requires A >Int 5000000 

endmodule
