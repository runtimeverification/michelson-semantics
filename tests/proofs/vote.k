requires "../../michelson.k"

module LEMMAS
  imports MICHELSON
 
  rule S:String in_keys(M:Map [ S:String <- I:Int]) => true

  rule #LookupMap(M:Map [S:String <- I:Int], S) => Some I

endmodule 

module VOTE
  imports MICHELSON
  imports LEMMAS
  
  rule <k>
       { 
           AMOUNT .AnnotationList ;
           PUSH .AnnotationList mutez .AnnotationList 5000000 ;
           COMPARE .AnnotationList ;
           GT .AnnotationList ;
           IF .AnnotationList { FAIL .AnnotationList } { } ;
           DUP .AnnotationList ; DIP .AnnotationList { CDR .AnnotationList ; DUP .AnnotationList } ;
           CAR .AnnotationList ; DUP .AnnotationList ;
           DIP .AnnotationList {
               GET .AnnotationList ;
               ASSERT_SOME .AnnotationList ;
               PUSH .AnnotationList int .AnnotationList 1 ;
               ADD .AnnotationList ;
               SOME .AnnotationList 
           } ; 
           UPDATE .AnnotationList ;
           NIL .AnnotationList operation .AnnotationList ;
           PAIR .AnnotationList 
       } => . </k>
        <stack> Pair S:String (M:Map)[S <- I:Int] => 
                Pair .List M[S <- I +Int 1]  </stack>
        <paramtype> string .AnnotationList  </paramtype>
        <storagetype> map .AnnotationList string .AnnotationList int .AnnotationList </storagetype>
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
