requires "lemmas.md"

module VERIFICATION
  imports LEMMAS
endmodule

module RETURN-SPEC
  imports VERIFICATION

  claim <k>
        {
          CDR .AnnotationList ;
          NIL .AnnotationList operation .AnnotationList ;
          AMOUNT .AnnotationList ;
          PUSH .AnnotationList mutez .AnnotationList 0 ;
          IFCMPEQ .AnnotationList { }
          {
             SOURCE .AnnotationList ;
             CONTRACT .AnnotationList unit .AnnotationList ;
             ASSERT_SOME .AnnotationList ;
             AMOUNT .AnnotationList ;
             UNIT .AnnotationList ;
             TRANSFER_TOKENS .AnnotationList ;
             CONS .AnnotationList
          } ;
          PAIR .AnnotationList
        } => .K </k>
        <stack> [ (pair unit unit) Pair Unit Unit ] ; .Stack => [ (pair (list operation) unit) Pair [ Transfer_tokens Unit #Mutez(A) #Address("SourceAddr") . %default ?_ ] ;; .InternalList Unit ] ; .Stack </stack>
        <paramtype> %default |-> unit .AnnotationList </paramtype>
        <storagetype> unit .AnnotationList </storagetype>
        <mybalance> #Mutez(0) </mybalance>
        <myamount> #Mutez(A) </myamount>
        <mynow> #Timestamp(0) </mynow>
        <myaddr> #Address("OwnAddr") </myaddr>
        <knownaddrs> #Address("SourceAddr") . %default |-> unit .AnnotationList </knownaddrs>
        <sourceaddr> #Address("SourceAddr") </sourceaddr>
        <senderaddr> #Address("SenderAddr") </senderaddr>
        <mychainid> #ChainId(_) </mychainid>
        <nonce> #Nonce(?_ => ?_) </nonce>
    requires A >Int 0


  claim <k>
        {
          CDR .AnnotationList ;
          NIL .AnnotationList operation .AnnotationList ;
          AMOUNT .AnnotationList ;
          PUSH .AnnotationList mutez .AnnotationList 0 ;
          IFCMPEQ .AnnotationList { }
          {
             SOURCE .AnnotationList ;
             CONTRACT .AnnotationList unit .AnnotationList ;
             ASSERT_SOME .AnnotationList ;
             AMOUNT .AnnotationList ;
             UNIT .AnnotationList ;
             TRANSFER_TOKENS .AnnotationList ;
             CONS .AnnotationList
          } ;
          PAIR .AnnotationList
        } => .K </k>
        <stack> [ (pair unit unit) Pair Unit Unit ] ; .Stack => [ (pair (list operation) unit) Pair .InternalList Unit ] ; .Stack </stack>
        <paramtype> %default |-> unit .AnnotationList </paramtype>
        <storagetype> unit .AnnotationList </storagetype>
        <mybalance> #Mutez(0) </mybalance>
        <myamount> #Mutez(0) </myamount>
        <mynow> #Timestamp(0) </mynow>
        <myaddr> #Address("OwnAddr") </myaddr>
        <knownaddrs> #Address("SourceAddr") . %default |-> unit .AnnotationList </knownaddrs>
        <sourceaddr> #Address("SourceAddr") </sourceaddr>
        <senderaddr> #Address("SenderAddr") </senderaddr>
        <mychainid> #ChainId(_) </mychainid>
endmodule
