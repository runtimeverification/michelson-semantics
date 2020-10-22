requires "lemmas.k"

module MULTISIG
  imports LEMMAS

  rule <k> UNPAIR .AnnotationList ;
           IF_LEFT .AnnotationList { DROP .AnnotationList ;
                                     NIL .AnnotationList operation .AnnotationList ;
                                     PAIR .AnnotationList
                                   }
                                   { PUSH .AnnotationList mutez .AnnotationList 0 ;
                                     AMOUNT .AnnotationList ;
                                     ASSERT_CMPEQ .AnnotationList ;
                                     SWAP .AnnotationList ;
                                     DUP .AnnotationList ;
                                     DIP .AnnotationList { SWAP .AnnotationList } ;
                                     DIP .AnnotationList { UNPAIR .AnnotationList ;
                                     DUP .AnnotationList ;
                                     SELF .AnnotationList ;
                                     ADDRESS .AnnotationList ;
                                     CHAIN_ID .AnnotationList ;
                                     PAIR .AnnotationList ;
                                     PAIR .AnnotationList ;
                                     PACK .AnnotationList ;
                                     DIP .AnnotationList { UNPAIR  .AnnotationList ;
                                                           DIP .AnnotationList { SWAP .AnnotationList }
                                                         } ;
                                     SWAP .AnnotationList } ;
                                     UNPAIR  .AnnotationList ;
                                     DIP .AnnotationList { SWAP .AnnotationList } ;
                                     ASSERT_CMPEQ .AnnotationList ;
                                     DIP .AnnotationList { SWAP .AnnotationList } ;
                                     UNPAIR   .AnnotationList ;
                                     DIP .AnnotationList { PUSH  .AnnotationList nat .AnnotationList 0 ;
                                                           SWAP .AnnotationList ;
                                                           ITER .AnnotationList { DIP .AnnotationList { SWAP .AnnotationList } ;
                                                                                  SWAP .AnnotationList ;
                                                                                  IF_CONS .AnnotationList { IF_SOME .AnnotationList { SWAP .AnnotationList ;
                                                                                                                                      DIP .AnnotationList { SWAP .AnnotationList ;
                                                                                                                                                            DIP .AnnotationList 2 { DUP .AnnotationList 2 } ;
                                                                                                                                                            { DUP .AnnotationList 3 ;
                                                                                                                                                              DIP .AnnotationList { CHECK_SIGNATURE .AnnotationList } ;
                                                                                                                                                              SWAP .AnnotationList ;
                                                                                                                                                              IF .AnnotationList { DROP .AnnotationList } { FAILWITH .AnnotationList }
                                                                                                                                                            } ;
                                                                                                                                                            PUSH .AnnotationList nat .AnnotationList 1 ;
                                                                                                                                                            ADD  .AnnotationList
                                                                                                                                                          }
                                                                                                                                    }

                                                                                                                                    { SWAP .AnnotationList ; DROP .AnnotationList }
                                                                                                          } { FAIL .AnnotationList } ;
                                                                                  SWAP .AnnotationList }
                                                         } ;
                                     ASSERT_CMPLE .AnnotationList ;
                                     IF_CONS .AnnotationList { FAIL .AnnotationList } { } ;
                                     DROP .AnnotationList ;
                                     DIP .AnnotationList { UNPAIR .AnnotationList ;
                                                           PUSH .AnnotationList nat .AnnotationList 1 ;
                                                           ADD  .AnnotationList ;
                                                           PAIR .AnnotationList
                                                         } ;
                                     IF_LEFT .AnnotationList { UNIT .AnnotationList ; EXEC .AnnotationList }
                                                             { DIP .AnnotationList { CAR .AnnotationList } ;
                                                               SWAP .AnnotationList ;
                                                               PAIR .AnnotationList ;
                                                               NIL .AnnotationList operation .AnnotationList
                                                             } ;
                                     PAIR .AnnotationList
            }
    => .K
       </k>
       <paramtype> (or .AnnotationList unit .AnnotationList
                      pair .AnnotationList (pair .AnnotationList nat .AnnotationList
                                 (or .AnnotationList (lambda .AnnotationList unit .AnnotationList (list .AnnotationList operation .AnnotationList ))
                                     pair .AnnotationList nat .AnnotationList
                                          (list .AnnotationList key .AnnotationList )))
                           (list .AnnotationList (option .AnnotationList signature .AnnotationList )))
       </paramtype>

       <paramvalue> #NoData </paramvalue>
       <storagetype> #NotSet </storagetype>
       <storagevalue> #NoData </storagevalue>
       <mybalance> #Mutez(0) </mybalance>
       <myamount> #Mutez(0) </myamount>
       <mynow> #Timestamp(0) </mynow>
       <myaddr> #Address("InvalidMyAddr") </myaddr>
       <knownaddrs> .Map </knownaddrs>
       <sourceaddr> #Address("InvalidSourceAddr") </sourceaddr>
       <senderaddr> #Address("InvalidSenderAddr") </senderaddr>
       <mychainid> #ChainId(.Bytes) </mychainid>
       <nonce> #Nonce(0) </nonce>
       <bigmaps> .Map </bigmaps>
       <script> #NoData </script>
       <stack> [ pair (or unit                                           // default operation
                          pair (pair nat                                 // counter
                                     (or (lambda unit (list operation))  // requested action
                                         pair nat                        // change keys - new threshold
                                              (list key)))               // change keys - new key list
                               (list (option signature)))
                      (pair nat                                          // stored counter
                            (pair nat                                    // threshold
                                  (list key)))                           // list of keys
                // ---------------------------------------------------------------------
                 Pair
                   (Right
                     (Pair
                       (Pair Count:Int
                             (Left #Lambda(unit, list operation, _:Block)))
                       SigList:List))
                   (Pair Count:Int (Pair Threshold:Int KeyList:List)) ] ; .Stack
            // -----------------------------------------------------------------
            => [ pair (list operation) (pair nat (pair nat (list key))) ?_:Pair ] ; .Stack
       </stack>
       <returncode> 1 </returncode>
       <assumeFailed> false </assumeFailed>
       <trace> .K </trace>
  requires isValue(list key,              KeyList)
   andBool isValue(list option signature, SigList)

       // <inputstack> { .StackElementList } </inputstack>
       // <expected> { .StackElementList } </expected>
       // <cutpoints> .Set </cutpoints>
       // <pre> .BlockList </pre>
       // <post> .BlockList </post>
       // <invs> .Map </invs>
       // <symbols> .Map </symbols>
endmodule

