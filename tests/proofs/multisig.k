requires "lemmas.k"

module MULTISIG
  imports LEMMAS

  rule <k>
    input { Stack_elt pair .AnnotationList ( or .AnnotationList unit  .AnnotationList pair  .AnnotationList ( pair  .AnnotationList nat  .AnnotationList or  .AnnotationList ( lambda  .AnnotationList unit .AnnotationList list .AnnotationList operation .AnnotationList ) pair  .AnnotationList nat  .AnnotationList list  .AnnotationList key .AnnotationList ) list  .AnnotationList option .AnnotationList signature .AnnotationList ) pair .AnnotationList nat  .AnnotationList pair .AnnotationList nat  .AnnotationList list  .AnnotationList key .AnnotationList $IN ; .StackElementList } ;
    code { UNPAIR .AnnotationList ;
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
         } ;
    output { Stack_elt pair .AnnotationList ( list .AnnotationList operation .AnnotationList ) pair .AnnotationList nat  .AnnotationList pair .AnnotationList nat  .AnnotationList list  .AnnotationList key .AnnotationList $OUT ; .StackElementList }
    => .K
       </k>
endmodule
