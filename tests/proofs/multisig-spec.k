requires "lemmas.md"

module VERIFICATION
  import LEMMAS

  // For now, we only verify the happy path -- we assume that all signatures are valid.
  rule <k> CHECK_SIGNATURE _A => . ... </k>
       <stack> [ key #Key(_) ]
             ; [ signature #Signature(_) ]
             ; [ bytes #Packed(_,_) ]
             ; SS
            => [ bool true ]
             ; SS
       </stack>

  // This function is used to show that each iteration maintains the invariant
  // VerifiedKeys + numValidSigs(RemainingSigs, RemainingKeys) >= Threshold

  syntax Int ::= numValidSigs(sigs: InternalList, keys: InternalList) [function, smtlib(numValidSigs)]
  rule numValidSigs([ Some #Signature(_) ] ;; Sigs, [ #Key(_) ] ;; Keys) => 1 +Int numValidSigs(Sigs, Keys) [simplification, smt-lemma]
  rule numValidSigs([ None ] ;; Sigs, _K ;; Keys) => numValidSigs(Sigs, Keys) [simplification, smt-lemma]
  rule numValidSigs(_, .InternalList) => 0 [simplification, smt-lemma]

  // Additional lemmas about this recursive function are needed.
  rule numValidSigs(_, _) >=Int 0 => true [simplification, smt-lemma]
  rule N:Int +Int ( 1 +Int numValidSigs(L0,L)) => (N:Int +Int 1) +Int numValidSigs(L0,L) [simplification, smt-lemma]

endmodule

module MULTISIG
  imports VERIFICATION

  // Main claim
  // ----------

  // The contract aborts if (These locations are marked in the source code):
  //
  // (1) if a non-zero amount is passed to the contract
  // (2) if any of the first size(KeyList) signatures are invalid
  // (3) if the size(SigList) <= size(KeyList)
  // (4) if the number of signatures verified is less than Threshold
  // (5) if extra signatures were passed to the contract

  // (1) is handeled by side condition (a)
  // (3), (5) by side condition (b)
  // (4) by side condition (c)

  // (2) is taken care of by the rule for CHECK_SIGNATURE in the VERIFICATION module.

  claim <k> UNPAIR .AnnotationList ;
           IF_LEFT .AnnotationList { DROP .AnnotationList ;
                                     NIL .AnnotationList operation .AnnotationList ;
                                     PAIR .AnnotationList
                                   }
                                   { PUSH .AnnotationList mutez .AnnotationList 0 ;
                                     AMOUNT .AnnotationList ;
                                     ASSERT_CMPEQ .AnnotationList ; // ---------------------------------------------------------------- (1)
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
                                     DIP .AnnotationList
                                     { UNPAIR  .AnnotationList ;
                                       DIP .AnnotationList { SWAP .AnnotationList }
                                     } ;
                                     SWAP .AnnotationList } ;
                                     UNPAIR  .AnnotationList ;
                                     DIP .AnnotationList { SWAP .AnnotationList } ;
                                     ASSERT_CMPEQ .AnnotationList ;
                                     DIP .AnnotationList { SWAP .AnnotationList } ;
                                     UNPAIR   .AnnotationList ;
                                     DIP .AnnotationList
                                     { PUSH  .AnnotationList nat .AnnotationList 0 ;
                                       SWAP .AnnotationList ;
                                       ITER .AnnotationList
                                       {
                                         DIP .AnnotationList { SWAP .AnnotationList } ;
                                         SWAP .AnnotationList ;
                                         IF_CONS .AnnotationList
                                         { IF_SOME .AnnotationList
                                           { SWAP .AnnotationList ;
                                             DIP .AnnotationList
                                             { SWAP .AnnotationList ;
                                               DIP .AnnotationList 2 { DUP .AnnotationList 2 } ;
                                               { DUP .AnnotationList 3 ;
                                                 DIP .AnnotationList { CHECK_SIGNATURE .AnnotationList } ;
                                                 SWAP .AnnotationList ;
                                                 IF .AnnotationList { DROP .AnnotationList } { FAILWITH .AnnotationList } // ------ (2)
                                                } ;
                                               PUSH .AnnotationList nat .AnnotationList 1 ;
                                               ADD  .AnnotationList
                                             }
                                           }
                                           { SWAP .AnnotationList ; DROP .AnnotationList }
                                         }
                                         { FAIL .AnnotationList } ; // ------------------------------------------------------------ (3)
                                         SWAP .AnnotationList
                                       }
                                     } ;
                                     ASSERT_CMPLE .AnnotationList ; // ------------------------------------------------------------ (4)
                                     IF_CONS .AnnotationList { FAIL .AnnotationList } { } ; // ------------------------------------ (5)
                                     DROP .AnnotationList ;
                                     DIP .AnnotationList
                                     { UNPAIR .AnnotationList ;
                                       PUSH .AnnotationList nat .AnnotationList 1 ;
                                       ADD  .AnnotationList ;
                                       PAIR .AnnotationList
                                     } ;
                                     IF_LEFT .AnnotationList
                                     { UNIT .AnnotationList ; EXEC .AnnotationList }
                                     { DIP .AnnotationList { CAR .AnnotationList } ;
                                       SWAP .AnnotationList ;
                                       PAIR .AnnotationList ;
                                       NIL .AnnotationList operation .AnnotationList
                                     } ;
                                     PAIR .AnnotationList
                                }
      => .K
    </k>
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
                          (Left #Lambda(unit, list operation, { #Uninterpreted(Id, unit, list operation) })))
                    SigList:InternalList))
                (Pair Count:Int (Pair Threshold:Int KeyList:InternalList)) ] ; .Stack
         // -----------------------------------------------------------------
         => [ pair ( list operation ) pair nat pair nat list key Pair uninterpreted ( Id , Unit ) Pair Count +Int 1 Pair Threshold KeyList:InternalList ] ; .Stack
    </stack>
    <myamount> #Mutez(Amount:Int) </myamount>
    <paramtype> (or .AnnotationList unit .AnnotationList
                   pair .AnnotationList (pair .AnnotationList nat .AnnotationList
                              (or .AnnotationList (lambda .AnnotationList unit .AnnotationList (list .AnnotationList operation .AnnotationList ))
                                  pair .AnnotationList nat .AnnotationList
                                       (list .AnnotationList key .AnnotationList )))
                        (list .AnnotationList (option .AnnotationList signature .AnnotationList )))
    </paramtype>
    requires Amount ==Int 0  // --------------------------------------------------------------------------------------------------- (a)
     andBool size(SigList) ==Int size(KeyList)  // -------------------------------------------------------------------------------- (b)
     andBool numValidSigs(SigList, KeyList) >=Int Threshold // -------------------------------------------------------------------- (c)

  // Circularity
  // -----------

  // This circularity is needed to show that the invariant
  // `VerifiedKeys +Int size(KeyList) >=Int ?VerifiedKeysFinal`
  // is preserved by the loop.

  claim <k> ITER .AnnotationList
            {
              DIP .AnnotationList { SWAP .AnnotationList } ;
              SWAP .AnnotationList ;
              IF_CONS .AnnotationList
              { IF_SOME .AnnotationList
                { SWAP .AnnotationList ;
                  DIP .AnnotationList { SWAP .AnnotationList ;
                                        DIP .AnnotationList 2 { DUP .AnnotationList 2 } ;
                                        { DUP .AnnotationList 3 ;
                                          DIP .AnnotationList { CHECK_SIGNATURE .AnnotationList } ;
                                          SWAP .AnnotationList ;
                                          IF .AnnotationList { DROP .AnnotationList } { FAILWITH .AnnotationList } // ---- (1)
                                         } ;
                                        PUSH .AnnotationList nat .AnnotationList 1 ;
                                        ADD  .AnnotationList
                                      }
                }
                { SWAP .AnnotationList ; DROP .AnnotationList }
              }
              { FAIL .AnnotationList } ;
              SWAP .AnnotationList
            } => .K
            ...
        </k>
        <stack> [ list key KeyList:InternalList ]
              ; [ nat VerifiedKeys ]
              ; [ list option signature SigList:InternalList ]
              ; [ bytes #Packed ( pair ( pair chain_id address ) pair nat or ( lambda unit list operation ) pair nat list key , Pair Pair #ChainId ( ChainId ) MyAddr:Address Pair Count:Int Left #Lambda ( unit , list operation , { #Uninterpreted(Id, unit, list operation) } ) ) ]
              ; [ or ( lambda unit list operation ) pair nat list key Left #Lambda ( unit , list operation , { #Uninterpreted(Id, unit, list operation) } ) ]
              ; [ pair nat pair nat list key Pair Count:Int Pair Threshold:Int KeyListInit:InternalList ]
              ; .Stack
             => [ nat ?VerifiedKeysFinal ]
              ; [ list option signature .InternalList ]
              ; [ bytes #Packed ( pair ( pair chain_id address ) pair nat or ( lambda unit list operation ) pair nat list key , Pair Pair #ChainId ( ChainId ) MyAddr:Address Pair Count:Int Left #Lambda ( unit , list operation , { #Uninterpreted(Id, unit, list operation) } ) ) ]
              ; [ or ( lambda unit list operation ) pair nat list key Left #Lambda ( unit , list operation , { #Uninterpreted(Id, unit, list operation) } ) ]
              ; [ pair nat pair nat list key Pair Count:Int Pair Threshold:Int KeyListInit:InternalList ]
              ; .Stack
        </stack>
        <myamount> #Mutez(Amount:Int) </myamount>
    requires Amount ==Int 0
     andBool size(SigList) ==Int size(KeyList)
     andBool VerifiedKeys >=Int 0
     andBool VerifiedKeys +Int numValidSigs(SigList, KeyList) >=Int Threshold
     ensures ?VerifiedKeysFinal >=Int VerifiedKeys
     andBool ?VerifiedKeysFinal >=Int Threshold
     andBool VerifiedKeys +Int size(KeyList) >=Int ?VerifiedKeysFinal

endmodule
