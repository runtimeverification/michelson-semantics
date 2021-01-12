requires "lemmas.md"

module VERIFICATION
  imports LEMMAS
endmodule

module VOTE-SPEC
  imports VERIFICATION

  rule <k> AMOUNT .AnnotationList ;
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
           PAIR .AnnotationList => .K
        </k>
        <stack> [(pair string (map string int)) Pair S:String M:Map ] ; .Stack
             => [(pair (list operation) (map string int)) Pair .InternalList  M[S <- (?I +Int 1)] ]  ; .Stack </stack>
        <myamount> #Mutez(A) </myamount>
    requires A >Int 5000000
     andBool A <Int 9223372036854775808
     andBool M[S] ==K ?I:Int
     andBool #lookup( M, S, int) ==K Some ?I:Int

  rule <k> AMOUNT .AnnotationList ;
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
        => Aborted(?_,?_,?_,?_)
        </k>
        <stack> [(pair string (map string int)) Pair S:String M:Map ] ; .Stack => ( Failed Unit ) </stack>
        <myamount> #Mutez(A) </myamount>
    requires A >Int 5000000
     andBool A <Int 9223372036854775808
     andBool notBool S in_keys(M)

  rule <k> AMOUNT .AnnotationList ;
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
        => Aborted(?_,?_,?_,?_)
        </k>
        <stack> [(pair string (map string int)) Pair S:String M:Map ] ; .Stack => ( Failed Unit ) </stack>
        <myamount> #Mutez(A) </myamount>
    requires 0 <=Int A
     andBool 5000000 >Int A

endmodule
