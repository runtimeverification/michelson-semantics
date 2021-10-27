module VERIFICATION
  imports MICHELSON
endmodule


module SUM-TO-N-SPEC
  imports VERIFICATION

  claim <k>
        LOOP_LEFT .AnnotationList {
            DUP .AnnotationList ;
            DIP .AnnotationList { CDR .AnnotationList } ;
            CAR .AnnotationList ;
            DUP .AnnotationList ;
            DIP .AnnotationList { ADD .AnnotationList } ;
            PUSH .AnnotationList nat .AnnotationList 1 ;
            SWAP .AnnotationList ;
            SUB .AnnotationList ;
            ISNAT .AnnotationList ;
            IF_NONE .AnnotationList { RIGHT .AnnotationList (pair .AnnotationList nat .AnnotationList nat .AnnotationList) } 
                                    { PAIR .AnnotationList ; LEFT .AnnotationList nat .AnnotationList } } => . </k>
        <stack> [(or (pair nat nat) nat) Left (Pair N:Int S:Int)] ; .Stack
             => [nat (S +Int ((N *Int (N +Int 1)) /Int 2))]       ; .Stack </stack>
      requires N >=Int 0
endmodule

