requires "lemmas.md"

module VERIFICATION
  imports MICHELSON
endmodule

module SUM-TO-N-SPEC
  imports VERIFICATION

  rule <k>
       LOOP_LEFT .AnnotationList { DUP .AnnotationList ;
                   DIP .AnnotationList { CDR .AnnotationList } ;
                   CAR .AnnotationList ;
                   DUP .AnnotationList ;
                   DIP .AnnotationList { ADD .AnnotationList } ;
                   PUSH .AnnotationList nat .AnnotationList 1 ;
                   SWAP .AnnotationList ;
                   SUB .AnnotationList ;
                   ISNAT .AnnotationList ;
                   IF_NONE .AnnotationList { RIGHT .AnnotationList (pair .AnnotationList nat .AnnotationList nat .AnnotationList)  } 
                                           { PAIR .AnnotationList ; LEFT .AnnotationList nat .AnnotationList } } => .K </k>
        <stack> [(or (pair int int) int) Left (Pair A:Int B:Int)] ; .Stack
             => [int (B +Int ((A *Int (A +Int 1)) /Int 2))]       ; .Stack </stack>
        <currentAccount> ADDR </currentAccount>
        <accounts> ADDR |-> #Account(... entrypoints : %default |-> nat .AnnotationList) ... </accounts>
        <storagetype> nat .AnnotationList </storagetype>
      requires A >=Int 0 andBool B >=Int 0
endmodule
