module VERIFICATION
  imports MICHELSON
endmodule


module SUM-TO-N-2-SPEC
  imports VERIFICATION

  claim <k> LOOP .AnnotationList {                         // N S
              DUP .AnnotationList ;                        // N N S
              DIP .AnnotationList {                        // N | N S
                ADD .AnnotationList                        // N | (N + S)
              } ;                                          // N (N + S)
              PUSH .AnnotationList nat .AnnotationList 1 ; // 1 N (N + S)
              SWAP .AnnotationList ;                       // N 1 (N + S)
              SUB .AnnotationList ;                        // (N - 1) (N + S)
              DUP .AnnotationList ;                        // (N - 1) (N - 1) (N + S)
              GT .AnnotationList                           // (N - 1 > 0) (N - 1) (N + S)
            } => . </k>
        <stack> [bool C:Bool] ; [int N:Int] ; [int S:Int]
             => [int 0] ; [int (S +Int N *Int (N +Int 1) /Int 2)] </stack>
      requires (N ==Int 0 andBool notBool C) orBool
               (N  >Int 0 andBool         C)
endmodule

