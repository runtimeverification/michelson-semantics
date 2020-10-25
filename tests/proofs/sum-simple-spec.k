requires "lemmas.md"

module VERIFICATION
  imports MICHELSON
  imports LEMMAS
endmodule

module SUM-SIMPLE-SPEC
  imports VERIFICATION

  rule <k> { PUSH .AnnotationList (int .AnnotationList) 2 ;
             PUSH .AnnotationList (int .AnnotationList) I1:Int ;
             EDIV .AnnotationList ;
             IF_NONE .AnnotationList { UNIT .AnnotationList ;
                                       FAILWITH .AnnotationList }
                                     { CDR .AnnotationList ;
                                       PUSH .AnnotationList (nat .AnnotationList) 1 ;
                                       COMPARE .AnnotationList ;
                                       EQ .AnnotationList } } ;
            #AssumeTrue ;
           { PUSH .AnnotationList (int .AnnotationList) 2 ;
             PUSH .AnnotationList (int .AnnotationList) I2:Int ;
             EDIV .AnnotationList ;
             IF_NONE .AnnotationList { UNIT .AnnotationList ;
                                       FAILWITH .AnnotationList }
                                     { CDR .AnnotationList ;
                                       PUSH .AnnotationList (nat .AnnotationList) 0 ;
                                       COMPARE .AnnotationList ;
                                       EQ .AnnotationList } } ;
             #AssumeTrue ;
           { PUSH .AnnotationList (int .AnnotationList) 2 ;
             PUSH .AnnotationList (int .AnnotationList) I1:Int ;
             PUSH .AnnotationList (int .AnnotationList) I2:Int ;
             ADD .AnnotationList ;
             EDIV .AnnotationList ;
             IF_NONE .AnnotationList { UNIT .AnnotationList ;
                                       FAILWITH .AnnotationList }
                                     { CDR .AnnotationList ;
                                       PUSH .AnnotationList (nat .AnnotationList) 1 ;
                                       COMPARE .AnnotationList ;
                                       EQ .AnnotationList } } ;
              #AssertTrue => .K </k>
        <stack> .Stack </stack>
        <assumeFailed> false => ?_ </assumeFailed>
endmodule
