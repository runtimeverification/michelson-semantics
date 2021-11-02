module VERIFICATION
  imports MICHELSON
endmodule


module LISTSUM-SPEC
  imports VERIFICATION

  claim <k> ITER .AnnotationList {
              DROP .AnnotationList ;
              PUSH .AnnotationList nat .AnnotationList 1 ;
              ADD .AnnotationList
            } => . </k>
        <stack> [ list int NS:InternalList ] ; [ int M:Int ]
             => [ int M +Int size(NS) ] </stack>

endmodule

