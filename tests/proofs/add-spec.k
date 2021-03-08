requires "lemmas.md"

module VERIFICATION
  import LEMMAS
endmodule

module ADD-SPEC
  imports VERIFICATION

  rule <k> ADD .AnnotationList => .K </k>
        <stack> [ int I1 ] ; [ int I2 ] ; .Stack => [ int I1 +Int I2 ] ; .Stack </stack>

endmodule
