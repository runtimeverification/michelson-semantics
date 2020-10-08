requires "lemmas.md"

module VERIFICATION
  import LEMMAS
endmodule

module ADD-SPEC
  imports VERIFICATION

  rule <k> ADD .AnnotationList => .K </k>
        <stack> [ int 1 ] ; [ int { M:Map [ I:Int ] }:>Data ] ; .Stack => [ int I2 +Int I ] ; .Stack </stack>
    requires { M:Map [ I:Int ] }:>Data ==K I2:Int
    ensures  isInt({ M:Map [ I:Int ] }:>Data)

endmodule
