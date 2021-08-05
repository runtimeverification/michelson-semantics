requires "../../michelson.md"

module VERIFICATION
  imports MICHELSON
endmodule

module ENTRYPOINTS-SPEC
  imports VERIFICATION

  claim <k> SELF .AnnotationList => . </k>
        <stack> .Stack => [ contract unit #Contract( SelfAddr . %default, unit ) ] ; .Stack </stack>
        <contracts> Accounts </contracts>
        <currentContract> SelfAddr </currentContract>
     requires #EntrypointExists(Accounts, SelfAddr, %default, unit .AnnotationList)
endmodule
