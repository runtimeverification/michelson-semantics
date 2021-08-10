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

  // contract_00, contract_04: Expected contract exists
  claim <k> (CONTRACT .AnnotationList #Type(unit)) => .K </k>
        <stack> [ address #Address(Addr) ] ; .Stack
             => [ (option contract unit) Some #Contract( #Address(Addr) . %default, unit ) ] ; .Stack
        </stack>
        <contracts> Accounts </contracts>
     requires #EntrypointExists(Accounts, #Address(Addr), %default, unit .AnnotationList)

  // contract_01, contract_05: contract does not exist
  claim <k> (CONTRACT .AnnotationList #Type(unit)) => .K </k>
        <stack> [ address #Address(Addr) ] ; .Stack
             => [ (option contract unit) None ] ; .Stack
        </stack>
        <contracts> Accounts </contracts>
     requires notBool #Address(Addr) in_keys(Accounts)

  // contract_03: Contract has different type
  claim <k> (CONTRACT .AnnotationList #Type(unit)) => .K </k>
        <stack> [ address #Address(Addr) ] ; .Stack
             => [ (option contract unit) None ] ; .Stack
        </stack>
        <contracts> Accounts </contracts>
     requires #EntrypointExists(Accounts, #Address(Addr), %default, nat .AnnotationList)

  // contract_06: Non-default entrypoint
  claim <k> (CONTRACT #token("%foo" , "FieldAnnotation") .AnnotationList #Type(unit)) => .K </k>
        <stack> [ address #Address(Addr) ] ; .Stack
             => [ (option contract unit) Some #Contract( #Address(Addr) . #token("%foo", "FieldAnnotation"), unit ) ] ; .Stack
        </stack>
        <contracts> Accounts </contracts>
     requires #EntrypointExists(Accounts, #Address(Addr), #token("%foo", "FieldAnnotation"), unit .AnnotationList)

  // contract_07: Wrong entry point
  claim <k> (CONTRACT #token("%baz" , "FieldAnnotation") .AnnotationList #Type(nat)) => .K </k>
        <stack> [ address #Address(Addr) ] ; .Stack
             => [ (option contract nat) None ] ; .Stack
        </stack>
        <contracts> Accounts </contracts>
     requires notBool #EntrypointExists(Accounts, #Address(Addr), #token("%baz", "FieldAnnotation"), nat .AnnotationList)

  // contract_08: Non-unit type
  claim <k> (CONTRACT .AnnotationList #Type(nat)) => .K </k>
        <stack> [ address #Address(Addr) ] ; .Stack
             => [ (option contract nat) Some #Contract( #Address(Addr) . %default, nat ) ] ; .Stack
        </stack>
        <contracts> Accounts </contracts>
     requires #EntrypointExists(Accounts, #Address(Addr), %default, nat .AnnotationList)

endmodule
