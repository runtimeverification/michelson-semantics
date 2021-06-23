This claim is a test claim for testing our infrastructure:

```k
module LQT-TOKEN-SPEC
  imports LQT-TOKEN-VERIFICATION
  claim <k> now 0 => . ... </k>
        <mynow> #Timestamp(0) </mynow>

  claim <k> (now 0 => .) ~> #lqtTokenCode ... </k>
        <mynow> #Timestamp(0) </mynow>

  claim <k> PUSH nat X:Int ; PUSH nat Y:Int ; PUSH nat Z:Int ; MUL ; EDIV ; IF_NONE {} { DUP ; CDR ; SWAP ; CAR ; PUSH nat 0 ; DIG 2 ; COMPARE ; EQ } => . ... </k>
        <stack> .Stack => [ bool ?_ ] ; [ nat (Z *Int Y) /Int X ] ; .Stack </stack>
    requires X >Int 0
     andBool Y >=Int 0
     andBool Z >=Int 0
endmodule
```

```k
module LQT-TOKEN-GETTOTALSUPPLY-SPEC
  imports LQT-TOKEN-VERIFICATION

  claim <k> #runProof(GetTotalSupplyParams(Entrypoint)) => .K ... </k>
        <stack> .Stack </stack>
        <tokens> _ </tokens>
        <allowances> _ </allowances>
        <adminAddress> _ </adminAddress>
        <totalSupply> TotalSupply </totalSupply>
        <nonce> #Nonce(Nonce => Nonce +Int 1) </nonce>
        <operations> _ => [ Transfer_tokens TotalSupply #Mutez(0) Entrypoint (Nonce) ] ;; .InternalList </operations>
        <myamount> #Mutez(Amount) </myamount>
    requires Amount ==Int 0

  claim <k> #runProof(GetTotalSupplyParams(_)) => Aborted(?_, ?_, ?_, ?_) ... </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <myamount> #Mutez(Amount) </myamount>
    requires notBool(Amount ==Int 0)
endmodule
```
