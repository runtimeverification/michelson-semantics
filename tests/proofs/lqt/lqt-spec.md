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

  claim <k> #runProof(GetTotalSupplyParams(Callback)) => .K ... </k>
        <stack> .Stack </stack>
        <totalSupply> TotalSupply </totalSupply>
        <nonce> #Nonce(Nonce => Nonce +Int 1) </nonce>
        <operations> _ => [ Transfer_tokens TotalSupply #Mutez(0) Callback (Nonce) ] ;; .InternalList </operations>
        <myamount> #Mutez(Amount) </myamount>
    requires Amount ==Int 0

  claim <k> #runProof(GetTotalSupplyParams(_)) => Aborted(?_, ?_, ?_, ?_) ... </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <myamount> #Mutez(Amount) </myamount>
    requires notBool(Amount ==Int 0)
endmodule
```

```k
module LQT-TOKEN-GETBALANCE-SPEC
  imports LQT-TOKEN-VERIFICATION

  claim <k> #runProof(GetBalanceParams(Address, Callback)) => .K ... </k>
        <stack> .Stack </stack>
        <tokens> Tokens </tokens>
        <nonce> #Nonce(Nonce => Nonce +Int 1) </nonce>
        <operations> _ => [ Transfer_tokens {Tokens[Address]}:>Int #Mutez(0) Callback (Nonce) ] ;; .InternalList </operations>
        <myamount> #Mutez(Amount) </myamount>
    requires Amount ==Int 0
     andBool Address in_keys(Tokens)

  claim <k> #runProof(GetBalanceParams(Address, Callback)) => .K ... </k>
        <stack> .Stack </stack>
        <tokens> Tokens </tokens>
        <nonce> #Nonce(Nonce => Nonce +Int 1) </nonce>
        <operations> _ => [ Transfer_tokens 0 #Mutez(0) Callback (Nonce) ] ;; .InternalList </operations>
        <myamount> #Mutez(Amount) </myamount>
    requires Amount ==Int 0
     andBool notBool Address in_keys(Tokens)

  claim <k> #runProof(GetBalanceParams(_, _)) => Aborted(?_, ?_, ?_, ?_) ... </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <myamount> #Mutez(Amount) </myamount>
    requires notBool(Amount ==Int 0)
endmodule
```

```k
module LQT-TOKEN-GETALLOWANCE-SPEC
  imports LQT-TOKEN-VERIFICATION

  claim <k> #runProof(GetAllowanceParams(Owner, Spender, Callback)) => .K ... </k>
        <stack> .Stack </stack>
        <allowances> Allowances </allowances>
        <nonce> #Nonce(Nonce => Nonce +Int 1) </nonce>
        <operations> _ => [ Transfer_tokens {Allowances[Pair Owner Spender]}:>Int #Mutez(0) Callback (Nonce) ] ;; .InternalList </operations>
        <myamount> #Mutez(Amount) </myamount>
    requires Amount ==Int 0
     andBool (Pair Owner Spender) in_keys(Allowances)

  claim <k> #runProof(GetAllowanceParams(Owner, Spender, Callback)) => .K ... </k>
        <stack> .Stack </stack>
        <allowances> Allowances </allowances>
        <nonce> #Nonce(Nonce => Nonce +Int 1) </nonce>
        <operations> _ => [ Transfer_tokens 0 #Mutez(0) Callback (Nonce) ] ;; .InternalList </operations>
        <myamount> #Mutez(Amount) </myamount>
    requires Amount ==Int 0
     andBool notBool (Pair Owner Spender) in_keys(Allowances)

  claim <k> #runProof(GetAllowanceParams(_, _, _)) => Aborted(?_, ?_, ?_, ?_) ... </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <myamount> #Mutez(Amount) </myamount>
    requires notBool(Amount ==Int 0)
endmodule
```

```k
module LQT-TOKEN-MINTORBURN-SPEC
  imports LQT-TOKEN-VERIFICATION

  claim <k> #runProof(MintOrBurnParams(Quantity, Address)) => .K ... </k>
        <stack> .Stack </stack>
        <tokens> Tokens => #updateTokens(Tokens, Address, {Tokens[Address]}:>Int +Int Quantity) </tokens>
        <myamount> #Mutez(Amount) </myamount>
        <adminAddress> Admin </adminAddress>
        <senderaddr> Sender </senderaddr>
        <totalSupply> TotalSupply => absInt(TotalSupply +Int Quantity) </totalSupply>
        <operations> _ => .InternalList </operations>
    requires Amount ==Int 0
     andBool Sender ==K Admin
     andBool {Tokens[Address]}:>Int +Int Quantity >=Int 0
     andBool Address in_keys(Tokens)

  claim <k> #runProof(MintOrBurnParams(Quantity, Address)) => .K ... </k>
        <stack> .Stack </stack>
        <tokens> Tokens => #updateTokens(Tokens, Address, Quantity) </tokens>
        <myamount> #Mutez(Amount) </myamount>
        <adminAddress> Admin </adminAddress>
        <senderaddr> Sender </senderaddr>
        <totalSupply> TotalSupply => absInt(TotalSupply +Int Quantity) </totalSupply>
        <operations> _ => .InternalList </operations>
    requires Amount ==Int 0
     andBool Sender ==K Admin
     andBool Quantity >=Int 0
     andBool notBool Address in_keys(Tokens)

   claim <k> #runProof(MintOrBurnParams(Quantity, Address)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <tokens> Tokens </tokens>
        <myamount> #Mutez(Amount) </myamount>
        <adminAddress> Admin </adminAddress>
        <senderaddr> Sender </senderaddr>
    requires Address in_keys(Tokens)
     andBool notBool( Amount ==Int 0
              andBool Sender ==K Admin
              andBool {Tokens[Address]}:>Int +Int Quantity >=Int 0
                    )

   claim <k> #runProof(MintOrBurnParams(Quantity, Address)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <tokens> Tokens </tokens>
        <myamount> #Mutez(Amount) </myamount>
        <adminAddress> Admin </adminAddress>
        <senderaddr> Sender </senderaddr>
    requires notBool Address in_keys(Tokens)
     andBool notBool( Amount ==Int 0
              andBool Sender ==K Admin
              andBool Quantity >=Int 0
                    )
endmodule
```
