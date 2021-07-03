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
        <tokens> Tokens => #incrementTokens(Tokens, Address, Quantity) </tokens>
        <myamount> #Mutez(Amount) </myamount>
        <adminAddress> Admin </adminAddress>
        <senderaddr> Sender </senderaddr>
        <totalSupply> TotalSupply => absInt(TotalSupply +Int Quantity) </totalSupply>
        <operations> _ => .InternalList </operations>
    requires Amount ==Int 0
     andBool Sender ==K Admin
     andBool #tokensFor(Tokens, Address) +Int Quantity >=Int 0
     andBool Address in_keys(Tokens)

   claim <k> #runProof(MintOrBurnParams(Quantity, Address)) => Aborted(?_, ?_, ?_, ?_) ... </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <tokens> Tokens </tokens>
        <myamount> #Mutez(Amount) </myamount>
        <adminAddress> Admin </adminAddress>
        <senderaddr> Sender </senderaddr>
    requires Address in_keys(Tokens)
     andBool notBool( Amount ==Int 0
              andBool Sender ==K Admin
              andBool #tokensFor(Tokens, Address) +Int Quantity >=Int 0
                    )
endmodule
```

```k
module LQT-TOKEN-APPROVE-SPEC
  imports LQT-TOKEN-VERIFICATION
```

```k
  claim <k> #runProof(ApproveParams(Spender, Value)) => .K ... </k>
        <stack> .Stack </stack>
        <allowances> Allowances => #updateAllowances(Allowances, Sender, Spender, Value) </allowances>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
        <operations> _ => .InternalList </operations>
    requires Amount ==Int 0
     andBool (#allowanceFor(Allowances, Sender, Spender) >Int 0 impliesBool Value ==Int 0)
```

```k
  claim <k> #runProof(ApproveParams(Spender, Value)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <allowances> Allowances </allowances>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
    requires notBool( Amount ==Int 0
              andBool (#allowanceFor(Allowances, Sender, Spender) >Int 0 impliesBool Value ==Int 0)
                    )
```

```k
endmodule
```


```k
module LQT-TOKEN-TRANSFER-DIRECT-SPEC
  imports LQT-TOKEN-VERIFICATION
```

```k
  claim <k> #runProof(TransferParams(From, To, Value)) => .K </k>
        <stack> .Stack </stack>
        <tokens> Tokens => #incrementTokens(#incrementTokens(Tokens, From, 0 -Int Value), To, Value)  </tokens>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
        <operations> _ => .InternalList </operations>
    requires Amount ==Int 0
     andBool #tokensFor(Tokens, From) >=Int Value
          // Direct spending
     andBool Sender ==K From
```

```k
  claim <k> #runProof(TransferParams(From, To, Value)) => Aborted(?_, ?_, ?_, ?_) ... </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <tokens> Tokens </tokens>
        <allowances> Allowances </allowances>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
    requires Sender ==K From
     andBool notBool( Amount ==Int 0
              andBool #tokensFor(Tokens, From) >=Int Value
                    )
```

```k
endmodule
```


```k
module LQT-TOKEN-TRANSFER-PROXY-SPEC
  imports LQT-TOKEN-VERIFICATION
```

The positive case here is split up into three different cases:

1.  the case where the value transferred is 0 --- in this case, the contract always succeeds because the minimum allowance is 0
2.  the case where the value transferred is greater than 0 _and_ where the receiver has a zero balance (i.e. the receiver account doesn't exist)
3.  the case where the value transferred is greater than 0 _and_ where the receiver has a non-zero balance (i.e. the receiver account exists)

```k
  claim <k> #runProof(TransferParams(From, To, Value)) => .K </k>
        <stack> .Stack </stack>
        <tokens> Tokens => #incrementTokens(#incrementTokens(Tokens, From, 0 -Int Value), To, Value)  </tokens>
        <allowances> Allowances => #updateAllowances(Allowances, From, Sender, #allowanceFor(Allowances, From, Sender) -Int Value) </allowances>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
        <operations> _ => .InternalList </operations>
    requires Amount ==Int 0
     andBool #tokensFor(Tokens, From) >=Int Value
          // Spend via proxy
     andBool Sender =/=K From
     andBool #allowanceFor(Allowances, From, Sender) >=Int Value
     andBool Value ==Int 0
```

```k
  claim <k> #runProof(TransferParams(From, To, Value)) => .K </k>
        <stack> .Stack </stack>
        <tokens> Tokens => #incrementTokens(#incrementTokens(Tokens, From, 0 -Int Value), To, Value)  </tokens>
        <allowances> Allowances => #updateAllowances(Allowances, From, Sender, #allowanceFor(Allowances, From, Sender) -Int Value) </allowances>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
        <operations> _ => .InternalList </operations>
    requires Amount ==Int 0
     andBool #tokensFor(Tokens, From) >=Int Value
     andBool notBool To in_keys(Tokens)
     andBool #tokensFor(Tokens, To) ==Int 0
          // Spend via proxy
     andBool Sender =/=K From
     andBool #allowanceFor(Allowances, From, Sender) >=Int Value
     andBool #allowanceKey(From, Sender) in_keys(Allowances)
     andBool Value >Int 0
```

```k
  claim <k> #runProof(TransferParams(From, To, Value)) => .K </k>
        <stack> .Stack </stack>
        <tokens> Tokens => #incrementTokens(#incrementTokens(Tokens, From, 0 -Int Value), To, Value)  </tokens>
        <allowances> Allowances => #updateAllowances(Allowances, From, Sender, #allowanceFor(Allowances, From, Sender) -Int Value) </allowances>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
        <operations> _ => .InternalList </operations>
    requires Amount ==Int 0
     andBool #tokensFor(Tokens, From) >=Int Value
     andBool To in_keys(Tokens)
     andBool #tokensFor(Tokens, To) >Int 0
          // Spend via proxy
     andBool Sender =/=K From
     andBool #allowanceFor(Allowances, From, Sender) >=Int Value
     andBool #allowanceKey(From, Sender) in_keys(Allowances)
     andBool Value >Int 0
```

We need only a single negative case to complete our proof.

```k
  claim <k> #runProof(TransferParams(From, To, Value)) => Aborted(?_, ?_, ?_, ?_) ... </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <tokens> Tokens  </tokens>
        <allowances> Allowances </allowances>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
    requires Sender =/=K From
     andBool notBool( Amount ==Int 0
              andBool #tokensFor(Tokens, From) >=Int Value
              andBool #allowanceFor(Allowances, From, Sender) >=Int Value
                    )
```

```k
endmodule
```

