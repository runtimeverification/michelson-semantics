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
     andBool #tokensFor(Tokens, Address) +Int Quantity ==Int 0
     andBool Address in_keys(Tokens)

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
     andBool #tokensFor(Tokens, Address) +Int Quantity ==Int 0
     andBool notBool Address in_keys(Tokens)

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
     andBool #tokensFor(Tokens, Address) +Int Quantity >Int 0
     andBool Address in_keys(Tokens)

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
     andBool #tokensFor(Tokens, Address) +Int Quantity >Int 0
     andBool notBool Address in_keys(Tokens)

   claim <k> #runProof(MintOrBurnParams(Quantity, Address)) => Aborted(?_, ?_, ?_, ?_) ... </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <tokens> Tokens </tokens>
        <myamount> #Mutez(Amount) </myamount>
        <adminAddress> Admin </adminAddress>
        <senderaddr> Sender </senderaddr>
    requires notBool( Amount ==Int 0
              andBool Sender ==K Admin
              andBool #tokensFor(Tokens, Address) +Int Quantity >=Int 0
                    )
              andBool Address in_keys(Tokens)


   claim <k> #runProof(MintOrBurnParams(Quantity, Address)) => Aborted(?_, ?_, ?_, ?_) ... </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <tokens> Tokens </tokens>
        <myamount> #Mutez(Amount) </myamount>
        <adminAddress> Admin </adminAddress>
        <senderaddr> Sender </senderaddr>
    requires notBool( Amount ==Int 0
              andBool Sender ==K Admin
              andBool #tokensFor(Tokens, Address) +Int Quantity >=Int 0
                    )
              andBool notBool Address in_keys(Tokens)

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
     andBool (Pair Sender Spender) in_keys(Allowances)

  claim <k> #runProof(ApproveParams(Spender, Value)) => .K ... </k>
        <stack> .Stack </stack>
        <allowances> Allowances => #updateAllowances(Allowances, Sender, Spender, Value) </allowances>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
        <operations> _ => .InternalList </operations>
    requires Amount ==Int 0
     andBool (#allowanceFor(Allowances, Sender, Spender) >Int 0 impliesBool Value ==Int 0)
     andBool notBool (Pair Sender Spender) in_keys(Allowances)
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
              andBool (Pair Sender Spender) in_keys(Allowances)

  claim <k> #runProof(ApproveParams(Spender, Value)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <allowances> Allowances </allowances>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
    requires notBool( Amount ==Int 0
              andBool (#allowanceFor(Allowances, Sender, Spender) >Int 0 impliesBool Value ==Int 0)
                    )
              andBool notBool (Pair Sender Spender) in_keys(Allowances)
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
     andBool From in_keys(Tokens)
     andBool To in_keys(Tokens)

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
     andBool notBool From in_keys(Tokens)
     andBool         To in_keys(Tokens)

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
     andBool         From in_keys(Tokens)
     andBool notBool To in_keys(Tokens)

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
     andBool notBool From in_keys(Tokens)
     andBool notBool To in_keys(Tokens)
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
              andBool From in_keys(Tokens)
              andBool To in_keys(Tokens)

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
              andBool notBool From in_keys(Tokens)
              andBool         To in_keys(Tokens)

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
              andBool         From in_keys(Tokens)
              andBool notBool To in_keys(Tokens)

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
              andBool notBool From in_keys(Tokens)
              andBool notBool To in_keys(Tokens)
```

```k
endmodule
```


```k
module LQT-TOKEN-TRANSFER-PROXY-SPEC
  imports LQT-TOKEN-VERIFICATION
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
          // Spend via proxy
     andBool Sender =/=K From
     andBool #allowanceFor(Allowances, From, Sender) >=Int Value
     andBool         (Pair From Sender) in_keys(Allowances)
     andBool         From in_keys(Tokens)
     andBool         To in_keys(Tokens)

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
     andBool notBool (Pair From Sender) in_keys(Allowances)
     andBool         From in_keys(Tokens)
     andBool         To in_keys(Tokens)
     andBool Value ==Int 0

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
     andBool notBool (Pair From Sender) in_keys(Allowances)
     andBool         From in_keys(Tokens)
     andBool         To in_keys(Tokens)
     andBool Value =/=Int 0

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
     andBool         (Pair From Sender) in_keys(Allowances)
     andBool notBool From in_keys(Tokens)
     andBool         To in_keys(Tokens)

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
     andBool notBool (Pair From Sender) in_keys(Allowances)
     andBool notBool From in_keys(Tokens)
     andBool         To in_keys(Tokens)

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
     andBool         (Pair From Sender) in_keys(Allowances)
     andBool         From in_keys(Tokens)
     andBool notBool To in_keys(Tokens)

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
     andBool notBool (Pair From Sender) in_keys(Allowances)
     andBool         From in_keys(Tokens)
     andBool notBool To in_keys(Tokens)

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
     andBool         (Pair From Sender) in_keys(Allowances)
     andBool notBool From in_keys(Tokens)
     andBool notBool To in_keys(Tokens)

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
     andBool notBool (Pair From Sender) in_keys(Allowances)
     andBool notBool From in_keys(Tokens)
     andBool notBool To in_keys(Tokens)

///////////////////////////////////////////////////////////////////////////////

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
              andBool         (Pair From Sender) in_keys(Allowances)
              andBool         From in_keys(Tokens)
              andBool         To in_keys(Tokens)

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
              andBool notBool (Pair From Sender) in_keys(Allowances)
              andBool         From in_keys(Tokens)
              andBool         To in_keys(Tokens)

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
              andBool         (Pair From Sender) in_keys(Allowances)
              andBool notBool From in_keys(Tokens)
              andBool         To in_keys(Tokens)

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
              andBool notBool (Pair From Sender) in_keys(Allowances)
              andBool notBool From in_keys(Tokens)
              andBool         To in_keys(Tokens)

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
              andBool         (Pair From Sender) in_keys(Allowances)
              andBool         From in_keys(Tokens)
              andBool notBool To in_keys(Tokens)

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
              andBool notBool (Pair From Sender) in_keys(Allowances)
              andBool         From in_keys(Tokens)
              andBool notBool To in_keys(Tokens)

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
              andBool         (Pair From Sender) in_keys(Allowances)
              andBool notBool From in_keys(Tokens)
              andBool notBool To in_keys(Tokens)

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
              andBool notBool (Pair From Sender) in_keys(Allowances)
              andBool notBool From in_keys(Tokens)
              andBool notBool To in_keys(Tokens)
```

```k
endmodule
```

