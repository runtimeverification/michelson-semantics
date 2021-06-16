```k
module LIQUIDITY-BAKING-SPEC
  imports LIQUIDITY-BAKING-VERIFICATION
```

```k
  claim <k> now 0 => . ... </k>
        <mynow> #Timestamp(0) </mynow>

  claim <k> (now 0 => .) ~> #liquidityBakingCode ... </k>
        <mynow> #Timestamp(0) </mynow>

  claim <k> PUSH nat X:Int ; PUSH nat Y:Int ; PUSH nat Z:Int ; MUL ; EDIV ; IF_NONE {} { DUP ; CDR ; SWAP ; CAR ; PUSH nat 0 ; DIG 2 ; COMPARE ; EQ } => . ... </k>
        <stack> .Stack => [ bool ?_ ] ; [ nat (Z *Int Y) /Int X ] ; .Stack </stack>
    requires X >Int 0
     andBool Y >=Int 0
     andBool Z >=Int 0
```

```k
endmodule
```

## Add Liquidity

Allows callers to "mint" liquidity in excahnge for tez.
Caller gains liquidity equal to `tez * storage.lqtTotal / storage.xtzPool`.

-   Storage updates:

```
Storage( lqtTotal:  LqtTotal  => LqtTotal  + lqt_minted ;
         tokenPool: TokenPool => TokenPool + tokens_deposited ;
         xtzPool:   XtzPool   => XtzPool   + Tezos.amount
       )
```

-   Operations:

1. call to `transfer` entrypoint of liquidity address: Send tokens from sender to self.
2. call to `mintOrBurn` entrypoint of token address: Adds liquidity for the sender.

### Positive case

-   Preconditions

1.  the deadline has not passed (i.e. the `Tezos.now < input.deadline`)
2.  the tokens transferred is less than `input.maxTokensDeposited`
3.  the liquidity minted is more than `input.minLqtMinted`
4.  xtzPool is positive

```k
module LIQUIDITY-BAKING-ADDLIQUIDITY-POSITIVE-SPEC
  imports LIQUIDITY-BAKING-VERIFICATION
```

For performance reasons, we split the claim depending on the result of the `#ceildiv` operation.
We have one case for when `#ceildiv` results in an upwards rounding, and one for the case when the numerator is divisible by the denominator.

```k
  claim <k> #runProof(AddLiquidity(Owner, MinLqtMinted, MaxTokensDeposited, #Timestamp(Deadline))) => . </k>
        <stack> .Stack </stack>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myamount> #Mutez(Amount) </myamount>
        <myaddr> SelfAddress </myaddr>
        <lqtTotal> OldLqt => OldLqt +Int (Amount *Int OldLqt) /Int XtzAmount </lqtTotal>
        <xtzPool> #Mutez(XtzAmount => XtzAmount +Int Amount) </xtzPool>
        <tokenPool> TokenAmount => TokenAmount +Int #ceildiv(Amount *Int TokenAmount, XtzAmount) </tokenPool>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <lqtAddress> LqtAddress:Address </lqtAddress>
        <senderaddr> Sender </senderaddr>
        <nonce> #Nonce(Nonce => Nonce +Int 2) </nonce>
        <knownaddrs> KnownAddresses </knownaddrs>
        <paramtype> #Type(#LiquidityBakingParamType()) </paramtype>
        <operations> _
                  => [ Transfer_tokens #TokenTransferData(Sender, SelfAddress, #ceildiv(Amount *Int TokenAmount, XtzAmount)) #Mutez(0) TokenAddress Nonce ] ;;
                     [ Transfer_tokens Pair ((Amount *Int OldLqt) /Int XtzAmount) Owner #Mutez(0) LqtAddress (Nonce +Int 1) ] ;;
                     .InternalList
        </operations>
    requires CurrentTime <Int Deadline
     andBool XtzAmount   >Int 0
     andBool #ceildiv(Amount *Int TokenAmount, XtzAmount) <=Int MaxTokensDeposited
     andBool (Amount *Int TokenAmount) %Int XtzAmount ==Int 0
     andBool MinLqtMinted <=Int (Amount *Int OldLqt) /Int XtzAmount
     andBool #IsLegalMutezValue(Amount +Int XtzAmount)

     andBool #EntrypointExists(KnownAddresses, TokenAddress,   %transfer, #TokenTransferType())
     andBool #EntrypointExists(KnownAddresses,   LqtAddress, %mintOrBurn, pair int %quantity .AnnotationList address %target .AnnotationList)
```

```k
  claim <k> #runProof(AddLiquidity(Owner, MinLqtMinted, MaxTokensDeposited, #Timestamp(Deadline))) => . </k>
        <stack> .Stack </stack>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myamount> #Mutez(Amount) </myamount>
        <myaddr> SelfAddress </myaddr>
        <lqtTotal> OldLqt => OldLqt +Int (Amount *Int OldLqt) /Int XtzAmount </lqtTotal>
        <xtzPool> #Mutez(XtzAmount => XtzAmount +Int Amount) </xtzPool>
        <tokenPool> TokenAmount => TokenAmount +Int #ceildiv(Amount *Int TokenAmount, XtzAmount) </tokenPool>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <lqtAddress> LqtAddress:Address </lqtAddress>
        <senderaddr> Sender </senderaddr>
        <nonce> #Nonce(Nonce => Nonce +Int 2) </nonce>
        <knownaddrs> KnownAddresses </knownaddrs>
        <paramtype> #Type(#LiquidityBakingParamType()) </paramtype>
        <operations> _
                  => [ Transfer_tokens #TokenTransferData(Sender, SelfAddress, #ceildiv(Amount *Int TokenAmount, XtzAmount)) #Mutez(0) TokenAddress Nonce ] ;;
                     [ Transfer_tokens Pair ((Amount *Int OldLqt) /Int XtzAmount) Owner #Mutez(0) LqtAddress (Nonce +Int 1) ] ;;
                     .InternalList
        </operations>
    requires CurrentTime <Int Deadline
     andBool #ceildiv(Amount *Int TokenAmount, XtzAmount) <=Int MaxTokensDeposited
     andBool #IsLegalMutezValue(Amount +Int XtzAmount)
     andBool MinLqtMinted <=Int (Amount *Int OldLqt) /Int XtzAmount
     andBool XtzAmount   >Int 0
     andBool (Amount *Int TokenAmount) %Int XtzAmount =/=Int 0
     andBool #EntrypointExists(KnownAddresses, TokenAddress,   %transfer, #TokenTransferType())
     andBool #EntrypointExists(KnownAddresses,   LqtAddress, %mintOrBurn, pair int %quantity .AnnotationList address %target .AnnotationList)
```

```k
endmodule
```

### Negative case

The execution fails if any of the following are true:
1.  the deadline has passed (i.e. the `Tezos.now < input.deadline`)
2.  the tokens transferred is more than `input.maxTokensDeposited`
3.  the liquidity minted is less than `input.minLqtMinted`
4.  xtzPool is 0

```k
module LIQUIDITY-BAKING-ADDLIQUIDITY-NEGATIVE-SPEC
  imports LIQUIDITY-BAKING-VERIFICATION
```

```k
  claim <k> #runProof(AddLiquidity(_Owner, MinLqtMinted, MaxTokensDeposited, #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myamount> #Mutez(Amount) </myamount>
        <lqtTotal> OldLqt </lqtTotal>
        <xtzPool> #Mutez(XtzAmount) </xtzPool>
        <tokenPool> TokenAmount </tokenPool>
    requires CurrentTime >=Int Deadline
        orBool #ceildiv(Amount *Int TokenAmount, XtzAmount) >Int MaxTokensDeposited
        orBool notBool #IsLegalMutezValue(Amount +Int XtzAmount)
        orBool XtzAmount ==Int 0
        orBool MinLqtMinted >Int (Amount *Int OldLqt) /Int XtzAmount
```

TODO: Deal with the case when the token contract or the liquidity token contract don't exist or have the wrong type.

```k
endmodule
```

## Remove Liquidity

The sender can burn liquidity tokens in exchange for tez and tokens sent to some address if the following conditions are satisfied:

1.  exactly 0 tez was transferred to this contract when it was invoked
2.  the current block time must be less than the deadline
3.  the amount of liquidity to be redeemed, when converted to xtz, is greater than `minXtzWithdrawn` and less than the amount of tez owned by the Liquidity Baking contract
4.  the amount of liquidity to be redeemed, when converted to tokens, is greater than `minTokensWithdrawn` and less than the amount of tokens owned by the Liquidity Baking contract
5.  the amount of liquidity to be redeemed is less than the total amount of liquidity and less than the amount of liquidity tokens owned by the sender
6.  the contract at address `storage.lqtAddress` has a well-formed `mintOrBurn` entrypoint
7.  the contract at address `storage.tokenAddress` has a well-formed `transfer` entrypoint

```k
module LIQUIDITY-BAKING-REMOVELIQUIDITY-POSITIVE-SPEC
  imports LIQUIDITY-BAKING-VERIFICATION

  claim <k> #runProof(RemoveLiquidity(To, LqtBurned, #Mutez(MinXtzWithdrawn), MinTokensWithdrawn, #Timestamp(Deadline))) => . </k>
        <stack> .Stack </stack>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myamount> #Mutez(0) </myamount>
        <myaddr> SelfAddress </myaddr>
        <lqtTotal> OldLqt => OldLqt -Int LqtBurned </lqtTotal>
        <xtzPool> #Mutez(XtzAmount => XtzAmount -Int (LqtBurned *Int XtzAmount) /Int OldLqt) </xtzPool>
        <tokenPool> TokenAmount => TokenAmount -Int (LqtBurned *Int TokenAmount) /Int OldLqt </tokenPool>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <lqtAddress> LqtAddress:Address </lqtAddress>
        <senderaddr> Sender </senderaddr>
        <nonce> #Nonce(Nonce => Nonce +Int 3) </nonce>
        <knownaddrs> KnownAddresses </knownaddrs>
        <paramtype> #Type(#LiquidityBakingParamType()) </paramtype> // 1027
        <operations> _
                  => [ Transfer_tokens (Pair (0 -Int LqtBurned) Sender) #Mutez(0) LqtAddress Nonce ] ;;
                     [ Transfer_tokens #TokenTransferData(SelfAddress, To, (LqtBurned *Int TokenAmount) /Int OldLqt) #Mutez(0) TokenAddress (Nonce +Int 1) ] ;;
                     [ Transfer_tokens Unit #Mutez((LqtBurned *Int XtzAmount) /Int OldLqt) To (Nonce +Int 2) ] ;;
                     .InternalList
        </operations>
    requires CurrentTime <Int Deadline
     andBool OldLqt >Int 0
     andBool OldLqt >=Int LqtBurned
     andBool MinXtzWithdrawn <=Int (LqtBurned *Int XtzAmount) /Int OldLqt
     andBool MinTokensWithdrawn <=Int (LqtBurned *Int TokenAmount) /Int OldLqt
     andBool #EntrypointExists(KnownAddresses, TokenAddress,   %transfer, #TokenTransferType())
     andBool #EntrypointExists(KnownAddresses,   LqtAddress, %mintOrBurn, pair int %quantity .AnnotationList address %target .AnnotationList)
     andBool #EntrypointExists(KnownAddresses,           To,    %default, unit)

     andBool #IsLegalMutezValue(XtzAmount)
     andBool #IsLegalMutezValue((LqtBurned *Int XtzAmount) /Int OldLqt)
     andBool TokenAmount >=Int (LqtBurned *Int TokenAmount) /Int OldLqt
     andBool XtzAmount   >=Int (LqtBurned *Int   XtzAmount) /Int OldLqt
```

```k
endmodule
```

```k
module LIQUIDITY-BAKING-REMOVELIQUIDITY-NEGATIVE-SPEC
  imports LIQUIDITY-BAKING-VERIFICATION

  claim <k> #runProof(RemoveLiquidity(_, _, _, _, _)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>

  claim <k> #runProof(RemoveLiquidity(_, _, _, _, _)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <myamount> #Mutez(Amount) </myamount>
    requires Amount >Int 0

  claim <k> #runProof(RemoveLiquidity(_, _, _, _, #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <mynow> #Timestamp(CurrentTime) </mynow>
    requires CurrentTime >=Int Deadline

  claim <k> #runProof(RemoveLiquidity(_, LqtBurned, _, _, _)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <lqtTotal> OldLqt </lqtTotal>
    requires OldLqt ==Int 0
      orBool OldLqt <Int LqtBurned

  claim <k> #runProof(RemoveLiquidity(_, LqtBurned, #Mutez(MinXtzWithdrawn), _, _)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <lqtTotal> OldLqt </lqtTotal>
        <xtzPool> #Mutez(XtzAmount) </xtzPool>
    requires MinXtzWithdrawn >Int (LqtBurned *Int XtzAmount) /Int OldLqt

endmodule
```

## Default

```k
module LIQUIDITY-BAKING-DEFAULT-SPEC
  imports LIQUIDITY-BAKING-VERIFICATION
```

Adds more money to the xtz reserves if the following conditions are satisifed:

1.  the updated token pool size is a legal mutez value

```k
  claim <k> #runProof(_Default) => . </k>
        <stack> .Stack </stack>
        <myamount> #Mutez(Amount) </myamount>
        <xtzPool> #Mutez(XtzPool => XtzPool +Int Amount) </xtzPool>
     requires #IsLegalMutezValue(XtzPool +Int Amount)
```

If any of the conditions are not satisfied, the call fails.

```k
  claim <k> #runProof(_Default) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <myamount> #Mutez(Amount) </myamount>
        <xtzPool> #Mutez(XtzPool) </xtzPool>
    requires notBool #IsLegalMutezValue(XtzPool +Int Amount)
```

```k
endmodule
```

## Token To XTZ

```k
module LIQUIDITY-BAKING-TOKENTOXTZ-POSITIVE-SPEC
  imports LIQUIDITY-BAKING-VERIFICATION
```

A buyer sends tokens to the Liquidity Baking contract and receives a corresponding amount of xtz, if the following conditions are satisfied:

1.  the current block time must be less than the deadline
2.  exactly 0 tez was transferred to this contract when it was invoked
3.  the amount of tokens sold, when converted into xtz using the current exchange rate, is greater than `minXtzBought`
4.  the amount of tokens sold, when converted into xtz using the current exchange rate, it is less than or equal to the xtz owned by the Liquidity Baking contract

```k
  claim <k> #runProof(TokenToXtz(To, TokensSold, #Mutez(MinXtzBought), #Timestamp(Deadline))) => . </k>
        <stack> .Stack </stack>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <xtzPool> #Mutez(XtzPool => XtzPool -Int #XtzBought(XtzPool, TokenPool, TokensSold)) </xtzPool>
        <tokenPool> TokenPool => TokenPool +Int TokensSold </tokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <senderaddr> Sender </senderaddr>
        <paramtype> #Type(#LiquidityBakingParamType()) </paramtype>
        <myaddr> SelfAddress:Address </myaddr>
        <nonce> #Nonce(N => N +Int 2) </nonce>
        <knownaddrs> KnownAddresses </knownaddrs>
        <operations> _
                  => [ Transfer_tokens #TokenTransferData(Sender, SelfAddress, TokensSold) #Mutez(0)                                          TokenAddress  N        ]
                  ;; [ Transfer_tokens Unit                                                #Mutez(#XtzBought(XtzPool, TokenPool, TokensSold)) To           (N +Int 1)]
                  ;; .InternalList
        </operations>
     requires Amount ==Int 0
      andBool CurrentTime <Int Deadline
      andBool (TokenPool >Int 0 orBool TokensSold >Int 0)
      andBool (TokenPool >=Int 0) // Type Invariant
      andBool #XtzBought(XtzPool, TokenPool, TokensSold) >=Int  MinXtzBought
      andBool #XtzBought(XtzPool, TokenPool, TokensSold) <=Int XtzPool
      andBool #IsLegalMutezValue(MinXtzBought)
      andBool #IsLegalMutezValue(#XtzBought(XtzPool, TokenPool, TokensSold))
      andBool #IsLegalMutezValue(XtzPool:Int -Int #XtzBought (XtzPool:Int, TokenPool:Int, TokensSold:Int))
      andBool #EntrypointExists(KnownAddresses, TokenAddress, %transfer,                             #TokenTransferType())
      andBool #EntrypointExists(KnownAddresses, To,           #token("%default", "FieldAnnotation"), #Type(unit))
endmodule
```

The following claims prove the negative case:

```k
module LIQUIDITY-BAKING-TOKENTOXTZ-NEGATIVE-1-SPEC
  imports LIQUIDITY-BAKING-VERIFICATION
  claim <k> #runProof(TokenToXtz(_To, _TokensSold, #Mutez(_MinXtzBought), #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <paramtype> #Type(#LiquidityBakingParamType()) </paramtype>
     requires notBool Amount ==Int 0
         orBool notBool CurrentTime <Int Deadline
endmodule
```

```k
module LIQUIDITY-BAKING-TOKENTOXTZ-NEGATIVE-2-SPEC
  imports LIQUIDITY-BAKING-VERIFICATION
  claim <k> #runProof(TokenToXtz(To, TokensSold, #Mutez(MinXtzBought), #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <xtzPool> #Mutez(XtzPool) </xtzPool>
        <tokenPool> TokenPool </tokenPool>
        <paramtype> #Type(#LiquidityBakingParamType()) </paramtype>
     requires notBool Amount ==Int 0
      andBool notBool CurrentTime <Int Deadline
      andBool #EntrypointExists(KnownAddresses, TokenAddress, %transfer,                             #TokenTransferType())
      andBool #EntrypointExists(KnownAddresses, To,           #token("%default", "FieldAnnotation"), #Type(unit))
      andBool notBool (TokenPool >Int 0 orBool TokensSold >Int 0)
endmodule
```

```k
module LIQUIDITY-BAKING-TOKENTOXTZ-NEGATIVE-3-SPEC
  imports LIQUIDITY-BAKING-VERIFICATION
  claim <k> #runProof(TokenToXtz(To, TokensSold, #Mutez(MinXtzBought), #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <xtzPool> #Mutez(XtzPool) </xtzPool>
        <tokenPool> TokenPool </tokenPool>
        <paramtype> #Type(#LiquidityBakingParamType()) </paramtype>
     requires notBool Amount ==Int 0
      andBool notBool CurrentTime <Int Deadline
      andBool #EntrypointExists(KnownAddresses, TokenAddress, %transfer,                             #TokenTransferType())
      andBool #EntrypointExists(KnownAddresses, To,           #token("%default", "FieldAnnotation"), #Type(unit))
      andBool notBool( #XtzBought(XtzPool, TokenPool, TokensSold) >=Int  MinXtzBought
               andBool #IsLegalMutezValue(MinXtzBought)
                     )
endmodule
```

```k
module LIQUIDITY-BAKING-TOKENTOXTZ-NEGATIVE-4-SPEC
  imports LIQUIDITY-BAKING-VERIFICATION
  claim <k> #runProof(TokenToXtz(To, TokensSold, #Mutez(MinXtzBought), #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <xtzPool> #Mutez(XtzPool) </xtzPool>
        <tokenPool> TokenPool </tokenPool>
        <paramtype> #Type(#LiquidityBakingParamType()) </paramtype>
     requires notBool Amount ==Int 0
      andBool notBool CurrentTime <Int Deadline
      andBool #EntrypointExists(KnownAddresses, TokenAddress, %transfer,                             #TokenTransferType())
      andBool #EntrypointExists(KnownAddresses, To,           #token("%default", "FieldAnnotation"), #Type(unit))
      andBool  #XtzBought(XtzPool, TokenPool, TokensSold) >=Int  MinXtzBought
      andBool #IsLegalMutezValue(MinXtzBought)
      andBool notBool( #XtzBought(XtzPool, TokenPool, TokensSold) <=Int XtzPool
               andBool #IsLegalMutezValue(#XtzBought(XtzPool, TokenPool, TokensSold))
               andBool #IsLegalMutezValue(XtzPool:Int -Int #XtzBought (XtzPool:Int, TokenPool:Int, TokensSold:Int))
                     )
endmodule
```

## XTZ To Token

A buyer sends xtz to the Liquidity Baking contract and receives a corresponding amount of tokens, if the following conditions are satisfied:

1.  the current block time must be less than the deadline
2.  when the `txn.amount` (in mutez) is converted into tokens using the current exchange rate, the purchased amount is greater than `minTokensBought`
3.  when the `txn.amount` (in mutez) is converted into tokens using the current exchange rate, it is less than or equal to the tokens owned by the Liquidity Baking contract

```k
module LIQUIDITY-BAKING-XTZTOTOKEN-POSITIVE-SPEC
  imports LIQUIDITY-BAKING-VERIFICATION
  claim <k> #runProof(XtzToToken(To, MinTokensBought, #Timestamp(Deadline))) => . </k>
        <stack> .Stack </stack>
        <paramtype> #Type(#LiquidityBakingParamType()) </paramtype>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress </tokenAddress>
        <xtzPool> #Mutez(XtzPool => XtzPool +Int Amount) </xtzPool>
        <tokenPool> TokenPool => TokenPool -Int #XtzBought(TokenPool, XtzPool, Amount) </tokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myaddr> SelfAddress </myaddr>
        <nonce> #Nonce(N => N +Int 1) </nonce>
        <knownaddrs> KnownAddresses </knownaddrs>
        <operations> _
                  => [ Transfer_tokens #TokenTransferData(SelfAddress, To, TokenID, #XtzBought(TokenPool, XtzPool, Amount)) #Mutez(0) TokenAddress N ]
                  ;; .InternalList
        </operations>
    requires CurrentTime <Int Deadline
     andBool (XtzPool >Int 0 orBool Amount >Int 0)
     andBool #XtzBought(TokenPool, XtzPool, Amount) >=Int MinTokensBought
     andBool #XtzBought(TokenPool, XtzPool, Amount) <=Int TokenPool
     andBool TokenPool -Int #XtzBought ( TokenPool , XtzPool , Amount ) >=Int 0
     andBool #IsLegalMutezValue(XtzPool +Int Amount)

     andBool #EntrypointExists(KnownAddresses, TokenAddress, %transfer, #TokenTransferType())
endmodule
```

```k
module LIQUIDITY-BAKING-XTZTOTOKEN-NEGATIVE-SPEC
  imports LIQUIDITY-BAKING-VERIFICATION
  claim <k> #runProof(XtzToToken(_To, MinTokensBought, #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <paramtype> #Type(#LiquidityBakingParamType()) </paramtype>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress </tokenAddress>
        <xtzPool> #Mutez(XtzPool) </xtzPool>
        <tokenPool> TokenPool </tokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <knownaddrs> KnownAddresses </knownaddrs>
        <operations> _ </operations>
    requires notBool ( CurrentTime <Int Deadline
               andBool (XtzPool >Int 0 orBool Amount >Int 0)
               andBool #XtzBought(TokenPool, XtzPool, Amount) >=Int MinTokensBought
               andBool #XtzBought(TokenPool, XtzPool, Amount) <=Int TokenPool
               andBool TokenPool -Int #XtzBought ( TokenPool , XtzPool , Amount ) >=Int 0
               andBool #IsLegalMutezValue(XtzPool +Int Amount)
                     )
     andBool #EntrypointExists(KnownAddresses, TokenAddress, %transfer, #TokenTransferType())
endmodule
```

## Token To Token

```k
module LIQUIDITY-BAKING-TOKENTOTOKEN-POSITIVE-SPEC
  imports LIQUIDITY-BAKING-VERIFICATION
```

A buyer sends tokens to the Liquidity Baking contract, converts its to xtz, and then immediately purchases a corresponding amount of tokens from a Dexter contract (such that all transactions succeed or fail atomically), if the following conditions are satisfied:

1.  the current block time must be less than the deadline
2.  exactly 0 tez was transferred to this contract when it was invoked
3.  the contract at address `outputDexterContract` has a well-formed `xtz_to_token` entrypoint
4.  the amount of tokens sold, when converted into xtz using the current exchange rate, it is less than or equal to the xtz owned by the Liquidity Baking contract
5.  the contract at address `storage.tokenAddress` must have a well-formed `transfer` entry point

```k
  claim <k> #runProof(TokenToToken(OutputDexterContract, MinTokensBought, To, TokensSold, #Timestamp(Deadline))) => . </k>
        <stack> .Stack </stack>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress </tokenAddress>
        <xtzPool> #Mutez(XtzPool => XtzPool -Int #XtzBought(XtzPool, TokenPool, TokensSold)) </xtzPool>
        <tokenPool> TokenPool => TokenPool +Int TokensSold </tokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <senderaddr> Sender </senderaddr>
        <myaddr> SelfAddress </myaddr>
        <paramtype> #Type(#LiquidityBakingParamType()) </paramtype>
        <nonce> #Nonce(N => N +Int 2) </nonce>
        <knownaddrs> KnownAddresses </knownaddrs>
        <operations> _
                  => [ Transfer_tokens #TokenTransferData(Sender, SelfAddress, TokenID, TokensSold) #Mutez(0)                                          TokenAddress          N        ]
                  ;; [ Transfer_tokens Pair To Pair MinTokensBought #Timestamp(Deadline)                   #Mutez(#XtzBought(XtzPool, TokenPool, TokensSold)) OutputDexterContract (N +Int 1)]
                  ;; .InternalList
        </operations>
     requires CurrentTime <Int Deadline
      andBool Amount ==Int 0
      andBool #XtzBought(XtzPool, TokenPool, TokensSold) <=Int XtzPool
      andBool (TokenPool >Int 0 orBool TokensSold >Int 0)
      andBool #IsLegalMutezValue(#XtzBought(XtzPool, TokenPool, TokensSold))
      andBool #EntrypointExists(KnownAddresses, TokenAddress,         %transfer,   #TokenTransferType())
      andBool #EntrypointExists(KnownAddresses, OutputDexterContract, %xtzToToken, pair (address %to) (pair (nat %minTokensBought) (timestamp %deadline)))
```

```k
endmodule
```

```k
module LIQUIDITY-BAKING-TOKENTOTOKEN-NEGATIVE-SPEC
  imports LIQUIDITY-BAKING-VERIFICATION
```

```k
  claim <k> #runProof(TokenToToken(OutputDexterContract, MinTokensBought, To, TokensSold, #Timestamp(Deadline)))
         => Aborted (?_, ?_, ?_, ?_ )
        </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress </tokenAddress>
        <xtzPool> #Mutez(XtzPool) </xtzPool>
        <tokenPool> TokenPool </tokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <senderaddr> Sender </senderaddr>
        <myaddr> SelfAddress </myaddr>
        <paramtype> #Type(#LiquidityBakingParamType()) </paramtype>
        <nonce> #Nonce(N => ?_) </nonce>
        <knownaddrs> KnownAddresses </knownaddrs>
        <operations> _ </operations>
     requires notBool( CurrentTime <Int Deadline
               andBool Amount ==Int 0
               andBool #XtzBought(XtzPool, TokenPool, TokensSold) <=Int XtzPool
               andBool (TokenPool >Int 0 orBool TokensSold >Int 0)
               andBool #IsLegalMutezValue(#XtzBought(XtzPool, TokenPool, TokensSold))
                     )
      andBool #EntrypointExists(KnownAddresses, TokenAddress,         %transfer,   #TokenTransferType())
      andBool #EntrypointExists(KnownAddresses, OutputDexterContract, %xtzToToken, pair (address %to) (pair (nat %minTokensBought) (timestamp %deadline)))
```

```k
endmodule
```
