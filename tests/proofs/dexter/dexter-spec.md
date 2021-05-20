```k
module DEXTER-SPEC
  imports DEXTER-VERIFICATION
```

```k
  claim <k> now 0 => . ... </k>
        <mynow> #Timestamp(0) </mynow>

  claim <k> (now 0 => .) ~> #dexterCode(true) ... </k>
        <mynow> #Timestamp(0) </mynow>

  claim <k> (now 0 => .) ~> #dexterCode(false) ... </k>
        <mynow> #Timestamp(0) </mynow>

  claim <k> (now 0 => .) ~> #dexterCode(_) ... </k>
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

-   Preconditions

1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
2.  the deadline has not passed (i.e. the `Tezos.now < input.deadline`)
3.  the tokens transferred is less than `input.maxTokensDeposited`
4.  the liquidity minted is more than `input.minLqtMinted`
5.  xtzPool is positive

```k
module DEXTER-ADDLIQUIDITY-POSITIVE-SPEC
  imports DEXTER-VERIFICATION
```

```k
  claim <k> #runProof(IsFA2, AddLiquidity(Owner, MinLqtMinted, MaxTokensDeposited, #Timestamp(Deadline))) => . </k>
        <stack> .Stack </stack>
        <selfIsUpdatingTokenPool> false </selfIsUpdatingTokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myamount> #Mutez(Amount) </myamount>
        <myaddr> SelfAddress </myaddr>
        <lqtTotal> OldLqt => OldLqt +Int (Amount *Int OldLqt) /Int XtzAmount </lqtTotal>
        <xtzPool> #Mutez(XtzAmount => XtzAmount +Int Amount) </xtzPool>
        <tokenPool> TokenAmount => TokenAmount +Int #ceildiv(Amount *Int TokenAmount, XtzAmount) </tokenPool>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <tokenId> TokenId </tokenId>
        <lqtAddress> LqtAddress:Address </lqtAddress>
        <senderaddr> Sender </senderaddr>
        <nonce> #Nonce(Nonce => Nonce +Int 2) </nonce>
        <knownaddrs> KnownAddresses </knownaddrs>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
        <operations> _
                  => [ Transfer_tokens #TokenTransferData(IsFA2, Sender, SelfAddress, TokenId, #ceildiv(Amount *Int TokenAmount, XtzAmount)) #Mutez(0) TokenAddress Nonce ] ;;
                     [ Transfer_tokens Pair ((Amount *Int OldLqt) /Int XtzAmount) Owner #Mutez(0) LqtAddress (Nonce +Int 1) ] ;;
                     .InternalList
        </operations>
    requires CurrentTime <Int Deadline
     andBool XtzAmount   >Int 0
     andBool #ceildiv(Amount *Int TokenAmount, XtzAmount) <=Int MaxTokensDeposited
     andBool MinLqtMinted <=Int (Amount *Int OldLqt) /Int XtzAmount
     andBool #IsLegalMutezValue(Amount +Int XtzAmount)

     andBool #EntrypointExists(KnownAddresses, TokenAddress,   %transfer, #TokenTransferType(IsFA2))
     andBool #EntrypointExists(KnownAddresses,   LqtAddress, %mintOrBurn, pair int %quantity .AnnotationList address %target .AnnotationList)
```

```k
endmodule
```

The execution fails if any of the following are true:
1.  the token pool is currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
2.  the deadline has passed (i.e. the `Tezos.now < input.deadline`)
3.  the tokens transferred is more than `input.maxTokensDeposited`
4.  the liquidity minted is less than `input.minLqtMinted`
5.  xtzPool is 0

```k
module DEXTER-ADDLIQUIDITY-NEGATIVE-SPEC
  imports DEXTER-VERIFICATION
```

```k
  claim <k> #runProof(_IsFA2, AddLiquidity(_Owner, _MinLqtMinted, _MaxTokensDeposited, #Timestamp(_Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <selfIsUpdatingTokenPool> true </selfIsUpdatingTokenPool>
```

```k
  claim <k> #runProof(_IsFA2, AddLiquidity(_Owner, _MinLqtMinted, _MaxTokensDeposited, #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <mynow> #Timestamp(CurrentTime) </mynow>
    requires CurrentTime >=Int Deadline
```

```k
  claim <k> #runProof(_IsFA2, AddLiquidity(_Owner, _MinLqtMinted, MaxTokensDeposited, #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myamount> #Mutez(Amount) </myamount>
        <xtzPool> #Mutez(XtzAmount) </xtzPool>
        <tokenPool> TokenAmount </tokenPool>
    requires #ceildiv(Amount *Int TokenAmount, XtzAmount) >Int MaxTokensDeposited
```

```k
  claim <k> #runProof(_IsFA2, AddLiquidity(_Owner, _MinLqtMinted, MaxTokensDeposited, #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <myamount> #Mutez(Amount) </myamount>
        <xtzPool> #Mutez(XtzAmount) </xtzPool>
    requires notBool #IsLegalMutezValue(Amount +Int XtzAmount)
```

```k
  claim <k> #runProof(_IsFA2, AddLiquidity(_Owner, _MinLqtMinted, MaxTokensDeposited, #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <myamount> #Mutez(Amount) </myamount>
        <lqtTotal> OldLqt </lqtTotal>
    requires MinLqtMinted >Int (Amount *Int OldLqt) /Int XtzAmount
```

```k
  claim <k> #runProof(_IsFA2, AddLiquidity(_Owner, _MinLqtMinted, MaxTokensDeposited, #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <xtzPool> #Mutez(0) </xtzPool>
```

TODO: Deal with the case when the token contract or the liquidity token contract don't exist or have the wrong type.

```k
endmodule
```

## Set Manager

```k
module DEXTER-SETMANAGER-SPEC
  imports DEXTER-VERIFICATION
```

The contract sets its manager to the provided manager address if the following conditions are satisfied:

1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
2.  exactly 0 tez was transferred to this contract when it was invoked
3.  the txn sender is the `storage.manager`

```k
  claim <k> #runProof(_IsFA2, SetManager(NewManager)) => . </k>
        <stack> .Stack </stack>
        <manager> Sender => NewManager </manager>
        <selfIsUpdatingTokenPool> false </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
    requires Amount ==Int 0
```

If any of the conditions are not satisfied, the call fails.

```k
  claim <k> #runProof(_IsFA2, SetManager(_NewManager)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <manager> CurrentManager </manager>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
    requires Amount >Int 0
      orBool Sender =/=K CurrentManager
      orBool IsUpdating
```

```k
endmodule
```

## Set Baker

```k
module DEXTER-SETBAKER-SPEC
  imports DEXTER-VERIFICATION
```

The contract sets its delegate to the value of `baker` (and optionally freezes the baker to that particular value) if the following conditions are satisfied:

1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
2.  exactly 0 tez was transferred to this contract when it was invoked
3.  the txn sender is the `storage.manager`
4.  the baker is _not_ already frozen

```k
  claim <k> #runProof(_IsFA2, SetBaker(Baker, FreezeBaker)) => . </k>
        <stack> .Stack </stack>
        <manager> Sender </manager>
        <selfIsUpdatingTokenPool> false </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
        <freezeBaker> false => FreezeBaker </freezeBaker>
        <nonce> #Nonce(N => N +Int 1) </nonce>
        <operations> _ => [ Set_delegate Baker N:Int ] ;; .InternalList </operations>
    requires Amount ==Int 0
```

If any of the conditions are not satisfied, the call fails.

```k
  claim <k> #runProof(_IsFA2, SetBaker(_Baker, _NewFreezeBaker)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <manager> CurrentManager </manager>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
        <freezeBaker> FreezeBaker </freezeBaker>
    requires Amount >Int 0
      orBool IsUpdating
      orBool FreezeBaker
      orBool Sender =/=K CurrentManager
```

```k
endmodule
```

## Set LQT Address

```k
module DEXTER-SETLQTADDRESS-SPEC
  imports DEXTER-VERIFICATION
```

The contract sets its liquidity pool adddress to the provided address if the following conditions are satisifed:

1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
2.  exactly 0 tez was transferred to this contract when it was invoked
3.  the txn sender is the `storage.manager`
4.  the liquidity pool address has _not_ already been set (i.e. `storage.lqtAddress == tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU`)

```k
  claim <k> #runProof(_IsFA2, SetLQTAddress(NewLQTAddress)) => . </k>
        <stack> .Stack </stack>
        <manager> Sender </manager>
        <selfIsUpdatingTokenPool> false </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
        <lqtAddress> LQTAddress => NewLQTAddress </lqtAddress>
    requires Amount ==Int 0
     andBool LQTAddress ==K #Address("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU")
```

If any of the conditions are not satisfied, the call fails.

```k
  claim <k> #runProof(_IsFA2, SetLQTAddress(_NewLQTAddress)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <manager> CurrentManager </manager>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
        <lqtAddress> LQTAddress </lqtAddress>
    requires Amount >Int 0
      orBool IsUpdating
      orBool Sender =/=K CurrentManager
      orBool LQTAddress =/=K #Address("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU")
```

```k
endmodule
```

## Update Token Pool

```k
module DEXTER-UPDATETOKENPOOL-SPEC
  imports DEXTER-VERIFICATION
```

The contract queries its underlying token contract for its own token balance if the following conditions are satisfied:

1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
2.  exactly 0 tez was transferred to this contract when it was invoked
3.  the txn sender must be equal to the txn source
4.  if we are running the FA2 version of Dexter, then check that the contract at address `storage.tokenAddress` has a well-typed FA2 `balance_of` entrypoint;
    otherwise, check that the contract at address `storage.tokenAddress` has a well-typed FA12 `get_balance` entrypoint.

- TODO: Generalize the Michelson `SELF` and `CONTRACT` instructions to properly use annotations.
        That way, we can use the actual, full #DexterParamType in the `paramtype` cell, and in the `KnownAddresses` map.
        Right now, we need to pretend to have a more specialized type.

```k
  claim <k> #runProof(IsFA2, UpdateTokenPool) => . </k>
        <stack> .Stack </stack>
        <selfIsUpdatingTokenPool> false => true </selfIsUpdatingTokenPool>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <tokenId> TokenId </tokenId>
        <myaddr> SelfAddress </myaddr>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
        <sourceaddr> Sender </sourceaddr>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
        <knownaddrs> KnownAddresses </knownaddrs>
        <operations> _ => [ Transfer_tokens Pair #UpdateTokenPoolTransferFrom(IsFA2, SelfAddress, TokenId) #Contract(SelfAddress, #Type(#DexterVersionSpecificParamType(IsFA2))) #Mutez(0) TokenAddress O ] ;; .InternalList </operations>
        <nonce> #Nonce(O) => #Nonce(O +Int 1) </nonce>
    requires Amount ==Int 0
     andBool TokenAddress in_keys(KnownAddresses)
     andBool KnownAddresses[TokenAddress] ==K #Contract(TokenAddress, #TokenContractType(IsFA2))
```

If any of the conditions are not satisfied, the call fails.
NOTE: The failure conditions are split into two claims with identical configuration and rewrites, but different side conditions.

```k
  claim <k> #runProof(IsFA2, UpdateTokenPool) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
        <sourceaddr> Source </sourceaddr>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
        <knownaddrs> KnownAddresses </knownaddrs>
    requires Amount >Int 0
     orBool (notBool TokenAddress in_keys(KnownAddresses))
     orBool IsUpdating
     orBool Sender =/=K Source
```

```k
  claim <k> #runProof(IsFA2, UpdateTokenPool) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <myamount> #Mutez(Amount) </myamount>
        <senderaddr> Sender </senderaddr>
        <sourceaddr> Source </sourceaddr>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
        <knownaddrs> KnownAddresses </knownaddrs>
    requires (TokenAddress in_keys(KnownAddresses)
      andBool KnownAddresses[TokenAddress] ==K #Contract(_, T)
      andBool T =/=K #TokenContractType(IsFA2))
```

```k
endmodule
```

## Default

```k
module DEXTER-DEFAULT-SPEC
  imports DEXTER-VERIFICATION
```

Adds more money to the xtz reserves if the following conditions are satisifed:

1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
2.  the updated token pool size is a legal mutez value

```k
  claim <k> #runProof(_IsFA2, Default) => . </k>
        <stack> .Stack </stack>
        <selfIsUpdatingTokenPool> false </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <xtzPool> #Mutez(XtzPool => XtzPool +Int Amount) </xtzPool>
     requires #IsLegalMutezValue(XtzPool +Int Amount)
```

If any of the conditions are not satisfied, the call fails.

```k
  claim <k> #runProof(_IsFA2, Default) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <xtzPool> #Mutez(XtzPool) </xtzPool>
    requires IsUpdating
      orBool notBool #IsLegalMutezValue(XtzPool +Int Amount)
```

```k
endmodule
```
