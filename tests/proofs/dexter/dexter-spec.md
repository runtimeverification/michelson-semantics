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

### Positive case

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
     andBool (Amount *Int TokenAmount) %Int XtzAmount ==Int 0
     andBool MinLqtMinted <=Int (Amount *Int OldLqt) /Int XtzAmount
     andBool #IsLegalMutezValue(Amount +Int XtzAmount)

     andBool #EntrypointExists(KnownAddresses, TokenAddress,   %transfer, #TokenTransferType(IsFA2))
     andBool #EntrypointExists(KnownAddresses,   LqtAddress, %mintOrBurn, pair int %quantity .AnnotationList address %target .AnnotationList)
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
     andBool (Amount *Int TokenAmount) %Int XtzAmount =/=Int 0
     andBool MinLqtMinted <=Int (Amount *Int OldLqt) /Int XtzAmount
     andBool #IsLegalMutezValue(Amount +Int XtzAmount)

     andBool #EntrypointExists(KnownAddresses, TokenAddress,   %transfer, #TokenTransferType(IsFA2))
     andBool #EntrypointExists(KnownAddresses,   LqtAddress, %mintOrBurn, pair int %quantity .AnnotationList address %target .AnnotationList)
```

```k
endmodule
```

### Negative case

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
  claim <k> #runProof(IsFA2, AddLiquidity(_Owner, MinLqtMinted, MaxTokensDeposited, #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myamount> #Mutez(Amount) </myamount>
        <lqtTotal> OldLqt </lqtTotal>
        <xtzPool> #Mutez(XtzAmount) </xtzPool>
        <tokenPool> TokenAmount </tokenPool>
    requires notBool IsFA2
     andBool ( IsUpdating
        orBool CurrentTime >=Int Deadline
        orBool #ceildiv(Amount *Int TokenAmount, XtzAmount) >Int MaxTokensDeposited
        orBool notBool #IsLegalMutezValue(Amount +Int XtzAmount)
        orBool MinLqtMinted >Int (Amount *Int OldLqt) /Int XtzAmount
        orBool XtzAmount ==Int 0
             )

  claim <k> #runProof(IsFA2, AddLiquidity(_Owner, MinLqtMinted, MaxTokensDeposited, #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myamount> #Mutez(Amount) </myamount>
        <lqtTotal> OldLqt </lqtTotal>
        <xtzPool> #Mutez(XtzAmount) </xtzPool>
        <tokenPool> TokenAmount </tokenPool>
    requires IsFA2
     andBool ( IsUpdating
        orBool CurrentTime >=Int Deadline
        orBool #ceildiv(Amount *Int TokenAmount, XtzAmount) >Int MaxTokensDeposited
        orBool notBool #IsLegalMutezValue(Amount +Int XtzAmount)
        orBool MinLqtMinted >Int (Amount *Int OldLqt) /Int XtzAmount
        orBool XtzAmount ==Int 0
             )
```

TODO: Deal with the case when the token contract or the liquidity token contract don't exist or have the wrong type.

```k
endmodule
```

## Remove Liquidity

The sender can burn liquidity tokens in exchange for tez and tokens sent to some address if the following conditions are satisfied:

1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
2.  exactly 0 tez was transferred to this contract when it was invoked
3.  the current block time must be less than the deadline
4.  the amount of liquidity to be redeemed, when converted to xtz, is greater than `minXtzWithdrawn` and less than the amount of tez owned by the Dexter contract
5.  the amount of liquidity to be redeemed, when converted to tokens, is greater than `minTokensWithdrawn` and less than the amount of tokens owned by the Dexter contract
6.  the amount of liquidity to be redeemed is less than the total amount of liquidity and less than the amount of liquidity tokens owned by the sender
7.  the contract at address `storage.lqtAddress` has a well-formed `mintOrBurn` entrypoint
8.  the contract at address `storage.tokenAddress` has a well-formed `transfer` entrypoint

```k
module DEXTER-REMOVELIQUIDITY-POSITIVE-SPEC
  imports DEXTER-VERIFICATION

  claim <k> #runProof(IsFA2, RemoveLiquidity(To, LqtBurned, #Mutez(MinXtzWithdrawn), MinTokensWithdrawn, #Timestamp(Deadline))) => . </k>
        <stack> .Stack </stack>
        <selfIsUpdatingTokenPool> false </selfIsUpdatingTokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myamount> #Mutez(0) </myamount>
        <myaddr> SelfAddress </myaddr>
        <lqtTotal> OldLqt => OldLqt -Int LqtBurned </lqtTotal>
        <xtzPool> #Mutez(XtzAmount => XtzAmount -Int (LqtBurned *Int XtzAmount) /Int OldLqt) </xtzPool>
        <tokenPool> TokenAmount => TokenAmount -Int (LqtBurned *Int TokenAmount) /Int OldLqt </tokenPool>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <tokenId> TokenId </tokenId>
        <lqtAddress> LqtAddress:Address </lqtAddress>
        <senderaddr> Sender </senderaddr>
        <nonce> #Nonce(Nonce => Nonce +Int 3) </nonce>
        <knownaddrs> KnownAddresses </knownaddrs>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype> // 1027
        <operations> _
                  => [ Transfer_tokens (Pair (0 -Int LqtBurned) Sender) #Mutez(0) LqtAddress Nonce ] ;;
                     [ Transfer_tokens #TokenTransferData(IsFA2, SelfAddress, To, TokenId,  (LqtBurned *Int TokenAmount) /Int OldLqt) #Mutez(0) TokenAddress (Nonce +Int 1) ] ;;
                     [ Transfer_tokens Unit #Mutez((LqtBurned *Int XtzAmount) /Int OldLqt) To (Nonce +Int 2) ] ;;
                     .InternalList
        </operations>
    requires CurrentTime <Int Deadline
     andBool OldLqt >Int 0
     andBool OldLqt >=Int LqtBurned
     andBool MinXtzWithdrawn <=Int (LqtBurned *Int XtzAmount) /Int OldLqt
     andBool MinTokensWithdrawn <=Int (LqtBurned *Int TokenAmount) /Int OldLqt
     andBool #EntrypointExists(KnownAddresses, TokenAddress,   %transfer, #TokenTransferType(IsFA2))
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
module DEXTER-REMOVELIQUIDITY-NEGATIVE-SPEC
  imports DEXTER-VERIFICATION

  claim <k> #runProof(_IsFA2, RemoveLiquidity(_, _, _, _, _)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <selfIsUpdatingTokenPool> true </selfIsUpdatingTokenPool>

  claim <k> #runProof(_IsFA2, RemoveLiquidity(_, _, _, _, _)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <myamount> #Mutez(Amount) </myamount>
    requires Amount >Int 0

  claim <k> #runProof(_IsFA2, RemoveLiquidity(_, _, _, _, #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <mynow> #Timestamp(CurrentTime) </mynow>
    requires CurrentTime >=Int Deadline

  claim <k> #runProof(_IsFA2, RemoveLiquidity(_, LqtBurned, _, _, _)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <lqtTotal> OldLqt </lqtTotal>
    requires OldLqt ==Int 0
      orBool OldLqt <Int LqtBurned

  claim <k> #runProof(_IsFA2, RemoveLiquidity(_, LqtBurned, #Mutez(MinXtzWithdrawn), _, _)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <lqtTotal> OldLqt </lqtTotal>
        <xtzPool> #Mutez(XtzAmount) </xtzPool>
    requires MinXtzWithdrawn >Int (LqtBurned *Int XtzAmount) /Int OldLqt

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

## Update Token Pool Internal

Summary: The underlying token contract updates the Dexter contract's view of its own token balance if the following conditions are satisifed:

1.  the token pool _is_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = true`)
2.  exactly 0 tez was transferred to this contract when it was invoked
3.  if using version FA2, the input parameter list is non-empty.
4.  the sender address is equal to `storage.tokenAddress`

```k
  claim <k> #runProof(false, UpdateTokenPoolInternalFA12(Balance)) => . </k>
        <stack> .Stack </stack>
        <selfIsUpdatingTokenPool> true => false </selfIsUpdatingTokenPool>
        <myamount> #Mutez(0) </myamount>
        <tokenPool> _ => Balance </tokenPool>
        <tokenAddress> TokenAddress </tokenAddress>
        <senderaddr> TokenAddress </senderaddr>
```

In the FA2, UpdateTokenPoolInternal ignores all balances additional balances passed in:

```k
  claim <k> #runProof(true, UpdateTokenPoolInternalFA2([ Pair Pair _ _ Balance ] ;; _BalanceOfResultRest)) => . </k>
        <stack> .Stack </stack>
        <selfIsUpdatingTokenPool> true => false </selfIsUpdatingTokenPool>
        <myamount> #Mutez(0) </myamount>
        <tokenPool> _ => Balance </tokenPool>
        <tokenAddress> TokenAddress </tokenAddress>
        <senderaddr> TokenAddress </senderaddr>
```

### Error conditions

If any of the conditions are not satisfied, the call fails.
Due to K failing to simplify side conditions when specified as a disjunction,
we use one claim per failing side condition.

#### FA12

For FA12, we reach a failing state when any of these conditions hold:

1.  `Amount >Int 0`
2.  `notBool IsUpdating`
3.  `TokenAddress =/=K Sender`

The following claims cover these cases:

```k
  claim <k> #runProof(false, UpdateTokenPoolInternalFA12(_)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress </tokenAddress>
        <senderaddr> Sender </senderaddr>
     requires Amount >Int 0
      andBool IsUpdating
      andBool TokenAddress ==K Sender
```

```k
  claim <k> #runProof(false, UpdateTokenPoolInternalFA12(_)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(_Amount) </myamount>
        <tokenAddress> _TokenAddress </tokenAddress>
        <senderaddr> _Sender </senderaddr>
     requires (notBool IsUpdating)
```

```k
  claim <k> #runProof(false, UpdateTokenPoolInternalFA12(_)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <selfIsUpdatingTokenPool> _IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(_Amount) </myamount>
        <tokenAddress> TokenAddress </tokenAddress>
        <senderaddr> Sender </senderaddr>
     requires TokenAddress =/=K Sender
```


#### FA2

For FA2, we reach a failing state when any of these conditions hold:

1.  `Amount >Int 0`
2.  `notBool IsUpdating`
3.  `TokenAddress =/=K Sender`
4.  `BalanceOfResult ==K .InternalList`

The following claims cover these cases:

```k
  claim <k> #runProof(true, UpdateTokenPoolInternalFA2(BalanceOfResult)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> #Address(TokenAddress) </tokenAddress>
        <senderaddr> #Address(Sender) </senderaddr>
     requires Amount >Int 0
      andBool IsUpdating
      andBool TokenAddress ==K Sender
      andBool BalanceOfResult =/=K .InternalList
```

```k
  claim <k> #runProof(true, UpdateTokenPoolInternalFA2(BalanceOfResult)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> #Address(TokenAddress) </tokenAddress>
        <senderaddr> #Address(Sender) </senderaddr>
     requires (notBool IsUpdating)
```

```k
  claim <k> #runProof(true, UpdateTokenPoolInternalFA2(BalanceOfResult)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> #Address(TokenAddress) </tokenAddress>
        <senderaddr> #Address(Sender) </senderaddr>
     requires TokenAddress =/=K Sender
```

```k
  claim <k> #runProof(true, UpdateTokenPoolInternalFA2(BalanceOfResult)) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ( Failed ?_ ) </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> #Address(TokenAddress) </tokenAddress>
        <senderaddr> #Address(Sender) </senderaddr>
     requires TokenAddress ==K Sender
      andBool BalanceOfResult ==K .InternalList
```

```k
endmodule
```

## Token To XTZ

### FA1.2

```k
module DEXTER-TOKENTOXTZ-FA12-POSITIVE-SPEC
  imports DEXTER-VERIFICATION
```

A buyer sends tokens to the Dexter contract and receives a corresponding amount of xtz, if the following conditions are satisfied:

1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
2.  the current block time must be less than the deadline
3.  exactly 0 tez was transferred to this contract when it was invoked
4.  the amount of tokens sold, when converted into xtz using the current exchange rate, is greater than `minXtzBought`
5.  the amount of tokens sold, when converted into xtz using the current exchange rate, it is less than or equal to the xtz owned by the Dexter contract

```k
  claim <k> #runProof(IsFA2, TokenToXtz(To, TokensSold, #Mutez(MinXtzBought), #Timestamp(Deadline))) => . </k>
        <stack> .Stack </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <xtzPool> #Mutez(XtzPool => XtzPool -Int #XtzBought(XtzPool, TokenPool, TokensSold)) </xtzPool>
        <tokenPool> TokenPool => TokenPool +Int TokensSold </tokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <senderaddr> Sender </senderaddr>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
        <myaddr> SelfAddress:Address </myaddr>
        <nonce> #Nonce(N => N +Int 2) </nonce>
        <knownaddrs> KnownAddresses </knownaddrs>
        <tokenId> TokenID </tokenId>
        <operations> _
                  => [ Transfer_tokens #TokenTransferData(IsFA2, Sender, SelfAddress, TokenID, TokensSold) #Mutez(0)                                          TokenAddress  N        ]
                  ;; [ Transfer_tokens Unit                                                                #Mutez(#XtzBought(XtzPool, TokenPool, TokensSold)) To           (N +Int 1)]
                  ;; .InternalList
        </operations>
     requires notBool IsFA2
      andBool notBool IsUpdating
      andBool Amount ==Int 0
      andBool CurrentTime <Int Deadline
      andBool (TokenPool >Int 0 orBool TokensSold >Int 0)
      andBool (TokenPool >=Int 0) // Type Invariant
      andBool #XtzBought(XtzPool, TokenPool, TokensSold) >=Int  MinXtzBought
      andBool #XtzBought(XtzPool, TokenPool, TokensSold) <=Int XtzPool
      andBool #IsLegalMutezValue(MinXtzBought)
      andBool #IsLegalMutezValue(#XtzBought(XtzPool, TokenPool, TokensSold))
      andBool #IsLegalMutezValue(XtzPool:Int -Int #XtzBought (XtzPool:Int, TokenPool:Int, TokensSold:Int))
      andBool #EntrypointExists(KnownAddresses, TokenAddress, %transfer,                             #TokenTransferType(IsFA2))
      andBool #EntrypointExists(KnownAddresses, To,           #token("%default", "FieldAnnotation"), #Type(unit))
endmodule
```

The following claims prove the negative case:

```k
module DEXTER-TOKENTOXTZ-FA12-NEGATIVE-1-SPEC
  imports DEXTER-VERIFICATION
  claim <k> #runProof(IsFA2, TokenToXtz(_To, _TokensSold, #Mutez(_MinXtzBought), #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
     requires notBool IsFA2
      andBool ( IsUpdating
         orBool notBool Amount ==Int 0
         orBool notBool CurrentTime <Int Deadline
              )
endmodule
```

```k
module DEXTER-TOKENTOXTZ-FA12-NEGATIVE-2-SPEC
  imports DEXTER-VERIFICATION
  claim <k> #runProof(IsFA2, TokenToXtz(To, TokensSold, #Mutez(MinXtzBought), #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <xtzPool> #Mutez(XtzPool) </xtzPool>
        <tokenPool> TokenPool </tokenPool>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
     requires notBool IsFA2
      andBool notBool IsUpdating
      andBool notBool Amount ==Int 0
      andBool notBool CurrentTime <Int Deadline
      andBool #EntrypointExists(KnownAddresses, TokenAddress, %transfer,                             #TokenTransferType(IsFA2))
      andBool #EntrypointExists(KnownAddresses, To,           #token("%default", "FieldAnnotation"), #Type(unit))
      andBool notBool (TokenPool >Int 0 orBool TokensSold >Int 0)
endmodule
```

```k
module DEXTER-TOKENTOXTZ-FA12-NEGATIVE-3-SPEC
  imports DEXTER-VERIFICATION
  claim <k> #runProof(IsFA2, TokenToXtz(To, TokensSold, #Mutez(MinXtzBought), #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <xtzPool> #Mutez(XtzPool) </xtzPool>
        <tokenPool> TokenPool </tokenPool>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
     requires notBool IsFA2
      andBool notBool IsUpdating
      andBool notBool Amount ==Int 0
      andBool notBool CurrentTime <Int Deadline
      andBool #EntrypointExists(KnownAddresses, TokenAddress, %transfer,                             #TokenTransferType(IsFA2))
      andBool #EntrypointExists(KnownAddresses, To,           #token("%default", "FieldAnnotation"), #Type(unit))
      andBool notBool( #XtzBought(XtzPool, TokenPool, TokensSold) >=Int  MinXtzBought
               andBool #IsLegalMutezValue(MinXtzBought)
                     )
endmodule
```

```k
module DEXTER-TOKENTOXTZ-FA12-NEGATIVE-4-SPEC
  imports DEXTER-VERIFICATION
  claim <k> #runProof(IsFA2, TokenToXtz(To, TokensSold, #Mutez(MinXtzBought), #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <xtzPool> #Mutez(XtzPool) </xtzPool>
        <tokenPool> TokenPool </tokenPool>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
     requires notBool IsFA2
      andBool notBool IsUpdating
      andBool notBool Amount ==Int 0
      andBool notBool CurrentTime <Int Deadline
      andBool #EntrypointExists(KnownAddresses, TokenAddress, %transfer,                             #TokenTransferType(IsFA2))
      andBool #EntrypointExists(KnownAddresses, To,           #token("%default", "FieldAnnotation"), #Type(unit))
      andBool  #XtzBought(XtzPool, TokenPool, TokensSold) >=Int  MinXtzBought
      andBool #IsLegalMutezValue(MinXtzBought)
      andBool notBool( #XtzBought(XtzPool, TokenPool, TokensSold) <=Int XtzPool
               andBool #IsLegalMutezValue(#XtzBought(XtzPool, TokenPool, TokensSold))
               andBool #IsLegalMutezValue(XtzPool:Int -Int #XtzBought (XtzPool:Int, TokenPool:Int, TokensSold:Int))
                     )
endmodule
```

### FA2

```k
module DEXTER-TOKENTOXTZ-FA2-POSITIVE-SPEC
  imports DEXTER-VERIFICATION
```

As before, a buyer sends tokens to the Dexter contract and receives a corresponding amount of xtz, if the following conditions are satisfied:

1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
2.  the current block time must be less than the deadline
3.  exactly 0 tez was transferred to this contract when it was invoked
4.  the amount of tokens sold, when converted into xtz using the current exchange rate, is greater than `minXtzBought`
5.  the amount of tokens sold, when converted into xtz using the current exchange rate, it is less than or equal to the xtz owned by the Dexter contract

```k
  claim <k> #runProof(IsFA2, TokenToXtz(To, TokensSold, #Mutez(MinXtzBought), #Timestamp(Deadline))) => . </k>
        <stack> .Stack </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <xtzPool> #Mutez(XtzPool => XtzPool -Int #XtzBought(XtzPool, TokenPool, TokensSold)) </xtzPool>
        <tokenPool> TokenPool => TokenPool +Int TokensSold </tokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <senderaddr> Sender </senderaddr>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
        <myaddr> SelfAddress:Address </myaddr>
        <nonce> #Nonce(N => N +Int 2) </nonce>
        <knownaddrs> KnownAddresses </knownaddrs>
        <tokenId> TokenID </tokenId>
        <operations> _
                  => [ Transfer_tokens #TokenTransferData(IsFA2, Sender, SelfAddress, TokenID, TokensSold) #Mutez(0)                                          TokenAddress  N        ]
                  ;; [ Transfer_tokens Unit                                                                #Mutez(#XtzBought(XtzPool, TokenPool, TokensSold)) To           (N +Int 1)]
                  ;; .InternalList
        </operations>
     requires IsFA2
      andBool notBool IsUpdating
      andBool Amount ==Int 0
      andBool CurrentTime <Int Deadline
      andBool (TokenPool >Int 0 orBool TokensSold >Int 0)
      andBool (TokenPool >=Int 0)
      andBool #XtzBought(XtzPool, TokenPool, TokensSold) >=Int  MinXtzBought
      andBool #XtzBought(XtzPool, TokenPool, TokensSold) <=Int XtzPool
      andBool #IsLegalMutezValue(MinXtzBought)
      andBool #IsLegalMutezValue(#XtzBought(XtzPool, TokenPool, TokensSold))
      andBool #IsLegalMutezValue(XtzPool:Int -Int #XtzBought (XtzPool:Int, TokenPool:Int, TokensSold:Int))
      andBool #EntrypointExists(KnownAddresses, TokenAddress, %transfer,                             #TokenTransferType(IsFA2))
      andBool #EntrypointExists(KnownAddresses, To,           #token("%default", "FieldAnnotation"), #Type(unit))
endmodule
```

The following cases prove the contract properly fails when these conditions aren't met.

```k
module DEXTER-TOKENTOXTZ-FA2-NEGATIVE-1-SPEC
  imports DEXTER-VERIFICATION
  claim <k> #runProof(IsFA2, TokenToXtz(To, TokensSold, #Mutez(MinXtzBought), #Timestamp(Deadline))) =>  Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
     requires IsFA2
      andBool ( IsUpdating
         orBool notBool Amount ==Int 0
         orBool notBool CurrentTime <Int Deadline
              )
endmodule
```

```k
module DEXTER-TOKENTOXTZ-FA2-NEGATIVE-2-SPEC
  imports DEXTER-VERIFICATION
  claim <k> #runProof(IsFA2, TokenToXtz(To, TokensSold, #Mutez(MinXtzBought), #Timestamp(Deadline))) =>  Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
        <tokenPool> TokenPool </tokenPool>
        <xtzPool> #Mutez(XtzPool) </xtzPool>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <knownaddrs> KnownAddresses </knownaddrs>
     requires IsFA2
      andBool notBool IsUpdating
      andBool Amount ==Int 0
      andBool CurrentTime <Int Deadline
      andBool (TokenPool >=Int 0)
      andBool notBool ( (TokenPool >Int 0 orBool TokensSold >Int 0)
                andBool #XtzBought(XtzPool, TokenPool, TokensSold) >=Int  MinXtzBought
                andBool #XtzBought(XtzPool, TokenPool, TokensSold) <=Int XtzPool
                      )
      andBool #IsLegalMutezValue(MinXtzBought)
      andBool #IsLegalMutezValue(#XtzBought(XtzPool, TokenPool, TokensSold))
      andBool #IsLegalMutezValue(XtzPool:Int -Int #XtzBought (XtzPool:Int, TokenPool:Int, TokensSold:Int))
      andBool #EntrypointExists(KnownAddresses, TokenAddress, %transfer,                             #TokenTransferType(IsFA2))
      andBool #EntrypointExists(KnownAddresses, To,           #token("%default", "FieldAnnotation"), #Type(unit))
endmodule
```

```k
module DEXTER-TOKENTOXTZ-FA2-NEGATIVE-3-SPEC
  imports DEXTER-VERIFICATION
  claim <k> #runProof(IsFA2, TokenToXtz(To, TokensSold, #Mutez(MinXtzBought), #Timestamp(Deadline))) =>  Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
        <tokenPool> TokenPool </tokenPool>
        <xtzPool> #Mutez(XtzPool) </xtzPool>
        <tokenAddress> TokenAddress:Address </tokenAddress>
        <knownaddrs> KnownAddresses </knownaddrs>
        <nonce> #Nonce(N => ?_) </nonce>
     requires IsFA2
      andBool notBool IsUpdating
      andBool Amount ==Int 0
      andBool CurrentTime <Int Deadline
      andBool (TokenPool >=Int 0)
      andBool (TokenPool >Int 0 orBool TokensSold >Int 0)
      andBool #XtzBought(XtzPool, TokenPool, TokensSold) >=Int  MinXtzBought
      andBool #XtzBought(XtzPool, TokenPool, TokensSold) <=Int XtzPool
      andBool notBool ( #IsLegalMutezValue(MinXtzBought)
                andBool #IsLegalMutezValue(#XtzBought(XtzPool, TokenPool, TokensSold))
                andBool #IsLegalMutezValue(XtzPool:Int -Int #XtzBought (XtzPool:Int, TokenPool:Int, TokensSold:Int))
                      )
      andBool #EntrypointExists(KnownAddresses, TokenAddress, %transfer,                             #TokenTransferType(IsFA2))
      andBool #EntrypointExists(KnownAddresses, To,           #token("%default", "FieldAnnotation"), #Type(unit))
```

```k
endmodule
```

## XTZ To Token

### FA 1.2

A buyer sends xtz to the Dexter contract and receives a corresponding amount of tokens, if the following conditions are satisfied:

1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
2.  the current block time must be less than the deadline
3.  when the `txn.amount` (in mutez) is converted into tokens using the current exchange rate, the purchased amount is greater than `minTokensBought`
4.  when the `txn.amount` (in mutez) is converted into tokens using the current exchange rate, it is less than or equal to the tokens owned by the Dexter contract

```k
module DEXTER-XTZTOTOKEN-FA12-POSITIVE-SPEC
  imports DEXTER-VERIFICATION
  claim <k> #runProof(IsFA2, XtzToToken(To, MinTokensBought, #Timestamp(Deadline))) => . </k>
        <stack> .Stack </stack>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress </tokenAddress>
        <xtzPool> #Mutez(XtzPool => XtzPool +Int Amount) </xtzPool>
        <tokenPool> TokenPool => TokenPool -Int #XtzBought(TokenPool, XtzPool, Amount) </tokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myaddr> SelfAddress </myaddr>
        <nonce> #Nonce(N => N +Int 1) </nonce>
        <tokenId> TokenID </tokenId>
        <knownaddrs> KnownAddresses </knownaddrs>
        <operations> _
                  => [ Transfer_tokens #TokenTransferData(IsFA2, SelfAddress, To, TokenID, #XtzBought(TokenPool, XtzPool, Amount)) #Mutez(0) TokenAddress N ]
                  ;; .InternalList
        </operations>
    requires notBool IsFA2
     andBool notBool IsUpdating
     andBool CurrentTime <Int Deadline
     andBool (XtzPool >Int 0 orBool Amount >Int 0)
     andBool #XtzBought(TokenPool, XtzPool, Amount) >=Int  MinTokensBought
     andBool #XtzBought(TokenPool, XtzPool, Amount) <=Int TokenPool
     andBool TokenPool -Int #XtzBought ( TokenPool , XtzPool , Amount ) >=Int 0
     andBool #IsLegalMutezValue(XtzPool +Int Amount)

     andBool #EntrypointExists(KnownAddresses, TokenAddress, %transfer, #TokenTransferType(IsFA2))
endmodule
```

```k
module DEXTER-XTZTOTOKEN-FA12-NEGATIVE-SPEC
  imports DEXTER-VERIFICATION
  claim <k> #runProof(IsFA2, XtzToToken(_To, MinTokensBought, #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress </tokenAddress>
        <xtzPool> #Mutez(XtzPool) </xtzPool>
        <tokenPool> TokenPool </tokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <knownaddrs> KnownAddresses </knownaddrs>
        <operations> _ </operations>
    requires notBool IsFA2
     andBool notBool ( notBool IsUpdating
               andBool CurrentTime <Int Deadline
               andBool (XtzPool >Int 0 orBool Amount >Int 0)
               andBool #XtzBought(TokenPool, XtzPool, Amount) >=Int MinTokensBought
               andBool #XtzBought(TokenPool, XtzPool, Amount) <=Int TokenPool
               andBool TokenPool -Int #XtzBought ( TokenPool , XtzPool , Amount ) >=Int 0
               andBool #IsLegalMutezValue(XtzPool +Int Amount)
                     )
     andBool #EntrypointExists(KnownAddresses, TokenAddress, %transfer, #TokenTransferType(IsFA2))
endmodule
```

```k
module DEXTER-XTZTOTOKEN-FA2-POSITIVE-SPEC
  imports DEXTER-VERIFICATION
  claim <k> #runProof(IsFA2, XtzToToken(To, MinTokensBought, #Timestamp(Deadline))) => . </k>
        <stack> .Stack </stack>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress </tokenAddress>
        <xtzPool> #Mutez(XtzPool => XtzPool +Int Amount) </xtzPool>
        <tokenPool> TokenPool => TokenPool -Int #XtzBought(TokenPool, XtzPool, Amount) </tokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myaddr> SelfAddress </myaddr>
        <nonce> #Nonce(N => N +Int 1) </nonce>
        <tokenId> TokenID </tokenId>
        <knownaddrs> KnownAddresses </knownaddrs>
        <operations> _
                  => [ Transfer_tokens #TokenTransferData(IsFA2, SelfAddress, To, TokenID, #XtzBought(TokenPool, XtzPool, Amount)) #Mutez(0) TokenAddress N ]
                  ;; .InternalList
        </operations>
    requires IsFA2
     andBool notBool IsUpdating
     andBool CurrentTime <Int Deadline
     andBool (XtzPool >Int 0 orBool Amount >Int 0)
     andBool #XtzBought(TokenPool, XtzPool, Amount) >=Int  MinTokensBought
     andBool #XtzBought(TokenPool, XtzPool, Amount) <=Int TokenPool
     andBool TokenPool -Int #XtzBought ( TokenPool , XtzPool , Amount ) >=Int 0
     andBool #IsLegalMutezValue(XtzPool +Int Amount)

     andBool #EntrypointExists(KnownAddresses, TokenAddress, %transfer, #TokenTransferType(IsFA2))
endmodule
```

```k
module DEXTER-XTZTOTOKEN-FA2-NEGATIVE-SPEC
  imports DEXTER-VERIFICATION
  claim <k> #runProof(IsFA2, XtzToToken(_To, MinTokensBought, #Timestamp(Deadline))) => Aborted(?_, ?_, ?_, ?_) </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress </tokenAddress>
        <xtzPool> #Mutez(XtzPool) </xtzPool>
        <tokenPool> TokenPool </tokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <knownaddrs> KnownAddresses </knownaddrs>
        <operations> _ </operations>
    requires IsFA2
     andBool notBool ( notBool IsUpdating
               andBool CurrentTime <Int Deadline
               andBool (XtzPool >Int 0 orBool Amount >Int 0)
               andBool #XtzBought(TokenPool, XtzPool, Amount) >=Int MinTokensBought
               andBool #XtzBought(TokenPool, XtzPool, Amount) <=Int TokenPool
               andBool TokenPool -Int #XtzBought ( TokenPool , XtzPool , Amount ) >=Int 0
               andBool #IsLegalMutezValue(XtzPool +Int Amount)
                     )
     andBool #EntrypointExists(KnownAddresses, TokenAddress, %transfer, #TokenTransferType(IsFA2))
endmodule
```

## Token To Token

```k
module DEXTER-TOKENTOTOKEN-POSITIVE-SPEC
  imports DEXTER-VERIFICATION
```

A buyer sends tokens to the Dexter contract, converts its to xtz, and then immediately purchases a corresponding amount of tokens from a different Dexter contract (such that all transactions succeed or fail atomically), if the following conditions are satisfied:

1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
2.  the current block time must be less than the deadline
3.  exactly 0 tez was transferred to this contract when it was invoked
4.  the contract at address `outputDexterContract` has a well-formed `xtz_to_token` entrypoint
5.  the amount of tokens sold, when converted into xtz using the current exchange rate, it is less than or equal to the xtz owned by the current Dexter contract
6.  the contract at address `storage.tokenAddress` must have a well-formed `transfer` entry point

```k
  claim <k> #runProof(IsFA2, TokenToToken(OutputDexterContract, MinTokensBought, To, TokensSold, #Timestamp(Deadline))) => . </k>
        <stack> .Stack </stack>
        <selfIsUpdatingTokenPool> SelfIsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress </tokenAddress>
        <xtzPool> #Mutez(XtzPool => XtzPool -Int #XtzBought(XtzPool, TokenPool, TokensSold)) </xtzPool>
        <tokenPool> TokenPool => TokenPool +Int TokensSold </tokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <senderaddr> Sender </senderaddr>
        <myaddr> SelfAddress </myaddr>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
        <nonce> #Nonce(N => N +Int 2) </nonce>
        <tokenId> TokenID </tokenId>
        <knownaddrs> KnownAddresses </knownaddrs>
        <operations> _
                  => [ Transfer_tokens #TokenTransferData(IsFA2, Sender, SelfAddress, TokenID, TokensSold) #Mutez(0)                                          TokenAddress          N        ]
                  ;; [ Transfer_tokens Pair To Pair MinTokensBought #Timestamp(Deadline)                   #Mutez(#XtzBought(XtzPool, TokenPool, TokensSold)) OutputDexterContract (N +Int 1)]
                  ;; .InternalList
        </operations>
     requires notBool IsFA2
      andBool notBool SelfIsUpdating
      andBool CurrentTime <Int Deadline
      andBool Amount ==Int 0
      andBool #XtzBought(XtzPool, TokenPool, TokensSold) <=Int XtzPool
      andBool (TokenPool >Int 0 orBool TokensSold >Int 0) 
      andBool #IsLegalMutezValue(#XtzBought(XtzPool, TokenPool, TokensSold))
      andBool #EntrypointExists(KnownAddresses, TokenAddress,         %transfer,   #TokenTransferType(IsFA2))
      andBool #EntrypointExists(KnownAddresses, OutputDexterContract, %xtzToToken, pair (address %to) (pair (nat %minTokensBought) (timestamp %deadline)))
```

```k
  claim <k> #runProof(IsFA2, TokenToToken(OutputDexterContract, MinTokensBought, To, TokensSold, #Timestamp(Deadline))) => . </k>
        <stack> .Stack </stack>
        <selfIsUpdatingTokenPool> SelfIsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress </tokenAddress>
        <xtzPool> #Mutez(XtzPool => XtzPool -Int #XtzBought(XtzPool, TokenPool, TokensSold)) </xtzPool>
        <tokenPool> TokenPool => TokenPool +Int TokensSold </tokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <senderaddr> Sender </senderaddr>
        <myaddr> SelfAddress </myaddr>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
        <nonce> #Nonce(N => N +Int 2) </nonce>
        <tokenId> TokenID </tokenId>
        <knownaddrs> KnownAddresses </knownaddrs>
        <operations> _
                  => [ Transfer_tokens #TokenTransferData(IsFA2, Sender, SelfAddress, TokenID, TokensSold) #Mutez(0)                                          TokenAddress          N        ]
                  ;; [ Transfer_tokens Pair To Pair MinTokensBought #Timestamp(Deadline)                   #Mutez(#XtzBought(XtzPool, TokenPool, TokensSold)) OutputDexterContract (N +Int 1)]
                  ;; .InternalList
        </operations>
     requires IsFA2
      andBool notBool SelfIsUpdating
      andBool CurrentTime <Int Deadline
      andBool Amount ==Int 0
      andBool #XtzBought(XtzPool, TokenPool, TokensSold) <=Int XtzPool
      andBool (TokenPool >Int 0 orBool TokensSold >Int 0) 
      andBool #IsLegalMutezValue(#XtzBought(XtzPool, TokenPool, TokensSold))
      andBool #EntrypointExists(KnownAddresses, TokenAddress,         %transfer,   #TokenTransferType(IsFA2))
      andBool #EntrypointExists(KnownAddresses, OutputDexterContract, %xtzToToken, pair (address %to) (pair (nat %minTokensBought) (timestamp %deadline)))
```

```k
endmodule
```

```k
module DEXTER-TOKENTOTOKEN-NEGATIVE-SPEC
  imports DEXTER-VERIFICATION
```

```k
  claim <k> #runProof(IsFA2, TokenToToken(OutputDexterContract, MinTokensBought, To, TokensSold, #Timestamp(Deadline)))
         => Aborted (?_, ?_, ?_, ?_ )
        </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <selfIsUpdatingTokenPool> SelfIsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress </tokenAddress>
        <xtzPool> #Mutez(XtzPool) </xtzPool>
        <tokenPool> TokenPool </tokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <senderaddr> Sender </senderaddr>
        <myaddr> SelfAddress </myaddr>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
        <nonce> #Nonce(N => ?_) </nonce>
        <tokenId> TokenID </tokenId>
        <knownaddrs> KnownAddresses </knownaddrs>
        <operations> _ </operations>
     requires notBool IsFA2
      andBool notBool( notBool SelfIsUpdating 
               andBool CurrentTime <Int Deadline
               andBool Amount ==Int 0
               andBool #XtzBought(XtzPool, TokenPool, TokensSold) <=Int XtzPool
               andBool (TokenPool >Int 0 orBool TokensSold >Int 0) 
               andBool #IsLegalMutezValue(#XtzBought(XtzPool, TokenPool, TokensSold))
                     )
      andBool #EntrypointExists(KnownAddresses, TokenAddress,         %transfer,   #TokenTransferType(IsFA2))
      andBool #EntrypointExists(KnownAddresses, OutputDexterContract, %xtzToToken, pair (address %to) (pair (nat %minTokensBought) (timestamp %deadline)))
```

```k
  claim <k> #runProof(IsFA2, TokenToToken(OutputDexterContract, MinTokensBought, To, TokensSold, #Timestamp(Deadline)))
         => Aborted (?_, ?_, ?_, ?_ )
        </k>
        <stack> .Stack => ?_:FailedStack </stack>
        <selfIsUpdatingTokenPool> SelfIsUpdating </selfIsUpdatingTokenPool>
        <myamount> #Mutez(Amount) </myamount>
        <tokenAddress> TokenAddress </tokenAddress>
        <xtzPool> #Mutez(XtzPool) </xtzPool>
        <tokenPool> TokenPool </tokenPool>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <senderaddr> Sender </senderaddr>
        <myaddr> SelfAddress </myaddr>
        <paramtype> #Type(#DexterVersionSpecificParamType(IsFA2)) </paramtype>
        <nonce> #Nonce(N => ?_) </nonce>
        <tokenId> TokenID </tokenId>
        <knownaddrs> KnownAddresses </knownaddrs>
        <operations> _ </operations>
     requires IsFA2
      andBool notBool( notBool SelfIsUpdating 
               andBool CurrentTime <Int Deadline
               andBool Amount ==Int 0
               andBool #XtzBought(XtzPool, TokenPool, TokensSold) <=Int XtzPool
               andBool (TokenPool >Int 0 orBool TokensSold >Int 0) 
               andBool #IsLegalMutezValue(#XtzBought(XtzPool, TokenPool, TokensSold))
                     )
      andBool #EntrypointExists(KnownAddresses, TokenAddress,         %transfer,   #TokenTransferType(IsFA2))
      andBool #EntrypointExists(KnownAddresses, OutputDexterContract, %xtzToToken, pair (address %to) (pair (nat %minTokensBought) (timestamp %deadline)))
```

```k
endmodule
```
