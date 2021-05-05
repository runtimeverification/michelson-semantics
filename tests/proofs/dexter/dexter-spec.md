```k
module DEXTER-SPEC
  imports DEXTER-VERIFICATION
```

```k
  /*
  claim <k> now 0 => . ... </k>
        <mynow> #Timestamp(0) </mynow>

  claim <k> (now 0 => .) ~> #dexterCode(true) ... </k>
        <mynow> #Timestamp(0) </mynow>

  claim <k> (now 0 => .) ~> #dexterCode(false) ... </k>
        <mynow> #Timestamp(0) </mynow>

  claim <k> (now 0 => .) ~> #dexterCode(_) ... </k>
        <mynow> #Timestamp(0) </mynow>
```

## Set Manager

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

## Set Baker

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

## Set LQT Address

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

## Default

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
  */
  claim <k> #runProof(true, RemoveLiquidity(To, LQTBurned, #Mutez(MinXtzWithdrawn), MinTokensWithdrawn, #Timestamp(Deadline))) => . </k>
        <stack> .Stack </stack>
        <selfIsUpdatingTokenPool> false </selfIsUpdatingTokenPool>
        <lqtAddress> LQTAddress </lqtAddress>
        <tokenAddress> TokenAddress </tokenAddress>
        <knownaddrs> LQTAddress   . %mintOrBurn |-> (pair int address):TypeName
                     TokenAddress . %transfer   |-> (pair address pair address nat):TypeName
        </knownaddrs>
        <mynow> #Timestamp(CurrentTime) </mynow>
        <myamount> #Mutez(Amount) </myamount>
        <lqtTotal> LQTTotal </lqtTotal>
    requires CurrentTime <Int Deadline
     andBool Amount ==Int 0
     andBool LQTTotal >Int 0
```

```k
endmodule
```
