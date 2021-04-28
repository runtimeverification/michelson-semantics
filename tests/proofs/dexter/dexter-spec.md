```k
module DEXTER-SPEC
  imports DEXTER-VERIFICATION

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

```
  claim <k> #runProof(IsFA2, SetManagerParams(NewManager)) => . ... </k>
        <manager> Sender => NewManager </manager>
        <selfIsUpdatingTokenPool> true </selfIsUpdatingTokenPool>
        <myamount> Amount </myamount>
        <senderaddr> Sender </senderaddr>
    ensures Mutez2Int(Amount) ==Int 0
```

If any of the conditions are not satisfied, the call fails.

```
  claim <k> #runProof(IsFA2, SetManagerParams(NewManager)) => Aborted(_, _, _, _) ... </k>
        <manager> CurrentManager </manager>
        <selfIsUpdatingTokenPool> IsUpdating </selfIsUpdatingTokenPool>
        <myamount> Amount </myamount>
        <senderaddr> Sender </senderaddr>
    ensures Mutez2Int(Amount) =/=Int 0
     orBool Sender =/=K CurrentManager
     orBool IsUpdating)
```

```k
endmodule
```
