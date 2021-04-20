# Dexter Verification Premable

This file contains K definitions which represent Dexter contract state.
We will use these K definitions to create a high-level specification
of the Dexter contract.

```k
requires "../lemmas.md"

module DEXTER-COMMON
  imports LEMMAS
```

## Storage Type

We define the contract's storage type using the following constructor:

```k
  syntax AbstractStorage ::= Storage(tokenPool               : Int,
                                     xtzPool                 : Mutez,
                                     lqtTotal                : Int,
                                     selfIsUpdatingTokenPool : Bool,
                                     freezeBaker             : Bool,
                                     manager                 : Address,
                                     tokenAddress            : Address,
                                     lqtAddress              : Address,
                                     tokenId                 : Nat)
```

```k
endmodule
```
