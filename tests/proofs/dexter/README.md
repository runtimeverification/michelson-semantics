# Dexter Verification

Our verification subject is the Michelson code corresponding to the LIGO [Dexter 2 contract](https://gitlab.com/dexter2tz/dexter2tz) at [commit id 8a5792a5](https://gitlab.com/dexter2tz/dexter2tz/-/tree/8a5792a56e0143042926c3ca8bff7d7068a541c3).

The goal of this project is to produce:

-   a series of proofs which specify that the intended behavior of each individual LIGO function is correct (which implies that the LIGO-to-Michelson compilation process is also correct in this case)
-   a series of proofs which demonstate high-level invariants over sequences of contract calls hold (e.g. it is not possible to produce a profit by exploiting rounding errors)

In this project, we will model the entrypoints in Dexter contract code and extract their high-level properties.
Note that we will base our high-level descriptions of each function of the LIGO contract code, while our verification will be performed at the the level of the compiled Michelson versions.

## Terminology Prerequisites

### Entrypoints

Note that we are slightly abusing language in the preceding sentence because Michelson has no native notion of separate contract function entrypoints.
Instead, different entrypoints are modelled by making the global contract input parameter a disjoint union of the parameter types for each possible entrypoint, and then dispatching to different code blocks based on which entry of the disjoint union was selected.
Due to Michelon's design, this means that each entrypoint can be understood to have the following typing:

```
entrypoint_input_type * storage_type -> (operation list) * storage_type
```

where:

1.  each `entrypoint_input_type` is a member of the disjoint union which is the global contract parameter type;
2.  each `operation` is a callback to be placed in a FIFO queue of callbacks that are executed once the current contract execution terminates.

By abuse of notation, we identify an entrypoint's typing rule with its corresponding `entrypoint_input_type`, since the other parts of the typing rule are constant.
We may abbreviate `entrypoint_input_type` to just `input` when it is clear from context.

### Storage Type

To simplify the pending discussion, we observe that the storage type of the Dexter contract is as follows (we may simplify any code examples for readability):

```
type storage =
  { tokenPool : nat ;
    xtzPool : tez ;
    lqtTotal : nat ;
    selfIsUpdatingTokenPool : bool ;
    freezeBaker : bool ;
    manager : address ;
    tokenAddress : address ;
#if FA2
    tokenId : nat ;
#endif
    lqtAddress : address ;
  }
```

The initial storage value is as follows:

```
{ tokenPool = 0n ;
    xtzPool = 0tz ;
    lqtTotal = $lqtTotal ;
    selfIsUpdatingTokenPool = false ;
    freezeBaker = false ;
    manager = $manager ;
    tokenAddress = $tokenAddress ;
    lqtAddress = ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address) ;
  }
```

where `$lqtTotal`, `$manager`, and `$tokenAddress` are all variables which must be instantiated with correctly typed values.

## Entrypoint Summaries

1.  [dexter.mligo.tz](https://gitlab.com/dexter2tz/dexter2tz/-/blob/8a5792a56e0143042926c3ca8bff7d7068a541c3/dexter.mligo.tz)
    1.  `add_liquidity`
    2.  `remove_liquidity`
    3.  `set_baker`
    4.  `set_manager`
    5.  `set_lqt_address`
    6.  `default_`
    7.  `update_token_pool`
    8.  `xtz_to_token`
    9.  `token_to_xtz`
    10. `token_to_token`

2.  [lqt_fa12.mligo.tz](https://gitlab.com/dexter2tz/dexter2tz/-/blob/8a5792a56e0143042926c3ca8bff7d7068a541c3/lqt_fa12.mligo.tz)

    Note that, in this case, we do not need to verify this implementation in particular; it is sufficient that we can model the behavior of an arbitrary contract which conforms to the FA1.2 standard.
    Such a contract will have the following entry points:

    1.  `transfer`
    2.  `approve`
    3.  `mintOrBurn`
    4.  `getAllowance`
    5.  `getBalance`
    6.  `getTotalSupply`

As reference materials for understanding the contract intent, we will consult:

1.  The [Dexter 2 origination script](https://gitlab.com/dexter2tz/dexter2tz/-/blob/master/origination.sh)
2.  The [LIGO documentation](https://ligolang.org/docs/intro/introduction)
3.  The [Michelson documentation](http://tezos.gitlab.io/008/michelson.html)
4.  The [FA 1.2 standard](https://gitlab.com/tzip/tzip/blob/master/proposals/tzip-7/tzip-7.md)
5.  The [FA 2 standard](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-12/tzip-12.md)
