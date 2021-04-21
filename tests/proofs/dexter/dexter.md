# Dexter Verification

## Prologue

Our verification subject is the Michelson code corresponding to the LIGO [Dexter 2 contract](https://gitlab.com/dexter2tz/dexter2tz) at [commit id 8a5792a5](https://gitlab.com/dexter2tz/dexter2tz/-/tree/8a5792a56e0143042926c3ca8bff7d7068a541c3).

The goal of this project is to produce:

-   a series of proofs which specify that the intended behavior of each individual LIGO function is correct (which implies that the LIGO-to-Michelson compilation process is also correct in this case)
-   a series of proofs which demonstate high-level invariants over sequences of contract calls hold (e.g. it is not possible to produce a profit by exploiting rounding errors)

In this project, we will model the entrypoints in Dexter contract code and extract their high-level properties.
Note that we will base our high-level descriptions of each function of the LIGO contract code, while our verification will be performed at the the level of the compiled Michelson versions.

We begin start our verification project by opening a new module context in which our verification will be performed.

```k
requires "../lemmas.md"
module DEXTER-VERIFICATION-SYNTAX
  imports UNIT-TEST-SYNTAX
endmodule
```

```k
module DEXTER-VERIFICATION
  imports DEXTER-VERIFICATION-SYNTAX
  imports LEMMAS
```

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

In our proofs, we will use the following abstract representation of the Dexter contract storage state.
For simplicity, we assume the that the `tokenId` field is always present, though the verification of the FA12 version of the contract will not touch this field.

```k
configuration <dexterTop>
                <michelsonTop/>
                <storage>
                  <tokenPool>               0                                                </tokenPool>
                  <xtzPool>                 #Mutez(0)                                        </xtzPool>
                  <selfIsUpdatingTokenPool> false                                            </selfIsUpdatingTokenPool>
                  <freezeBaker>             false                                            </freezeBaker>
                  <manager>                 #Address("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU") </manager>
                  <lqtTotal>                0                                                </lqtTotal>
                  <tokenAddress>            #Address("")                                     </tokenAddress>
                  <lqtAddress>              #Address("")                                     </lqtAddress>
                  <tokenId>                 0                                                </tokenId>
                </storage>
              </dexterTop>
```

## Entrypoint Summaries

1.  [dexter.mligo.tz](https://gitlab.com/dexter2tz/dexter2tz/-/blob/8a5792a56e0143042926c3ca8bff7d7068a541c3/dexter.mligo.tz)

    1.  `add_liquidity`: Allows callers to "mint" liquidity in excahnge for tez.
        Caller gains liquidity equal to `tez * storage.lqtTotal / storage.xtzPool`.

        -   Input:

            ```
            type add_liquidity =
            { owner : address ;
              minLqtMinted : nat ;
              maxTokensDeposited : nat ;
              deadline : timestamp ;
            }
            ```

        -   Storage updates:

            ```
            Storage( lqtTotal:  LqtTotal  => LqtTotal  + lqt_minted ;
                     tokenPool: TokenPool => TokenPool + tokens_deposited ;
                     xtzPool:   XtzPool   => XtzPool   + Tezos.amount
                   )
            ```

        -   Operations:

            1. self call to `transfer` entrypoint: Send tokens from sender to self.
            2. self call to `mintOrBurn` entrypoint: Adds liquidity for the sender.

        -   Preconditions

            1.  the token pool _is_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
            2.  the deadline has not passed (i.e. the `Tezos.now >= input.deadline`)
            3.  the tez transferred is less than `input.maxTokensDeposited`
            4.  the liquidity minted is more than `input.minLqtMinted`

    2.  `remove_liquidity`

    3.  `set_baker`

        -   Input:

            ```
            type set_baker =
              { baker : key_hash option ;
                freezeBaker : bool ;
              }
            ```

        -   Output:

            ```
            ( [ set_delegate(baker) ], { storage with freezeBaker = freezeBaker } )
            ```

        -   Summary: The contract sets its delegate to the value of `baker` (and optionally freezes the baker to that particular value) if the following conditions are satisfied:

            1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
            2.  exactly 0 tez was transferred to this contract when it was invoked
            3.  the txn sender is the `storage.manager`
            4.  the baker is _not_ already frozen

    4.  `set_manager`

        -   Input:

            ```
            type set_manager = address // named new_manager
            ```

        -   Output:

            ```
            ( [], { storage with manager = new_manager } )
            ```

        -   Summary: The contract sets its manager to the provided manager address if the following conditions are satisfied:

            1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
            2.  exactly 0 tez was transferred to this contract when it was invoked
            3.  the txn sender is the `storage.manager`

    5.  `set_lqt_address`

        -   Input:

            ```
            type set_lqt_address = address // named lqtAddress
            ```

        -   Output:

            ```
            ( [], { storage with lqtAddress = lqtAddress } )
            ```

        -   Summary: The contract sets its liquidity pool adddress to the provided address if the following conditions are satisifed:

            1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
            2.  exactly 0 tez was transferred to this contract when it was invoked
            3.  the txn sender is the `storage.manager`
            4.  the liquidity pool address has already been set (i.e. `storage.lqtAddress1 != tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU`)

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

## Epilogue

We close out our module context now, which contains all of the information necessary to complete our proof.

```k
endmodule
```

```k
module DEXTER-SPEC
  imports DEXTER-VERIFICATION

  claim <k> now 0 => . ... </k>
        <mynow> #Timestamp(0) </mynow>

endmodule
```