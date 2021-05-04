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
requires "dexter-compiled.md"
module DEXTER-VERIFICATION-SYNTAX
  imports MICHELSON-INTERNAL-SYNTAX
```

```k
endmodule
```

```k
module DEXTER-VERIFICATION
  imports DEXTER-COMPILED
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

Calling functions produces not only storage changes but also a list of callbacks.
We serialize these to the `<operations>` cell for ease of writing specifications.

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
                <operations> .InternalList </operations>
              </dexterTop>
```

## Entrypoint Summaries

We define our list of entrypoints below.
Each entrypoint is given a unique abstract parameter type that we use to simplify our proof structure.

```k
  syntax EntryPointParams
  syntax Bool ::= wellTypedParams(Bool, EntryPointParams) [function, functional]
  // ---------------------------------------------------------------------------
  rule wellTypedParams(_, _) => false [owise]
```

1.  [dexter.mligo.tz](https://gitlab.com/dexter2tz/dexter2tz/-/blob/8a5792a56e0143042926c3ca8bff7d7068a541c3/dexter.mligo.tz)

    1.  `add_liquidity`: Allows callers to "mint" liquidity in excahnge for tez.
        Caller gains liquidity equal to `tez * storage.lqtTotal / storage.xtzPool`.

        -   Input:

            ```k
            syntax EntryPointParams   ::= AddLiquidityParams
            syntax AddLiquidityParams ::= AddLiquidity(owner              : Address,
                                                       minLqtMinted       : Int,
                                                       maxTokensDeposited : Int,
                                                       deadline           : Timestamp)
            rule wellTypedParams(_IsFA2, AddLiquidity(_Owner,
                                                       MinLqtMinted,
                                                       MaxTokensDeposited,
                                                       Deadline))
                  => true
            requires MinLqtMinted       >=Int 0
             andBool MaxTokensDeposited >=Int 0
             andBool #IsLegalTimestamp(Deadline)
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

            1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
            2.  the deadline has not passed (i.e. the `Tezos.now >= input.deadline`)
            3.  the tez transferred is less than `input.maxTokensDeposited`
            4.  the liquidity minted is more than `input.minLqtMinted`

    2.  `remove_liquidity`

        -   Input:

            ```k
            syntax EntryPointParams      ::= RemoveLiquidityParams
            syntax RemoveLiquidityParams ::= RemoveLiquidity(to                 : Address,
                                                             lqtBurned          : Int,
                                                             minXtzWithdrawn    : Mutez,
                                                             minTokensWithdrawn : Int,
                                                             deadline           : Timestamp)
            rule wellTypedParams(_IsFA2, RemoveLiquidity(_To,
                                                          LqtBurned,
                                                          MinXtzWithdrawn,
                                                          MinTokensWithdrawn,
                                                          Deadline))
                  => true
            requires LqtBurned >=Int 0
             andBool #IsLegalMutezValue(MinXtzWithdrawn)
             andBool MinTokensWithdrawn >=Int 0
             andBool #IsLegalTimestamp(Deadline)
            ```

        -   Output:

            ```
            ( [ Transfer_tokens ( lqtBurned, txn.sender )                0mutez         storage.lqtAddress   %mintOrBurn
                Transfer_tokens ( self.address, to_, $tokens_withdrawn ) 0mutez         storage.tokenAddress %transfer
                Transfer_tokens ()                                       $xtz_withdrawn _to
              ], { storage with tokenPool -= $tokens_withdrawn,
                                xtzPool   -= $xtz_withdrawn,
                                lqtTotal  -= lqtBurned} )
            ```

            where `$xtz_withdrawn    = storage.xtzPool *    (lqtBurned / storage.lqtTotal)`
              and `$tokens_withdrawn = storage.tokenPool *  (lqtBurned / storage.lqtTotal)`

        -   Summary: The sender can burn liquidity tokens in exchange for tez and tokens sent to some address if the following conditions are satisfied:

            1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
            2.  exactly 0 tez was transferred to this contract when it was invoked
            3.  the current block time must be less than the deadline
            4.  the amount of liquidity to be redeemed, when converted to xtz, is greater than `minXtzWithdrawn` and less than the amount of tez owned by the Dexter contract
            5.  the amount of liquidity to be redeemed, when converted to tokens, is greater than `minTokensWithdrawn` and less than the amount of tokens owned by the Dexter contract
            6.  the amount of liquidity to be redeemed is less than the total amount of liquidity and less than the amount of liquidity tokens owned by the sender
            7.  the contract at address `storage.lqtAddress` has a well-formed `mintOrBurn` entrypoint
            8.  the contract at address `storage.tokenAddress` has a well-formed `transfer` entrypoint

    3.  `set_baker`

        -   Input:

            ```k
            syntax EntryPointParams ::= SetBakerParams
            syntax SetBakerParams   ::= SetBaker(baker       : OptionData,
                                                 freezeBaker : Bool)
            rule wellTypedParams(_IsFA2, SetBaker(None,        _)) => true
            rule wellTypedParams(_IsFA2, SetBaker(Some D:Data, _)) => true requires isKeyHash(D)
            ```

        -   Output:

            ```
            ( [ set_delegate(baker) ], { storage with freezeBaker = freezeBaker } )
            ```

    4.  `set_manager`

        -   Input:

            ```k
            syntax EntryPointParams ::= SetManagerParams
            syntax SetManagerParams ::= SetManager(newManager : Address)
            rule wellTypedParams(_IsFA2, _:SetManagerParams) => true
            ```

        -   Output:

            ```
            ( [], { storage with manager = new_manager } )
            ```

    5.  `set_lqt_address`

        -   Input:

            ```k
            syntax EntryPointParams    ::= SetLQTAddressParams
            syntax SetLQTAddressParams ::= SetLQTAddress(lqtAddress : Address)
            rule wellTypedParams(_IsFA2, _:SetLQTAddressParams) => true
            ```

        -   Output:

            ```
            ( [], { storage with lqtAddress = lqtAddress } )
            ```

    6.  `default_`

        -   Input:

            ```k
            syntax EntryPointParams ::= DefaultParams
            syntax DefaultParams    ::= "Default"
            rule wellTypedParams(_IsFA2, Default) => true
            ```

        -   Output:

            ```
            ( [], { storage with xtzPool += txn.amount } )
            ```

        -   Summary: Adds more money to the xtz reserves if the following conditions are satisifed:

            1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)

    7.  `update_token_pool`

        -   Input:

            ```k
            syntax EntryPointParams      ::= UpdateTokenPoolParams
            syntax UpdateTokenPoolParams ::= "UpdateTokenPool"
            rule wellTypedParams(_IsFA2, UpdateTokenPool) => true
            ```

        -   Output:

            ```
            ( [ Transfer_tokens Params 0xtz storage.tokenAddress ], { storage with selfIsUpdatingTokenPool = true } )
            ```

            where, in version FA2, `Params = Pair (self.address, storage.tokenId)` and in version FA12, `Params = self.address`

    8.  `update_token_pool_internal`

        -   Input:

            ```k
            syntax EntryPointParams                  ::= UpdateTokenPoolInternalParams
            syntax UpdateTokenPoolInternalParams     ::= UpdateTokenPoolInternalFA12Params
                                                       | UpdateTokenPoolInternalFA2Params
            syntax UpdateTokenPoolInternalFA12Params ::= UpdateTokenPoolInternalFA12(balance         : Int)
            syntax UpdateTokenPoolInternalFA2Params  ::= UpdateTokenPoolInternalFA2 (balanceOfResult : InternalList)

            rule wellTypedParams(IsFA2, UpdateTokenPoolInternalFA12(Balance))         => true requires (notBool IsFA2) andBool Balance >=Int 0
            rule wellTypedParams(IsFA2, UpdateTokenPoolInternalFA2 (BalanceOfResult)) => true requires          IsFA2  andBool validBalanceOfParams(BalanceOfResult)

            syntax Bool ::= validBalanceOfParams(InternalList) [function, functional]
                          | validBalanceOfEntry(Data)          [function, functional]
            // ----------------------------------------------------------
            rule validBalanceOfParams(.InternalList) => true
            rule validBalanceOfParams([ D:Data ] ;; IL:InternalList)
              => validBalanceOfEntry(D) andBool validBalanceOfParams(IL)

            rule validBalanceOfEntry(Pair (Pair _:Address N1:Int) N2:Int)
              => N1 >=Int 0 andBool N2 >=Int 0
            rule validBalanceOfEntry(_:Data) => false [owise]
            ```

        -   Output:

            ```
            ( [], { storage with tokenPool = $tokenPool selfIsUpdatingTokenPool = false } )
            ```

            where, in version FA2, `$tokenPool` is the second projection of the tuple at the head of the input list;
            in version FA12, `$tokenPool` is equal to the input.

        -   Summary: The underlying token contract updates the Dexter contract's view of its own token balance if the following conditions are satisifed:

            1.  the token pool _is_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = true`)
            2.  exactly 0 tez was transferred to this contract when it was invoked
            3.  if using version FA2, the input parameter list is non-empty.

    9.  `xtz_to_token`

        -   Input:

        ```k
        syntax EntryPointParams ::= XtzToTokenParams
        syntax XtzToTokenParams ::= XtzToToken(to              : Address,
                                               minTokensBought : Int,
                                               deadline        : Timestamp)
        rule wellTypedParams(_IsFA2, XtzToToken(_To,
                                                 MinTokensBought,
                                                 Deadline))
              => true
        requires MinTokensBought >=Int 0
         andBool #IsLegalTimestamp(Deadline)
        ```

        -   Output:

            ```
            ( [ Transfer_tokens ( self.address, to_, $bought ) 0xtz storage.tokenAddress %transfer ],
              { storage with xtzPool += txn.amount ; tokenPool -= $bought } )
            ```

            where `$bought` is the current total of tokens exchanged from `txn.amount` by the formula:

            `(txn.amount * 997n * storage.tokenPool) / (xtzPool * 1000n + (txn.amount * 997n))`

        -   Summary: A buyer sends xtz to the Dexter contract and receives a corresponding amount of tokens, if the following conditions are satisfied:

            1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
            2.  the current block time must be less than the deadline
            3.  when the `txn.amount` (in mutez) is converted into tokens using the current exchange rate, the purchased amount is greater than `minTokensBought`
            4.  when the `txn.amount` (in mutez) is converted into tokens using the current exchange rate, it is less than or equal to the tokens owned by the Dexter contract

    10. `token_to_xtz`

        -   Input:

        ```k
        syntax EntryPointParams ::= TokenToXtzParams
        syntax TokenToXtzParams ::= TokenToXtz(to           : Address,
                                               tokensSold   : Int,
                                               minXtzBought : Mutez,
                                               deadline     : Timestamp)
        rule wellTypedParams(_IsFA2, TokenToXtz(_To,
                                                 TokensSold,
                                                 MinXtzBought,
                                                 Deadline))
             => true
        requires TokensSold >=Int 0
         andBool #IsLegalMutezValue(MinXtzBought)
         andBool #IsLegalTimestamp(Deadline)
        ```

        -   Output:

            ```
            ( [ Transfer_tokens ( txn.sender, self.address, tokensSold ) 0xtz self.tokenAddress %transfer ]
              [ Transfer_tokens () $bought _to ],
              { storage with xtzPool -= $bought ; tokenPool += tokensSold } )
            ```

            where `$bought` is the current total of xtz exchanged from `tokensSold` by the formula:

            `(tokensSold * 997n * storage.xtzPool) / (storage.tokenPool * 1000n + (tokensSold * 997n))`

        -   Summary: A buyer sends tokens to the Dexter contract and receives a corresponding amount of xtz, if the following conditions are satisfied:

            1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
            2.  the current block time must be less than the deadline
            3.  exactly 0 tez was transferred to this contract when it was invoked
            4.  the amount of tokens sold, when converted into xtz using the current exchange rate, is greater than `minXtzBought`
            5.  the amount of tokens sold, when converted into xtz using the current exchange rate, it is less than or equal to the xtz owned by the Dexter contract

    11. `token_to_token`

        -   Input:

        ```k
        syntax EntryPointParams   ::= TokenToTokenParams
        syntax TokenToTokenParams ::= TokenToToken(outputDexterContract : Address,
                                                   minTokensBought      : Int,
                                                   to                   : Address,
                                                   tokensSold           : Int,
                                                   deadline             : Timestamp)
        rule wellTypedParams(_IsFA2, TokenToToken(_OutputDexterContract,
                                                   MinTokensBought,
                                                  _To,
                                                   TokensSold,
                                                   Deadline))
             => true
        requires MinTokensBought >=Int 0
         andBool TokensSold >=Int 0
         andBool #IsLegalTimestamp(Deadline)
        ```

        -   Output:

            ```
            ( [ Transfer_tokens ( txn.sender, self.address, tokensSold ) 0xtz self.tokenAddress %transfer     ]
              [ Transfer_tokens { to_, minTokensBought, deadline } $bought outputDexterContract %xtz_to_token ],
              { storage with xtzPool -= $bought ; tokenPool += tokensSold } )
            ```

            where `$bought` is the current total of xtz exchanged from `tokensSold` by the formula:

            `(tokensSold * 997n * storage.xtzPool) / (storage.tokenPool * 1000n + (tokensSold * 997n))`

        -   Summary: A buyer sends tokens to the Dexter contract, converts its to xtz, and then immediately purchases a corresponding amount of tokens from a different Dexter contract (such that all transactions succeed or fail atomically), if the following conditions are satisfied:

            1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
            2.  the current block time must be less than the deadline
            3.  exactly 0 tez was transferred to this contract when it was invoked
            4.  the contract at address `outputDexterContract` has a well-formed `xtz_to_token` entrypoint
            5.  the amount of tokens sold, when converted into xtz using the current exchange rate, it is less than or equal to the xtz owned by the current Dexter contract
            6.  the contract at address `storage.tokenAddress` must have a well-formed `transfer` entry point

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

## State Abstraction

We use a few helper routines to convert between our abstract and concrete proof state.
We first define functions which build our parameter and our storage types.

```k
  syntax TypeName ::= #DexterParamType(Bool)                [function, functional]
                    | #DexterVersionSpecificParamType(Bool) [function, functional]
  // -----------------------------------------------------------------------------
  rule #DexterParamType(IsFA2)
    => (or
          (or
             (or
                (or (pair address                        // addLiquidity
                       pair nat
                         pair nat timestamp)
                    unit)                                // default
                (or (pair address                        // removeLiquidity
                       pair nat
                         pair mutez
                           pair nat timestamp)
                    (pair option key_hash bool)))        // setBaker
             (or
                (or address                              // setLqtAddress
                    address)                             // setManager
                (or (pair address                        // tokenToToken
                       pair nat
                         pair address
                           pair nat timestamp)
                    (pair address                        // tokenToXtz
                       pair nat
                         pair mutez timestamp))))
          (or
             (or unit                                    // updateTokenPool
                 #DexterVersionSpecificParamType(IsFA2)) // updateTokenPoolInternal
             (pair address                               // xtzToToken
                pair nat timestamp)))

  rule #DexterVersionSpecificParamType(true)  => list pair pair address nat nat
  rule #DexterVersionSpecificParamType(false) => nat

  syntax TypeName ::= #DexterStorageType(Bool)                [function, functional]
                    | #DexterVersionSpecificStorageType(Bool) [function, functional]
  // -------------------------------------------------------------------------------
  rule #DexterStorageType(IsFA2)
    => pair nat
         pair mutez
           pair nat
             pair bool
               pair bool
                 pair address
                   pair address
                     #DexterVersionSpecificStorageType(IsFA2)

  rule #DexterVersionSpecificStorageType(true)  => pair nat address
  rule #DexterVersionSpecificStorageType(false) => address
```

We also define a functions that serialize and deserialize our abstract parameters and state.

```k
  syntax Data ::= #LoadDexterParams(Bool, EntryPointParams) [function, functional]
  // -----------------------------------------------------------------------------
  rule #LoadDexterParams(_IsFA2, AddLiquidity(Owner, MinLqtMinted, MaxTokensDeposited, Deadline))
    => Left Left Left Left Pair Owner Pair MinLqtMinted Pair MaxTokensDeposited Deadline

  rule #LoadDexterParams(_IsFA2, Default)
    => Left Left Left Right Unit

  rule #LoadDexterParams(_IsFA2, RemoveLiquidity(To, LqtBurned, MinXtzWithdrawn, MinTokensWithdrawn, Deadline))
    => Left Left Right Left Pair To Pair LqtBurned Pair MinXtzWithdrawn Pair MinTokensWithdrawn Deadline

  rule #LoadDexterParams(_IsFA2, SetBaker(Baker, FreezeBaker))
    => Left Left Right Right Pair Baker FreezeBaker

  rule #LoadDexterParams(_IsFA2, SetLQTAddress(LqtAddress))
    => Left Right Left Left LqtAddress

  rule #LoadDexterParams(_IsFA2, SetManager(NewManager))
    => Left Right Left Right NewManager

  rule #LoadDexterParams(_IsFA2, TokenToToken(OutputDexterContract, MinTokensBought, To, TokensSold, Deadline))
    => Left Right Right Left Pair OutputDexterContract Pair MinTokensBought Pair To Pair TokensSold Deadline

  rule #LoadDexterParams(_IsFA2, TokenToXtz(To, TokensSold, MinXtzBought, Deadline))
    => Left Right Right Right Pair To Pair TokensSold Pair MinXtzBought Deadline

  rule #LoadDexterParams(_IsFA2, UpdateTokenPool)
    => Right Left Left Unit

  rule #LoadDexterParams(false,  UpdateTokenPoolInternalFA12(Balance))
    => Right Left Right Balance

  rule #LoadDexterParams(true,   UpdateTokenPoolInternalFA2(BalanceOfResult))
    => Right Left Right BalanceOfResult

  rule #LoadDexterParams(_IsFA2, XtzToToken(To, MinTokensBought, Deadline))
    => Right Right Pair To Pair MinTokensBought Deadline

  syntax KItem ::= #loadDexterState(Bool, EntryPointParams)
  // ------------------------------------------------------
  rule <k> #loadDexterState(IsFA2, Params) => . ... </k>
       <stack> .Stack
            => [ pair #DexterParamType(IsFA2) #DexterStorageType(IsFA2)
                 Pair #LoadDexterParams(IsFA2, Params)
                   Pair TokenPool
                     Pair XTZPool
                       Pair LQTTotal
                         Pair IsUpdatingTokenPool
                           Pair IsBakerFrozen
                             Pair Manager
                               Pair TokenAddress
                                 #if IsFA2
                                   #then Pair TokenId LQTAddress
                                   #else LQTAddress
                                 #fi ]
       </stack>
       <tokenPool>               TokenPool           </tokenPool>
       <xtzPool>                 XTZPool             </xtzPool>
       <lqtTotal>                LQTTotal            </lqtTotal>
       <selfIsUpdatingTokenPool> IsUpdatingTokenPool </selfIsUpdatingTokenPool>
       <freezeBaker>             IsBakerFrozen       </freezeBaker>
       <manager>                 Manager             </manager>
       <tokenAddress>            TokenAddress        </tokenAddress>
       <tokenId>                 TokenId             </tokenId>
       <lqtAddress>              LQTAddress          </lqtAddress>

  syntax KItem ::= #storeDexterState(Bool)
                 | #storeDexterState(Bool, Data)
  // -------------------------------------------
  rule <k> #storeDexterState(IsFA2) => #storeDexterState(IsFA2, VersionSpecificData) ... </k>
       <stack> [ pair list operation StorageType:TypeName
                 Pair OpList
                   Pair TokenPool
                     Pair XTZPool
                       Pair LQTTotal
                         Pair IsUpdatingTokenPool
                           Pair IsBakerFrozen
                             Pair Manager
                               Pair TokenContract
                                 VersionSpecificData ]
            => .Stack
       </stack>
       <tokenPool>               _ => TokenPool           </tokenPool>
       <xtzPool>                 _ => XTZPool             </xtzPool>
       <lqtTotal>                _ => LQTTotal            </lqtTotal>
       <selfIsUpdatingTokenPool> _ => IsUpdatingTokenPool </selfIsUpdatingTokenPool>
       <freezeBaker>             _ => IsBakerFrozen       </freezeBaker>
       <manager>                 _ => Manager             </manager>
       <tokenAddress>            _ => TokenContract       </tokenAddress>
       <operations>              _ => OpList              </operations>
    requires StorageType ==K #DexterStorageType(IsFA2)

  rule <k> #storeDexterState(IsFA2, Pair TokenId LQTContract) => .K ... </k>
       <tokenId>    _ => TokenId     </tokenId>
       <lqtAddress> _ => LQTContract </lqtAddress>
    requires IsFA2

  rule <k> #storeDexterState(IsFA2, LQTContract) => .K ... </k>
       <lqtAddress> _ => LQTContract </lqtAddress>
    requires notBool IsFA2
```

If the contract execution fails, storage is not updated.

```k
  rule <k> Aborted(_, _, _, _) ~> (#storeDexterState(_) => .) ... </k>
```

## Resulting Operations Abstractions

```k
  syntax Data ::= #UpdateTokenPoolTransferFrom(Bool, Address, Int) [function, functional]
 // -------------------------------------------------------------------------------------
  rule #UpdateTokenPoolTransferFrom(IsFA2, SelfAddress, _TokenId) =>        SelfAddress                            requires notBool IsFA2
  rule #UpdateTokenPoolTransferFrom(IsFA2, SelfAddress,  TokenId) => [ Pair SelfAddress TokenId ] ;; .InternalList requires         IsFA2

  syntax Type ::= #TokenContractType(Bool) [function, functional]
 // -------------------------------------------------------------
  rule #TokenContractType(false) => #Type(pair address                   (contract #DexterVersionSpecificParamType(false)))
  rule #TokenContractType(true)  => #Type(pair (list (pair address nat)) (contract #DexterVersionSpecificParamType(true)) )
```

## Putting It All Together

All contract call specifications have common steps:

1. Load parameters and storage onto the stack.
2. Execute the Dexter contract code.
3. Save the resulting storage.

If all steps are completed, only the Dexter-specific storage is updated.

```k
  syntax KItem ::= #runProof(Bool, EntryPointParams)
 // ------------------------------------------------
  rule <k> #runProof(IsFA2, Params)
        => #loadDexterState(IsFA2, Params)
        ~> #dexterCode(IsFA2)
        ~> #storeDexterState(IsFA2)
        ...
       </k>
       <operations> OpList </operations>
    ensures wellTypedParams(IsFA2, Params)
    andBool OpList ==K .InternalList
```

## Epilogue

We close out our module context now, which contains all of the information necessary to complete our proof.

```k
endmodule
```
