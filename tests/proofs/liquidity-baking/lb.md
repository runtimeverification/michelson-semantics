# Liquidity Baking Verification

## Prologue

Our verification subject is the Michelson code corresponding to the LIGO [Liquidity Baking contract](https://gitlab.com/dexter2tz/dexter2tz/-/tree/liquidity_baking) at [commit id d9864388](https://gitlab.com/dexter2tz/dexter2tz/-/tree/d98643881fe14996803997f1283e84ebd2067e35).

The goal of this project is to produce:

-   a series of proofs which specify that the intended behavior of each individual LIGO function is correct (which implies that the LIGO-to-Michelson compilation process is also correct in this case)
-   a series of proofs which demonstate high-level invariants over sequences of contract calls hold (e.g. it is not possible to produce a profit by exploiting rounding errors)

In this project, we will model the entrypoints in the Liquidity Baking contract code and extract their high-level properties.
Note that we will base our high-level descriptions of each function of the LIGO contract code, while our verification will be performed at the the level of the compiled Michelson versions.

We begin start our verification project by opening a new module context in which our verification will be performed.

```k
requires "../lemmas.md"
requires "lb-compiled.md"
module LIQUIDITY-BAKING-VERIFICATION-SYNTAX
  imports MICHELSON-INTERNAL-SYNTAX
```

```k
endmodule
```

## Liqudity Baking Lemmas

```k
module LIQUIDITY-BAKING-LEMMAS
  imports MICHELSON
```

```k
  rule X /Int 1 => X [simplification]
  rule X *Int 1 => X [simplification]
```

```k
endmodule
```

```k
module LIQUIDITY-BAKING-VERIFICATION
  imports LIQUIDITY-BAKING-COMPILED
  imports LIQUIDITY-BAKING-VERIFICATION-SYNTAX
  imports LIQUIDITY-BAKING-LEMMAS
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

In our proofs, we will use the following abstract representation of the Liquidity Baking contract storage state.

Calling functions produces not only storage changes but also a list of callbacks.
We serialize these to the `<operations>` cell for ease of writing specifications.

```k
configuration <dexterTop>
                <michelsonTop/>
                <storage>
                  <tokenPool>    0            </tokenPool>
                  <xtzPool>      #Mutez(0)    </xtzPool>
                  <lqtTotal>     0            </lqtTotal>
                  <tokenAddress> #Address("") </tokenAddress>
                  <lqtAddress>   #Address("") </lqtAddress>
                </storage>
                <operations> .InternalList </operations>
              </dexterTop>
```

## Entrypoint Summaries

We define our list of entrypoints below.
Each entrypoint is given a unique abstract parameter type that we use to simplify our proof structure.

```k
  syntax EntryPointParams
  syntax Bool ::= wellTypedParams(EntryPointParams) [function, functional]
  // ---------------------------------------------------------------------
```

1.  [dexter.liquidity_baking.mligo.tz](https://gitlab.com/dexter2tz/dexter2tz/-/blob/d98643881fe14996803997f1283e84ebd2067e35/dexter.liquidity_baking.mligo.tz)

    1.  `add_liquidity`: Allows callers to "mint" liquidity in excahnge for tez.
        Caller gains liquidity equal to `tez * storage.lqtTotal / storage.xtzPool`.

        -   Input:

            ```k
            syntax EntryPointParams   ::= AddLiquidityParams
            syntax AddLiquidityParams ::= AddLiquidity(owner              : Address,
                                                       minLqtMinted       : Int,
                                                       maxTokensDeposited : Int,
                                                       deadline           : Timestamp)
            rule wellTypedParams(AddLiquidity(_Owner,
                                               MinLqtMinted,
                                               MaxTokensDeposited,
                                               Deadline))
                 =>         MinLqtMinted       >=Int 0
                    andBool MaxTokensDeposited >=Int 0
                    andBool #IsLegalTimestamp(Deadline)
                 [simplification]
            ```

    2.  `remove_liquidity`

        -   Input:

            ```k
            syntax EntryPointParams      ::= RemoveLiquidityParams
            syntax RemoveLiquidityParams ::= RemoveLiquidity(to                 : Address,
                                                             lqtBurned          : Int,
                                                             minXtzWithdrawn    : Mutez,
                                                             minTokensWithdrawn : Int,
                                                             deadline           : Timestamp)
            rule wellTypedParams(RemoveLiquidity(_To,
                                                  LqtBurned,
                                                  MinXtzWithdrawn,
                                                  MinTokensWithdrawn,
                                                  Deadline))
                  =>         LqtBurned >=Int 0
                     andBool #IsLegalMutezValue(MinXtzWithdrawn)
                     andBool MinTokensWithdrawn >=Int 0
                     andBool #IsLegalTimestamp(Deadline)
                     [simplification]
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

    3.  `default_`

        -   Input:

            ```k
            syntax EntryPointParams ::= DefaultParams
            syntax DefaultParams    ::= "Default"
            rule wellTypedParams(Default) => true [simplification]
            ```

        -   Output:

            ```
            ( [], { storage with xtzPool += txn.amount } )
            ```

    4.  `xtz_to_token`

        -   Input:

        ```k
        syntax EntryPointParams ::= XtzToTokenParams
        syntax XtzToTokenParams ::= XtzToToken(to              : Address,
                                               minTokensBought : Int,
                                               deadline        : Timestamp)
        rule wellTypedParams(XtzToToken(_To, MinTokensBought, Deadline))
          => MinTokensBought >=Int 0 andBool #IsLegalTimestamp(Deadline)
             [simplification]
        ```

        -   Output:

            ```
            ( [ Transfer_tokens ( self.address, to_, #TokensBought ) 0xtz storage.tokenAddress %transfer ],
              { storage with xtzPool += txn.amount ; tokenPool -= #TokensBought } )
            ```

            where `#TokensBought` is the current total of tokens exchanged from `txn.amount` defined by
            the macro given below.

    5. `token_to_xtz`

        -   Input:

        ```k
        syntax EntryPointParams ::= TokenToXtzParams
        syntax TokenToXtzParams ::= TokenToXtz(to           : Address,
                                               tokensSold   : Int,
                                               minXtzBought : Mutez,
                                               deadline     : Timestamp)
        rule wellTypedParams(TokenToXtz(_To, TokensSold, MinXtzBought, Deadline))
          =>         TokensSold >=Int 0
             andBool #IsLegalMutezValue(MinXtzBought)
             andBool #IsLegalTimestamp(Deadline)
             [simplification]
        ```

        -   Output:

            ```
            ( [ Transfer_tokens ( txn.sender, self.address, tokensSold ) 0xtz self.tokenAddress %transfer ]
              [ Transfer_tokens () #XtzBought _to ],
              { storage with xtzPool -= #XtzBought ; tokenPool += tokensSold } )
            ```

            where `#XtzBought` is the current total of xtz exchanged from `tokensSold` by the formula
            defined by the macro below.

    6. `token_to_token`

        -   Input:

        ```k
        syntax EntryPointParams   ::= TokenToTokenParams
        syntax TokenToTokenParams ::= TokenToToken(outputDexterContract : Address,
                                                   minTokensBought      : Int,
                                                   to                   : Address,
                                                   tokensSold           : Int,
                                                   deadline             : Timestamp)
        rule wellTypedParams(TokenToToken(_OutputDexterContract,
                                                   MinTokensBought,
                                                  _To,
                                                   TokensSold,
                                                   Deadline))
          =>         MinTokensBought >=Int 0
             andBool TokensSold >=Int 0
             andBool #IsLegalTimestamp(Deadline)
             [simplification]
        ```

        -   Output:

            ```
            ( [ Transfer_tokens ( txn.sender, self.address, tokensSold ) 0xtz self.tokenAddress %transfer     ]
              [ Transfer_tokens { to_, minTokensBought, deadline } #XtzBought outputDexterContract %xtz_to_token ],
              { storage with xtzPool -= #XtzBought ; tokenPool += tokensSold } )
            ```

            where `#XtzBought` is the current total of xtz exchanged from `TokensSold` by the formula

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

1.  The [Liquidity Baking TZIP draft](https://gitlab.com/tzip/tzip/-/blob/master/drafts/current/draft-liquidity_baking.md)
2.  The [LIGO documentation](https://ligolang.org/docs/intro/introduction)
3.  The [Michelson documentation](http://tezos.gitlab.io/008/michelson.html)
4.  The [FA 1.2 standard](https://gitlab.com/tzip/tzip/blob/master/proposals/tzip-7/tzip-7.md)
5.  The [Tezos BTC (TZBTC) contract source code](https://github.com/tz-wrapped/tezos-btc/)

## State Abstraction

We use a few helper routines to convert between our abstract and concrete proof state.
We first define functions which build our parameter and our storage types.

```k
  syntax TypeName ::= #LiquidityBakingParamType()  [function, functional]
  // --------------------------------------------------------------------
  rule #LiquidityBakingParamType()
    => (or
          (or
             (or (pair address                      // addLiquidity
                    (pair nat
                       (pair nat timestamp)))
                  unit)                             // default
             (or (pair address                      // removeLiquidity
                    (pair nat
                       (pair mutez
                          (pair nat timestamp))))
                 (pair address                      // tokenToToken
                    (pair nat
                       (pair address
                          (pair nat timestamp))))))
          (or (pair address                         // tokenToXtz
                 (pair nat
                   (pair mutez timestamp)))
              (pair address                         // xtzToToken
                 (pair nat timestamp))))

  syntax TypeName ::= #LiquidityBakingStorageType() [function, functional]
  // ---------------------------------------------------------------------
  rule #LiquidityBakingStorageType()
    => (pair nat
          (pair mutez
             (pair nat
               (pair address
                 address))))
```

We also define a functions that serialize and deserialize our abstract parameters and state.

```k
  syntax Data ::= #LoadLiquidityBakingParams(EntryPointParams) [function, functional]
  // --------------------------------------------------------------------------------
  rule #LoadLiquidityBakingParams(AddLiquidity(Owner, MinLqtMinted, MaxTokensDeposited, Deadline))
    => Left Left Left Pair Owner Pair MinLqtMinted Pair MaxTokensDeposited Deadline

  rule #LoadLiquidityBakingParams(Default)
    => Left Left Right Unit

  rule #LoadLiquidityBakingParams(RemoveLiquidity(To, LqtBurned, MinXtzWithdrawn, MinTokensWithdrawn, Deadline))
    => Left Right Left Pair To Pair LqtBurned Pair MinXtzWithdrawn Pair MinTokensWithdrawn Deadline

  rule #LoadLiquidityBakingParams(TokenToToken(OutputDexterContract, MinTokensBought, To, TokensSold, Deadline))
    => Left Right Right Pair OutputDexterContract Pair MinTokensBought Pair To Pair TokensSold Deadline

  rule #LoadLiquidityBakingParams(TokenToXtz(To, TokensSold, MinXtzBought, Deadline))
    => Right Left Pair To Pair TokensSold Pair MinXtzBought Deadline

  rule #LoadLiquidityBakingParams(XtzToToken(To, MinTokensBought, Deadline))
    => Right Right Pair To Pair MinTokensBought Deadline

  syntax KItem ::= #loadLiquidityBakingState(EntryPointParams)
  // ---------------------------------------------------------
  rule <k> #loadLiquidityBakingState(Params) => . ... </k>
       <stack> .Stack
            => [ pair #LiquidityBakingParamType() #LiquidityBakingStorageType()
                 Pair #LoadLiquidityBakingParams(Params)
                   Pair TokenPool
                     Pair XTZPool
                       Pair LQTTotal
                         Pair TokenAddress
                           LQTAddress ]
       </stack>
       <tokenPool>    TokenPool    </tokenPool>
       <xtzPool>      XTZPool      </xtzPool>
       <lqtTotal>     LQTTotal     </lqtTotal>
       <tokenAddress> TokenAddress </tokenAddress>
       <lqtAddress>   LQTAddress   </lqtAddress>

  syntax KItem ::= #storeLiquidityBakingState()
  // ------------------------------------------
  rule <k> #storeLiquidityBakingState() => . ... </k>
       <stack> [ pair list operation StorageType:TypeName
                 Pair OpList
                   Pair TokenPool
                     Pair XTZPool
                       Pair LQTTotal
                         Pair TokenContract
                           LQTAddress ]
            => .Stack
       </stack>
       <tokenPool>     _ => TokenPool     </tokenPool>
       <xtzPool>       _ => XTZPool       </xtzPool>
       <lqtTotal>      _ => LQTTotal      </lqtTotal>
       <tokenAddress>  _ => TokenContract </tokenAddress>
       <operations>    _ => OpList        </operations>
       <lqtAddress>    _ => LQTAddress    </lqtAddress>

    requires StorageType ==K #LiquidityBakingStorageType()

```

If the contract execution fails, storage is not updated.

```k
  rule <k> Aborted(_, _, _, _) ~> (#storeLiquidityBakingState() => .) ... </k>
```

## Proof Helper Functions

```k
  syntax Type ::= #TokenContractType() [function, functional]
 // ---------------------------------------------------------
  rule #TokenContractType() => #Type(pair address (contract pair address pair address nat))  [simplification]

  syntax Type ::= #TokenTransferType() [function, functional]
 // ---------------------------------------------------------
  rule #TokenTransferType() => #Type(pair address pair address nat) [simplification]

  syntax Data ::= #TokenTransferData(Address, Address, Int) [function, functional]
 // ------------------------------------------------------------------------------
  rule #TokenTransferData(From, To, TokenAmt) =>  Pair From Pair To TokenAmt [simplification]

  syntax Int ::= #ceildiv   (Int, Int) [function]
               | #ceildivAux(Int, Int) [function, functional]
 // ---------------------------------------------------------
  rule #ceildiv   (X, Y) => #ceildivAux(X, Y) requires Y =/=Int 0
  rule #ceildivAux(_, Y) => 0                 requires Y  ==Int 0
  rule #ceildivAux(X, Y) => X /Int Y          requires Y  =/=Int 0 andBool         X %Int Y ==Int 0
  rule #ceildivAux(X, Y) => X /Int Y +Int 1   requires Y  =/=Int 0 andBool notBool X %Int Y ==Int 0

  syntax Int ::= #CurrencyBought(Int, Int, Int) [function, functional, smtlib(xtzbought), no-evaluators]
 // ----------------------------------------------------------------------------------------------------
  rule (ToSellAmt *Int 999 *Int ToBuyCurrencyTotal) /Int (ToSellCurrencyTotal *Int 1000 +Int (ToSellAmt *Int 999))
    => #CurrencyBought(ToBuyCurrencyTotal, ToSellCurrencyTotal, ToSellAmt)
    [simplification]

  syntax Int ::= #XtzNetBurn(Int) [macro]
 // -------------------------------------
  rule #XtzNetBurn(XtzAmount) => #mulDiv( XtzAmount , 999 , 1000 )

  syntax Int ::= #XtzBurnAmount(Int) [macro]
 // ----------------------------------------
  rule #XtzBurnAmount(XtzAmount) => XtzAmount -Int #XtzNetBurn ( XtzAmount )
```

```k
  syntax Bool ::= #EntrypointExists(Map, Address, FieldAnnotation, Type) [macro]
 // ----------------------------------------------------------------------------
  rule #EntrypointExists(KnownAddresses, Addr, FieldAnnot, EntrypointType)
    => Addr . FieldAnnot  in_keys(KnownAddresses) andBool
       KnownAddresses[Addr . FieldAnnot] ==K #Name(EntrypointType)

  syntax Bool ::= #LocalEntrypointExists(Map, FieldAnnotation, Type) [macro]
 // ------------------------------------------------------------------------
  rule #LocalEntrypointExists(LocalEntrypoints, FieldAnnot, EntrypointType)
    => FieldAnnot in_keys(LocalEntrypoints) andBool
       LocalEntrypoints[FieldAnnot] ==K #Name(EntrypointType)
```

### Avoiding Interpreting Functions

If a function value does not play well with the prover or SMT solver, it can be rewritten to `#uninterpreted`.
This function has no evaluation rules, so the prover can make no assumptions about it -- it will be assumed it can take on any value.

```k
  syntax Int ::= #mulMod(Int, Int, Int) [function, functional, smtlib(mulMod), no-evaluators]
               | #mulDiv(Int, Int, Int) [function, functional, smtlib(mulDiv), no-evaluators]
 // -----------------------------------------------------------------------------------------
  rule (X *Int Y) %Int Z => #mulMod(X, Y, Z) [simplification]
  rule (X *Int Y) /Int Z => #mulDiv(X, Y, Z) [simplification]
```

## Putting It All Together

All contract call specifications have common steps:

1. Load parameters and storage onto the stack.
2. Execute the Liquidity Baking contract code.
3. Save the resulting storage.

If all steps are completed, only the Liquidity Baking specific storage is updated.

```k
  syntax KItem ::= #runProof(EntryPointParams)
 // ------------------------------------------
  rule <k> #runProof(Params)
        => #loadLiquidityBakingState(Params)
        ~> #liquidityBakingCode
        ~> #storeLiquidityBakingState()
        ...
       </k>
       <myamount> #Mutez(Amount) </myamount>
       <xtzPool> #Mutez(XtzPool) </xtzPool>
       <tokenPool> TokenPool </tokenPool>
       <operations> OpList </operations>
       <returncode> ReturnCode </returncode>
    ensures wellTypedParams(Params)
    andBool #IsLegalMutezValue(Amount)
    andBool #IsLegalMutezValue(XtzPool)
    andBool TokenPool >=Int 0
    andBool OpList ==K .InternalList
    andBool ReturnCode ==Int 111
```

## Epilogue

We close out our module context now, which contains all of the information necessary to complete our proof.

```k
endmodule
```
