# Liquidity Baking Contract Verification

The liquidity baking (LB) smart contract is a Uniswap-style constant product market maker (CPMM) for two assets: Tez and wrapped Bitcoin (tzBTC).
In this report, we verify two important safety properties of the LB smart contract which we first state informally below:

1.  *Safety for liquidity providers (LPs)* - LP shares never decrease in redemption value
2.  *Safety for traders* - trades on the CPMM have a bounded exchange rate

Before we formally define these properties, we describe how CPMMs work.

## Constant Product Market Makers in Theory

**Introduction:**
A constant product market maker (CPMM) is an exchange for two assets *A* and *B* that holds a varying amount of both assets, as long as both amounts have a constant product.
Let the ordered 2-tuple *(X,Y)* denote a CPMM where naturals *X* and *Y* represent the exchange-held reserves of assets *A* and *B* respectively.
We also have the exchange function *E* which is defined as follows:

```
            V * w
E(w,U,V) =  -----
            U + w
```

where *U* and *V* are the amounts of assets *A* or *B*.

Network participants interact with the CPMM in a two-step, asynchronous, and unordered manner.
For example, suppose both Alice and Bob wish to perform trades on the exchange.
They will both need to *submit* their trade operations to the exchange operation pool.
Some time later, the exchange will *apply* their operations.
In this example, suppose Alice submits operation *o₁* first and then Bob submits operation *o₂*.
The exchange may apply *o₁* first; on the other hand, it is just as likely that it will apply *o₂* first.
It all depends on network conditions.
For this reason, we use the notation *{o₁, o₂, ..., oₖ}* to represent the pending set of *submitted* operations.

**Model:**
Now we can describe CPMMs as a state machine starting from state _init_ with the following rules:

1.  The `create(X,Y)` rule has the form:

    `init => (X, Y){ }`

    with `X > 0` and `Y > 0`.
    This rule describes how a liquidity provider (LP) can use their assets to create a CPMM with an initially empty operation set.

2.  The `submit(o)` rule has the form:

    `(X, Y){ ... } => (X, Y){ ..., o }`

    with `o` a valid operation (i.e., either a `sell-A(x)`, `sell-B(y)`, or `redeem`) and `...` representing the rest of the (possibly empty) operation set.
    The rule describes how a network participant may *submit* an operation to the CPMM.

3.  The `sell-A(x)` rule has the form:

    `(X, Y){ ..., sell-A(x), ... } => (X + x, Y - E(x,X,Y)){ ..., ... }`

    with `x <= X`.
    This rule describes how the exchange applies a trade operation selling asset *A* to obtain *B*.

4.  The `sell-B(y)` rule has the form:

    `(X, Y){ ..., sell-B(y), ... } => (X - E(y,Y,X), Y + y){ ..., ... }`

    with `y <= Y`.
    This rule describes, symmetrically, how the exchange applies a trade operation selling asset *B* to obtain *A*.

5.  The `redeem` rule has the form:

    `(X, Y){ ..., redeem, ... } => (0, 0){ ..., ... }`

    This rules describes how the exchange applies an LP redemption operation.
    This represents the LP redeeming their stored assets and shutting down the CPMM exchange.
    At this point, all applicable trades are zero-valued and thus useless.

Note that the exchange function is designed so that trades preserve the constant product, i.e., for the `sell-A(x)` rule we have:

```
X * Y = [X + x] * [Y - E(x,X,Y)]
      = [X + x] * [ Y - {Y * x / (X + x)}]
      = {[X + x] * Y} - Y * x
      = X * Y + (Y * x) - (Y * x)
      = X * Y
```

The case for the `sell-B(y)` follows by a symmetric calculation.

**Conclusions:**
So, the above model, while theoretically convenient, has some important problems:

1.  (viability) there is no immediate incentive to provide assets (i.e. liquidity) to the exchange, limiting CPMM creation and operation
2.  (scalability) the entire amount of liquidity must be provided by one party, limiting the growth of the exchange reserves

Note also that the theoretical model above does not contain any authorization logic that would be needed for a real implementation.

## Constant Product Market Makers in Practice

**Introduction:**
To counteract the viability and scalability issues, pratical CPMMs make two slight adjustments to the theoretical model:

1.  fees are assessed on every transaction (i.e. a percentage of the amount sold in terms of either asset *A* or *B*) and added to the exchange reserves
2.  a system of shares is created that allows multiple parties to:
    -   provide both assets to the exchange reserves in proportion and obtain a corresponding amount of shares
    -   destory shares and redeem a corresponding amount of exchange-held reserves

Let us reformulate our CPMM model in light of the above considerations.
A CPMM is now an ordered 4-tuple *(L,P,X,Y)* where:

-   natural *L* is total number of _liquidity_ shares
-   *P* is in the range *[0,1]* is the trade scaling factor and represents the percentage of trades paid into the exchange reserves as a _fee_, e.g., if there is a 25% fee on trades, then *P=.75*, and if there are no fees, then *P=1*
-   naturals *X* and *Y* represent the exchange-held _reserves_ of assets *A* and *B* respectively.

To account for fees, we develop a new exchange function:

```
              V * w * P
E(w,P,U,V) =  ---------
              U + w * P
```

Now we refine our previous CPMMs as a state machine model:

1.  The `create(l,p,x,y)` rule has the form:

    `init => (l, p, x, y){ }`

    with `l > 0` and `p ∈ [0,1]` and `x > 0` and `y > 0`.
    This rule describes how an initial liquidity provider (LP) can use their assets to create a CPMM.

2.  The `submit(o)` rule has the form:

    `(L, P, X, Y){ ... } => (L, P, X, Y){ ..., o }`

    with `o` a valid operation (i.e., either a `sell-A(x)`, `sell-B(y)`, `redeem(n)`, or `add(n)`) and `...` representing the rest of the (possibly empty) operation set.
    The rule describes how a network participant may *submit* an operation to the CPMM.

3.  The `sell-A(x)` rule has the form:

    `(L, P, X, Y){ ..., sell-A(x), ... } => (L, P, X + x, Y - E(x,X,Y)){ ..., ... }`

    with `x <= X`.
    This rule describes how the exchange applies a trade operation selling asset *A* to obtain *B*.

4.  The `sell-B(y)` rule has the form:

    `(L, P, X, Y){ ..., sell-B(y), ... } => (L, P, X - E(y,Y,X), Y + y){ ..., ... }`

    with `y <= Y`.
    This rule describes, symmetrically, how the exchange applies a trade operation selling asset *B* to obtain *A*.

4.  The `redeem(n)` rule has the form:

    `(L, P, X, Y){ ..., redeem(n), ... } => (L - L*n, P, X - X*n, Y - Y*n){ ..., ... }`

    with `0 < n <= 1`.
    This rules describes how the exchange applies an LP liquidity share redemption operation, where liquidity shares are _redeemed_ for stored assets.
    When `n = 1`, this is equivalent to shutting down the CPMM exchange, i.e., the last LP removed their remaining liquidity.

5.  The `add(n)` rule has the form:

    `(L, P, X, Y){ ..., add(n), ... } => (L + L*n, P, X + X*n, Y + Y*n){ ..., ... }`

    with `0 < n`.
    This rules describes how the exchange applies an LP liquidity share minting operation, where liquidity shares are _minted_ by storing assets.
    This rules describes how a LP can _mint_ liquidity shares by storing assets.

The above model is equivalent to our original model under the following conditions:

1.  The `create(l,p,x,y)` rule is only applied when *P = 1*;
2.  The `redeem(n)` rule is only applied when *n = 1*;
3.  The `add(n)` rule is *never* applied.

### CPMM Invariant Analysis

Recall that our theoretical CPMM model without fees satisfied the _constant product_ invariant after trades.
In case of CPMMs with fees, is the the product of the pre-trade assets always equal to the product of the post-trade assets?
Suppose we started off in a state `(L, P, X, Y)` with product _k = X * Y_.
Then suppose the `sell-B(b)` rule was applied to obtain the resulting state:

`(L, P, X - E(b,P,Y,X), Y + b)`

In resulting state, we have the product:

```
                                 ┌  X     X * b * P ┐   ┌       ┐
[ X - E(b,P,Y,X) ] * [ Y + b ] = │  -  -  --------- │ * │ Y + b │
                                 └  1     Y + b * P ┘   └       ┘

                                 ┌ X * [ Y + b * P ] - [ X * b * P ] ┐   ┌       ┐
                               = │ ----------------------------------│ * │ Y + b │
                                 └             Y + b * P             ┘   └       ┘

                                 ┌ X * Y + X * b * P - X * b * P ┐   ┌       ┐
                               = │ ------------------------------│ * │ Y + b │
                                 └            Y + b * P          ┘   └       ┘

                                 ┌   X * Y   ┐   ┌ Y + b ┐
                               = │ --------- │ * │ ----- │
                                 └ Y + b * P ┘   └   1   ┘

                                 ┌       ┐   ┌   Y + b   ┐
                               = │ X * Y │ * │ --------- │
                                 └       ┘   └ Y + b * P ┘

                                     ┌   Y + b   ┐
                               = k * │ --------- │
                                     └ Y + b * P ┘

                               >= k
```

Thus, we see that the answer to our question is:

-   if *P = 1*, then the product is constant in both states.
-   if *P < 1*, then the product _increases_, since `b * P < b`.

A symmetric calculations shows that the same rule applies in the case of rule `sell-A`.
In either case, this derivation shows us by applying trades, the redemeption value of liquidity shares _never decreases_.

## Safety Property Formalization

We now come back to our two safety properties that we state informally in the beginning of our document:

1.  *Safety for liquidity providers (LPs)* - LP shares never decrease in redemption value
2.  *Safety for traders* - trades on the CPMM have a bounded exchange rate

We can now formalize these properties using our model.
Consider an arbitrary CPMM in the form `(L, P, X, Y)`.

1.  *Safety for liquidity providers (LPs)*

    If `(L, P, X, Y) =>* (L', P, X', Y')` and *L' > 0*, then:

    _X' * Y' / X * Y >= L'^2 / L^2_

2.  *Safety for traders*

    If a trader selects an exchange rate `e`, then a state transition `(L, P, X, Y) => (L', P, X', Y')` via `sell-A(x)` or `sell-B(y)` will only apply if:

    -    _x * e >= E(x,P,X,Y)_ when applying `sell-A(x)`
    -    _y * e >= E(y,P,Y,X)_ when applying `sell-B(y)`

Note that property (1) follows directly from our simplified model, but property (2) is not provable (and indeed) does not make sense in our simplified model.
The reason is that property (2) is needed because, in real implementations, there is a difference between when:

-   the time _t₀_ when a trader decides he wants to make a trade; and
-   the time _t₁_ when the trade is actually performed on the CPMM exchange.

The problem arises in that the exchange rate that the CPMM provides at time t₁ may be _different_ from the rate available at time t₀.
The bounded exchange rate property ensures that a trader will only perform trades that match his desired level of risk/reward.
