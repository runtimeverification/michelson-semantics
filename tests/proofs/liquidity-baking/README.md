# Liquidity Baking Contract Verification

The liquidity baking (LB) system is a Uniswap-style constant product market maker (CPMM) for two assets: Tez and wrapped Bitcoin (tzBTC) where arbitary users can decide to become a liquidity provider (LP) and obtain liquidity share tokens by providing reserves of each asset to the CPMM.
The system actually consists of three separate smart contracts which work together in concert:

```
      [Dexter]
         │
[LQT]────┴────[tzBTC]
```

-   the main contract *Dexter* is responsible for:
    -    all system entrypoints (i.e. users interacting with the system should *only* need to interact with this contract)
    -    the CPMM logic
    -    managing the Tez asset reserves
-   the *LQT* contract is a _FA1.2_ contract (with an additional `%mintOrBurn` entrypoint) responsible for managing the liquidity share tokens (as far as we can tell, *LQT* stands for liquidity token)
-   the *tzBTC* contract is a _FA1.2_ contract responsible for managing the tzBTC asset reserves

In this report, we use the K Framework and K Michelson semantics to verify two important safety properties of LB which we first state informally below:

1.  *Liquidity share value security* - LP shares never decrease in *redemption value* (note this is *different* from *monetary value*, see below)
2.  *Operation safety* - all trades and liquidity redemptions/deposits have a bounded exchange rate and time in which they are applicable.

Before we formally define these properties, we describe how CPMMs work using a simple state machine model.
This simple state machine model is _not_ equivalent to any smart contract.
Instead, it provides an intuition which ideally helps lay people and experts understand why the safety properties above are sufficient.
We conclude this section by discussing where to go from here.

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
For this reason, we use the notation *{o₁, o₂ ... oₖ}* to represent the pending set of *submitted* operations.
We let *...* represent a (possibly empty) subset of a set.
Thus, the notation *{o ...}* represents a set with an element *o* and does *not* mean that the first element in set is *o* (in any case, sets are unordered).
Let *valid(oₖ)* be a predicate that defines the set of valid operations.

Finally, to properly model asynchronous operations, our exchange is equipped with a clock *[T]* where *T* is a natural number which represents the current exchange time.

**Model:**
Now we can describe CPMMs as a state machine starting from state `init` with the following rules:

```
rule init => (X, Y)[0]{ } requires X > 0 ∧ Y > 0
rule (X, Y)[T]{ ... } => (X, Y)[T + 1]{ ... }
rule (X, Y)[T]{ ... } => (X, Y)[T]{ ... o } requires valid(o)
rule (X, Y)[T]{ sell-A(x) ... } => (X + x, Y - E(x,X,Y))[T]{ ... } requires x <= X
rule (X, Y)[T]{ sell-B(y) ... } => (X - E(y,Y,X), Y + y)[T]{ ... } requires y <= Y
rule (X, Y)[T]{ redeem    ... } => (0, 0)               [T]{ ... }
```

We give a brief description of each rule in order:

1.  A liquidity provider (LP) may use their assets to create a CPMM with an initially empty operation set.
2.  The system clock may tick forward.
3.  A network participant may *submit* a valid operation to the CPMM.
4.  The exchange may apply a trade operation selling asset *A* to obtain *B*.
5.  Symmetrically, the exchange may apply a trade operation selling asset *B* to obtain *A*.
6.  The exchange may apply an LP redemption operation, i.e., the LP redeems their stored assets and shuts down the CPMM exchange.
    At this point, all applicable trades are zero-valued and effectively no-ops.

**Exchange-rate Analysis:**
Any good exchange-rate function should be monotonic, i.e., we should get more units of money out if we put more units of moeny in.
Formally, we want to check that:

`w <= w' => E(w,U,V) <= E(w',U,V)`

The result follows by a chain of inequalities:

```
E(w,U,V) <= E(w',U,V) => (V * w) / (U + w) <= (V * w') / (U + w')
                      => w / (U + w)       <= w' / (U + w')
                      => w * (U + w')      <= w' * (U + w)
                      => w * U + w * w'    <= w' * U + w * w'
                      => w * U             <= w' * U
                      => w                 <= w'
```

assuming that _U > 0_ and _V > 0_ since otherwise the two division steps (i.e. dividing by _V_ and dividing _U_) become impossible.

**Invariant Analysis:**
Note that the exchange function is designed so that trades preserve the constant product, i.e., for the `sell-A(x)` operation we have:

```
X * Y = [X + x] * [Y - E(x,X,Y)]
      = [X + x] * [ Y - {Y * x / (X + x)}]
      = {[X + x] * Y} - Y * x
      = X * Y + (Y * x) - (Y * x)
      = X * Y
```

The case for the `sell-B(y)` follows by a symmetric calculation.

**Remarks:**
While our theoretical model is convenient and illustrates the sense in which CPMMs have a *constant* product, it has a few serious drawbacks:

1.  (incentive) there is no immediate incentive to provide assets (i.e. liquidity) to the exchange, limiting CPMM creation and operation
2.  (scalability) the entire amount of liquidity must be provided by one party, limiting the growth of the exchange reserves

## Constant Product Market Makers in Practice

**Introduction:**
To counteract the incentive and scalability issues, pratical CPMMs make two slight adjustments to the theoretical model:

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

`E(w,P,U,V) = E(w * P,U,V)`

**Model:**
Now we refine our previous CPMM state machine model:

```
rule init => (l, p, x, y)[0]{ } requires l > 0 ∧ p ∈ [0,1] ∧ x > 0 ∧ y > 0
rule (X, Y)[T]{ ... } => (X, Y)[T + 1]{ ... }
rule (L, P, X, Y)[T]{ ... } => (L, P, X, Y)[T]{ ... o } requires valid(o)
rule (L, P, X, Y)[T]{ sell-A(x) ... } => (L, P, X + x, Y - E(x,P,X,Y)) [T]{ ... } requires x <= X
rule (L, P, X, Y)[T]{ sell-B(y) ... } => (L, P, X - E(y,P,Y,X), Y + y) [T]{ ... } requires y <= Y
rule (L, P, X, Y)[T]{ redeem(n) ... } => (L - L*n, P, X - X*n, Y - Y*n)[T]{ ... } requires 0 <= n <= 1
rule (L, P, X, Y)[T]{ add(n)    ... } => (L + L*n, P, X + X*n, Y + Y*n)[T]{ ... } requires 0 <= n
```

These rules are very similar to our original rules, with a few distinctions:

-   The `init` rule now creates a 4-tuple CPMM.
-   The `sell-A(x)` and `sell-B(y)` operations now compute exchange rates using a fee.
-   The `redeem(n)` operation is parametric on the amount `n` of liquidity shares redeemed.
    When `n = 1`, this is equivalent to shutting down the exchange, i.e., the last LP removed their remaining liquidity; afterwards, all applicable trades and liquidity redemptions/additions are no-ops.
-   The new `add(n)` rule describes how an LP can mint new liquidity shares by storing assets in the reserves in exchange for new shares.

The above model is equivalent to our original model under the following conditions:

1.  When initializing a new exchange, the only allowed trade scaling factor is *P = 1*;
2.  The `redeem(n)` operation is only applied when *n = 1*;
3.  The `add(n)` operation is *never* applied.

**Exchange-rate with Fees Analysis**:
We want to understand how the exchange rate function _E()_ varies as the trade scaling factor _P_ grows or shrinks.
Using the equality _E(w,P,U,V) = E(w * P,U,V)_ and monotonicity, observe that _E(w,P,U,V)_ shrinks as _P_ shrinks.
Again, this result only holds when _U_ and _V_ are both greater than _0_.
Finally, note that _E(w,1,U,V) = E(w * 1,U,V) = E(w,U,V)_.

**Invariant Analysis:**
Recall that our theoretical CPMM model without fees satisfied the _constant product_ invariant after trades.
In case of CPMMs with fees, is the the product of the pre-trade assets always equal to the product of the post-trade assets?
Suppose we started off in a state `(L, P, X, Y)` with product _k = X * Y_.
Then suppose the `sell-B(b)` operation was applied to obtain the resulting state:

`(L, P, X - E(y,P,Y,X), Y + y)`

Thus, we have:

```
X * Y =  [ X - E(y,Y,X)     ] * [ Y + y ]
      <= [ X - E(y * P,Y,X) ] * [ Y + y ]
      =  [ X - E(y,P,Y,X)   ] * [ Y + y ]
```

Thus, we see that the answer to our question is:

-   if *P = 1*, then the product is constant in both states.
-   if *P < 1*, then the product _increases_ by monotonicity of the exchange-rate function.

A symmetric calculations shows that the same rule applies in the case of the `sell-A(a)` operation.
In either case, this derivation shows us by applying trades, the redemeption value of liquidity shares _never decreases_.

**Liquidity Shares vs. Exchange Reserves Analysis:**
If our CPMM state is `(L, P, X, Y)`, during `redeem` or `add` operations, how do liquidity shares *L* scale with respect to the product of our exchange reserves _X*Y_?
Let us consider the `redeem` and `add` operations in turn.

First suppose we apply a `redeem(n)` operation.
Then our updated state tuple is:

`(L - L*n, P, X - X*n, Y - Y*n) = (L * (1 - n), P, X * (1 - n), Y * (1 - n))`.

In other words, we update *L*, *X*, and *Y* by the constant factor *q = 1 - n* such that *0 <= n <= 1* which gives *0 <= q <= 1*.
Now suppose we apply an `add(n)` operation.
Then our updated state tuple is:

`(L + L*n, P, X + X*n, Y + Y*n) = (L * (1 + n), P, X * (1 + n), Y * (1 + n))`.

Symmetrically, this means we update *L*, *X*, and *Y* by the constant factor *q = 1 + n* such that *0 <= n* which gives *1 <= q*.
By this reasoning, we see that the disjunction of these two rules is equivalent to the rule below:

_(*)_ `rule (L, P, X, Y) => (L * q, P, X * q, Y * q) requires 0 <= q`.

Thus, as `L` scales to `L*q`, we see that the product of our exchange reserves `X*Y` scales to `(X*q)*(Y*q)` or `(X*Y)*q^2`.
Thus, `L` scales proportionally with `sqrt(X*Y)`, i.e., the *geoemtric mean of X and Y*.

If the reader is unconvinced, we can carry our a simple calculation to verify our intuition.
Let us assume `L * m = sqrt(X*Y)`.
Now, scale both sides as indicated by rule `(*)` above.
We want to show that `L*q * m = sqrt((X*q)*(Y*q))`.
By arithmetic, we have `L*q * m = sqrt((X*q)*(Y*q)) = sqrt(X*Y*q^2) = q*sqrt(X*Y)`.
Finally, we divide both sides by `q` to obtain the equality we already assumed: `L* m = sqrt(X*Y)`.

In words, our answer is *liquidity scales proportionally with the geometric mean of X and Y*.

**Remarks:**
The new model is clearly an improvement over the previous model because it addressed the incentive and scalability issues.
However, it still has non-trivial and non-obvious issues which relate to the asynchronous nature of operations:

1.  (slippage) in the time between trade/redeem/add operation submission and application, exchange rates may vary, making such operations incredibly risky
2.  (time-unboundedness) after an operation is submitted, it may be applied at *any* time in the future, even when that makes no sense for the submitter

To counteract this risk of price variance, practical implementations allow operations to inlcude both: exchange rates and time bounds.

**Revised Rules:**
We can formalize this notion by revising our trade and liquidity redemption/addition operations to include explicit bounds.
These four rules will then replace the last four rules of the former model.

```
rule (L, P, X, Y)[T]{ sell-A(d,e,x)   ... } => (L, P, X + x, Y - E(x,P,X,Y)) [T]{ ... } requires x <= X      ∧ T <= d ∧ E(x,P,X,Y) >= x*e
rule (L, P, X, Y)[T]{ sell-B(d,e,y)   ... } => (L, P, X - E(y,P,Y,X), Y + y) [T]{ ... } requires y <= Y      ∧ T <= d ∧ E(y,P,Y,X) >= y*e
rule (L, P, X, Y)[T]{ redeem(d,a,b,n) ... } => (L - L*n, P, X - X*n, Y - Y*n)[T]{ ... } requires 0 <= n <= 1 ∧ T <= d ∧ X*n >= a ∧ Y*n >= b
rule (L, P, X, Y)[T]{ add(d,a,b,n)    ... } => (L + L*n, P, X + X*n, Y + Y*n)[T]{ ... } requires 0 <= n      ∧ T <= d ∧ X*n <= a ∧ Y*n <= b
```

These revised operations are identical to their former counterparts except:

-   all operations now include an explicit deadline, denoted by variable `d`
-   the `sell-A(d,e,x)` and `sell-B(d,e,y)` operations now include a minimum exchange rate parameter `e` 
-   the `redeem(d,a,b,n)` operation now includes parameters `a` and `b`, the *minimum* redeemed units of asset *A* and *B* respectively
-   the `add(d,a,b,n)` operation now includes parameters `a` and `b`, the *maximum* stored units of asset *A* and *B* respectively

## Safety Property Formalization

We now come back to our two safety properties that we state informally in the beginning of our document:

1.  *Liquidity share value security* - LP shares never decrease in *redemption value*
2.  *Operation safety* - all trades and liquidity redemptions/deposits have a bounded exchange rate and time in which they are applicable.

We can now formalize these properties using our model.
Consider an arbitrary CPMM in the form `(L, P, X, Y)`.

1.  *Liquidity share value security*

    If `(L, P, X, Y)[T]{ ... } =>* (L', P, X', Y')[T']{ ... }` then:

    _X' * Y' / X * Y >= L'^2 / L^2_

2.  *Operation safety*

    All operations have bounded exchange rates and application deadlines.
    In other words:

    If `(L, P, X, Y)[T]{ o ... } => (L', P, X', Y')[T]{ ... }` then:

    _T <= deadline(o) ∧ bounded-exchange(P,X,Y,o)_

    where `deadline(o)` projects the deadline from an operation and `bounded-exchange(P,X,Y,o)` is defined as:

    ```
    bounded-exchange(P,X,Y,sell-A(d,e,x))   = E(x,P,X,Y) >= x*e
    bounded-exchange(P,X,Y,sell-B(d,e,y))   = E(y,P,Y,X) >= y*e
    bounded-exchange(P,X,Y,redeem(d,a,b,n)) = X*n >= a ∧ Y*n >= b
    bounded-exchange(P,X,Y,add(d,a,b,n))    = X*n <= a ∧ Y*n <= b
    ```

Both properties are satisfied by our simplified model.
Property (1) follows by an induction argument while property (2) follows by definition.

## Integer Arithmetic Considerations

In computer programs, we do not have access to infinite precision numbers (i.e. real numbers).
We typically either use fixed-width or arbitrary width integers (with rounding or truncation) or floating point numbers.
In this section, we discuss which of the properties that we proved above hold when we restrict ourselves to arbitrary width integer arithmetic with truncation.
Note that some of the expressions we used above do not work well (or at all) in the realm of integer arithmetic because they contain fractional values.
In such cases, we can use algebra to re-formulate such expressions to minimize division operations (and thus, minimize rounding error).
By doing so, we may also gain performance, since division is a fairly expensive operation.
For the purposes of this section, let

```
 a
---
 b
```

and:

`a / b`

denote fractions using _real_ division and let:

```
 a
---I
 b
```

and:

`a // b`

denote fractions using _integer_ division with truncation.

**Invariant Analysis without Fees:**:
Recall that our original exchange rate function `E(w,U,V)` was chosen so that:

`[ X - E(y,Y,X) ] * [ Y + y ] == X * Y`

However, in the world of integer arithmetic, the above equality no longer holds, becase integer division is not exact.
As a counter example, suppose our exchange has two assets with reserves of asset A and B set to _10_ and _10_ respectively.
Now suppose we wish to by as many units of asset A as possible for _5_ units of asset B.
When we plug these values into our formula, we obtain:

```
[ 10 - E(5,10,10) ] * [ 10 + 5 ] = [ 10 - ( 10 * 5 / 10 + 15 ) ] * 15
                                 = [ 10 - ( 50 / 15 ) ] * 15
                                 = [ 10 - ( 10 / 3 ) ] * 15
                                 = [ 10 - 3.333... ] * 15
                                 = [ 10 - 3 ] * 15
                                 = 7 * 15
                                 = 105
                                 > 100
                                 = 10 * 10
```

The crucial problem arises because we must truncate the division _10 / 3_, which gives us _3_ instead of the infinite decimal expansion _3.333..._
In fact, because we are truncating a number that is subtracted, the end result is that, in case of arbitrary-width integer arithmetic with truncation, even *without a fee*, the product of our asset reserves *never decreases, but may increase*.

**Exchange-rate with Fees Analysis:**:
Recall that our exchange rate function with fees was:

```
              V * w * P
E(w,P,U,V) =  ---------
              U + w * P
```

where *P* is a scaling factor in the range *[0,1]*.
To make this operation usable under integer arithmetic, we can rewrite the exchange function as follows:

```
                        V * w * Q
Ei(w,P = Q/R,U,V) =  ---------------I
                     (U * R) + w * Q
```

where the division operator `---I`, as mentioned above, is integer division with truncation.
By a straightforward calculation, we can show that the above is provably equal to our original function when using real division, i.e.,

```
   V * w * Q       (1 / R)    V * w * Q/R   V * w * P
--------------- * --------- = ----------- = --------- = E(w,P = Q/R,U,V)
(U * R) + w * Q    (1 / R)    U + w * Q/R   U + w * P
```

We also note that:

```
                       V * w * Q           V * w * Q
Ei(w,P = Q/R,U,V) = ---------------I <= --------------- = E(w,P = Q/R,U,V)
                    (U * R) + w * Q     (U * R) + w * Q
```

since integer division will truncate the fractional part, giving a smaller value.

**Exchange-rate with Compounded Fees Analysis:**
Suppose that, instead of our exchange function applying a fee which is a single scaling factor, it applies the same scaling factor twice in succession?
That is, suppose we have this exchange function with a compound fee:

```
                                           V * w * P^2
EC(w,P = Q/R,U,V) = E(w,P = Q^2/R^2,U,V) = ----------- = E((w * Q) / R,P = Q/R,U,V)
                                           U + w * P^2
```

Now suppose that we reformulate it in the world of integer arithmetic using the following formulation:

```
                        V * ((w * Q) // R) * Q
ECi(w,P = Q/R,U,V) = -----------------------------I = Ei((w * Q) // R,P = Q/R,U,V)
                     (U * R) + ((w * Q) // R)  * Q
```

Note that this formulation _does not_ minimize rounding error, but, as shown above, _does_ allow us to re-use our exchange function we defined earlier.
With our above knowledge, we can _almost_ prove the following:

```
ECi(w,P = Q/R,U,V) =  Ei((w * Q) // R,P = Q/R,U,V)
                   <= E ((w * Q) // R,P = Q/R,U,V)
                   <= E ((w * Q) /  R,P = Q/R,U,V)
                   =  EC(w,P = Q/R,U,V)
```

The first, second, and fourth steps follow all by either definition or by our analysis in the subsection above.
The second step follows because `(w * Q) // R <= (w * Q) / R` by integer division truncation and monotonicity.

**Invariant Analysis with Fees or Compounded Fees:**
Given our exchange-rate analysis in the preceeding subsections, we have:

```
X * Y =  [ X - E(y,Y,X)                     ] * [ Y + y ]
      <= [ X - E((y * Q) / R,Y,X)           ] * [ Y + y ]
      =  [ X - E(y,P = Q/R,Y,X)             ] * [ Y + y ]
      <= [ X - Ei(y,P = Q/R,Y,X)            ] * [ Y + y ]
      <= [ X - Ei((y * Q) // R,P = Q/R,Y,X) ] * [ Y + y ]
      =  [ X - ECi(y,P = Q/R,Y,X)           ] * [ Y + y ]
```

where:

1.  the equality steps follow by definition;
2.  the second and third inequality steps follow by monotonicity;
3.  the first inequality follows by our exchange rate with fees analysis.

**Exchange Reserves Non-negativity Analysis:**
After trades, are the exhange reserves always non-negative?
The answer to this is: yes.
It can be shown by examining the exchange function directly, but there is an easier proof.
We have already shown that product of our exchange reserves can _never decrease_.
That is, if our exchange currently has asset reserves _X_ and _Y_ and a series of trades is performed producing reserves _X'_ and _Y'_, we know that _X * Y <= X' * Y'_.
Since our exchange reserves are assumed to be non-negative integers, this means that we know:

`0 <= X * Y <= [ X - Ei(y,P,Y,X) ] * [ Y + y ]`.

By _Y_ and _y_ being non-negative, we also know:

`Y + y > 0`.

If `X - Ei(y,P,Y,X)` could be negative, then we would have the contradiction:

`0 <= X * Y <= [ X - Ei(y,P,Y,X) ] * [ Y + y ] < 0`.

Thus, our exchange function to convert asset _B_ into asset _A_ can never exceed the total units of asset _A_.

## Next Steps

The simplified models in this section ignore critical features needed in real implementations such as:

-   authorization logic for performing various actions
-   logic for maintaining an account's balance in assets *A* and *B*
-   means of integrating the CPMM with the underlying assets (which are either blockchain primitives or smart contracts)
-   logic to programmatically issue transactions to interact with other smart contracts (if necessary)
-   etc...

Our functional specification proofs in K and hand-proofs cover these additional details (and more) needed to provide a complete proof of our two safety properties list above.
