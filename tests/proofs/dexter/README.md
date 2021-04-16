# Dexter Verification

Our verification subject is the Michelson code corresponding to the LIGO
[Dexter 2 contract](https://gitlab.com/dexter2tz/dexter2tz).

The goal of this project is to produce:

-   a series of proofs which specify that the intended behavior of each
    individual LIGO function is correct (which implies that the
    LIGO-to-Michelson compilation process is also correct)
-   a series of proofs which demonstate high-level invariants over sequences
    of contract calls hold (e.g. it is not possible to produce a profit by
    exploiting rounding errors)

In this project, we will model the following functions in the following
files to extract their high-level properties:

1.  [dexter.mligo.tz](https://gitlab.com/dexter2tz/dexter2tz/-/blob/master/dexter.mligo.tz)
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
    11. `update_token_pool`

2.  [lqt_fa12.mligo.tz](https://gitlab.com/dexter2tz/dexter2tz/-/blob/master/dexter.fa2.mligo.tz)
    Note that, in this case, we do not need to verify this implementation in
    particular; it is sufficient that we can model the behavior of an arbitrary
    contract which conforms to the FA1.2 standard. Such a contract will have
    the following entry points:

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
