# Properties

Note that the real arithmetic is used throughout the document.

## Storage Invariant

### Internal vs External
- `xtzPool == Tezos.balance`
- `tokenPool <= tokenAddress.balanceOf(Tezos.self_address)`
- `lqtTotal == lqtAddress.totalSupply()`

NOTE: `<` is possible when someone "donates" tokens (i.e., directly sending tokens to dexter without going through any dexter entrypoints).

### Reserve vs Liquidity

```
(new(xtzPool) * new(tokenPool)) / (old(xtzPool) * old(tokenPool)) >= (new(lqtTotal) / old(lqtTotal))^2
```

NOTE: `>` is due to the service fees, the rounding errors, and/or potential “donated” funds.

## (Full) Functional Correctness

These properties describe the end-to-end behavior of each entry point transaction.
Note that the "Reserve vs Liquidity" property can be proven from these.

### add_liquidity()

- `new(xtzPool) == old(xtzPool) * (1 + alpha)`
- `new(tokenPool) == ceil(old(tokenPool) * (1 + alpha))`
- `new(lqtTotal) == floor(old(lqtTotal) * (1 + alpha))`

where:
```
alpha := Tezos.amount / old(xtzPool)
```

### remove_liquidity()

- `new(xtzPool) == ceil(old(xtzPool) * (1 - beta))`
- `new(tokenPool) == ceil(old(tokenPool) * (1 - beta))`
- `new(lqtTotal) == old(lqtTotal) * (1 - beta)`

where:
```
beta := lqtBurned / old(lqtTotal)
```

### xtz_to_token()

- `new(xtzPool) == old(xtzPool) + Tezos.amount`
- `new(tokenPool) == old(tokenPool) - floor(tokenOut(0.997 * Tezos.amount))`
- `new(lqtTotal) == old(lqtTotal)`

where:
```
tokenOut(xtzIn) := old(tokenPool) - old(xtzPool) * old(tokenPool) / (old(xtzPool) + xtzIn)
```

### token_to_xtz()

- `new(xtzPool) == old(xtzPool) - floor(xtzOut(0.997 * tokensSold))`
- `new(tokenPool) == old(tokenPool) + tokensSold`
- `new(lqtTotal) == old(lqtTotal)`

where:
```
xtzOut(tokenIn) := old(xtzPool) - old(xtzPool) * old(tokenPool) / (old(tokenPool) + tokenIn)
```

## Operation-level Properties

### Terminology

Internal:
- X : storage.xtzPool
- T : storage.tokenPool
- L : storage.lqtTotal

External:
- B : Tezos.balance
- D : the token balance of dexter
- S : the total supply of liquidity tokens

### Invariant

Given the operation list, Ops,
- 0 < X == B - toSend(Ops)
- 0 < T <= D + toTransfer(Ops)
- 0 < L == S + toMintBurn(Ops)

toSend(Ops): 
```
rule toSend(Tezos.transaction _ x _ ~> Ops) => toSend(Ops) + x
rule toSend(_ ~> Ops) => toSend(Ops) [owise]
```

toTransfer(Ops):
```
rule toTransfer(token_transfer _ from to t ~> Ops) => toTransfer(Ops) + t when from =/= dexter /\ to == dexter
rule toTransfer(token_transfer _ from to t ~> Ops) => toTransfer(Ops) - t when from == dexter
rule toTransfer(_ ~> Ops) => toTransfer(Ops) [owise]
```

toMintBurn(Ops):
```
rule toMintBurn(mint_or_burn _ _ l ~> Ops) => toMintBurn(Ops) + l
rule toMintBurn(_ ~> Ops) => toMintBurn(Ops) [owise]
```

### Opearation-level Specs

#### Common:
- caller cannot be dexter.
- `to` may be dexter.

#### AddLiquidity(x, to)
- B' = B + x
- X' = X + x
- T' = T + t
- L' = L + l
- Ops' = token_transfer(caller, dexter, t) ~> mint(to, l) ~> Ops

where:
- t = x * T up/Int X
- l = x * L   /Int X

#### RemoveLiquidity(l, to)
- B' = B
- X' = X - x
- T' = T - t
- L' = L - l
- Ops' = burn(caller, l) ~> token_transfer(dexter, to, t) ~> xtz_transfer(to, x) ~> Ops

where:
- x = l * X /Int L
- t = l * T /Int L

NOTE:
- It needs to check `l < L`, otherwise the contract becomes nonfunctional.
- Given `l < L`, we have `x < X` and `t < T`, thus `X' > 0` and `T' > 0`.

#### XtzToToken(x, to)
- B' = B + x
- X' = X + x
- T' = T - t
- L' = L
- Ops' = token_transfer(dexter, to, t) ~> Ops

where:
- t = 997 * x * T /Int (1000 * X + 997 * x)

NOTE:
- We have `t < T`, thus `T' > 0`.

#### TokenToXtz(t, to)
- B' = B
- X' = X - x
- T' = T + t
- L' = L
- Ops' = token_transfer(caller, dexter, t) ~> xtz_transfer(to, x) ~> Ops

where:
- x = 997 * t * X /Int (1000 * T + 997 * t)

NOTE:
- We have `x < X`, thus `X' > 0`.

#### TokenToToken(t, to, dexter2)
- B' = B
- X' = X - x
- T' = T + t
- L' = L
- Ops' = token_transfer(caller, dexter, t) ~> dexter2.xtz_to_token(x, to) ~> Ops

where:
- x is defined the same with TokenToXtz

NOTE:
- The following two should be equivalent:
  - dexter1.token_to_token(t, to, dexter2)
  - dexter1.token_to_xtz(t, caller) ~> dexter2.xtz_to_token(x, to)

