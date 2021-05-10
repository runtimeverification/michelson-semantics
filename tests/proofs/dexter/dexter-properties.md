# Properties

Note that the real arithmetic is used throughout the document.

## Storage Invariant

### Internal vs External
- `xtzPool <= Tezos.balance`
- `tokenPool <= tokenAddress.balanceOf(Tezos.self_address)`
- `lqtTotal == lqtAddress.totalSupply()`

NOTE: `<` is possible when someone "donates" XTZ or tokens (i.e., sending funds without going through `add_liquidity()`).

### Reserve vs Liquidity

```
(new(xtzPool) * new(tokenPool)) / (old(xtzPool) * old(tokenPool)) >= (new(lqtTotal) / old(lqtTotal))^2
```

NOTE: `>` is due to the service fees, the rounding errors, and/or potential “donated” funds.

## (Full) Functional Correctness

Note that the "Reserve vs Liquidity" property can be driven from the below.

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

