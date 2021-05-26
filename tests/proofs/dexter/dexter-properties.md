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

```
claim [inv]:
<operations> (Op => Ops') ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<mybalance>   #Mutez(B => B')   </mybalance>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Transactions(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Transactions(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

syntax Int ::= Transactions(OpList) [function]
rule Transactions([Transaction _ X _] ;; Ops) => Transactions(Ops) -Int X
rule Transactions(_ ;; Ops) => Transactions(Ops) [owise]
rule Transactions(.List) => 0

syntax Int ::= Transfers(OpList) [function]
rule Transfers([Transfer From To T] ;; Ops) => Transfers(Ops) +Int T when From =/=K DEXTER andBool To ==K DEXTER
rule Transfers([Transfer From To T] ;; Ops) => Transfers(Ops) -Int T when From  ==K DEXTER
rule Transfers(_ ;; Ops) => Transfers(Ops) [owise]
rule Transfers(.List) => 0

syntax Int ::= MintBurns(OpList) [function]
rule MintBurns([Mint _ L] ;; Ops) => MintBurns(Ops) +Int L
rule MintBurns([Burn _ L] ;; Ops) => MintBurns(Ops) -Int L
rule MintBurns(_ ;; Ops) => MintBurns(Ops) [owise]
rule MintBurns(.List) => 0
```

```
proof [inv]:
- split Op
  - case Op == AddLiquidity
    - apply [inv-add-liquidity]
  - case Op == RemoveLiquidity
    - apply [inv-remove-liquidity]
  - case Op == XtzToToken
    - apply [inv-xtz-to-token]
  - case Op == TokenToXtz
    - apply [inv-token-to-xtz]
  - case Op == TokenToToken
    - apply [inv-token-to-token]
  - case Op == UpdateTokenPool | UpdateTokenPoolInternal
    - apply [inv-update-token-pool]
  - case Op == SetBaker | SetManager | SetLqtAddress
    - apply [inv-setters]
  - case Op == Default
    - apply [inv-default]
  - case Op == _ // arbitrary external calls
    - apply [inv-external]
```

```
proposition [sender-is-not-dexter]:
[[ Sender =/=K DEXTER => true ]]
<operations>  Op ;; _   </operations>
<senderaddr>  Sender    </senderaddr>
requires Op ==K AddLiquidity
  orBool Op ==K RemoveLiquidity
  orBool Op ==K XtzToToken
  orBool Op ==K TokenToXtz
  orBool Op ==K TokenToToken
  orBool Op ==K UpdateTokenPool | UpdateTokenPoolInternal
  orBool Op ==K SetBaker | SetManager | SetLqtAddress
  orBool Op ==K Default

proof [sender-is-not-dexter]:
- DEXTER is a smart contract
- No entrypoint of DEXTER calls itself
```

```
claim [inv-add-liquidity]:
<operations>  ( [ AddLiquidity(Owner, _, _, Deadline) ] => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<mybalance>   #Mutez(B => B')   </mybalance>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
<senderaddr>  Sender            </senderaddr>
requires 0 <Int X  andBool X  ==Int B  +Int Transactions(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Transactions(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-add-liquidity]:
- assume IS_VALID(Deadline) // otherwise, it reverts, and we conclude
- Sender =/=K DEXTER by [sender-is-not-dexter]
- apply [add-liquidity]
- unify RHS
  - Ops' == [ Transfer Sender DEXTER TokensDeposited ] ;; [ Mint Owner LqtMinted ]
  - X' == X +Int XtzDeposited
  - T' == T +Int ( XtzDeposited *Int T up/Int X #as TokensDeposited )
  - L' == L +Int ( XtzDeposited *Int L   /Int X #as LqtMinted )
  - B' == B +Int XtzDeposited
  - D' == D
  - S' == S
- X' >Int 0 by X >Int 0 and XtzDeposited >=Int 0
- T' >Int 0 by T >Int 0 and TokensDeposited >=Int 0
- L' >Int 0 by L >Int 0 and LqtMinted >=Int 0
- X' ==Int X +Int XtzDeposited
     ==Int B +Int Transactions(AddLiquidity(...) ;; Ops) +Int XtzDeposited by premise
     ==Int B' +Int Transactions(AddLiquidity(...) ;; Ops) by B'
     ==Int B' +Int Transactions(Ops) by Transactions
     ==Int B' +Int Transactions([ Mint Owner LqtMinted ] ;; Ops) by Transactions
     ==Int B' +Int Transactions([ Transfer Sender DEXTER TokensDeposited ] ;; [ Mint Owner LqtMinted ] ;; Ops) by Transactions
     ==Int B' +Int Transactions(Ops' ;; Ops) by Ops'
- T' ==Int T +Int TokensDeposited
     <=Int D +Int Transfers(AddLiquidity(...) ;; Ops) +Int TokensDeposited by premise
     ==Int D' +Int Transfers(AddLiquidity(...) ;; Ops) +Int TokensDeposited by D'
     ==Int D' +Int Transfers(Ops) +Int TokensDeposited by Transactions
     ==Int D' +Int Transfers([ Mint Owner LqtMinted ] ;; Ops) +Int TokensDeposited by Transactions
     ==Int D' +Int Transfers([ Transfer Sender DEXTER TokensDeposited ] ;; [ Mint Owner LqtMinted ] ;; Ops) by Transactions and Sender =/=K DEXTER
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops'
- L' ==Int L +Int LqtMinted
     ==Int S +Int MintBurns(AddLiquidity(...) ;; Ops) +Int LqtMinted by premise
     ==Int S' +Int MintBurns(AddLiquidity(...) ;; Ops) +Int LqtBurned by S'
     ==Int S' +Int MintBurns(Ops) +Int LqtBurned by Transactions
     ==Int S' +Int MintBurns([ Mint Owner LqtMinted ] ;; Ops) by Transactions
     ==Int S' +Int MintBurns([ Transfer Sender DEXTER TokensDeposited ] ;; [ Mint Owner LqtMinted ] ;; Ops) by Transactions
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops'
```

```
claim [inv-remove-liquidity]:
<operations>  ( [ RemoveLiquidity(To, LqtBurned, _, _, Deadline) ] => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<mybalance>   #Mutez(B => B')   </mybalance>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
<senderaddr>  Sender            </senderaddr>
requires 0 <Int X  andBool X  ==Int B  +Int Transactions(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Transactions(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-remove-liquidity]:
- assume IS_VALID(Deadline) // otherwise, it reverts, and we conclude
- assume LqtBurned <Int L // TODO: it needs to revert otherwise
- apply [remove-liquidity]
- unify RHS
  - Ops' == [ Burn Sender LqtBurned ] ;; [ Transfer DEXTER To TokensWithdrawn ] ;; [ Transaction To XtzWithdrawn () ]
  - X' == X -Int ( LqtBurned *Int X /Int L #as XtzWithdrawn )
  - T' == T -Int ( LqtBurned *Int T /Int L #as TokensWithdrawn )
  - L' == L -Int LqtBurned
  - B' == B
  - D' == D
  - S' == S
- X' >Int 0 by X >Int 0 and LqtBurned <Int L
- T' >Int 0 by T >Int 0 and LqtBurned <Int L
- L' >Int 0 by LqtBurned <Int L
- X' ==Int X -Int XtzWithdrawn
     ==Int B +Int Transactions(RemoveLiquidity(...) ;; Ops) -Int XtzWithdrawn by premise
     ==Int B' +Int Transactions(RemoveLiquidity(...) ;; Ops) -Int XtzWithdrawn by B'
     ==Int B' +Int Transactions(Ops) -Int XtzWithdrawn by Transactions
     ==Int B' +Int Transactions([ Transaction To XtzWithdrawn () ] ;; Ops) by Transactions
     ==Int B' +Int Transactions([ Transfer DEXTER To TokensWithdrawn ] ;; [ Transaction To XtzWithdrawn () ] ;; Ops) by Transactions
     ==Int B' +Int Transactions([ Burn Sender LqtBurned ] ;; [ Transfer DEXTER To TokensWithdrawn ] ;; [ Transaction To XtzWithdrawn () ] ;; Ops) by Transactions
     ==Int B' +Int Transactions(Ops' ;; Ops) by Ops'
- T' ==Int T -Int TokensWithdrawn
     <=Int D +Int Transfers(RemoveLiquidity(...) ;; Ops) -Int TokensWithdrawn by premise
     ==Int D' +Int Transfers(RemoveLiquidity(...) ;; Ops) -Int TokensWithdrawn by D'
     ==Int D' +Int Transfers(Ops) -Int TokensWithdrawn by Transactions
     ==Int D' +Int Transfers([ Transaction To XtzWithdrawn () ] ;; Ops) -Int TokensWithdrawn by Transactions
     ==Int D' +Int Transfers([ Transfer DEXTER To TokensWithdrawn ] ;; [ Transaction To XtzWithdrawn () ] ;; Ops) by Transactions
     ==Int D' +Int Transfers([ Burn Sender LqtBurned ] ;; [ Transfer DEXTER To TokensWithdrawn ] ;; [ Transaction To XtzWithdrawn () ] ;; Ops) by Transactions
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops'
- L' ==Int L -Int LqtBurned
     ==Int S +Int MintBurns(RemoveLiquidity(...) ;; Ops) -Int LqtBurned by premise
     ==Int S' +Int MintBurns(RemoveLiquidity(...) ;; Ops) -Int LqtBurned by S'
     ==Int S' +Int MintBurns(Ops) -Int LqtBurned by Transactions
     ==Int S' +Int MintBurns([ Transaction To XtzWithdrawn () ] ;; Ops) -Int LqtBurned by Transactions
     ==Int S' +Int MintBurns([ Transfer DEXTER To TokensWithdrawn ] ;; [ Transaction To XtzWithdrawn () ] ;; Ops) -Int LqtBurned by Transactions
     ==Int S' +Int MintBurns([ Burn Sender LqtBurned ] ;; [ Transfer DEXTER To TokensWithdrawn ] ;; [ Transaction To XtzWithdrawn () ] ;; Ops) by Transactions
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops'
```

```
claim [inv-xtz-to-token]:
<operations>  ( [ XtzToToken(To, _, Deadline) ] => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<mybalance>   #Mutez(B => B')   </mybalance>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
<senderaddr>  Sender            </senderaddr>
requires 0 <Int X  andBool X  ==Int B  +Int Transactions(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Transactions(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-xtz-to-token]:
- assume IS_VALID(Deadline) // otherwise, it reverts, and we conclude
- apply [xtz-to-token]
- unify RHS
  - Ops' == [ Transfer DEXTER To TokensBought ]
  - X' == X +Int XtzSold
  - T' == T -Int ( 997 *Int XtzSold *Int T /Int (1000 *Int X +Int 997 *Int XtzSold) #as TokensBought )
  - L' == L
  - B' == B +Int XtzSold
  - D' == D
  - S' == S
- X' >Int 0 by X >Int 0 and XtzSold >=Int 0
- TokensBought <Int T by T >Int 0 and X >Int 0 and XtzSold >=Int 0 // TODO: double-check
- T' >Int 0 by TokensBought <Int T
- L' >Int 0 by L >Int 0
- X' ==Int X +Int XtzSold
     ==Int B +Int Transactions(XtzToToken(...) ;; Ops) +Int XtzSold by premise
     ==Int B' +Int Transactions(XtzToToken(...) ;; Ops) by B'
     ==Int B' +Int Transactions(Ops) by Transactions
     ==Int B' +Int Transactions([ Transfer DEXTER To TokensBought ] ;; Ops) by Transactions
     ==Int B' +Int Transactions(Ops' ;; Ops) by Ops'
- T' ==Int T -Int TokensBought
     <=Int D +Int Transfers(XtzToToken(...) ;; Ops) -Int TokensBought by premise
     ==Int D' +Int Transfers(XtzToToken(...) ;; Ops) -Int TokensBought by D'
     ==Int D' +Int Transfers(Ops) -Int TokensBought by Transactions
     ==Int D' +Int Transfers([ Transfer DEXTER To TokensBought ] ;; Ops) by Transactions
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops'
- L' ==Int L
     ==Int S +Int MintBurns(XtzToToken(...) ;; Ops) by premise
     ==Int S' +Int MintBurns(XtzToToken(...) ;; Ops) by S'
     ==Int S' +Int MintBurns(Ops) by Transactions
     ==Int S' +Int MintBurns([ Transfer DEXTER To TokensBought ] ;; Ops) by Transactions
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops'
```

```
claim [inv-token-to-xtz]:
<operations>  ( [ TokenToXtz(To, TokensSold, _, Deadline) ] => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<mybalance>   #Mutez(B => B')   </mybalance>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
<senderaddr>  Sender            </senderaddr>
requires 0 <Int X  andBool X  ==Int B  +Int Transactions(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Transactions(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-token-to-xtz]:
- assume IS_VALID(Deadline) // otherwise, it reverts, and we conclude
- Sender =/=K DEXTER by [sender-is-not-dexter]
- apply [token-to-xtz]
- unify RHS
  - Ops' == [ Transfer Sender DEXTER TokensSold ] ;; [ Transaction To XtzBought () ]
  - X' == X -Int ( 997 *Int TokensSold *Int X /Int (1000 *Int T +Int 997 *Int TokensSold) #as XtzBought )
  - T' == T +Int TokensSold
  - L' == L
  - B' == B
  - D' == D
  - S' == S
- XtzBought <Int X by X >Int 0 and T >Int 0 and TokensSold >=Int 0 // TODO: double-check
- X' >Int 0 by XtzBought <Int X
- T' >Int 0 by T >Int 0 and TokensSold >=Int 0
- L' >Int 0 by L >Int 0
- X' ==Int X -Int XtzBought
     ==Int B +Int Transactions(TokenToXtz(...) ;; Ops) -Int XtzBought by premise
     ==Int B' +Int Transactions(TokenToXtz(...) ;; Ops) -Int XtzBought by B'
     ==Int B' +Int Transactions(Ops) -Int XtzBought by Transactions
     ==Int B' +Int Transactions([ Transaction To XtzBought () ] ;; Ops) by Transactions
     ==Int B' +Int Transactions([ Transfer Sender DEXTER TokensSold ] ;; [ Transaction To XtzBought () ] ;; Ops) by Transactions
     ==Int B' +Int Transactions(Ops' ;; Ops) by Ops'
- T' ==Int T +Int TokensSold
     <=Int D +Int Transfers(TokenToXtz(...) ;; Ops) +Int TokensSold by premise
     ==Int D' +Int Transfers(TokenToXtz(...) ;; Ops) +Int TokensSold by D'
     ==Int D' +Int Transfers(Ops) +Int TokensSold by Transactions
     ==Int D' +Int Transfers([ Transaction To XtzBought () ] ;; Ops) +Int TokensSold by Transactions
     ==Int D' +Int Transfers([ Transfer Sender DEXTER TokensSold ] ;; [ Transaction To XtzBought () ] ;; Ops) by Transactions and Sender =/=K DEXTER
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops'
- L' ==Int L
     ==Int S +Int MintBurns(TokenToXtz(...) ;; Ops) by premise
     ==Int S' +Int MintBurns(TokenToXtz(...) ;; Ops) by S'
     ==Int S' +Int MintBurns(Ops) by Transactions
     ==Int S' +Int MintBurns([ Transaction To XtzBought () ] ;; Ops) by Transactions
     ==Int S' +Int MintBurns([ Transfer Sender DEXTER TokensSold ] ;; [ Transaction To XtzBought () ] ;; Ops) by Transactions
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops'
```

```
claim [inv-token-to-token]:
<operations>  ( [ TokenToToken(OutputDexterContract, MinTokensBought, To, TokensSold, Deadline) ] => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<mybalance>   #Mutez(B => B')   </mybalance>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
<senderaddr>  Sender            </senderaddr>
requires 0 <Int X  andBool X  ==Int B  +Int Transactions(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Transactions(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-token-to-token]:
- assume IS_VALID(Deadline) // otherwise, it reverts, and we conclude
- Sender =/=K DEXTER by [sender-is-not-dexter]
- apply [token-to-token]
- unify RHS
  - Ops' == [ Transfer Sender DEXTER TokensSold ] ;; [ Transaction OutputDexterContract XtzBought XtzToToken(To, MinTokensBought, Deadline) ]
  - X' == X -Int ( 997 *Int TokensSold *Int X /Int (1000 *Int T +Int 997 *Int TokensSold) #as XtzBought )
  - T' == T +Int TokensSold
  - L' == L
  - B' == B
  - D' == D
  - S' == S
- XtzBought <Int X by X >Int 0 and T >Int 0 and TokensSold >=Int 0 // TODO: double-check
- X' >Int 0 by XtzBought <Int X
- T' >Int 0 by T >Int 0 and TokensSold >=Int 0
- L' >Int 0 by L >Int 0
- X' ==Int X -Int XtzBought
     ==Int B +Int Transactions(TokenToToken(...) ;; Ops) -Int XtzBought by premise
     ==Int B' +Int Transactions(TokenToToken(...) ;; Ops) -Int XtzBought by B'
     ==Int B' +Int Transactions(Ops) -Int XtzBought by Transactions
     ==Int B' +Int Transactions([ Transaction OutputDexterContract XtzBought XtzToToken(To, MinTokensBought, Deadline) ] ;; Ops) by Transactions
     ==Int B' +Int Transactions([ Transfer Sender DEXTER TokensSold ] ;; [ Transaction OutputDexterContract XtzBought XtzToToken(To, MinTokensBought, Deadline) ] ;; Ops) by Transactions
     ==Int B' +Int Transactions(Ops' ;; Ops) by Ops'
- T' ==Int T +Int TokensSold
     <=Int D +Int Transfers(TokenToToken(...) ;; Ops) +Int TokensSold by premise
     ==Int D' +Int Transfers(TokenToToken(...) ;; Ops) +Int TokensSold by D'
     ==Int D' +Int Transfers(Ops) +Int TokensSold by Transactions
     ==Int D' +Int Transfers([ Transaction OutputDexterContract XtzBought XtzToToken(To, MinTokensBought, Deadline) ] ;; Ops) +Int TokensSold by Transactions
     ==Int D' +Int Transfers([ Transfer Sender DEXTER TokensSold ] ;; [ Transaction OutputDexterContract XtzBought XtzToToken(To, MinTokensBought, Deadline) ] ;; Ops) by Transactions and Sender =/=K DEXTER
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops'
- L' ==Int L
     ==Int S +Int MintBurns(TokenToToken(...) ;; Ops) by premise
     ==Int S' +Int MintBurns(TokenToToken(...) ;; Ops) by S'
     ==Int S' +Int MintBurns(Ops) by Transactions
     ==Int S' +Int MintBurns([ Transaction OutputDexterContract XtzBought XtzToToken(To, MinTokensBought, Deadline) ] ;; Ops) by Transactions
     ==Int S' +Int MintBurns([ Transfer Sender DEXTER TokensSold ] ;; [ Transaction OutputDexterContract XtzBought XtzToToken(To, MinTokensBought, Deadline) ] ;; Ops) by Transactions
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops'
```

```
claim [inv-update-token-pool]:
<operations>  ( [ UpdateTokenPool() ] => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<mybalance>   #Mutez(B => B')   </mybalance>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
<sourceaddr>  Source            </sourceaddr>
<senderaddr>  Sender            </senderaddr>
<selfIsUpdatingTokenPool> false => true </selfIsUpdatingTokenPool>
requires 0 <Int X  andBool X  ==Int B  +Int Transactions(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Transactions(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-update-token-pool]:
- assume Source ==K Sender // otherwise, it reverts, and we conclude
- apply [update-token-pool]
- unify RHS
  - Ops' == [ Transaction TOKEN 0 BalanceOf(DEXTER, UpdateTokenPoolInternal) ]
  - X' == X
  - T' == T
  - L' == L
  - B' == B
  - D' == D
  - S' == S
- X' >Int 0 by X >Int 0
- T' >Int 0 by T >Int 0
- L' >Int 0 by L >Int 0
- X' ==Int X
     ==Int B +Int Transactions(UpdateTokenPool() ;; Ops) by premise
     ==Int B' +Int Transactions(UpdateTokenPool() ;; Ops) by B'
     ==Int B' +Int Transactions(Ops) by Transactions
     ==Int B' +Int Transactions([ Transaction TOKEN 0 BalanceOf(DEXTER, UpdateTokenPoolInternal) ] ;; Ops) by Transactions
     ==Int B' +Int Transactions(Ops' ;; Ops) by Ops'
- T' ==Int T
     <=Int D +Int Transfers(UpdateTokenPool() ;; Ops) by premise
     ==Int D' +Int Transfers(UpdateTokenPool() ;; Ops) by D'
     ==Int D' +Int Transfers(Ops) by Transactions
     ==Int D' +Int Transfers([ Transaction TOKEN 0 BalanceOf(DEXTER, UpdateTokenPoolInternal) ] ;; Ops) by Transactions
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops'
- L' ==Int L
     ==Int S +Int MintBurns(UpdateTokenPool() ;; Ops) by premise
     ==Int S' +Int MintBurns(UpdateTokenPool() ;; Ops) by S'
     ==Int S' +Int MintBurns(Ops) by Transactions
     ==Int S' +Int MintBurns([ Transaction TOKEN 0 BalanceOf(DEXTER, UpdateTokenPoolInternal) ] ;; Ops) by Transactions
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops'
```

```
claim [inv-update-token-pool-internal]:
<operations>  ( [ UpdateTokenPoolInternal(TokenPool) ] => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<mybalance>   #Mutez(B => B')   </mybalance>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
<senderaddr>  Sender            </senderaddr>
<selfIsUpdatingTokenPool> true => false </selfIsUpdatingTokenPool>
requires 0 <Int X  andBool X  ==Int B  +Int Transactions(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Transactions(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-update-token-pool-internal]:
- assume Sender ==K TOKEN // otherwise, it reverts, and we conclude
- assert TokenPool // TODO:
- apply [update-token-pool-internal]
- unify RHS
  - Ops' == .List
  - X' == X
  - T' == TokenPool
  - L' == L
  - B' == B
  - D' == D
  - S' == S
- X' >Int 0 by X >Int 0
- T' >Int 0 by // TODO:
- L' >Int 0 by L >Int 0
- X' ==Int X
     ==Int B +Int Transactions(UpdateTokenPoolInternal() ;; Ops) by premise
     ==Int B' +Int Transactions(UpdateTokenPoolInternal() ;; Ops) by B'
     ==Int B' +Int Transactions(Ops) by Transactions
     ==Int B' +Int Transactions(Ops' ;; Ops) by Ops'
- T' // TODO:
- L' ==Int L
     ==Int S +Int MintBurns(UpdateTokenPoolInternal() ;; Ops) by premise
     ==Int S' +Int MintBurns(UpdateTokenPoolInternal() ;; Ops) by S'
     ==Int S' +Int MintBurns(Ops) by Transactions
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops'
```

```
claim [inv-default]:
<operations>  ( [ Default() ] => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<mybalance>   #Mutez(B => B')   </mybalance>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
<selfIsUpdatingTokenPool> IsUpdatingTokenPool </selfIsUpdatingTokenPool>
requires 0 <Int X  andBool X  ==Int B  +Int Transactions(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Transactions(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-default]:
- assume IsUpdatingTokenPool ==K false // otherwise, it reverts, and we conclude
- apply [default]
- unify RHS
  - Ops' == .List
  - X' == X +Int Amount
  - T' == T
  - L' == L
  - B' == B +Int Amount
  - D' == D
  - S' == S
- X' >Int 0 by X >Int 0 and Amount >=Int 0
- T' >Int 0 by T >Int 0
- L' >Int 0 by L >Int 0
- X' ==Int X +Int Amount
     ==Int B +Int Transactions(Default() ;; Ops) +Int Amount by premise
     ==Int B' +Int Transactions(Default() ;; Ops) by B'
     ==Int B' +Int Transactions(Ops) by Transactions
     ==Int B' +Int Transactions(Ops' ;; Ops) by Ops'
- T' ==Int T
     <=Int D +Int Transfers(Default() ;; Ops) by premise
     ==Int D' +Int Transfers(Default() ;; Ops) by D'
     ==Int D' +Int Transfers(Ops) by Transactions
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops'
- L' ==Int L
     ==Int S +Int MintBurns(Default() ;; Ops) by premise
     ==Int S' +Int MintBurns(Default() ;; Ops) by S'
     ==Int S' +Int MintBurns(Ops) by Transactions
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops'
```

### Opearation-level Specs

#### Common:
- caller cannot be dexter.
- `to` may be dexter.

```
syntax Bool ::= IS_VALID(Int) [macro]
rule [is-valid]:
[[ IS_VALID(Deadline) => IsUpdatingTokenPool ==K false andBool Now <Int Deadline ]]
<selfIsUpdatingTokenPool> IsUpdatingTokenPool </selfIsUpdatingTokenPool>
<mynow> #Timestamp(Now) </mynow>
```

```
syntax Address ::= DEXTER [macro]
rule [dexter]:
[[ DEXTER => D ]]
<myaddr> D </myaddr>
```

#### AddLiquidity(Owner, MinLqtMinted, MaxTokensDeposited, Deadline)

```
rule [add-liquidity]:
<operations>  ( [ AddLiquidity(Owner, MinLqtMinted, MaxTokensDeposited, Deadline) ] => OpsEmitted ) ;; _ </operations>
<xtzPool>     #Mutez(X => X +Int XtzDeposited)      </xtzPool>
<tokenPool>          T => T +Int TokensDeposited    </tokenPool>
<lqtTotal>           L => L +Int LqtMinted          </lqtTotal>
<mybalance>   #Mutez(B => B +Int XtzDeposited)      </mybalance>
<tokenDexter>        D                              </tokenDexter>
<lqtSupply>          S                              </lqtSupply>
<senderaddr>  Sender                                </senderaddr>
requires IS_VALID(Deadline)
ensures  TokensDeposited ==Int XtzDeposited *Int T up/Int X
 andBool LqtMinted       ==Int XtzDeposited *Int L   /Int X
 andBool OpsEmitted ==K [ Transfer Sender DEXTER TokensDeposited ]
                     ;; [ Mint Owner LqtMinted ]
 andBool TokensDeposited <=Int MaxTokensDeposited
 andBool LqtMinted       >=Int MinLqtMinted
```

#### RemoveLiquidity(To, LqtBurned, MinXtzWithdrawn, MinTokensWithdrawn, Deadline)

NOTE:
- It needs to check `l < L`, otherwise the contract becomes nonfunctional.
- Given `l < L`, we have `x < X` and `t < T`, thus `X' > 0` and `T' > 0`.

```
rule [remove-liquidity]:
<operations>  ( [ RemoveLiquidity(To, LqtBurned, MinXtzWithdrawn, MinTokensWithdrawn, Deadline) ] => OpsEmitted ) ;; _ </operations>
<xtzPool>     #Mutez(X => X -Int XtzWithdrawn)      </xtzPool>
<tokenPool>          T => T -Int TokensWithdrawn    </tokenPool>
<lqtTotal>           L => L -Int LqtBurned          </lqtTotal>
<mybalance>   #Mutez(B)                             </mybalance>
<tokenDexter>        D                              </tokenDexter>
<lqtSupply>          S                              </lqtSupply>
<senderaddr>  Sender                                </senderaddr>
requires IS_VALID(Deadline)
 andBool LqtBurned <Int L
ensures  XtzWithdrawn    ==Int LqtBurned *Int X /Int L
 andBool TokensWithdrawn ==Int LqtBurned *Int T /Int L
 andBool OpsEmitted ==K [ Burn Sender LqtBurned ]
                     ;; [ Transfer DEXTER To TokensWithdrawn ]
                     ;; [ Transaction To XtzWithdrawn () ]
 andBool XtzWithdrawn    >=Int MinXtzWithdrawn
 andBool TokensWithdrawn >=Int MinTokensWithdrawn
```

#### XtzToToken(To, MinTokensBought, Deadline)

NOTE:
- `TokensBought < T` if `T > 0`

```
rule [xtz-to-token]:
<operations>  ( [ XtzToToken(To, MinTokensBought, Deadline) ] => OpsEmitted ) ;; _ </operations>
<xtzPool>     #Mutez(X => X +Int XtzSold)       </xtzPool>
<tokenPool>          T => T -Int TokensBought   </tokenPool>
<lqtTotal>           L                          </lqtTotal>
<mybalance>   #Mutez(B => B +Int XtzSold)       </mybalance>
<tokenDexter>        D                          </tokenDexter>
<lqtSupply>          S                          </lqtSupply>
<senderaddr>  Sender                            </senderaddr>
requires IS_VALID(Deadline)
ensures  TokensBought ==Int 997 *Int XtzSold *Int T /Int (1000 *Int X +Int 997 *Int XtzSold)
 andBool OpsEmitted ==K [ Transfer DEXTER To TokensBought ]
 andBool TokensBought >=Int MinTokensBought
```

#### TokenToXtz(To, TokensSold, MinXtzBought, Deadline)

NOTE:
- `XtzBought <Int X` if `X > 0`

```
rule [token-to-xtz]:
<operations>  ( [ TokenToXtz(To, TokensSold, MinXtzBought, Deadline) ] => OpsEmitted ) ;; _ </operations>
<xtzPool>     #Mutez(X => X -Int XtzBought)     </xtzPool>
<tokenPool>          T => T +Int TokensSold     </tokenPool>
<lqtTotal>           L                          </lqtTotal>
<mybalance>   #Mutez(B)                         </mybalance>
<tokenDexter>        D                          </tokenDexter>
<lqtSupply>          S                          </lqtSupply>
<senderaddr>  Sender                            </senderaddr>
requires IS_VALID(Deadline)
ensures  XtzBought ==Int 997 *Int TokensSold *Int X /Int (1000 *Int T +Int 997 *Int TokensSold)
 andBool OpsEmitted ==K [ Transfer Sender DEXTER TokensSold ]
                     ;; [ Transaction To XtzBought () ]
 andBool XtzBought >=Int MinXtzBought
```

#### TokenToToken(OutputDexterContract, MinTokensBought, To, TokensSold, Deadline)

NOTE:
- The following two should be equivalent:
  - `[ Transaction DEXTER 0 TokenToToken(OutputDexterContract, MinTokensBought, To, TokensSold, Deadline) ]`
  - `[ Transaction DEXTER 0 TokenToXtz(Sender, TokensSold, 0, Deadline) ] ;; [ Transaction OutputDexterContract XtzBought XtzToToken(To, MinTokensBought, Deadline) ]`

```
rule [token-to-token]:
<operations>  ( [ TokenToToken(OutputDexterContract, MinTokensBought, To, TokensSold, Deadline) ] => OpsEmitted ) ;; _ </operations>
<xtzPool>     #Mutez(X => X -Int XtzBought)     </xtzPool>
<tokenPool>          T => T +Int TokensSold     </tokenPool>
<lqtTotal>           L                          </lqtTotal>
<mybalance>   #Mutez(B)                         </mybalance>
<tokenDexter>        D                          </tokenDexter>
<lqtSupply>          S                          </lqtSupply>
<senderaddr>  Sender                            </senderaddr>
requires IS_VALID(Deadline)
ensures  XtzBought ==Int 997 *Int TokensSold *Int X /Int (1000 *Int T +Int 997 *Int TokensSold)
 andBool OpsEmitted ==K [ Transfer Sender DEXTER TokensSold ]
                     ;; [ Transaction OutputDexterContract XtzBought XtzToToken(To, MinTokensBought, Deadline) ]
```

#### UpdateTokenPool()

```
rule [update-token-pool]:
<operations>  ( [ UpdateTokenPool() ] => OpsEmitted ) ;; _ </operations>
<xtzPool>     #Mutez(X) </xtzPool>
<tokenPool>          T  </tokenPool>
<lqtTotal>           L  </lqtTotal>
<mybalance>   #Mutez(B) </mybalance>
<tokenDexter>        D  </tokenDexter>
<lqtSupply>          S  </lqtSupply>
<senderaddr>  Sender    </senderaddr>
<sourceaddr>  Source    </sourceaddr>
<selfIsUpdatingTokenPool> false => true </selfIsUpdatingTokenPool>
requires Sender ==K Source
ensures  XtzBought ==Int 997 *Int TokensSold *Int X /Int (1000 *Int T +Int 997 *Int TokensSold)
 andBool OpsEmitted ==K [ Transaction TOKEN 0 BalanceOf(DEXTER, UpdateTokenPoolInternal) ]

rule [update-token-pool-internal]:
<operations>  ( [ UpdateTokenPoolInternal(TokenPool) ] => .List ) ;; _ </operations>
<xtzPool>     #Mutez(X)             </xtzPool>
<tokenPool>          T => TokenPool </tokenPool>
<lqtTotal>           L              </lqtTotal>
<mybalance>   #Mutez(B)             </mybalance>
<tokenDexter>        D              </tokenDexter>
<lqtSupply>          S              </lqtSupply>
<senderaddr>  TOKEN                 </senderaddr>
<selfIsUpdatingTokenPool> true => false </selfIsUpdatingTokenPool>
```

#### Default()

```
rule [default]:
<operations>  ( [ Default() ] => .List ) ;; _ </operations>
<xtzPool>     #Mutez(X => X +Int Amount)    </xtzPool>
<tokenPool>          T                      </tokenPool>
<lqtTotal>           L                      </lqtTotal>
<mybalance>   #Mutez(B => B +Int Amount)    </mybalance>
<tokenDexter>        D                      </tokenDexter>
<lqtSupply>          S                      </lqtSupply>
<selfIsUpdatingTokenPool> false </selfIsUpdatingTokenPool>
```

