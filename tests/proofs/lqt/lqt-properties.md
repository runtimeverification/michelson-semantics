# LQT Token Proofs

In this document, we prove the properties of the Lqt Token contract needed
to verify the Dexter system. These properties are:

1.  The MintOrBurn() entrypoint updates total liquidity if prerequisites are met.

    ```
    claim [lqt-mint-burn]:
        <operations> ( [ Transaction DEXTER LQT Amount MintOrBurn(_, Value) ] => .List ) ;; _ </operations>
        <tokens> Tokens => Tokens' </tokens>
        <totalSupply> S => S +Int Value </totalSupply>
      requires Amount ==Int 0
      andBool Tokens[Address] + Value  >= 0
    ```

2.  `[only-lqt-mint-burn]`:

    * Only MintOrBurn() can update the total liquidity supply,
    * only Dexter is permitted to call this entrypoint
    * MintOrBurn doesn't emit any operations.

    ```
    claim [only-lqt-mint-burn]:
        <operations> (Transaction Sender To Amount Op => .List) ;; _ </operations>
        <adminAddress> DEXTER </adminAddress>
        <tokens> Tokens => Tokens' </tokens>
        <totalSupply> S => S' </totalSupply>
      ensures  S' =/=Int S impliesBool  ( Op     == MintOrBurn(Address, Quantity)
                                      and To     == Lqt
                                      and Amount == 0
                                      and S' ==Int S +Int Quantity
                                      and Sender == Dexter
                                        )
    ```

These are proved in the following subsections.

## Lemmas

First, we need a lemma to prove our main properties:
"The sum of the balances in all accounts equals `totalSupply`" is an invariant.

```
syntax Int ::= Sum(Map) [function]
rule Sum(.Map) => 0
rule Sum(_ |-> N Rest) => N +Int Sum(Rest)
```

```
claim [sum-of-increment]: Sum(#incrementTokens(M, Addr, Quantity)) => Sum(M) +Int Quantity
proof:
    - Induction on structure of M
        - case M == .Map
            - We have Sum(#incrementTokens(.Map, Addr, Quantity)) == Sum(Addr |-> Quantity) == Quantity == Sum(.Map) + Quantity
        - case M == (Addr |-> OldValue) M'
            - case OldValue <> -Quantity
                - We have Sum(#incrementTokens((Addr |-> OldValue) M', Addr, Quantity))
                       == Sum((Addr |-> OldValue +Int Quantity) M')
                       == OldValue + Quantity + Sum(M')
                       == Sum(Addr |-> OldValue, M') + Quantity
                       == Sum(M) + Quantity
            - case OldValue == -Quantity
                - We have Sum(#incrementTokens((Addr |-> OldValue) M', Addr, Quantity))
                       == Sum(M')
                       == OldValue + Quantity + Sum(M')
                       == Sum(Addr |-> OldValue, M') + Quantity
                       == Sum(M) + Quantity
        - case M == (Addr' |-> Value) M' and Addr' =/= Addr
            - We have Sum(#incrementTokens((Addr' |-> Value) M', Addr, Quantity))
                   == Sum((Addr' |-> Value) #incrementTokens(M', Addr, Quantity))
                   == Value + Sum(#incrementTokens(M', Addr, Quantity))
                   == Value + Quantity + Sum(M')
                   == Quantity + Sum(M)
```

```
claim [inv-totalSupply-is-sum]:
    <operations>  ( [ Transaction Sender LQT Amount CallParams ] #as Op => Ops' ) ;; Ops </operations>
    <totalSupply> TotalSupply => TotalSupply' </totalSupply>
    <tokens> Tokens  => Tokens'  </tokens>
  requires TotalSupply  ==Int Sum(Tokens)
  ensures  TotalSupply' ==Int Sum(Tokens')
```

```
proof:
  - split CallParams
      - case CallParams == TransferParams(From, To, Value)
          - case From == Sender
              - unify RHS
              - apply [LQT-TOKEN-TRANSFER-DIRECT-SPEC]
                - case Aborted:
                  We have TotalSupply == TotalSupply'
                      and Tokens  == Tokens'
                - else:
                    apply [sum-of-increment]
                    apply [sum-of-increment]
          - case From <> Sender
              - apply [LQT-TOKEN-TRANSFER-PROXY-SPEC]
                - case Aborted:
                    We have TotalSupply == TotalSupply'
                        and Tokens  == Tokens'
                - else:
                    apply [sum-of-increment]
                    apply [sum-of-increment]
      - case CallParams == MintOrBurn(Quantity, Address)
          - apply [LQT-TOKEN-MINTORBURN-SPEC]
            - case Aborted:
                We have TotalSupply == TotalSupply'
                    and Tokens  == Tokens'
            - else:
                apply [sum-of-increment]
      - else:
          - We have TotalSupply == TotalSupply'
                and Tokens  == Tokens'
```

## Only `MintOrBurn` entrypoint can change the total liquidity

```
claim [only-lqt-mint-burn]:
    <operations> (Transaction Sender To Amount Op => _) ;; _ </operations>
    <adminAddress> DEXTER </adminAddress>
    <tokens> Tokens => Tokens' </tokens>
    <totalSupply> S => S' </totalSupply>
  ensures  S' =/=Int S impliesBool  ( Op     == MintOrBurn(Address, Quantity)
                                  and To     == Lqt
                                  and Amount == 0
                                  and S' ==Int S +Int Quantity
                                  and Sender == Dexter
                                    )
```

```
proof
 - case To <> Lqt
     - Lqt storage cannot be changed by other contracts directly
 - case To == Lqt
    - case Op == MintOrBurn
         - case Amount == 0 and Sender == DEXTER
            - apply [LQT-TOKEN-MINTORBURN-SPEC]
                - case Aborted:
                    We have S' == S
                - else:
                    We have  S' == absInt(TotalSupply +Int Quantity)
                        and  Tokens[Address] +Int Quantity >= 0
                    By well-typedness we have: Tokens[*] >= 0 
                    By [inv-totalSupply-is-sum] we get: TotalSupply == Sum
                    proving: S' == TotalSupply +Int Quantity
         - case Amount <> 0 and Sender <> DEXTER
             We have Aborted ==> S == S'
    - else:
        - apply [LQT-TOKEN-*-SPEC]
          we have S' == S
```

## The `MintOrBurn` updates liquidity if prerequisites are met


```
claim [lqt-mint-burn]:
    <operations> ( [ Transaction DEXTER LQT Amount MintOrBurn(_, Value) ] => .List ) ;; _ </operations>
    <tokens> Tokens => Tokens' </tokens>
    <totalSupply> S => S +Int Value </totalSupply>
  requires Amount ==Int 0
  andBool Tokens[Address] + Value  >= 0
```

```
proof:
- apply [LQT-TOKEN-MINTORBURN-SPEC]
```
