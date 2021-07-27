# Lqt Token Invariants

In this document we prove some important invariants about the Lqt Token contract.

## Sum of balances in account equals `totalSupply`

```
syntax Int ::= Sum(Map) [function]
rule Sum(.Map) => 0
rule Sum(_ |-> N Rest) => N +Int Sum(Rest)
```

We need the following lemma:

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
    <allowancesL> Allowances  => Allowances'  </allowances>
  requires TotalSupply  ==Int Sum(Allowances)
  ensures  TotalSupply' ==Int Sum(Allowances')
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
                      and Allowances  == Allowances'
                - else:
                    apply [sum-of-increment]
                    apply [sum-of-increment]
          - case From <> Sender
              - apply [LQT-TOKEN-TRANSFER-PROXY-SPEC]
                - case Aborted:
                    We have TotalSupply == TotalSupply'
                        and Allowances  == Allowances'
                - else:
                    apply [sum-of-increment]
                    apply [sum-of-increment]
      - case CallParams == MintOrBurn(Quantity, Address)
          - apply [LQT-TOKEN-MINTORBURN-SPEC]
            - case Aborted:
                We have TotalSupply == TotalSupply'
                    and Allowances  == Allowances'
            - else:
                apply [sum-of-increment]
      - else:
          - We have TotalSupply == TotalSupply'
                and Allowances  == Allowances'
```
