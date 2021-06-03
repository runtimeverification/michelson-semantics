# Correctness Properties of Dexter Contract

We formulate and prove correctness properties of Dexter over the sequence of arbitrary transactions.

## Faithfulness of State Variables

In the Dexter contract, the token exchange rate and the liquidity price are determined by the three state variables (XtzPool, TokenPool, LqtTotal) which refers to the XTZ reserve, the token reserve, and the total liquidity supply, respectively.

The first invariant states that the Dexter state variables faithfully represent the actual pool reserves and liquidity supply.  That is, XtzPool and TokenPool must be equal to the actual XTZ and token reserves, and LqtTotal must be equal to the actual total liquidity supply.  Note that the Dexter entrypoint functions immediately update the state variables, while the actual reserves or supply are to be updated later by the continuation operations emitted by the entrypoints.  More precisely, the actual token reserve may be possibly larger than TokenPool, since one can “donate” tokens to Dexter (i.e., directly sending tokens to Dexter without going through any of the Dexter entrypoint functions).  Note that, however, the actual XTZ reserve must be equal to the XtzPool value, since directly sending XTZ to Dexter will be captured by the Default() entrypoint.  (Indeed, we assume that, in Tezos, there is no way to “secretly” send XTZ to Dexter without triggering the Dexter entrypoint functions.  Note that, in Ethereum, it is _possible_ to send Ether to a smart contract without ever executing the contract code.)

The following claim `[inv-top-level]` states that the invariant holds at the completion of every top-level transaction.  Note that a top-level transaction is a transaction created by a non-contract account (i.e., a transaction whose sender is equal to the source), and the completion of a transaction involves the full execution “tree” following the DFS model adopted in the Florence upgrade.

The `<xtzPool>`, `<tokenPool>`, and `<lqtTotal>` cells denote the Dexter state variables, XtzPool, TokenPool, and LqtTotal, respectively.  The `<xtzDexter>`, `<tokenDexter>`, and `<lqtSupply>` cells denote the actual XTZ and token reserves, and total liquidity supply, respectively.

```
claim [inv-top-level]:
<operations> ( [ Transaction Sender _ _ _ ] #as Op =>* .List ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<xtzDexter>   #Mutez(B => B')   </xtzDexter>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
<sourceaddr>  Source            </sourceaddr>
requires Sender ==K Source
 andBool 0 <Int X  andBool X  ==Int B
 andBool 0 <Int T  andBool T  <=Int D
 andBool 0 <Int L  andBool L  ==Int S
ensures  0 <Int X' andBool X' ==Int B'
 andBool 0 <Int T' andBool T' <=Int D'
 andBool 0 <Int L' andBool L' ==Int S'

proof [inv-top-level]:
- Sender =/=K DEXTER by [top-level]
- TopLevelOps(Ops, Source) by [dfs]
- Sends(Op ;; Ops) ==Int 0 andBool Transfers(Op ;; Ops) ==Int 0 andBool MintBurns(Op ;; Ops) ==Int 0 by Sender =/=K DEXTER and TopLevelOps(Ops, Source)
- apply [inv-trans]
- unify RHS
  - Ops' == .List
- Sends(Ops) ==Int 0 andBool Transfers(Ops) ==Int 0 andBool MintBurns(Ops) ==Int 0 by Sender =/=K DEXTER
- Sends(Ops' ;; Ops) ==Int 0 andBool Transfers(Ops' ;; Ops) ==Int 0 andBool MintBurns(Ops' ;; Ops) ==Int 0 by Ops'
- qed
```

```
claim [inv-trans]:
<operations> (Op =>* Ops') ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<xtzDexter>   #Mutez(B => B')   </xtzDexter>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-trans]:
- by induction on =>* and [inv]
```

The following claim `[inv]` generalizes the top-level claim `[inv-top-level]` at the operational level.  It says that the execution of an _arbitrary_ operation always preserves a certain relationship between the Dexter state variables and the actual pool reserves and liquidity supply.  Intuitively, the Dexter state variables must reflect the “ultimate” value of the pool reserves and liquidity supply, which will be updated by the continuation operations in the future.

```
claim [inv]:
<operations> (Op => Ops') ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<xtzDexter>   #Mutez(B => B')   </xtzDexter>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)
```

Specifically, Sends(Ops) denotes the amount of XTZ that will be sent to others by the operations Ops, Transfers(Ops) denotes the amount of tokens that will be sent to or received from others by Ops, and MintBurns(Ops) denotes the amount of liquidity that will be minted or burned by Ops.  Thus, the difference between the Dexter state variable XtzPool and the current XTZ reserve must be equal to Sends(Ops), the difference between TokenPool and the current token reserve must be equal to Transfers(Ops), and the difference between LqtTotal and the current liquidity supply must be equal to MintBurns(Ops), where Ops is the current continuation operations to be executed.

Note that Sends, Transfers, and MintBurns count only Dexter-emitted operations, thus they are defined to be zero for operations that are not generated by Dexter.  (See the proposition `[only-dexter]`.)  Since no top-level operations can be generated by Dexter, they are defined to be zero for any top-level operations.

```
syntax Int ::= Sends(OpList) [function]
rule Sends([ Transaction DEXTER _ X _ ] ;; Ops) => Sends(Ops) -Int X
rule Sends(_ ;; Ops) => Sends(Ops) [owise]
rule Sends(.List) => 0

syntax Int ::= Transfers(OpList) [function]
rule Transfers([ Transaction DEXTER TOKEN 0 Transfer(From, To, T) ] ;; Ops) => Transfers(Ops) +Int T when From =/=K DEXTER andBool To ==K DEXTER
rule Transfers([ Transaction DEXTER TOKEN 0 Transfer(From, To, T) ] ;; Ops) => Transfers(Ops) -Int T when From  ==K DEXTER
rule Transfers(_ ;; Ops) => Transfers(Ops) [owise]
rule Transfers(.List) => 0

syntax Int ::= MintBurns(OpList) [function]
rule MintBurns([ Transaction DEXTER LQT 0 Mint(_, L) ] ;; Ops) => MintBurns(Ops) +Int L
rule MintBurns([ Transaction DEXTER LQT 0 Burn(_, L) ] ;; Ops) => MintBurns(Ops) -Int L
rule MintBurns(_ ;; Ops) => MintBurns(Ops) [owise]
rule MintBurns(.List) => 0
```

Below we prove the invariant `[inv]` by induction on the sequence of operations and case analysis over different types of operations.

```
proof [inv]:
- assume that the invariant has held in each of the previous operations
- let Op = Transaction Sender Target Amount CD
- split Sender, Target
  - case Sender <> DEXTER and Target == DEXTER
    - split CD
      - case CD == AddLiquidity _
        - apply [inv-add-liquidity]
      - case CD == RemoveLiquidity _
        - apply [inv-remove-liquidity]
      - case CD == XtzToToken _
        - apply [inv-xtz-to-token]
      - case CD == TokenToXtz _
        - apply [inv-token-to-xtz]
      - case CD == TokenToToken _
        - apply [inv-token-to-token]
      - case CD == UpdateTokenPool _
        - apply [inv-update-token-pool]
      - case CD == UpdateTokenPoolInternal _
        - apply [inv-update-token-pool-internal]
      - case CD == SetBaker _ | CD == SetManager _ | CD == SetLqtAddress _
        - apply [inv-setter]
      - case CD == Default _
        - apply [inv-default]
  - case Sender == DEXTER and Target == DEXTER
    - split CD
      - case CD == XtzToToken _
        - apply [inv-xtz-to-token]
      - case CD == Default _
        - apply [inv-default]
      - case _
        - apply [sender-is-not-dexter]
  - case Sender == DEXTER and Target <> DEXTER
    - split Op
      - case Op == Transaction DEXTER TOKEN 0 (Transfer _)
        - apply [inv-token-transfer]
      - case Op == Transaction DEXTER TOKEN 0 BalanceOf(DEXTER, UpdateTokenPoolInternal) ]
        - apply [inv-token-balance-of]
      - case Op == Transaction DEXTER LQT 0 (Mint _)
        - apply [inv-lqt-mint]
      - case Op == Transaction DEXTER LQT 0 (Burn _)
        - apply [inv-lqt-burn]
      - case Op == Transaction DEXTER _ _ Default()
        - apply [inv-send]
      - case Op == Transaction DEXTER _ _ (XtzToToken _)
        - apply [inv-send]
      - case _
        - apply [dexter-emitted-ops]
  - case Sender <> DEXTER and Target <> DEXTER
    - Sends(Op) == Transfers(Op) == MintBurns(Op) == 0 by Sends, Transfers, MintBurns
    - Sends(Ops') == Transfers(Ops') == MintBurns(Ops') == 0 by [only-dexter]
    - split Target
      - case Target == TOKEN
        - (X', T', L', B', S') == (X, T, L, B, S)
        - CD == Transfer(From, To, Value)
          - From <> DEXTER by [token-transfer]'s assertion
          - D' >= D by [token-security]
        - CD <> (Transfer _)
          - D' >= D by [token-security]
      - case Target == LQT
        - (X', T', L', B', D') == (X, T, L, B, D)
        - CD <> (Mint _) and CD <> (Burn _) by [lqt-mint] and [lqt-burn]
          - S' == S
      - case Target <> TOKEN and Target <> LQT
        - (X', T', L', B', D', S') == (X, T, L, B, D, S)
```

### Proof for Dexter Entrypoint Functions

We prove the invariant for each Dexter entrypoint.

#### AddLiquidity

```
claim [inv-add-liquidity]:
<operations>  ( [ Transaction Sender DEXTER XtzDeposited AddLiquidity(Owner, _, _, _) ] #as Op => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<xtzDexter>   #Mutez(B => B')   </xtzDexter>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-add-liquidity]:
- Sender =/=K DEXTER by [sender-is-not-dexter]
- apply [add-liquidity]
- unify RHS
  - Ops' == ( [ Transaction DEXTER TOKEN 0 Transfer(Sender, DEXTER, TokensDeposited) ] #as Op1 )
         ;; ( [ Transaction DEXTER LQT 0 Mint(Owner, LqtMinted) ] #as Op2 )
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
     ==Int B +Int Sends(Op ;; Ops) +Int XtzDeposited by premise
     ==Int B' +Int Sends(Op ;; Ops) by B'
     ==Int B' +Int Sends(Ops) by Sends and Sender =/=K DEXTER
     ==Int B' +Int Sends(Op2 ;; Ops) by Sends
     ==Int B' +Int Sends(Op1 ;; Op2 ;; Ops) by Sends
     ==Int B' +Int Sends(Ops' ;; Ops) by Ops'
- T' ==Int T +Int TokensDeposited
     <=Int D +Int Transfers(Op ;; Ops) +Int TokensDeposited by premise
     ==Int D' +Int Transfers(Op ;; Ops) +Int TokensDeposited by D'
     ==Int D' +Int Transfers(Ops) +Int TokensDeposited by Transfers
     ==Int D' +Int Transfers(Op2 ;; Ops) +Int TokensDeposited by Transfers
     ==Int D' +Int Transfers(Op1 ;; Op2 ;; Ops) by Transfers and Sender =/=K DEXTER
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops'
- L' ==Int L +Int LqtMinted
     ==Int S +Int MintBurns(Op ;; Ops) +Int LqtMinted by premise
     ==Int S' +Int MintBurns(Op ;; Ops) +Int LqtBurned by S'
     ==Int S' +Int MintBurns(Ops) +Int LqtBurned by MintBurns
     ==Int S' +Int MintBurns(Op2 ;; Ops) by MintBurns
     ==Int S' +Int MintBurns(Op1 ;; Op2 ;; Ops) by MintBurns
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops'
```

#### RemoveLiquidity

```
claim [inv-remove-liquidity]:
<operations>  ( [ Transaction Sender DEXTER Amount RemoveLiquidity(To, LqtBurned, _, _, _) ] #as Op => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<xtzDexter>   #Mutez(B => B')   </xtzDexter>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-remove-liquidity]:
- apply [remove-liquidity]
  - Amount ==Int 0 by assert
  - LqtBurned <Int L by assert
- unify RHS
  - Ops' == ( [ Transaction DEXTER LQT   0            Burn(Sender, LqtBurned) ] #as Op1 )
         ;; ( [ Transaction DEXTER TOKEN 0            Transfer(DEXTER, To, TokensWithdrawn) ] #as Op2 )
         ;; ( [ Transaction DEXTER To    XtzWithdrawn Default() ] #as Op3 )
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
     ==Int B +Int Sends(Op ;; Ops) -Int XtzWithdrawn by premise
     ==Int B' +Int Sends(Op ;; Ops) -Int XtzWithdrawn by B'
     ==Int B' +Int Sends(Ops) -Int XtzWithdrawn by Sends and Amount ==Int 0
     ==Int B' +Int Sends(Op3 ;; Ops) by Sends
     ==Int B' +Int Sends(Op2 ;; Op3 ;; Ops) by Sends
     ==Int B' +Int Sends(Op1 ;; Op2 ;; Op3 ;; Ops) by Sends
     ==Int B' +Int Sends(Ops' ;; Ops) by Ops'
- T' ==Int T -Int TokensWithdrawn
     <=Int D +Int Transfers(Op ;; Ops) -Int TokensWithdrawn by premise
     ==Int D' +Int Transfers(Op ;; Ops) -Int TokensWithdrawn by D'
     ==Int D' +Int Transfers(Ops) -Int TokensWithdrawn by Transfers
     ==Int D' +Int Transfers(Op3 ;; Ops) -Int TokensWithdrawn by Transfers
     ==Int D' +Int Transfers(Op2 ;; Op3 ;; Ops) by Transfers
     ==Int D' +Int Transfers(Op1 ;; Op2 ;; Op3 ;; Ops) by Transfers
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops'
- L' ==Int L -Int LqtBurned
     ==Int S +Int MintBurns(Op ;; Ops) -Int LqtBurned by premise
     ==Int S' +Int MintBurns(Op ;; Ops) -Int LqtBurned by S'
     ==Int S' +Int MintBurns(Ops) -Int LqtBurned by MintBurns
     ==Int S' +Int MintBurns(Op3 ;; Ops) -Int LqtBurned by MintBurns
     ==Int S' +Int MintBurns(Op2 ;; Op3 ;; Ops) -Int LqtBurned by MintBurns
     ==Int S' +Int MintBurns(Op1 ;; Op2 ;; Op3 ;; Ops) by MintBurns
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops'
```

#### XtzToToken

```
claim [inv-xtz-to-token]:
<operations>  ( [ Transaction Sender DEXTER XtzSold XtzToToken(To, _, _) ] #as Op => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<xtzDexter>   #Mutez(B => B')   </xtzDexter>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-xtz-to-token]:
- apply [xtz-to-token]
- unify RHS
  - Ops' == [ Transaction DEXTER TOKEN 0 Transfer(DEXTER, To, TokensBought) ]
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
- split Sender
  - case Sender <> DEXTER
    - X' ==Int X +Int XtzSold
         ==Int B +Int Sends(Op ;; Ops) +Int XtzSold by premise
         ==Int B' +Int Sends(Op ;; Ops) by B'
         ==Int B' +Int Sends(Ops) by Sends
         ==Int B' +Int Sends(Ops' ;; Ops) by Sends
  - case Sender == DEXTER
    - X' ==Int X +Int XtzSold
         ==Int B +Int Sends(Op ;; Ops) +Int XtzSold by premise
         ==Int B' +Int Sends(Op ;; Ops) +Int XtzSold by B'
         ==Int B' +Int (Sends(Ops) -Int XtzSold) +Int XtzSold by Sends
         ==Int B' +Int Sends(Ops) by simp
         ==Int B' +Int Sends(Ops' ;; Ops) by Sends
- T' ==Int T -Int TokensBought
     <=Int D +Int Transfers(Op ;; Ops) -Int TokensBought by premise
     ==Int D' +Int Transfers(Op ;; Ops) -Int TokensBought by D'
     ==Int D' +Int Transfers(Ops) -Int TokensBought by Transfers
     ==Int D' +Int Transfers(Ops' ;; Ops) by Transfers
- L' ==Int L
     ==Int S +Int MintBurns(Op ;; Ops) by premise
     ==Int S' +Int MintBurns(Op ;; Ops) by S'
     ==Int S' +Int MintBurns(Ops) by MintBurns
     ==Int S' +Int MintBurns(Ops' ;; Ops) by MintBurns
```

#### TokenToXtz

```
claim [inv-token-to-xtz]:
<operations>  ( [ Transaction Sender DEXTER Amount TokenToXtz(To, TokensSold, _, _) ] #as Op => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<xtzDexter>   #Mutez(B => B')   </xtzDexter>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-token-to-xtz]:
- Sender =/=K DEXTER by [sender-is-not-dexter]
- apply [token-to-xtz]
  - Amount ==Int 0 by assert
- unify RHS
  - Ops' == ( [ Transaction DEXTER TOKEN 0         Transfer(Sender, DEXTER, TokensSold) ] #as Op1 )
         ;; ( [ Transaction DEXTER To    XtzBought Default() ] #as Op2 )
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
     ==Int B +Int Sends(Op ;; Ops) -Int XtzBought by premise
     ==Int B' +Int Sends(Op ;; Ops) -Int XtzBought by B'
     ==Int B' +Int Sends(Ops) -Int XtzBought by Sends and Amount ==Int 0
     ==Int B' +Int Sends(Op2 ;; Ops) by Sends
     ==Int B' +Int Sends(Op1 ;; Op2 ;; Ops) by Sends
     ==Int B' +Int Sends(Ops' ;; Ops) by Ops'
- T' ==Int T +Int TokensSold
     <=Int D +Int Transfers(Op ;; Ops) +Int TokensSold by premise
     ==Int D' +Int Transfers(Op ;; Ops) +Int TokensSold by D'
     ==Int D' +Int Transfers(Ops) +Int TokensSold by Transfers
     ==Int D' +Int Transfers(Op2 ;; Ops) +Int TokensSold by Transfers
     ==Int D' +Int Transfers(Op1 ;; Op2 ;; Ops) by Transfers and Sender =/=K DEXTER
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops'
- L' ==Int L
     ==Int S +Int MintBurns(Op ;; Ops) by premise
     ==Int S' +Int MintBurns(Op ;; Ops) by S'
     ==Int S' +Int MintBurns(Ops) by MintBurns
     ==Int S' +Int MintBurns(Op2 ;; Ops) by MintBurns
     ==Int S' +Int MintBurns(Op1 ;; Op2 ;; Ops) by MintBurns
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops'
```

#### TokenToToken

```
claim [inv-token-to-token]:
<operations>  ( [ Transaction Sender DEXTER Amount TokenToToken(OutputDexterContract, MinTokensBought, To, TokensSold, Deadline) ] #as Op => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<xtzDexter>   #Mutez(B => B')   </xtzDexter>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-token-to-token]:
- Sender =/=K DEXTER by [sender-is-not-dexter]
- apply [token-to-token]
  - Amount ==Int 0 by assert
- unify RHS
  - Ops' == ( [ Transaction DEXTER TOKEN                0         Transfer(Sender, DEXTER, TokensSold) ] #as Op1 )
         ;; ( [ Transaction DEXTER OutputDexterContract XtzBought XtzToToken(To, MinTokensBought, Deadline) ] #as Op2 )
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
     ==Int B +Int Sends(Op ;; Ops) -Int XtzBought by premise
     ==Int B' +Int Sends(Op ;; Ops) -Int XtzBought by B'
     ==Int B' +Int Sends(Ops) -Int XtzBought by Sends and Amount ==Int 0
     ==Int B' +Int Sends(Op2 ;; Ops) by Sends
     ==Int B' +Int Sends(Op1 ;; Op2 ;; Ops) by Sends
     ==Int B' +Int Sends(Ops' ;; Ops) by Ops'
- T' ==Int T +Int TokensSold
     <=Int D +Int Transfers(Op ;; Ops) +Int TokensSold by premise
     ==Int D' +Int Transfers(Op ;; Ops) +Int TokensSold by D'
     ==Int D' +Int Transfers(Ops) +Int TokensSold by Transfers
     ==Int D' +Int Transfers(Op2 ;; Ops) +Int TokensSold by Transfers
     ==Int D' +Int Transfers(Op1 ;; Op2 ;; Ops) by Transfers and Sender =/=K DEXTER
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops'
- L' ==Int L
     ==Int S +Int MintBurns(Op ;; Ops) by premise
     ==Int S' +Int MintBurns(Op ;; Ops) by S'
     ==Int S' +Int MintBurns(Ops) by MintBurns
     ==Int S' +Int MintBurns(Op2 ;; Ops) by MintBurns
     ==Int S' +Int MintBurns(Op1 ;; Op2 ;; Ops) by MintBurns
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops'
```

#### UpdateTokenPool and UpdateTokenPoolInternal

```
claim [inv-update-token-pool]:
<operations>  ( [ Transaction Sender DEXTER Amount UpdateTokenPool() ] #as Op => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<xtzDexter>   #Mutez(B => B')   </xtzDexter>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-update-token-pool]:
- apply [update-token-pool]
  - Amount ==Int 0 by assert
- unify RHS
  - Ops' == [ Transaction DEXTER TOKEN 0 BalanceOf(DEXTER, UpdateTokenPoolInternal) ]
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
     ==Int B +Int Sends(Op ;; Ops) by premise
     ==Int B' +Int Sends(Op ;; Ops) by B'
     ==Int B' +Int Sends(Ops) by Sends and Amount ==Int 0
     ==Int B' +Int Sends(Ops' ;; Ops) by Sends
- T' ==Int T
     <=Int D +Int Transfers(Op ;; Ops) by premise
     ==Int D' +Int Transfers(Op ;; Ops) by D'
     ==Int D' +Int Transfers(Ops) by Transfers
     ==Int D' +Int Transfers(Ops' ;; Ops) by Transfers
- L' ==Int L
     ==Int S +Int MintBurns(Op ;; Ops) by premise
     ==Int S' +Int MintBurns(Op ;; Ops) by S'
     ==Int S' +Int MintBurns(Ops) by MintBurns
     ==Int S' +Int MintBurns(Ops' ;; Ops) by MintBurns
```

```
claim [inv-update-token-pool-internal]:
<operations>  ( [ Transaction Sender DEXTER Amount UpdateTokenPoolInternal(TokenPool) ] #as Op => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<xtzDexter>   #Mutez(B => B')   </xtzDexter>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-update-token-pool-internal]:
- apply [update-token-pool-internal]
  - Amount ==Int 0 by assert
  - Sender ==K TOKEN by assert
- apply [lemma-token-pool-internal]
  - T <=Int TokenPool <=Int D
  - Transfers(Op ;; Ops) ==Int 0
- unify RHS
  - Ops' == .List
  - X' == X
  - T' == TokenPool
  - L' == L
  - B' == B
  - D' == D
  - S' == S
- X' >Int 0 by X >Int 0
- T' >Int 0 by T >Int 0 and TokenPool >=Int T
- L' >Int 0 by L >Int 0
- X' ==Int X
     ==Int B +Int Sends(Op ;; Ops) by premise
     ==Int B' +Int Sends(Op ;; Ops) by B'
     ==Int B' +Int Sends(Ops) by Sends and Amount ==Int 0
     ==Int B' +Int Sends(Ops' ;; Ops) by Ops'
- T' ==Int TokenPool
     <=Int D
     ==Int D +Int Transfers(Op ;; Ops)
     ==Int D +Int Transfers(Ops) by Transfers
     ==Int D +Int Transfers(Ops' ;; Ops) by Ops'
- L' ==Int L
     ==Int S +Int MintBurns(Op ;; Ops) by premise
     ==Int S' +Int MintBurns(Op ;; Ops) by S'
     ==Int S' +Int MintBurns(Ops) by MintBurns
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops'
```

```
claim [lemma-update-token-pool-internal]:
[[ T <=Int TokenPool andBool TokenPool <=Int D andBool Transfers(Ops) ==Int 0 => true ]]
<operations>  [ Transaction TOKEN DEXTER 0 UpdateTokenPoolInternal(TokenPool) ] ;; Ops </operations>
<tokenPool>   T </tokenPool>
<tokenDexter> D </tokenDexter>

proof sketch:
- Let Tx1 be the parent transaction who emitted the current transaction, and Tx2 be the parent transaction of Tx1.
- Tx1 must be `Transaction DEXTER TOKEN 0 BalanceOf(DEXTER, UpdateTokenPoolInternal)`, and Tx2 must be `Transaction _ DEXTER 0 UpdateTokenPool()`.  Moreover, Tx2 must be top-level.
- Let Ops1 be the remaining transactions in the queue emitted by Tx1 (that will be executed after the current transaction), and Ops2 be the remaining top-level transactions (that will be executed after the completion of Tx1).
- Then, we have `Ops ==K Ops1 ;; Ops2`.  Since the sender of the transactions in `Ops` is not DEXTER, we have `Transfers(Ops) ==Int 0`.
- Now, by IH, `[inv]` holds for Tx1.  Let T0 be the <tokenPool> value at Tx1, D0 be the <tokenDexter> value at Tx1, and Ops0 be the <operations> value at Tx1.  Then, by `[inv]`, we have `T0 <=Int D0 +Int Transfers(Ops0)`.
- Note that Ops0 must be a certain operation sequence (say Ops0Pre) followed by the current transaction followed by Ops.  Since the sender of the transactions in `Ops0` is not DEXTER, we have `Transfers(Ops0) ==Int 0`.
- By `[token-balance-of]`, we have `TokenPool ==Int D0`.
- During the execution of Ops0Pre, the <tokenPool> value does not change, because no DEXTER entrypoint function can be executed (since `<selfIsUpdatingTokenPool>` is true).  Thus, we have `T ==Int T0`
- Also, during the execution of Ops0Pre, the <tokenDexter> value cannot decrease, because only DEXTER can spend its own tokens.  Thus, we have `D >=Int D0`.
- Thus, we have `T <=Int TokenPool andBool TokenPool <=Int D`, which concludes.
```

#### Default

```
claim [inv-default]:
<operations>  ( [ Transaction Sender DEXTER Amount Default() ] #as Op => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<xtzDexter>   #Mutez(B => B')   </xtzDexter>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-default]:
- apply [default]
- unify RHS
  - Ops' == .List
  - X' == X +Int Amount
  - T' == T
  - L' == L
  - D' == D
  - S' == S
- X' >Int 0 by X >Int 0 and Amount >=Int 0
- T' >Int 0 by T >Int 0
- L' >Int 0 by L >Int 0
- split Sender
  - case Sender <> DEXTER
    - X' ==Int X +Int Amount
         ==Int B +Int Sends(Op ;; Ops) +Int Amount by premise
         ==Int B' +Int Sends(Op ;; Ops) by B'
         ==Int B' +Int Sends(Ops) by Sends
         ==Int B' +Int Sends(Ops' ;; Ops) by Ops'
  - case Sender == DEXTER
    - X' ==Int X +Int Amount
         ==Int B +Int Sends(Op ;; Ops) +Int Amount by premise
         ==Int B' +Int Sends(Op ;; Ops) +Int Amount by B'
         ==Int B' +Int (Sends(Ops) -Int Amount) +Int Amount by Sends
         ==Int B' +Int Sends(Ops) by simp
         ==Int B' +Int Sends(Ops' ;; Ops) by Ops'
- T' ==Int T
     <=Int D +Int Transfers(Op ;; Ops) by premise
     ==Int D' +Int Transfers(Op ;; Ops) by D'
     ==Int D' +Int Transfers(Ops) by Transfers
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops'
- L' ==Int L
     ==Int S +Int MintBurns(Op ;; Ops) by premise
     ==Int S' +Int MintBurns(Op ;; Ops) by S'
     ==Int S' +Int MintBurns(Ops) by MintBurns
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops'
```

#### Setters

```
claim [inv-setter]:
<operations>  ( [ Transaction Sender DEXTER Amount CD ] #as Op => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<xtzDexter>   #Mutez(B => B')   </xtzDexter>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
requires CD ==K (SetBaker _) orBool CD ==K (SetManager _) orBool CD ==K (SetLqtAddress _)
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-setter]:
- split CD
  - case CD == (SetBaker _)
    - apply [set-baker]
  - case CD == (SetManager _)
    - apply [set-manager]
  - case CD == (SetLqtAddress _)
    - apply [set-lqt-address]
- unify RHS
  - Ops' == [ SetDelegate _ ] | Ops' == .List
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
     ==Int B +Int Sends(Op ;; Ops) by premise
     ==Int B' +Int Sends(Op ;; Ops) by B'
     ==Int B' +Int Sends(Ops) by Sends
     ==Int B' +Int Sends(Ops' ;; Ops) by Ops'
- T' ==Int T
     <=Int D +Int Transfers(Op ;; Ops) by premise
     ==Int D' +Int Transfers(Op ;; Ops) by D'
     ==Int D' +Int Transfers(Ops) by Transfers
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops'
- L' ==Int L
     ==Int S +Int MintBurns(Op ;; Ops) by premise
     ==Int S' +Int MintBurns(Op ;; Ops) by S'
     ==Int S' +Int MintBurns(Ops) by MintBurns
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops'
```

### Proof for External Contract Calls

We prove the invariant for external contract calls.

Since the external contracts are unknown and arbitrary, we need to make certain assumptions on the behavior of external contracts that are required for the functional correctness and security of the Dexter contract.  The specific assumptions we made are presented later in this document.  The invariant proof is based on the assumptions, and it is important to independently verify that the token and liquidity contract implementations satisfy the assumptions.

There exist different types of external calls made by Dexter as follows:
- Simply send XTZ to others
- Call another Dexter contract's XtzToToken()
- Call the token contract's Transfer() or BalanceOf()
- Call the liquidity contract's Mint() or Burn()

#### XTZ Transfers

The following claim formulates the invariant for the first two types of external calls.

```
claim [inv-send]:
<operations>  ( [ Transaction DEXTER Target Amount CD ] #as Op => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<xtzDexter>   #Mutez(B => B')   </xtzDexter>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
requires Target =/=K DEXTER
 andBool ( CD ==K Default() orBool CD ==K (XtzToToken _) )
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-send]:
- apply [send]
- unify RHS
  - X' == X
  - T' == T
  - L' == L
  - B' == B -Int Amount
  - D' == D
  - S' == S
- X' >Int 0 by X >Int 0
- T' >Int 0 by T >Int 0
- L' >Int 0 by L >Int 0
- X' ==Int X
     ==Int B +Int Sends(Op ;; Ops) by premise
     ==Int (B' +Int Amount) +Int Sends(Op ;; Ops) by B'
     ==Int (B' +Int Amount) +Int (Sends(Ops) -Int Amount) by Sends
     ==Int B' +Int Sends(Ops) by simp
     ==Int B' +Int Sends(Ops' ;; Ops) by Ops' and [only-dexter]
- T' ==Int T
     <=Int D +Int Transfers(Op ;; Ops) by premise
     ==Int D' +Int Transfers(Op ;; Ops) by D'
     ==Int D' +Int Transfers(Ops) by Transfers
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops' and [only-dexter]
- L' ==Int L
     ==Int S +Int MintBurns(Op ;; Ops) by premise
     ==Int S' +Int MintBurns(Op ;; Ops) by S'
     ==Int S' +Int MintBurns(Ops) by MintBurns
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops' and [only-dexter]
```

#### Token Transfers and Balance Lookups

The following two claims are for the token contract calls, Transfer() and BalanceOf().

```
claim [inv-token-transfer]:
<operations>  ( [ Transaction _ TOKEN Amount Transfer(From, To, Value) ] #as Op => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<xtzDexter>   #Mutez(B => B')   </xtzDexter>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-token-transfer]:
- apply [token-transfer]
  - Amount ==Int 0 by assert
- unify RHS
  - X' == X
  - T' == T
  - L' == L
  - B' == B
  - S' == S
- X' >Int 0 by X >Int 0
- T' >Int 0 by T >Int 0
- L' >Int 0 by L >Int 0
- X' ==Int X
     ==Int B +Int Sends(Op ;; Ops) by premise
     ==Int B' +Int Sends(Op ;; Ops) by B'
     ==Int B' +Int Sends(Ops) by Sends and Amount ==Int 0
     ==Int B' +Int Sends(Ops' ;; Ops) by Ops' and [only-dexter]
- L' ==Int L
     ==Int S +Int MintBurns(Op ;; Ops) by premise
     ==Int S' +Int MintBurns(Op ;; Ops) by S'
     ==Int S' +Int MintBurns(Ops) by MintBurns
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops' and [only-dexter]
- split From, To
  - case From == DEXTER and To <> DEXTER
    - T' ==Int T
         <=Int D +Int Transfers(Op ;; Ops) by premise
         ==Int (D' +Int Value) +Int Transfers(Op ;; Ops) by D'
         ==Int (D' +Int Value) +Int (Transfers(Ops) -Int Value) by Transfers
         ==Int D' +Int Transfers(Ops) by simp
         ==Int D' +Int Transfers(Ops' ;; Ops) by Ops' and [only-dexter]
  - case From <> DEXTER and To == DEXTER
    - T' ==Int T
         <=Int D +Int Transfers(Op ;; Ops) by premise
         ==Int (D' -Int Value) +Int Transfers(Op ;; Ops) by D'
         ==Int (D' -Int Value) +Int (Transfers(Ops) +Int Value) by Transfers
         ==Int D' +Int Transfers(Ops) by simp
         ==Int D' +Int Transfers(Ops' ;; Ops) by Ops' and [only-dexter]
  - case From == DEXTER and To == DEXTER
    - T' ==Int T
         <=Int D +Int Transfers(Op ;; Ops) by premise
         ==Int D' +Int Transfers(Op ;; Ops) by D'
         ==Int D' +Int (Transfers(Ops) -Int Value) by Transfers
         <=Int D' +Int Transfers(Ops) by simp
         ==Int D' +Int Transfers(Ops' ;; Ops) by Ops' and [only-dexter]
  - case From <> DEXTER and To <> DEXTER
    - T' ==Int T
         <=Int D +Int Transfers(Op ;; Ops) by premise
         ==Int D' +Int Transfers(Op ;; Ops) by D'
         ==Int D' +Int Transfers(Ops) by Transfers
         ==Int D' +Int Transfers(Ops' ;; Ops) by Ops' and [only-dexter]
```

```
claim [inv-token-balance-of]:
<operations>  ( [ Transaction DEXTER TOKEN Amount BalanceOf(DEXTER, UpdateTokenPoolInternal) ] #as Op => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<xtzDexter>   #Mutez(B => B')   </xtzDexter>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-token-balance-of]:
- apply [token-balance-of]
  - Amount ==Int 0 by assert
- unify RHS
  - Ops' == OpsPre ;; [ Transaction TOKEN DEXTER 0 UpdateTokenPoolInternal(D) ] ;; OpsPost
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
     ==Int B +Int Sends(Op ;; Ops) by premise
     ==Int B' +Int Sends(Op ;; Ops) by B'
     ==Int B' +Int Sends(Ops) by Sends and Amount ==Int 0
     ==Int B' +Int Sends(Ops' ;; Ops) by Ops' and [only-dexter]
- T' ==Int T
     <=Int D +Int Transfers(Op ;; Ops) by premise
     ==Int D' +Int Transfers(Op ;; Ops) by D'
     ==Int D' +Int Transfers(Ops) by Transfers
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops' and [only-dexter]
- L' ==Int L
     ==Int S +Int MintBurns(Op ;; Ops) by premise
     ==Int S' +Int MintBurns(Op ;; Ops) by S'
     ==Int S' +Int MintBurns(Ops) by MintBurns
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops' and [only-dexter]
```

#### Liquidity Mints and Burns

The following two claims are for the liquidity contract calls, Mint() and Burn().

```
claim [inv-lqt-mint]:
<operations>  ( [ Transaction Sender LQT Amount Mint(_, Value) ] #as Op => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<xtzDexter>   #Mutez(B => B')   </xtzDexter>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-lqt-mint]:
- apply [lqt-mint]
  - Sender ==K DEXTER by assert
  - Amount ==Int 0 by assert
- unify RHS
  - X' == X
  - T' == T
  - L' == L
  - B' == B
  - D' == D
  - S' == S +Int Value
- X' >Int 0 by X >Int 0
- T' >Int 0 by T >Int 0
- L' >Int 0 by L >Int 0
- X' ==Int X
     ==Int B +Int Sends(Op ;; Ops) by premise
     ==Int B' +Int Sends(Op ;; Ops) by B'
     ==Int B' +Int Sends(Ops) by Sends and Amount ==Int 0
     ==Int B' +Int Sends(Ops' ;; Ops) by Ops' and [only-dexter]
- T' ==Int T
     <=Int D +Int Transfers(Op ;; Ops) by premise
     ==Int D' +Int Transfers(Op ;; Ops) by D'
     ==Int D' +Int Transfers(Ops) by Transfers
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops' and [only-dexter]
- L' ==Int L
     ==Int S +Int MintBurns(Op ;; Ops) by premise
     ==Int (S' -Int Value) +Int MintBurns(Op ;; Ops) by S'
     ==Int (S' -Int Value) +Int (MintBurns(Ops) +Int Value) by MintBurns
     ==Int S' +Int MintBurns(Ops) by simp
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops' and [only-dexter]

claim [inv-lqt-burn]:
<operations>  ( [ Transaction Sender LQT Amount Burn(_, Value) ] #as Op => Ops' ) ;; Ops </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
<xtzDexter>   #Mutez(B => B')   </xtzDexter>
<tokenDexter>        D => D'    </tokenDexter>
<lqtSupply>          S => S'    </lqtSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-lqt-mint-burn]:
- apply [lqt-mint-burn]
  - Sender ==K DEXTER by assert
  - Amount ==Int 0 by assert
- unify RHS
  - X' == X
  - T' == T
  - L' == L
  - B' == B
  - D' == D
  - S' == S -Int Value
- X' >Int 0 by X >Int 0
- T' >Int 0 by T >Int 0
- L' >Int 0 by L >Int 0
- X' ==Int X
     ==Int B +Int Sends(Op ;; Ops) by premise
     ==Int B' +Int Sends(Op ;; Ops) by B'
     ==Int B' +Int Sends(Ops) by Sends and Amount ==Int 0
     ==Int B' +Int Sends(Ops' ;; Ops) by Ops' and [only-dexter]
- T' ==Int T
     <=Int D +Int Transfers(Op ;; Ops) by premise
     ==Int D' +Int Transfers(Op ;; Ops) by D'
     ==Int D' +Int Transfers(Ops) by Transfers
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops' and [only-dexter]
- L' ==Int L
     ==Int S +Int MintBurns(Op ;; Ops) by premise
     ==Int (S' +Int Value) +Int MintBurns(Op ;; Ops) by S'
     ==Int (S' +Int Value) +Int (MintBurns(Ops) -Int Value) by MintBurns
     ==Int S' +Int MintBurns(Ops) by simp
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops' and [only-dexter]
```

### Assumptions for Tezos Execution Environment

We assume that the Tezos execution model does not allow any nonstandard behaviors regarding the Dexter smart contract execution.  Specifically, we make the following assumptions:
- Only the Dexter entrypoint functions can emit a transaction whose sender is Dexter.
- As a smart contract, Dexter can emit only _internal_ transactions (i.e., transactions whose source is not Dexter).
- The types of Dexter-emitted internal transactions are fixed (i.e., no arbitrary transactions can be emitted by Dexter).
- Dexter _never_ calls on itself except the following cases:
  - RemoveLiquidity(To, ...) where To is Dexter, which calls Default() on itself.
  - TokenToXtz(To, ...) where To is Dexter, which calls Default() on itself.
  - TokenToToken(OutputDexterContract, ...) where OutputDexterContract is Dexter, which calls XtzToToken(...) on itself.

The above assumptions are formulated in the following propositions below.

The following proposition `[sender-is-not-dexter]` states that no entrypoint of Dexter may call on itself except Default and XtzToToken.

```
proposition [sender-is-not-dexter]:
[[ Sender =/=K DEXTER => true ]]
<operations> [ Transaction Sender DEXTER _ CD ] ;; _ </operations>
requires CD ==K AddLiquidity _
  orBool CD ==K RemoveLiquidity _
  orBool CD ==K TokenToXtz _
  orBool CD ==K TokenToToken _
  orBool CD ==K UpdateTokenPool _
  orBool CD ==K UpdateTokenPoolInternal _
  orBool CD ==K SetBaker _
  orBool CD ==K SetManager _
  orBool CD ==K SetLqtAddress _
```

The following proposition `[dexter-emitted-ops]` enumerates all possible Dexter-emitted operations.

```
proposition [dexter-emitted-ops]:
[[ (
          ( Target ==K TOKEN andBool Amount ==Int 0 andBool CD ==K (Transfer _) )
   orBool ( Target ==K TOKEN andBool Amount ==Int 0 andBool CD ==K BalanceOf(DEXTER, UpdateTokenPoolInternal) )
   orBool ( Target ==K LQT   andBool Amount ==Int 0 andBool CD ==K (MintBurn _) )
   orBool (                                                 CD ==K Default() )
   orBool (                                                 CD ==K (XtzToToken _) )
   ) => true ]]
<operations> [ Transaction DEXTER Target Amount CD ] ;; _ </operations>
```

The following proposition `[only-dexter]` states that no one other than Dexter can emit operations whose sender is Dexter.  Note that Sends, Transfers, and MintBurns are defined to be zero for operations whose sender is not Dexter.

```
proposition [only-dexter]:
<operations> ( [ Transaction _ Target _ _ ] => Ops ) ;; _ </operations>
requires Target =/=K DEXTER
ensures  Sends(Ops) ==Int 0
 andBool Transfers(Ops) ==Int 0
 andBool MintBurns(Ops) ==Int 0
```

The following proposition `[top-level]` states that a top-level transaction (i.e., a transaction whose sender is equal to the source) cannot be generated by Dexter.

```
proposition [top-level]:
[[ Sender ==K Source impliesBool Sender =/=K DEXTER => true ]]
<operations> [ Transaction Sender _ _ _ ] ;; Ops </operations>
<sourceaddr> Source </sourceaddr>
```

The DFS execution model guarantees that if the current operation is top-level, then all the continuation operations are top-level with the same sender which is the source.  The following proposition `[dfs]` formulates that.

```
proposition [dfs]:
[[ Sender ==K Source impliesBool TopLevelOps(Ops, Source) => true ]]
<operations> [ Transaction Sender _ _ _ ] ;; Ops </operations>
<sourceaddr> Source </sourceaddr>

syntax Bool ::= TopLevelOps(OpList, Address) [function]
rule TopLevelOps([ Transaction Sender _ _ _ ] ;; Ops, Source) => TopLevelOps(Ops, Source) ]] requires Sender ==K Source
rule TopLevelOps([ Transaction Sender _ _ _ ] ;; _, Source) => false ]] requires Sender =/=K Source
rule TopLevelOps(.List, _) => true
```

### Assumptions for External Contracts

We make assumptions on the behaviors of external contracts, especially the token and liquidity contracts.  _*These assumptions are required for the proof of the invariant, and thus it is important to verify that these are satisfied by the given implementation of the token and liquidity contracts.*_  If some of these assumptions are not satisfied for good reasons, then the proof needs to be revisited.

We assume that _only_ Dexter can spend its own token, and no others can.  Specifically, for example, there must _not_ exist any authorized users who are permitted to spend (some of) Dexter-owned tokens (in any certain cases).  For another example, there must _not_ exist a way to (even temporarily) borrow tokens from Dexter.

We also assume that the token transfer operation must update the balance before emitting continuation operations.  For example, the token contract must _not_ implement the “pull pattern” where the transfer operation does not immediately update the balance but only allows the receiver to claim the transferred amount later as a separate transaction.  Note that such a delayed update of balance may lead to an exploit.  For example, a malicious user calls XtzToToken() and then calls UpdateTokenPool() before claiming the bought tokens.  Later he claims the tokens, which makes TokenPool to be larger than the actual token reserve, and distorts the token exchange price.  (The delayed balance update may conform to the FA2 standard, violating the atomicity requirement, but it is not clear whether it conforms to the FA1.2 or not.)

These assumptions are formulated in the following proposition `[token-transfer]`.

```
rule [token-transfer]:
<operations> ( [ Transaction Sender TOKEN Amount Transfer(From, To, Value) ] => Ops' ) ;; Ops </operations>
<tokenDexter> D => D' </tokenDexter>
assert   Amount ==Int 0
 andBool From ==K DEXTER impliesBool Sender ==K DEXTER
ensures  ( From  ==K DEXTER andBool To =/=K DEXTER ) impliesBool D' ==Int D -Int Value
 andBool ( From =/=K DEXTER andBool To  ==K DEXTER ) impliesBool D' ==Int D +Int Value
 andBool ( From  ==K DEXTER andBool To  ==K DEXTER ) impliesBool D' ==Int D
 andBool ( From =/=K DEXTER andBool To =/=K DEXTER ) impliesBool D' ==Int D
```

Moreover, we assume that the only way to alter the token balance of Dexter is the Transfer() function call.  No other functions can affect the token balance of Dexter.  The following proposition formulates that.

```
proposition [token-security]:
<operations> (Op => _) ;; _ </operations>
<tokenDexter> D => D' </tokenDexter>
ensures  D' <Int D impliesBool ( Op ==K Transaction DEXTER TOKEN 0 Transfer(DEXTER, To, Value) andBool To =/=K DEXTER andBool Value >Int 0 )
 andBool D' >Int D impliesBool Op ==K Transaction _ TOKEN 0 Transfer(_, DEXTER, _)
```



----

```
rule [token-balance-of]:
<operations> ( [ Transaction DEXTER TOKEN Amount BalanceOf(DEXTER, UpdateTokenPoolInternal) ] => Ops' ) ;; Ops </operations>
<tokenDexter> D </tokenDexter>
assert   Amount ==Int 0
ensures  Ops' ==K OpsPre ;; [ Transaction TOKEN DEXTER 0 UpdateTokenPoolInternal(D) ] ;; OpsPost
```

NOTE:
- even if Target is either TOKEN or LQT, both the token balance of dexter and the lqt supply must not change

```
rule [send]:
<operations> ( [ Transaction DEXTER Target Amount CD ] => Ops' ) ;; _ </operations>
<xtzDexter> #Mutez(B => B -Int Amount) </xtzDexter>
requires Target =/=K DEXTER
 andBool ( CD ==K Default() orBool CD ==K (XtzToToken _) )
```

NOTE:
- only dexter can mint or burn

```
rule [lqt-mint]:
<operations> ( [ Transaction Sender LQT Amount Mint(_, Value) ] => Ops' ) ;; _ </operations>
<lqtSupply> S => S +Int Value </lqtSupply>
assert   Sender ==K DEXTER
 andBool Amount ==Int 0

rule [lqt-burn]:
<operations> ( [ Transaction Sender LQT Amount Burn(_, Value) ] => Ops' ) ;; _ </operations>
<lqtSupply> S => S -Int Value </lqtSupply>
assert   Sender ==K DEXTER
 andBool Amount ==Int 0
```

### Abstract Behaviors of Dexter Entrypoints


#### Constants and Macros

```
syntax Address ::= DEXTER [constant]

syntax Address ::= TOKEN [constant]

syntax Address ::= LQT [macro]
rule [[ LQT => Lqt ]] <lqtAddress> Lqt </lqtAddress>
```

```
syntax Bool ::= IS_VALID(Int) [macro]
rule [is-valid]:
[[ IS_VALID(Deadline) => IsUpdatingTokenPool ==K false andBool Now <Int Deadline ]]
<selfIsUpdatingTokenPool> IsUpdatingTokenPool </selfIsUpdatingTokenPool>
<mynow> #Timestamp(Now) </mynow>
```

#### AddLiquidity(Owner, MinLqtMinted, MaxTokensDeposited, Deadline)

```
rule [add-liquidity]:
<operations>  ( [ Transaction Sender DEXTER XtzDeposited AddLiquidity(Owner, MinLqtMinted, MaxTokensDeposited, Deadline) ] => OpsEmitted ) ;; _ </operations>
<xtzPool>     #Mutez(X => X +Int XtzDeposited)      </xtzPool>
<tokenPool>          T => T +Int TokensDeposited    </tokenPool>
<lqtTotal>           L => L +Int LqtMinted          </lqtTotal>
<xtzDexter>   #Mutez(B => B +Int XtzDeposited)      </xtzDexter>
<tokenDexter>        D                              </tokenDexter>
<lqtSupply>          S                              </lqtSupply>
requires Sender =/=K DEXTER // TODO: double-check if it is guaranteed
assert   IS_VALID(Deadline)
 andBool TokensDeposited <=Int MaxTokensDeposited
 andBool LqtMinted       >=Int MinLqtMinted
ensures  TokensDeposited ==Int XtzDeposited *Int T up/Int X
 andBool LqtMinted       ==Int XtzDeposited *Int L   /Int X
 andBool OpsEmitted ==K [ Transaction DEXTER TOKEN 0 Transfer(Sender, DEXTER, TokensDeposited) ]
                     ;; [ Transaction DEXTER LQT   0 Mint(Owner, LqtMinted) ]
```

#### RemoveLiquidity(To, LqtBurned, MinXtzWithdrawn, MinTokensWithdrawn, Deadline)

NOTE:
- It needs to check `l < L`, otherwise the contract becomes nonfunctional.
- Given `l < L`, we have `x < X` and `t < T`, thus `X' > 0` and `T' > 0`.

```
rule [remove-liquidity]:
<operations>  ( [ Transaction Sender DEXTER Amount RemoveLiquidity(To, LqtBurned, MinXtzWithdrawn, MinTokensWithdrawn, Deadline) ] => OpsEmitted ) ;; _ </operations>
<xtzPool>     #Mutez(X => X -Int XtzWithdrawn)      </xtzPool>
<tokenPool>          T => T -Int TokensWithdrawn    </tokenPool>
<lqtTotal>           L => L -Int LqtBurned          </lqtTotal>
<xtzDexter>   #Mutez(B)                             </xtzDexter>
<tokenDexter>        D                              </tokenDexter>
<lqtSupply>          S                              </lqtSupply>
assert   IS_VALID(Deadline)
 andBool Amount ==Int 0
 andBool LqtBurned <Int L // TODO: ask to fix
 andBool XtzWithdrawn    >=Int MinXtzWithdrawn
 andBool TokensWithdrawn >=Int MinTokensWithdrawn
ensures  XtzWithdrawn    ==Int LqtBurned *Int X /Int L
 andBool TokensWithdrawn ==Int LqtBurned *Int T /Int L
 andBool OpsEmitted ==K [ Transaction DEXTER LQT   0            Burn(Sender, LqtBurned) ]
                     ;; [ Transaction DEXTER TOKEN 0            Transfer(DEXTER, To, TokensWithdrawn) ]
                     ;; [ Transaction DEXTER To    XtzWithdrawn Default() ]
```

#### XtzToToken(To, MinTokensBought, Deadline)

NOTE:
- `TokensBought < T` if `T > 0`

```
rule [xtz-to-token]:
<operations>  ( [ Transaction Sender DEXTER XtzSold XtzToToken(To, MinTokensBought, Deadline) ] => OpsEmitted ) ;; _ </operations>
<xtzPool>     #Mutez(X => X +Int XtzSold)       </xtzPool>
<tokenPool>          T => T -Int TokensBought   </tokenPool>
<lqtTotal>           L                          </lqtTotal>
<xtzDexter>   #Mutez(B => B')                   </xtzDexter>
<tokenDexter>        D                          </tokenDexter>
<lqtSupply>          S                          </lqtSupply>
assert   IS_VALID(Deadline)
 andBool TokensBought >=Int MinTokensBought
ensures  TokensBought ==Int 997 *Int XtzSold *Int T /Int (1000 *Int X +Int 997 *Int XtzSold)
 andBool OpsEmitted ==K [ Transaction DEXTER TOKEN 0 Transfer(DEXTER, To, TokensBought) ]
 andBool Sender =/=K DEXTER impliesBool B' ==Int B +Int XtzSold
 andBool Sender  ==K DEXTER impliesBool B' ==Int B
```

#### TokenToXtz(To, TokensSold, MinXtzBought, Deadline)

NOTE:
- `XtzBought <Int X` if `X > 0`

```
rule [token-to-xtz]:
<operations>  ( [ Transaction Sender DEXTER Amount TokenToXtz(To, TokensSold, MinXtzBought, Deadline) ] => OpsEmitted ) ;; _ </operations>
<xtzPool>     #Mutez(X => X -Int XtzBought)     </xtzPool>
<tokenPool>          T => T +Int TokensSold     </tokenPool>
<lqtTotal>           L                          </lqtTotal>
<xtzDexter>   #Mutez(B)                         </xtzDexter>
<tokenDexter>        D                          </tokenDexter>
<lqtSupply>          S                          </lqtSupply>
assert   IS_VALID(Deadline)
 andBool Amount ==Int 0
 andBool XtzBought >=Int MinXtzBought
ensures  XtzBought ==Int 997 *Int TokensSold *Int X /Int (1000 *Int T +Int 997 *Int TokensSold)
 andBool OpsEmitted ==K [ Transaction DEXTER TOKEN 0         Transfer(Sender, DEXTER, TokensSold) ]
                     ;; [ Transaction DEXTER To    XtzBought Default() ]
```

#### TokenToToken(OutputDexterContract, MinTokensBought, To, TokensSold, Deadline)

NOTE:
- The following two should be equivalent:
  - `[ Transaction DEXTER 0 TokenToToken(OutputDexterContract, MinTokensBought, To, TokensSold, Deadline) ]`
  - `[ Transaction DEXTER 0 TokenToXtz(Sender, TokensSold, 0, Deadline) ] ;; [ Transaction OutputDexterContract XtzBought XtzToToken(To, MinTokensBought, Deadline) ]`

```
rule [token-to-token]:
<operations>  ( [ Transaction Sender DEXTER Amount TokenToToken(OutputDexterContract, MinTokensBought, To, TokensSold, Deadline) ] => OpsEmitted ) ;; _ </operations>
<xtzPool>     #Mutez(X => X -Int XtzBought)     </xtzPool>
<tokenPool>          T => T +Int TokensSold     </tokenPool>
<lqtTotal>           L                          </lqtTotal>
<xtzDexter>   #Mutez(B)                         </xtzDexter>
<tokenDexter>        D                          </tokenDexter>
<lqtSupply>          S                          </lqtSupply>
assert   IS_VALID(Deadline)
 andBool Amount ==Int 0
ensures  XtzBought ==Int 997 *Int TokensSold *Int X /Int (1000 *Int T +Int 997 *Int TokensSold)
 andBool OpsEmitted ==K [ Transaction DEXTER TOKEN                0         Transfer(Sender, DEXTER, TokensSold) ]
                     ;; [ Transaction DEXTER OutputDexterContract XtzBought XtzToToken(To, MinTokensBought, Deadline) ]
```

#### UpdateTokenPool()

```
rule [update-token-pool]:
<operations>  ( [ Transaction Sender DEXTER Amount UpdateTokenPool() ] => OpsEmitted ) ;; _ </operations>
<xtzPool>     #Mutez(X) </xtzPool>
<tokenPool>          T  </tokenPool>
<lqtTotal>           L  </lqtTotal>
<xtzDexter>   #Mutez(B) </xtzDexter>
<tokenDexter>        D  </tokenDexter>
<lqtSupply>          S  </lqtSupply>
<sourceaddr>  Source    </sourceaddr>
<selfIsUpdatingTokenPool> IsUpdatingTokenPool => true </selfIsUpdatingTokenPool>
assert   IsUpdatingTokenPool ==K false
 andBool Amount ==Int 0
 andBool Sender ==K Source
ensures  OpsEmitted ==K [ Transaction DEXTER TOKEN 0 BalanceOf(DEXTER, UpdateTokenPoolInternal) ]

rule [update-token-pool-internal]:
<operations>  ( [ Transaction Sender DEXTER Amount UpdateTokenPoolInternal(TokenPool) ] => .List ) ;; _ </operations>
<xtzPool>     #Mutez(X)             </xtzPool>
<tokenPool>          T => TokenPool </tokenPool>
<lqtTotal>           L              </lqtTotal>
<xtzDexter>   #Mutez(B)             </xtzDexter>
<tokenDexter>        D              </tokenDexter>
<lqtSupply>          S              </lqtSupply>
<selfIsUpdatingTokenPool> IsUpdatingTokenPool => false </selfIsUpdatingTokenPool>
assert   IsUpdatingTokenPool ==K true
 andBool Amount ==Int 0
 andBool Sender ==K TOKEN
```

#### Default()

```
rule [default]:
<operations>  ( [ Transaction Sender DEXTER Amount Default() ] => .List ) ;; _ </operations>
<xtzPool>     #Mutez(X => X +Int Amount)    </xtzPool>
<tokenPool>          T                      </tokenPool>
<lqtTotal>           L                      </lqtTotal>
<xtzDexter>   #Mutez(B => B')               </xtzDexter>
<tokenDexter>        D                      </tokenDexter>
<lqtSupply>          S                      </lqtSupply>
<selfIsUpdatingTokenPool> IsUpdatingTokenPool </selfIsUpdatingTokenPool>
assert   IsUpdatingTokenPool ==K false
ensures  Sender =/=K DEXTER impliesBool B' ==Int B +Int Amount
 andBool Sender  ==K DEXTER impliesBool B' ==Int B
```

#### Setters

```
rule [set-baker]:
<operations>  ( [ Transaction Sender DEXTER Amount SetBaker(Baker, FreezeBaker) ] => [ SetDelegate Baker ] ) ;; _ </operations>
<selfIsUpdatingTokenPool>   IsUpdatingTokenPool             </selfIsUpdatingTokenPool>
<freezeBaker>               IsBakerFrozen => FreezeBaker    </freezeBaker>
<manager>                   Manager                         </manager>
assert   IsUpdatingTokenPool ==K false
 andBool Amount ==Int 0
 andBool Sender ==K Manager
 andBool IsBakerFrozen ==K false
```

```
rule [set-manager]:
<operations>  ( [ Transaction Sender DEXTER Amount SetManager(NewManager) ] => .List ) ;; _ </operations>
<selfIsUpdatingTokenPool>   IsUpdatingTokenPool     </selfIsUpdatingTokenPool>
<manager>                   Manager => NewManager   </manager>
assert   IsUpdatingTokenPool ==K false
 andBool Amount ==Int 0
 andBool Sender ==K Manager
```

```
rule [set-lqt-address]:
<operations>  ( [ Transaction Sender DEXTER Amount SetLqtAddress(LqtAddress) ] => .List ) ;; _ </operations>
<selfIsUpdatingTokenPool>   IsUpdatingTokenPool             </selfIsUpdatingTokenPool>
<manager>                   Manager                         </manager>
<lqtAddress>                InitialLqtAddress => LqtAddress </lqtAddress>
assert   IsUpdatingTokenPool ==K false
 andBool Amount ==Int 0
 andBool Sender ==K Manager
 andBool InitialLqtAddress ==K 0
```





----

Note that the real arithmetic is used throughout the document.

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
