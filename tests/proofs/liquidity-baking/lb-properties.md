# Liquidity Baking Safety Property Proofs

This document is all about proving two safety properties for the liquidity baking (LB) system which includes three separate smart contracts that we refer to as *Dexter*, *LQT*, and *tzBTC*.
We assume that the reader is familiar with Uniswap-style CPMMs and the basic LB system structure.
See the [README](./README.md) for an overview of relevant background material, including a description of the safety properties we wish to prove.
For convenience, we restate our desired safety properties here:

-   *Liquidity share value security* - LP shares never decrease in *redemption value* (note this is *different* from *monetary value*, see below)
-   *Operation safety* - all trades and liquidity redemptions/deposits have a bounded exchange rate and time in which they are applicable.

Our proof occurs in several parts.

*(a)* The first part is a mechanized proof that high-level K specifications are consistent with the low-level Michelson code of the *Dexter* and *LQT* smart contracts.
      The proof scripts are contained in the [lb.md](./lb.md) and [lb-spec.md](./lb-spec) and [lqt.md](../lqt/lqt.md) and [lqt-spec.md](../lqt/lqt-spec.md) files.
      Note that do not provide similar mechanized proofs for the *tzBTC* contract since it is an *upgradeable* contract whose logic may vary over time.
      Instead, we axiomatize how we expect this contract to behave in our assumptions section below, such that our proofs will hold if the *tzBTC* satisfies the required assumptions.

*(b)* The second part contains by-hand proofs for safety properties (1)-(2) based on the results from part *(a)*.
      Before diving into the details, see the proof sketch below.

## Proof Sketch

The proof of the *operation safety* property is more straightforward.
It follows from the K specification definitions and the assumptions about external smart contracts listed below.
On the other hand, the proof of the *liquidity share value security* property is non-trivial.
This proof occurs in two steps:

-   we prove a *state variable faithfulness* invariant, i.e., *Dexter* contract variables are consistent with particular balances/storage values in the *Dexter*, *LQT*, and *tzBTC* contracts;
-   we use this lemma to prove the *liquidity share value security* property

There are many other lemmas used along the way that we will mention as needed.
Before diving into our proof, we start with the needed notation and assumptions.

## Notation

To simplify our description of entrypoint behavior, we write entrypoint names like functions which take arguments.
Given that each contract has a fixed number of important entrypoints, we list them all here for reference:

*LB* Entrypoints:

-   `AddLiquidity(owner : Address, minLqtMinted : Nat, maxTokensDeposited : Nat, deadline : Timestamp)`
-   `RemoveLiquidity(to : Address, lqtBurned : Nat, minXtzWithdrawn : Mutez, minTokensWithdrawn : Nat, deadline : Timestamp)`
-   `Default`
-   `XtzToToken(to : Address, minTokensBought : Nat, deadline : Timestamp)`
-   `TokenToXtz(to : Address, tokensSold : Nat, minXtzBought : Mutez, deadline : Timestamp)`
-   `TokenToToken(outputDexterContract : Address, minTokensBought : Nat, to : Address, tokensSold : Nat, deadline : Timestamp)`

*LQT* Entrypoints (includes the FA1.2 entrypoints as well as `MintOrBurn`):

-   `GetTotalSupply(callback: Entrypoint)`
-   `GetBalance(owner: Address, callback: Entrypoint)`
-   `GetAllowance(owner: Address, spender: Address, callback: Entrypoint)`
-   `Approve(spender: Address, value: Nat)`
-   `Transfer(from: Address, to: Address, value: Nat)`
-   `MintOrBurn(quantity: Int, target: Address)`

*tzBTC* Entrypoints (since this contract is FA1.2 compliant, it should include at least these entrypoints):

-   `GetTotalSupply(callback: Entrypoint)`
-   `GetBalance(owner: Address, callback: Entrypoint)`
-   `GetAllowance(owner: Address, spender: Address, callback: Entrypoint)`
-   `Approve(spender: Address, value: Nat)`
-   `Transfer(from: Address, to: Address, value: Nat)`

## Assumptions for Tezos Execution Environment

We assume that the Tezos execution model does not allow any weird behaviors regarding the Dexter smart contract execution.  Specifically, we make the following assumptions:

- Only the Dexter entrypoint functions can emit operations whose sender is Dexter.
- As a smart contract, Dexter can emit only _internal_ operations (i.e., operations whose source is not Dexter).
- The types of Dexter-emitted internal operations are fixed (i.e., no arbitrary operations can be emitted by Dexter).
- Dexter _never_ calls on itself except the following cases:
  - RemoveLiquidity(To, ...) where To is Dexter, which calls Default() on itself.
  - TokenToXtz(To, ...) where To is Dexter, which calls Default() on itself.
  - TokenToToken(OutputDexterContract, ...) where OutputDexterContract is Dexter, which calls XtzToToken() on itself.

The above assumptions are formulated in a series of propositions below.

The following proposition `[sender-is-not-dexter]` states that no entrypoint of Dexter may call on itself except Default() and XtzToToken().

```
proposition [sender-is-not-dexter]:
[[ Sender =/=K DEXTER => true ]]
<operations> [ Transaction Sender DEXTER _ CallParams ] ;; _ </operations>
requires CallParams ==K AddLiquidity _
  orBool CallParams ==K RemoveLiquidity _
  orBool CallParams ==K TokenToXtz _
  orBool CallParams ==K TokenToToken _
```

The following proposition `[dexter-emitted-ops]` enumerates all possible Dexter-emitted operations.

```
proposition [dexter-emitted-ops]:
[[ (
          ( Target ==K TOKEN andBool Amount ==Int 0 andBool CallParams ==K (Transfer _) )
   orBool ( Target ==K LQT   andBool Amount ==Int 0 andBool CallParams ==K (MintBurn _) )
   orBool (                                                 CallParams ==K Default() )
   orBool (                                                 CallParams ==K (XtzToToken _) )
   ) => true ]]
<operations> [ Transaction DEXTER Target Amount CallParams ] ;; _ </operations>
```

The following proposition `[only-dexter]` states that no one other than Dexter can emit operations whose sender is Dexter.  Note that Sends, Transfers, and MintBurns are defined to be zero for operations whose sender is not Dexter.  Also, obviously, the state variables and XTZ balance of Dexter cannot be updated without executing the Dexter contract.

```
proposition [only-dexter]:
<operations> ( [ Transaction _ Target _ _ ] => Ops ) ;; _ </operations>
<xtzPool>         #Mutez(X => X')   </xtzPool>
<tokenPool>              T => T'    </tokenPool>
<lqtTotal>               L => L'    </lqtTotal>
<dexter.balance>  #Mutez(B => B')   </dexter.balance>
requires Target =/=K DEXTER
ensures  Sends(Ops) ==Int 0
 andBool Transfers(Ops) ==Int 0
 andBool MintBurns(Ops) ==Int 0
 andBool X' ==Int X
 andBool T' ==Int T
 andBool L' ==Int L
 andBool B' ==Int B
```

The following proposition `[top-level]` states that a top-level operation (i.e., an operation whose sender is equal to the source) cannot be generated by Dexter.

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
rule TopLevelOps([ Transaction Sender _ _ _ ] ;; Ops, Source) => TopLevelOps(Ops, Source)
  requires Sender ==K Source
rule TopLevelOps([ Transaction Sender _ _ _ ] ;; _, Source) => false
  requires Sender =/=K Source
rule TopLevelOps(.List, _) => true
```

## Requirements for External Contracts

We make assumptions on the behaviors of external contracts, especially the token and liquidity contracts.  _**These assumptions are required for the proof of the invariant, and thus it is important to verify that these are satisfied by the given implementation of the token and liquidity contracts.**_  If some of these assumptions are not satisfied for good reasons, then the proof needs to be revisited.

We assume that _only_ Dexter can spend its own token, and no others can.  Specifically, for example, there must _not_ exist any authorized users who are permitted to spend (some of) Dexter-owned tokens (in any certain cases).  For another example, there must _not_ exist a way to (even temporarily) borrow tokens from Dexter.

We also assume that the token transfer operation must update the balance before emitting continuation operations.  For example, the token contract must _not_ implement the so-called "pull pattern" where the transfer operation does not immediately update the balance but only allows the receiver to claim the transferred amount later as a separate transaction.

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
proposition [only-token-transfer]:
<operations> (Op => _) ;; _ </operations>
<tokenDexter> D => D' </tokenDexter>
ensures  D' <Int D impliesBool (         Op ==K Transaction DEXTER TOKEN 0 Transfer(DEXTER, To, Value)
                                 andBool To =/=K DEXTER
                                 andBool Value >Int 0
                               )
 andBool D' >Int D impliesBool Op ==K Transaction _ TOKEN 0 Transfer(_, DEXTER, _)
```

For the LQT Token contract, we require one safety property and another liveness property
that together state that the total number of liquidity shares may be updated if and only if
the LB Dexter contract calls the `%mintOrBurn` entrypoint. Both these are proved in [lqt-properties.md](../lqt/lqt-properties.md).

For the other unknown external contract calls, the only functions Dexter can call are Default() and XtzToToken().  We assume that such external calls can affect only the XTZ balance of Dexter (even if the target contract is the token or liquidity contract).  The following rule `[send]` formulates that.

```
rule [send]:
<operations> ( [ Transaction DEXTER Target Amount CallParams ] => Ops' ) ;; _ </operations>
<dexter.balance> #Mutez(B => B -Int Amount) </dexter.balance>
requires Target =/=K DEXTER
 andBool ( CallParams ==K Default() orBool CallParams ==K (XtzToToken _) )
```


## State Variable Faithfulness

In the Dexter contract, the token exchange rate and the liquidity share price are determined by the three state variables (XtzPool, TokenPool, LqtTotal) which keep track of the XTZ reserve, the token (tzBTC) reserve, and the total liquidity token supply, respectively.

The first invariant we consider is that the Dexter state variables faithfully represent the actual pool reserves and liquidity supply.  That is, XtzPool and TokenPool must be equal to the actual XTZ and token reserves, and LqtTotal must be equal to the actual total liquidity supply. We show this holds inductively, i.e. we assume that the invariant is satisfied, and then show that no sequence of transactions can cause it to be violated.

Note that the Dexter entrypoint functions immediately update these state variables, while the actual reserves or supply will be updated later by the continuation operations emitted by the entrypoints.  Moreover, the actual token reserve may be possibly larger than TokenPool, since one can "donate" tokens to Dexter (i.e., directly sending tokens to Dexter without going through any of the Dexter entrypoint functions).  Note that, however, the actual XTZ reserve must be equal to the XtzPool value, since directly sending XTZ to Dexter will be captured by the Default() entrypoint.  (Indeed, we assume that, in Tezos, there is no way to "secretly" send XTZ to Dexter without triggering any Dexter entrypoint.  Note that, in Ethereum, it is _possible_ to send Ether to a smart contract without ever executing the contract code, either by making the contract the recipient of mining rewards or selfdestruct rewards.)

The following claim `[inv-top-level]` states that the invariant holds at the completion of every top-level operation.  Note that a top-level operation is the one created by an implicit account (i.e., an operation whose sender is equal to the source), and the completion of an operation involves the full execution "tree" following the DFS model adopted in the Florence upgrade.

The `<xtzPool>`, `<tokenPool>`, and `<lqtTotal>` cells denote the Dexter state variables, XtzPool, TokenPool, and LqtTotal, respectively.  The `<dexter.balance>`, `<tokenDexter>`, and `<lqt.totalSupply>` cells denote the actual XTZ and token reserves, and total liquidity supply, respectively.

```
claim [inv-top-level]:
<operations> ( [ Transaction Sender _ _ _ ] #as Op =>* .List ) ;; Ops </operations>
<xtzPool>           #Mutez(X => X')   </xtzPool>
<tokenPool>                T => T'    </tokenPool>
<lqtTotal>                 L => L'    </lqtTotal>
<dexter.balance>    #Mutez(B => B')   </dexter.balance>
<tokenDexter>              D => D'    </tokenDexter>
<lqt.totalSupply>          S => S'    </lqt.totalSupply>
<sourceaddr>        Source            </sourceaddr>
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
- by Sender =/=K DEXTER and TopLevelOps(Ops, Source) derive:
          Sends(Op ;; Ops)     ==Int 0
  andBool Transfers(Op ;; Ops) ==Int 0
  andBool MintBurns(Op ;; Ops) ==Int 0
- apply [inv-trans]
- unify RHS
  - Ops' == .List
- Sends(Ops) ==Int 0 andBool Transfers(Ops) ==Int 0 andBool MintBurns(Ops) ==Int 0 by Sender =/=K DEXTER
- Sends(Ops' ;; Ops) ==Int 0 andBool Transfers(Ops' ;; Ops) ==Int 0 andBool MintBurns(Ops' ;; Ops) ==Int 0 by Ops'
- qed
```

```
claim [inv-trans]:
<operations>      (Op =>* Ops') ;; Ops </operations>
<xtzPool>          #Mutez(X => X')     </xtzPool>
<tokenPool>               T => T'      </tokenPool>
<lqtTotal>                L => L'      </lqtTotal>
<dexter.balance>   #Mutez(B => B')     </dexter.balance>
<tokenDexter>             D => D'      </tokenDexter>
<lqt.totalSupply>         S => S'      </lqt.totalSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-trans]:
- by induction on =>* and [inv]
```

The following claim `[inv]` generalizes the top-level claim `[inv-top-level]` over every intermediate operation.  It says that the execution of an _arbitrary_ operation always preserves a certain relationship between the Dexter state variables and the actual pool reserves and liquidity supply.  Intuitively, the Dexter state variables must reflect the "ultimate" value of the pool reserves and liquidity supply over the course of intermediate steps of execution, which will be eventualy updated by the continuation operations in the future.

```
claim [inv]:
<operations>     (Op => Ops') ;; Ops </operations>
<xtzPool>          #Mutez(X => X')   </xtzPool>
<tokenPool>               T => T'    </tokenPool>
<lqtTotal>                L => L'    </lqtTotal>
<dexter.balance>   #Mutez(B => B')   </dexter.balance>
<tokenDexter>             D => D'    </tokenDexter>
<lqt.totalSupply>         S => S'    </lqt.totalSupply>
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
rule Transfers([ Transaction DEXTER TOKEN 0 Transfer(From, To, T) ] ;; Ops) => Transfers(Ops) +Int T
  when From =/=K DEXTER andBool To ==K DEXTER
rule Transfers([ Transaction DEXTER TOKEN 0 Transfer(From, To, T) ] ;; Ops) => Transfers(Ops) -Int T
  when From  ==K DEXTER
rule Transfers(_ ;; Ops) => Transfers(Ops) [owise]
rule Transfers(.List) => 0

syntax Int ::= MintBurns(OpList) [function]
rule MintBurns([ Transaction DEXTER LQT 0 MintOrBurn(_, L) ] ;; Ops) => MintBurns(Ops) +Int L
rule MintBurns(_ ;; Ops) => MintBurns(Ops) [owise]
rule MintBurns(.List) => 0
```

Below we prove the claim `[inv]` by the induction on sequences of operations, and the case analysis over different types of operations.

```
proof [inv]:
- assume that the invariant has held in each of the previous operations
- let Op = Transaction Sender Target Amount CallParams
- split Sender, Target
  - case Sender <> DEXTER and Target == DEXTER
    - split CallParams
      - case CallParams == AddLiquidity _
        - apply [inv-add-liquidity]
      - case CallParams == RemoveLiquidity _
        - apply [inv-remove-liquidity]
      - case CallParams == XtzToToken _
        - apply [inv-xtz-to-token]
      - case CallParams == TokenToXtz _
        - apply [inv-token-to-xtz]
      - case CallParams == TokenToToken _
        - apply [inv-token-to-token]
      - case CallParams == Default _
        - apply [inv-default]
  - case Sender == DEXTER and Target == DEXTER
    - split CallParams
      - case CallParams == XtzToToken _
        - apply [inv-xtz-to-token]
      - case CallParams == Default _
        - apply [inv-default]
      - case _
        - apply [sender-is-not-dexter]
  - case Sender == DEXTER and Target <> DEXTER
    - split Op
      - case Op == Transaction DEXTER TOKEN 0 (Transfer _)
        - apply [inv-token-transfer]
      - case Op == Transaction DEXTER LQT 0 (MintOrBurn _)
        - apply [inv-lqt-mint-burn]
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
        - (X', T', L', B') == (X, T, L, B) by [only-dexter]
        - S' == S by [only-lqt-mint-burn]
        - split CallParams
          - case CallParams == Transfer(From, To, Value)
            - From <> DEXTER by [token-transfer]'s assertion
            - D' >= D by [only-token-transfer]
          - case CallParams <> (Transfer _)
            - D' >= D by [only-token-transfer]
      - case Target == LQT
        - (X', T', L', B') == (X, T, L, B) by [only-dexter]
        - D' == D by [only-token-transfer]
        - CallParams <> (MintOrBurn _) by [lqt-mint-burn]
          - S' == S
      - case Target <> TOKEN and Target <> LQT
        - (X', T', L', B') == (X, T, L, B) by [only-dexter]
        - D' == D by [only-token-transfer]
        - S' == S by [only-lqt-mint-burn]
```

### Proof for Dexter Entrypoint Functions

We prove the claim `[inv]` for each Dexter entrypoint.

#### AddLiquidity

```
claim [inv-add-liquidity]:
<operations>   ( [ Transaction Sender DEXTER XtzDeposited AddLiquidity(Owner, _, _, _) ] #as Op => Ops' )
            ;; Ops
</operations>
<xtzPool>          #Mutez(X => X')   </xtzPool>
<tokenPool>               T => T'    </tokenPool>
<lqtTotal>                L => L'    </lqtTotal>
<dexter.balance>   #Mutez(B => B')   </dexter.balance>
<tokenDexter>             D => D'    </tokenDexter>
<lqt.totalSupply>         S => S'    </lqt.totalSupply>
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
         ;; ( [ Transaction DEXTER LQT 0 MintOrBurn(Owner, LqtMinted) ] #as Op2 )
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
<xtzPool>        #Mutez(X => X')   </xtzPool>
<tokenPool>             T => T'    </tokenPool>
<lqtTotal>              L => L'    </lqtTotal>
<dexter.balance> #Mutez(B => B')   </dexter.balance>
<tokenDexter>           D => D'    </tokenDexter>
<lqt.totalSupply>       S => S'    </lqt.totalSupply>
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
  - Ops' == ( [ Transaction DEXTER LQT   0            MintOrBurn(Sender, LqtBurned) ] #as Op1 )
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
<xtzPool>        #Mutez(X => X')   </xtzPool>
<tokenPool>             T => T'    </tokenPool>
<lqtTotal>              L => L'    </lqtTotal>
<dexter.balance> #Mutez(B => B')   </dexter.balance>
<tokenDexter>           D => D'    </tokenDexter>
<lqt.totalSupply>       S => S'    </lqt.totalSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-xtz-to-token]:
- let XtzSoldNetBurn = XtzSold *Int 999 /Int 1000
      TokensBought   = 999 *Int XtzSoldNetBurn *Int T /Int (1000 *Int X +Int 999 *Int XtzSoldNetBurn)
- apply [xtz-to-token]
- unify RHS
  - Ops' == ( [ Transaction DEXTER TOKEN 0                           Transfer(DEXTER, To, TokensBought) ] #as Op1 )
         ;; ( [ Transaction DEXTER Null  XtzSold -Int XtzSoldNetBurn Default ] #as Op2 )
  - X' == X +Int XtzSoldNetBurn
  - T' == T -Int TokensBought
  - L' == L
  - B' == #if Sender <> Dexter #then B +Int XtzSold #else B #fi
  - D' == D
  - S' == S
- X' >Int 0 by X >Int 0 and XtzSoldNetBurn >=Int 0
- TokensBought <Int T by simp
- T' >Int 0 by TokensBought <Int T
- L' >Int 0 by L >Int 0
- split Sender
  - case Sender <> DEXTER
    - X' ==Int X +Int XtzSoldNetBurn
         ==Int B +Int Sends(Op ;; Ops) +Int XtzSoldNetBurn by premise
         ==Int B +Int Sends(Ops) +Int XtzSoldNetBurn by Sends
         ==Int B' -Int XtzSold +Int Sends(Ops) +Int XtzSoldNetBurn by B'
         ==Int B' +Int Sends(Op2 ;; Ops) by Sends
         ==Int B' +Int Sends(Op1 ;; Op2 ;; Ops) by Sends
         ==Int B' +Int Sends(Ops' ;; Ops) by Ops'
  - case Sender == DEXTER
    - X' ==Int X +Int XtzSoldNetBurn
         ==Int B +Int Sends(Op ;; Ops) +Int XtzSoldNetBurn by premise
         ==Int B +Int Sends(Ops) -Int XtzSold +Int XtzSoldNetBurn by Sends
         ==Int B' +Int Sends(Ops) -Int XtzSold +Int XtzSoldNetBurn by B'
         ==Int B' +Int Sends(Op2 ;; Ops) by Sends
         ==Int B' +Int Sends(Op1 ;; Op2 ;; Ops) by Sends
         ==Int B' +Int Sends(Ops' ;; Ops) by Ops'
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
<xtzPool>        #Mutez(X => X')   </xtzPool>
<tokenPool>             T => T'    </tokenPool>
<lqtTotal>              L => L'    </lqtTotal>
<dexter.balance> #Mutez(B => B')   </dexter.balance>
<tokenDexter>           D => D'    </tokenDexter>
<lqt.totalSupply>       S => S'    </lqt.totalSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-token-to-xtz]:
- let XtzBought        = 999 *Int TokensSold *Int X /Int (1000 *Int T +Int 999 *Int TokensSold)
      XtzBoughtNetBurn = XtzBought * 999 / 1000
- Sender =/=K DEXTER by [sender-is-not-dexter]
- apply [token-to-xtz]
  - Amount ==Int 0 by assert
- unify RHS
  - Ops' == ( [ Transaction DEXTER TOKEN 0                            Transfer(Sender, DEXTER, TokensSold) ] #as Op1 )
         ;; ( [ Transaction DEXTER To    XtzBoughtNetBurn             Default() ] #as Op2 )
         ;; ( [ Transaction DEXTER NULL  XtzBought - XtzBoughtNetBurn Default() ] #as Op3 )
  - X' == X -Int XtzBought
  - T' == T +Int TokensSold
  - L' == L
  - B' == B
  - D' == D
  - S' == S
- XtzBought <Int X by simp
- X' >Int 0 by XtzBought <Int X
- T' >Int 0 by T >Int 0 and TokensSold >=Int 0
- L' >Int 0 by L >Int 0
- X' ==Int X -Int XtzBought
     ==Int B +Int Sends(Op ;; Ops) -Int XtzBought by premise
     ==Int B' +Int Sends(Op ;; Ops) -Int XtzBought by B'
     ==Int B' +Int Sends(Ops) -Int XtzBought by Sends and Amount ==Int 0
     ==Int B' +Int Sends(Op3 ;; Ops) -Int XtzBoughtNetBurn  by Sends
     ==Int B' +Int Sends(Op2 ;; Op3 ;; Ops) by Sends
     ==Int B' +Int Sends(Op1 ;; Op2 ;; Op3 ;; Ops) by Sends
     ==Int B' +Int Sends(Ops' ;; Ops) by Ops'
- T' ==Int T +Int TokensSold
     <=Int D +Int Transfers(Op ;; Ops) +Int TokensSold by premise
     ==Int D' +Int Transfers(Op ;; Ops) +Int TokensSold by D'
     ==Int D' +Int Transfers(Ops) +Int TokensSold by Transfers
     ==Int D' +Int Transfers(Op3 ;; Ops) +Int TokensSold by Transfers
     ==Int D' +Int Transfers(Op2 ;; Op3 ;; Ops) +Int TokensSold by Transfers
     ==Int D' +Int Transfers(Op1 ;; Op2 ;; Op3 ;; Ops) by Transfers and Sender =/=K DEXTER
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops'
- L' ==Int L
     ==Int S +Int MintBurns(Op ;; Ops) by premise
     ==Int S' +Int MintBurns(Op ;; Ops) by S'
     ==Int S' +Int MintBurns(Ops) by MintBurns
     ==Int S' +Int MintBurns(Op3 ;; Ops) by MintBurns
     ==Int S' +Int MintBurns(Op2 ;; Op3 ;; Ops) by MintBurns
     ==Int S' +Int MintBurns(Op1 ;; Op2 ;; Op3 ;; Ops) by MintBurns
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops'
```

#### TokenToToken

```
claim [inv-token-to-token]:
<operations>
   ( [ Transaction Sender DEXTER Amount TokenToToken(OutputDexter, MinTokensBought, To, TokensSold, Deadline) ] #as Op ) ;; Ops
=>
   Ops' ;; Ops
</operations>
<xtzPool>        #Mutez(X => X')   </xtzPool>
<tokenPool>             T => T'    </tokenPool>
<lqtTotal>              L => L'    </lqtTotal>
<dexter.balance> #Mutez(B => B')   </dexter.balance>
<tokenDexter>           D => D'    </tokenDexter>
<lqt.totalSupply>       S => S'    </lqt.totalSupply>
requires 0 <Int X  andBool X  ==Int B  +Int Sends(Op ;; Ops)
 andBool 0 <Int T  andBool T  <=Int D  +Int Transfers(Op ;; Ops)
 andBool 0 <Int L  andBool L  ==Int S  +Int MintBurns(Op ;; Ops)
ensures  0 <Int X' andBool X' ==Int B' +Int Sends(Ops' ;; Ops)
 andBool 0 <Int T' andBool T' <=Int D' +Int Transfers(Ops' ;; Ops)
 andBool 0 <Int L' andBool L' ==Int S' +Int MintBurns(Ops' ;; Ops)

proof [inv-token-to-token]:
- let XtzBought        = 999 *Int TokensSold *Int X /Int (1000 *Int T +Int 999 *Int TokensSold)
      XtzBoughtNetBurn = XtzBought * 999 / 1000
- Sender =/=K DEXTER by [sender-is-not-dexter]
- apply [token-to-token]
  - Amount ==Int 0 by assert
- unify RHS
  - Ops' == ( [ Transaction DEXTER TOKEN        0                            Transfer(Sender, DEXTER, TokensSold) ] #as Op1 )
         ;; ( [ Transaction DEXTER OutputDexter XtzBoughtNetBurn             XtzToToken(To, MinTokensBought, Deadline) ] #as Op2 )
         ;; ( [ Transaction DEXTER NULL         XtzBought - XtzBoughtNetBurn Default() ] #as Op3 )
  - X' == X -Int XtzBought
  - T' == T +Int TokensSold
  - L' == L
  - B' == B
  - D' == D
  - S' == S
- XtzBought <Int X by simp
- X' >Int 0 by XtzBought <Int X
- T' >Int 0 by T >Int 0 and TokensSold >=Int 0
- L' >Int 0 by L >Int 0
- X' ==Int X -Int XtzBought
     ==Int B +Int Sends(Op ;; Ops) -Int XtzBought by premise
     ==Int B' +Int Sends(Op ;; Ops) -Int XtzBought by B'
     ==Int B' +Int Sends(Ops) -Int XtzBought by Sends and Amount ==Int 0
     ==Int B' +Int Sends(Op3 ;; Ops) -Int XtzBoughtNetBurn  by Sends
     ==Int B' +Int Sends(Op2 ;; Op3 ;; Ops) by Sends
     ==Int B' +Int Sends(Op1 ;; Op2 ;; Op3 ;; Ops) by Sends
     ==Int B' +Int Sends(Ops' ;; Ops) by Ops'
- T' ==Int T +Int TokensSold
     <=Int D +Int Transfers(Op ;; Ops) +Int TokensSold by premise
     ==Int D' +Int Transfers(Op ;; Ops) +Int TokensSold by D'
     ==Int D' +Int Transfers(Ops) +Int TokensSold by Transfers
     ==Int D' +Int Transfers(Op3 ;; Ops) +Int TokensSold by Transfers
     ==Int D' +Int Transfers(Op2 ;; Op3 ;; Ops) +Int TokensSold by Transfers
     ==Int D' +Int Transfers(Op1 ;; Op2 ;; Op3 ;; Ops) by Transfers and Sender =/=K DEXTER
     ==Int D' +Int Transfers(Ops' ;; Ops) by Ops'
- L' ==Int L
     ==Int S +Int MintBurns(Op ;; Ops) by premise
     ==Int S' +Int MintBurns(Op ;; Ops) by S'
     ==Int S' +Int MintBurns(Ops) by MintBurns
     ==Int S' +Int MintBurns(Op3 ;; Ops) by MintBurns
     ==Int S' +Int MintBurns(Op2 ;; Op3 ;; Ops) by MintBurns
     ==Int S' +Int MintBurns(Op1 ;; Op2 ;; Op3 ;; Ops) by MintBurns
     ==Int S' +Int MintBurns(Ops' ;; Ops) by Ops'
```

#### Default

```
claim [inv-default]:
<operations>  ( [ Transaction Sender DEXTER Amount Default() ] #as Op => Ops' ) ;; Ops </operations>
<xtzPool>        #Mutez(X => X')   </xtzPool>
<tokenPool>             T => T'    </tokenPool>
<lqtTotal>              L => L'    </lqtTotal>
<dexter.balance> #Mutez(B => B')   </dexter.balance>
<tokenDexter>           D => D'    </tokenDexter>
<lqt.totalSupply>       S => S'    </lqt.totalSupply>
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

### Proof for External Contract Calls

We prove the claim `[inv]` for any external contract calls from Dexter.

Since external contracts are unknown and arbitrary, we need to make certain assumptions on the behavior of external contracts that are required for the functional correctness and security of the Dexter contract.  The specific assumptions we made are presented later in this document.  The invariant proof is based on the assumptions, and it is important to independently verify that the token and liquidity contract implementations satisfy the assumptions.

There exist different types of external calls made by Dexter as follows:
- Simply send XTZ to others
- Call another Dexter contract's XtzToToken()
- Call the token contract's Transfer()
- Call the liquidity contract's MintOrBurn()

#### XTZ Transfers

The following claim formulates the invariant for the first two types of external calls.

```
claim [inv-send]:
<operations>  ( [ Transaction DEXTER Target Amount CallParams ] #as Op => Ops' ) ;; Ops </operations>
<xtzPool>        #Mutez(X => X')   </xtzPool>
<tokenPool>             T => T'    </tokenPool>
<lqtTotal>              L => L'    </lqtTotal>
<dexter.balance> #Mutez(B => B')   </dexter.balance>
<tokenDexter>           D => D'    </tokenDexter>
<lqt.totalSupply>       S => S'    </lqt.totalSupply>
requires Target =/=K DEXTER
 andBool ( CallParams ==K Default() orBool CallParams ==K (XtzToToken _) )
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

#### Token Transfers

The following claim is for the token contract call Transfer().

```
claim [inv-token-transfer]:
<operations>  ( [ Transaction _ TOKEN Amount Transfer(From, To, Value) ] #as Op => Ops' ) ;; Ops </operations>
<xtzPool>        #Mutez(X => X')   </xtzPool>
<tokenPool>             T => T'    </tokenPool>
<lqtTotal>              L => L'    </lqtTotal>
<dexter.balance> #Mutez(B => B')   </dexter.balance>
<tokenDexter>           D => D'    </tokenDexter>
<lqt.totalSupply>       S => S'    </lqt.totalSupply>
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
  - D' == #if     From == DEXTER and To <> DEXTER #then D -Int Value
          #elseif From <> DEXTER and To == DEXTER #then D +Int Value
          #else                                         D
          #fi
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

#### Liquidity Mints and Burns

The following claim is for the liquidity token contract call MintorBurn().

```
claim [inv-lqt-mint-burn]:
<operations>  ( [ Transaction Sender LQT Amount MintOrBurn(_, Value) ] #as Op => .List ) ;; Ops </operations>
<xtzPool>        #Mutez(X => X')   </xtzPool>
<tokenPool>             T => T'    </tokenPool>
<lqtTotal>              L => L'    </lqtTotal>
<dexter.balance> #Mutez(B => B')   </dexter.balance>
<tokenDexter>           D => D'    </tokenDexter>
<lqt.totalSupply>       S => S'    </lqt.totalSupply>
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
  - S' == S +Int Value
- X' >Int 0 by X >Int 0
- T' >Int 0 by T >Int 0
- L' >Int 0 by L >Int 0
- X' ==Int X
     ==Int B +Int Sends(Op ;; Ops) by premise
     ==Int B' +Int Sends(Op ;; Ops) by B'
     ==Int B' +Int Sends(Ops) by Sends and Amount ==Int 0
- T' ==Int T
     <=Int D +Int Transfers(Op ;; Ops) by premise
     ==Int D' +Int Transfers(Op ;; Ops) by D'
     ==Int D' +Int Transfers(Ops) by Transfers
- L' ==Int L
     ==Int S +Int MintBurns(Op ;; Ops) by premise
     ==Int (S' -Int Value) +Int MintBurns(Op ;; Ops) by S'
     ==Int (S' -Int Value) +Int (MintBurns(Ops) +Int Value) by MintBurns
     ==Int S' +Int MintBurns(Ops) by simp
```

### Abstract Behaviors of Dexter Entrypoints

We formulate the behavior of each Dexter entrypoint over an abstract configuration.  This formulation over the abstract configuration has been refined to the concrete configuration of the Michelson semantics, and then verified against the compiled bytecode of Dexter using the K framework.  The soundness of the refinement, which is currently in our trust base, allows us to conclude that the proved properties also hold for the Dexter bytecode.

#### Abstract Configuration

The abstract configuration consists of the following components (called "cells" in the K framework):
- `<operations>`: the sequence of operations to be executed
- `<xtzPool>`: the XtzPool state variable
- `<tokenPool>`: the TokenPool state variable
- `<lqtTotal>`: the LqtTotal state variable
- `<dexter.balance>`: the XTZ balance of Dexter
- `<tokenDexter>`: the token balance of Dexter (stored in the token contract storage)
- `<lqt.totalSupply>`: the total liquidity supply (stored in the liquidity contract storage)
- `<sourceaddr>`: the source of the current operation
- `<manager>`: the manager account address
- `<lqtAddress>`: the liquidity contract address
- `<freezeBaker>`: the FreezeBaker lock
- `<mynow>`: the current timestamp

Note that both `<xtzPool>` and `<dexter.balance>` are of the XTZ currency type, ranging from 0 to `2^64 - 1`.  Throughout this document, we implicitly assume that they are _defined_ only when their values are within the valid range, otherwise they are undefined, meaning that any execution involving undefined currency values will fail or revert.

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
[[ IS_VALID(Deadline) => Now <Int Deadline ]]
<mynow> #Timestamp(Now) </mynow>
```

#### AddLiquidity

The following rule formulates the behaviors of the AddLiquidity() entrypoint.  It states that, when it succeeds, the entrypoint execution updates the three state variables and the XTZ balance of Dexter.  The execution succeeds when the `assert` conditions are satisfied, and fails otherwise.

```
rule [add-liquidity]:
<operations>  ( [ Transaction Sender DEXTER XtzDeposited AddLiquidity(Owner, MinLqtMinted, MaxTokensDeposited, Deadline) ]
             => OpsEmitted
              ) ;; _
</operations>
<xtzPool>        #Mutez(X => X +Int XtzDeposited)      </xtzPool>
<tokenPool>             T => T +Int TokensDeposited    </tokenPool>
<lqtTotal>              L => L +Int LqtMinted          </lqtTotal>
<dexter.balance> #Mutez(B => B')                       </dexter.balance>
<tokenDexter>           D                              </tokenDexter>
<lqt.totalSupply>       S                              </lqt.totalSupply>
assert   IS_VALID(Deadline)
 andBool TokensDeposited <=Int MaxTokensDeposited
 andBool LqtMinted       >=Int MinLqtMinted
ensures  TokensDeposited ==Int XtzDeposited *Int T up/Int X
 andBool LqtMinted       ==Int XtzDeposited *Int L   /Int X
 andBool OpsEmitted ==K [ Transaction DEXTER TOKEN 0 Transfer(Sender, DEXTER, TokensDeposited) ]
                     ;; [ Transaction DEXTER LQT   0 MintOrBurn(Owner, LqtMinted) ]
 andBool Sender =/=K DEXTER impliesBool B' ==Int B +Int XtzDeposited
 andBool Sender  ==K DEXTER impliesBool B' ==Int B
```

#### RemoveLiquidity

The following rule formulates the behaviors of the RemoveLiquidity entrypoint.  As in the previous formulation, this specifies both success and failure cases depending on the satisfiability of the assertion conditions.

Note that the assertion explicitly includes the condition `LqtBurned <Int L` that requires the liquidity amount to be burned should be _strictly_ less than the total liquidity supply.  This condition is required for the functional correctness of Dexter, as already mentioned in the Dexter source code comment.  However, the current Dexter implementation does _not_ check the condition, leaving the potential for Dexter becoming nonfunctional by mistake or corrupted admin users.  _**It is strongly recommended to add an explicit input validation for LqtBurned.**_

```
rule [remove-liquidity]:
<operations>  ( [ Transaction Sender DEXTER Amount RemoveLiquidity(To, LqtBurned, MinXtzWithdrawn, MinTokensWithdrawn, Deadline) ]
          => OpsEmitted ) ;; _
</operations>
<xtzPool>        #Mutez(X => X -Int XtzWithdrawn)      </xtzPool>
<tokenPool>             T => T -Int TokensWithdrawn    </tokenPool>
<lqtTotal>              L => L -Int LqtBurned          </lqtTotal>
<dexter.balance> #Mutez(B)                             </dexter.balance>
<tokenDexter>           D                              </tokenDexter>
<lqt.totalSupply>       S                              </lqt.totalSupply>
assert   IS_VALID(Deadline)
 andBool Amount ==Int 0
 andBool LqtBurned <Int L
 andBool XtzWithdrawn    >=Int MinXtzWithdrawn
 andBool TokensWithdrawn >=Int MinTokensWithdrawn
ensures  XtzWithdrawn    ==Int LqtBurned *Int X /Int L
 andBool TokensWithdrawn ==Int LqtBurned *Int T /Int L
 andBool OpsEmitted ==K [ Transaction DEXTER LQT   0            MintOrBurn(Sender, LqtBurned) ]
                     ;; [ Transaction DEXTER TOKEN 0            Transfer(DEXTER, To, TokensWithdrawn) ]
                     ;; [ Transaction DEXTER To    XtzWithdrawn Default() ]
```

#### XtzToToken

```
rule [xtz-to-token]:
<operations>  ( [ Transaction Sender DEXTER XtzSold XtzToToken(To, MinTokensBought, Deadline) ] => OpsEmitted )
           ;; _
</operations>
<xtzPool>        #Mutez(X => X +Int XtzSoldNetBurn) </xtzPool>
<tokenPool>             T => T -Int TokensBought    </tokenPool>
<lqtTotal>              L                           </lqtTotal>
<dexter.balance> #Mutez(B => B')                    </dexter.balance>
<tokenDexter>           D                           </tokenDexter>
<lqt.totalSupply>       S                           </lqt.totalSupply>
assert   IS_VALID(Deadline)
 andBool TokensBought >=Int MinTokensBought
ensures  XtzSoldNetBurn ==Int XtzSold *Int 999 /Int 1000
 andBool TokensBought ==Int 999 *Int XtzSoldNetBurn *Int T /Int (1000 *Int X +Int 999 *Int XtzSoldNetBurn)
 andBool OpsEmitted ==K [ Transaction DEXTER TOKEN 0                           Transfer(DEXTER, To, TokensBought) ]
                     ;; [ Transaction DEXTER NULL  XtzSold -Int XtzSoldNetBurn Default() ]
 andBool Sender =/=K DEXTER impliesBool B' ==Int B +Int XtzSold
 andBool Sender  ==K DEXTER impliesBool B' ==Int B
```

#### TokenToXtz

```
rule [token-to-xtz]:
<operations>  ( [ Transaction Sender DEXTER Amount TokenToXtz(To, TokensSold, MinXtzBought, Deadline) ] => OpsEmitted )
          ;; _
</operations>
<xtzPool>        #Mutez(X => X -Int XtzBought)     </xtzPool>
<tokenPool>             T => T +Int TokensSold     </tokenPool>
<lqtTotal>              L                          </lqtTotal>
<dexter.balance> #Mutez(B)                         </dexter.balance>
<tokenDexter>           D                          </tokenDexter>
<lqt.totalSupply>       S                          </lqt.totalSupply>
assert   IS_VALID(Deadline)
 andBool Amount ==Int 0
 andBool XtzBought >=Int MinXtzBought
ensures  XtzBought ==Int 999 *Int TokensSold *Int X /Int (1000 *Int T +Int 999 *Int TokensSold)
 andBool XtzBoughtNetBurn ==Int XtzBought *Int 999 /Int 1000
 andBool OpsEmitted ==K [ Transaction DEXTER TOKEN 0                            Transfer(Sender, DEXTER, TokensSold) ]
                     ;; [ Transaction DEXTER To    XtzBoughtNetBurn             Default() ]
                     ;; [ Transaction DEXTER NULL  XtzBought - XtzBoughtNetBurn Default() ]
```

#### TokenToToken

Note that it is straightforward to prove the equivalence between the following two methods for the token-to-token exchange:

- Alice sends only a single transaction to Dexter:  
  `transaction Alice DEXTER 0 TokenToToken(OutputDexterContract, MinTokensBought, To, TokensSold, Deadline)`.
- Alice first sends a transaction to Dexter:  
  `Transaction Alice DEXTER 0 TokenToXtz(Alice, TokensSold, 0, Deadline)`, and then immediately sends another transaction to OutputDexterContract, `Transaction Alice OutputDexterContract XtzBought XtzToToken(To, MinTokensBought, Deadline)`, where XtzBought is the amount she received from the first transaction, provided that no transactions have been made to OutputDexterContract between the two transactions.

```
rule [token-to-token]:
<operations>  ( [ Transaction Sender DEXTER Amount TokenToToken(OutputDexterContract, MinTokensBought, To, TokensSold, Deadline) ]
             => OpsEmitted ) ;; _
</operations>
<xtzPool>        #Mutez(X => X -Int XtzBought)     </xtzPool>
<tokenPool>             T => T +Int TokensSold     </tokenPool>
<lqtTotal>              L                          </lqtTotal>
<dexter.balance> #Mutez(B)                         </dexter.balance>
<tokenDexter>           D                          </tokenDexter>
<lqt.totalSupply>       S                          </lqt.totalSupply>
assert   IS_VALID(Deadline)
 andBool Amount ==Int 0
ensures  XtzBought ==Int 999 *Int TokensSold *Int X /Int (1000 *Int T +Int 999 *Int TokensSold)
 andBool XtzBoughtNetBurn ==Int XtzBought *Int 999 /Int 1000
 andBool OpsEmitted ==K [ Transaction DEXTER TOKEN 0  Transfer(Sender, DEXTER, TokensSold) ]
                     ;; [ Transaction DEXTER OutputDexterContract XtzBoughtNetBurn XtzToToken(To, MinTokensBought, Deadline) ]
                     ;; [ Transaction DEXTER NULL XtzBought - XtzBoughtNetBurn Default() ]
```

#### Default

```
rule [default]:
<operations>  ( [ Transaction Sender DEXTER Amount Default() ] => .List ) ;; _ </operations>
<xtzPool>        #Mutez(X => X +Int Amount)    </xtzPool>
<tokenPool>             T                      </tokenPool>
<lqtTotal>              L                      </lqtTotal>
<dexter.balance> #Mutez(B => B')               </dexter.balance>
<tokenDexter>           D                      </tokenDexter>
<lqt.totalSupply>       S                      </lqt.totalSupply>
ensures  Sender =/=K DEXTER impliesBool B' ==Int B +Int Amount
 andBool Sender  ==K DEXTER impliesBool B' ==Int B
```

## Liquidity Share Value Security

The property `[inv]` states the relationship between the Dexter state variables and the actual pool reserves and liquidity supply.  Now we formulate another property regarding the relationship over the Dexter state variables themselves.

Let XtzPool, TokenPool, and LqtTotal be the current value of the Dexter state variables.  Suppose that an operation updates the state variables to new values, say, XtzPool', TokenPool', and LqtTotal', respectively.  Then, for any (successful) execution of an arbitrary operation, we must have:
```
  XtzPool' * TokenPool'        LqtTotal'
  ---------------------  >=  ( -------- )^2
  XtzPool  * TokenPool         LqtTotal
```
where the division is the real arithmetic division (i.e., no rounding).

Note that the above property (together with the `[inv]` property) says that the liquidity share price (i.e., the multiplication of the amounts of XTZ and tokens to be redeemed per unit liquidity) _never_ decreases.  Intuitively, this implies the following desired properties:
- When adding liquidity, users _cannot_ mint more liquidity shares than they should.
- When removing liquidity, users _cannot_ redeem more assets than they should.
- When exchanging tokens, users _cannot_ receive more XTZ or tokens than they should.

(Note that, however, this property has _nothing_ to do with the _USD value_ of the liquidity share.  Indeed, the USD value of the liquidity share could decrease due to the so-called "Impermanent Loss" problem.)

The following claim `[pool]` formulates the liquidity share price property.

```
claim [pool]:
<operations> (Op => _) ;; _ </operations>
<xtzPool>     #Mutez(X => X')   </xtzPool>
<tokenPool>          T => T'    </tokenPool>
<lqtTotal>           L => L'    </lqtTotal>
requires X >Int 0 andBool T >Int 0 andBool L >Int 0
ensures  (X' *Int T') /Real (X *Int T) >=Real (L' /Real L) ^Real 2
```

```
proof [pool]:
- let Op = Transaction _ Target Amount CallParams
- split Target
  - case Target == DEXTER
    - split CallParams
      - case CallParams == AddLiquidity _
        - apply [add-liquidity]
        - unify RHS
          - X' == X +Int XtzDeposited
          - T' == T +Int TokensDeposited
          - L' == L +Int LqtMinted
          - XtzDeposited    == Amount
          - TokensDeposited == XtzDeposited *Int T up/Int X
          - LqtMinted       == XtzDeposited *Int L   /Int X
        - let TokensDepositedReal = XtzDeposited *Int T /Real X
        - let LqtMintedReal       = XtzDeposited *Int L /Real X
        - TokensDeposited >=Real TokensDepositedReal
        - LqtMinted       <=Real LqtMintedReal
        - (X' *Int T') /Real (X *Int T)
             ==Real ((X +Int XtzDeposited) *Int  (T +Int  TokensDeposited    )) /Real (X *Int T) by X' and T'
             >=Real ((X +Int XtzDeposited) *Real (T +Real TokensDepositedReal)) /Real (X *Int T)
                         by TokensDeposited >=Real TokensDepositedReal
             ==Real (1 +Real XtzDeposited /Real X) *Real (1 +Real XtzDeposited /Real X) by simp(Real)
             ==Real (1 +Real XtzDeposited /Real X) ^Real 2 by simp(Real)
             ==Real ((L +Real LqtMintedReal) /Real L) ^Real 2 by simp(Real)
             >=Real ((L +Real LqtMinted    ) /Real L) ^Real 2 by LqtMinted <=Real LqtMintedReal
             ==Real (L' /Real L) ^Real 2 by L'
      - case CallParams == RemoveLiquidity(_, LqtBurned, _, _, _)
        - apply [remove-liquidity]
        - unify RHS
          - X' == X -Int XtzWithdrawn
          - T' == T -Int TokensWithdrawn
          - L' == L -Int LqtBurned
          - XtzWithdrawn    == LqtBurned *Int X /Int L
          - TokensWithdrawn == LqtBurned *Int T /Int L
        - let XtzWithdrawnReal    = LqtBurned *Int X /Real L
        - let TokensWithdrawnReal = LqtBurned *Int T /Real L
        - XtzWithdrawn    <=Real XtzWithdrawnReal
        - TokensWithdrawn <=Real TokensWithdrawnReal
        - (X' *Int T') /Real (X *Int T)
            ==Real ((X -Int  XtzWithdrawn    ) *Int  (T -Int  TokensWithdrawn    )) /Real (X *Int T) by X' and T'
            >=Real ((X -Real XtzWithdrawnReal) *Real (T -Real TokensWithdrawnReal)) /Real (X *Int T)
                        by XtzWithdrawn <=Real XtzWithdrawnReal and TokensWithdrawn <=Real TokensWithdrawnReal
            ==Real (1 -Real LqtBurned /Real L) *Real (1 -Real LqtBurned /Real L) by simp(Real)
            ==Real (1 -Real LqtBurned /Real L) ^Real 2 by simp(Real)
            ==Real ((L -Real LqtBurned) /Real L) ^Real 2 by simp(Real)
            ==Real (L' /Real L) ^Real 2 by L'
      - case CallParams == XtzToToken _
        - apply [xtz-to-token]
        - unify RHS
          - X' == X +Int XtzSold
          - T' == T -Int TokensBought
          - L' == L
          - XtzSold == Amount
          - XtzSoldNetBurn == XtzSold *Int 999 /Int 1000
          - TokensBought == 999 *Int XtzSoldNetBurn *Int T /Int (1000 *Int X +Int 999 *Int XtzSoldNetBurn)
        - let XtzSoldNetBurnReal == XtzSold *Int 999 /Real 1000
              TokensBoughtReal = 999 *Real XtzSoldNetBurnReal *Real T /Real (1000 *Real X +Real 999 *Real XtzSoldNetBurnReal)
        - TokensBought <=Real TokensBoughtReal
                    by monotonicity of the exchange rate function
                    and the fact that the integer-version is less than the real valued version
        - (X' *Int T') /Real (X *Int T)
            ==Real ((X +Int XtzSold) *Int  (T -Int  TokensBought    )) /Real (X *Int T) by X' and T'
            >=Real ((X +Int XtzSold) *Real (T -Real TokensBoughtReal)) /Real (X *Int T) by TokensBought <=Real TokensBoughtReal
            ==Real (X +Real XtzSold) /Real (X +Real 0.999 *Real XtzSold) by simp(Real)
            >=Real 1 by simp(Real)
            ==Real (L' /Real L) ^Real 2 by L' == L
      - case CallParams == TokenToXtz(_, TokensSold, _, _) | CallParams == TokenToToken(_, _, _, TokensSold, _)
        - apply [token-to-xtz] or [token-to-token]
        - unify RHS
          - X' == X -Int XtzBought
          - T' == T +Int TokensSold
          - L' == L
          - XtzBought == 999 *Int TokensSold *Int X /Int (1000 *Int T +Int 999 *Int TokensSold)
        - let XtzBoughtReal = 999 *Int TokensSold *Int X /Real (1000 *Int T +Int 999 *Int TokensSold)
        - XtzBought <=Real XtzBoughtReal
        - (X' *Int T') /Real (X *Int T)
            ==Real ((X -Int  XtzBought    ) *Int  (T +Int TokensSold)) /Real (X *Int T) by X' and T'
            >=Real ((X -Real XtzBoughtReal) *Real (T +Int TokensSold)) /Real (X *Int T) by XtzBought <=Real XtzBoughtReal
            ==Real (T +Real TokensSold) /Real (T +Real 0.999 *Real TokensSold) by simp(Real)
            >=Real 1 by simp(Real)
            ==Real (L' /Real L) ^Real 2 by L' == L
      - case CallParams == Default _
        - apply [default]
        - unify RHS
          - X' == X +Int Amount
          - T' == T
          - L' == L
        - (X' *Int T') /Real (X *Int T) ==Real ((X +Int Amount) *Int T) /Real (X *Int T) by X' and T'
                                        >=Real 1 by Amount >=Int 0
                                        ==Real (L' /Real L) ^Real 2 by L' == L
  - case Target <> DEXTER
    - (X', T', L') == (X, T, L) by [only-dexter]
    - (X' *Int T') /Real (X *Int T) ==Real 1 by X' == X and T' == T
                                    ==Real (L' /Real L) ^Real 2 by L' == L
```

## Operation Safety

In this section, we discuss the operation safety property a bit further.
Recall our definition of operation safety: all trades and liquidity redemptions/deposits have a bounded exchange rate and time in which they are applicable.
To see if this holds, we need only look at the five entrypoint specifications relating to these operations.
For convenience, we reiterate them below:

-   `AddLiquidity(owner, minLqtMinted, maxTokensDeposited, deadline)`: `amount -> maxTokenDeposited, minLiquidityMinted`
-   `RemoveLiquidity(to, lqtBurned, minXtzWithdrawn, minTokensWithdrawn, deadline)`: `lqtBurned -> minXtzWithdrawn, minTokensWithDrawn`
-   `XtzToToken(to, minTokensBought, deadline)`: `amount -> minTokensBought`
-   `TokenToXtz(to, tokensSold, minXtzBought, deadline)`: `tokensSold -> minXtzBought`
-   `TokenToToken(outputDexterContract, minTokensBought, to, tokensSold, deadline)`: `tokensSold -> minTokensBought`

To see that each operation has a bounded time in which it is valid, it is enough to check that its functional spec asserts that its _deadline_ has not passed; this is clearly the case.
To see that each operation has a bounded exchange rate in which it is valid, it is enough for each operation to specify the worst possible exchange rate the sender is willing to accept.
This is only slightly more complicated to show, because the kind of exchange depends on the entrypoint called.
Next to each entrypoint above, we used the notation `input -> (min/max)bound1 [, (max/min)bound2]` (with the bracketed partial optional) which represents the input and worst-case bounds on the exchange rate.
The output prefix `min` describes the minimum amount a sender is willing to receive in exchange for sending assets.
The output prefix `max` describes the maximum amount a sender is required to _additionally_ send in exchange for receiving assets.
All that remains to be done is correlate these output bounds with the actual movement of assets.
Recall that the LB system manages three kinds of assets: Tez, tzBTC, and liquidity tokens (LT).
Each of these assets can be sent to LB and received from LB.
To send these assets to LB, the transaction must have the form (where `Sender` refers to entity sending assets to Dexter):

-   Tez - the `Amount` argument in a transaction of the form `[ Transaction Sender DEXTER Amount _ ]`
-   tzBTC - the `Tokens` argument in a transaction of the form `[ Transaction DEXTER TOKEN 0 Transfer(Sender, DEXTER, Tokens) ]`
-   LT - the `Lt` argument in a transaction of the form `[ Transaction DEXTER LQT mintOrBurn(Sender, Lt) ]` with `Lt` negative

To receive these assets from LB, the transaction must have the form (where `Receiver` refers to the entity expected to receive assets from Dexter):

-   Tez - the `Amount` argument in a transaction of the form `[ Transaction DEXTER Receiver Amount Default() ]`
-   tzBTC - the `Tokens` argument in a transaction of the form `[ Transaction DEXTER TOKEN 0 Transfer(DEXTER, Receiver, Tokens) ]`
-   LT - the `Lt` argument in a transaction of the form `[ Transaction DEXTER LQT mintOrBurn(Receiver, Lt) ]` with `Lt` positive
-   other assets - when using `TokenToToken`, the `MinTokensBought` argument in a transaction of the form  
    `[ Transaction DEXTER outputDexterContract _ XtzToToken(Receiver, MinTokensBought, _) ]`

Checking that each entrypoint properly bounds its worst-case exchange rates amounts to checking that:

-   all bounds above are properly applied to all Dexter emitted transactions of the above forms (excepting transactions of the form `[ Transaction DEXTER NULL Amount Default() ]`, i.e., burned fees).

This property immediately follows by our functional correctness specifications.
Note that we must assume that `outputDexterContract` is, in fact, a valid dexter contract.
Otherwise, its `XtzToToken` entrypoint may _not_ satisfy the same bounds guarantees that we have listed above.

### Bounds on fees

Note that the calculations in this section discount rouding due to integer division.

### Tokens to XTZ

When converting from XTZ to Tokens and vice-verca two fees are charges,
the first is sent to the liquity pool increasing the value of each share;
the second is destroyed by sending it to the null address.

Ideally, in a CPMM a user would expect the following exchange rate for selling tokens:

```
expected_xtz_recieved = (TokensSold * XtzPool)
                        ------------------------
                        (TokensSold + TokenPool)
```

However, an amount dependant on the size of the token and xtz pools is kept with
the contract:

```
currency_bought = (TokensSold * 999 * XtzPool)
                  ---------------------------------------
                  (TokenPool * 1000 + (TokensSold * 999))
```

A further 0.1% of this amount is burnt by sending it to the null address, and the user receives:

```
xtz_recieved = (TokensSold * 999 * XtzPool)          *  999
               -----------------------------------------------------
               (TokensSold * 999 + TokenPool * 1000) * 1000
```

The total fee charged is given by this expression:

```
fee =  (TokensSold * XtzPool)     (TokensSold * 999 * XtzPool)          *  999
       ------------------------ - --------------------------------------------
       (TokensSold + TokenPool)   (TokensSold * 999 + TokenPool * 1000) * 1000
```

As fraction of the expected xtz this becomes:

```
xtz_fee_percent =      (TokensSold * 999 * XtzPool)          *  999       (TokensSold + TokenPool)
                   1 - --------------------------------------------   *   ------------------------
                       (TokensSold * 999 + TokenPool * 1000) * 1000       (TokensSold * XtzPool)

                =      999 * 999 * (TokensSold + TokenPool)
                   1 - --------------------------------------------
                       (TokensSold * 999 + TokenPool * 1000) * 1000
```

The fee as a ratio thus depends on the relative values of `TokensSold` and `TokenPool`.
When `TokensSold == TokenPool`, we get the minimum fee of:

```
min_fee_charged  =      2 * 999 * 999
                     1 - -------------
                         1999 * 1000

                  = 0.00149974987

                 ~= 0.15%
```

As the size of the token pool increases, this fee increases asymptotically to:

```
max  =      999 * 999 * (0 + TokenPool)
        1 - ------------------------------
            ( 0 + TokenPool * 1000) * 1000

     =      999 * 999
        1 - -----------
            1000 * 1000

     = 0.001999

     = 0.1999%
```

### XTZ to Tokens

Similarly, when exchanging tokens for XTZ,
one-thousandth of the amount is burnt before charging a fee:

```
tokens_received  =  (Amount * 999 / 1000) * 999 * TokenPool
                    -----------------------------------------------
                    (XtzPool * 1000) + (Amount * 999  / 1000) * 999

                 =  999 * 999 * Amount * TokenPool
                    ------------------------------------------
                    XtzPool * 1000 * 1000 + Amount * 999 * 999
```

while, in an ideal CPMM a user would expect:

```
tokens_expected  =  Amount * TokenPool
                    ------------------
                    XtzPool + Amount
```

The fee charged is thus:

```
fee = Amount * TokenPool     999 * 999 * Amount * TokenPool
      ------------------  -  ------------------------------------------
      XtzPool + Amount       XtzPool * 1000 * 1000 + Amount * 999 * 999
```

As a percentage, this becomes:

```
fee =        999 * 999 * Amount * TokenPool               XtzPool + Amount
       1  -  ------------------------------------------ * ------------------
             XtzPool * 1000 * 1000 + Amount * 999 * 999   Amount * TokenPool

    =        999 * 999 * (XtzPool + Amount)
       1  -  ------------------------------------------
             XtzPool * 1000 * 1000 + Amount * 999 * 999
```

The fee depends on the ratio of `XtzPool` to `Amount`
The fee tends to 0 when the limit of `XtzPool` is 0 -- i.e. when `Amount` is large compared to `XtzPool`.

```
min_fee_charged =        999 * 999 * Amount
                   1  -  ------------------
                         Amount * 999 * 999

                = 0
```

The fee increases as `Amount` becomes small compared to `XtzPool` with the limit becoming approximately 0.2% in the limiting case:

```
max_fee_charged =        999 * 999 * XtzPool
                   1  -  ------------------
                         XtzPool * 1000 * 1000

                =  1  -  0.998001

                =  0.1999 %
```
