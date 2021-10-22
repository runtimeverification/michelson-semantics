# LQT Verification

## Prologue

Our verification subject is the Michelson code corresponding to the LIGO [LQT contract](https://gitlab.com/dexter2tz/dexter2tz/-/tree/liquidity_baking) at [commit id d9864388](https://gitlab.com/dexter2tz/dexter2tz/-/tree/d98643881fe14996803997f1283e84ebd2067e35).

The goal of this project is to produce:

-   a series of proofs which specify that the intended behavior of each individual LIGO function is correct (which implies that the LIGO-to-Michelson compilation process is also correct in this case)
-   a series of proofs which demonstate high-level invariants over sequences of contract calls hold (e.g. it is not possible to produce a profit by exploiting rounding errors)

In this project, we will model the entrypoints in the LQT Token contract code and extract their high-level properties.
Note that we will base our high-level descriptions of each function of the LIGO contract code,
Lqthile our verification will be performed at the the level of the compiled Michelson versions.

We begin start our verification project by opening a new module context in which our verification will be performed.

```k
requires "../lemmas.md"
requires "lqt-compiled.md"

module LQT-TOKEN-VERIFICATION-SYNTAX
  imports MICHELSON-INTERNAL-SYNTAX
```

```k
endmodule
```

## LQT Token Lemmas

```k
module LQT-TOKEN-LEMMAS
  imports MICHELSON
```

```k
  rule X /Int 1 => X [simplification]
  rule X *Int 1 => X [simplification]

  rule X +Int (0 -Int Y) => X -Int Y [simplification]
  rule X -Int  0         => X        [simplification]
  rule X +Int  0         => X        [simplification]
```

```k
endmodule
```

```k
module LQT-TOKEN-VERIFICATION
  imports LQT-TOKEN-COMPILED
  imports LQT-TOKEN-VERIFICATION-SYNTAX
  imports LQT-TOKEN-LEMMAS
```

## Terminology Prerequisites

### Entrypoints

Note that we are slightly abusing language in the preceding sentence because Michelson has no native notion of separate contract function entrypoints.
Instead, different entrypoints are modelled by making the global contract input parameter a disjoint union of the parameter types for each possible entrypoint, and then dispatching to different code blocks based on which entry of the disjoint union was selected.
Due to Michelon's design, this means that each entrypoint can be understood to have the following typing:

```
entrypoint_input_type * storage_type -> (operation list) * storage_type
```

where:

1.  each `entrypoint_input_type` is a member of the disjoint union which is the global contract parameter type;
2.  each `operation` is a callback to be placed in a FIFO queue of callbacks that are executed once the current contract execution terminates.

By abuse of notation, we identify an entrypoint's typing rule with its corresponding `entrypoint_input_type`, since the other parts of the typing rule are constant.
We may abbreviate `entrypoint_input_type` to just `input` when it is clear from context.

### Storage Type

In our proofs, we will use the following abstract representation of the LQT contract storage state.

Calling functions produces not only storage changes but also a list of callbacks.
We serialize these to the `<operations>` cell for ease of writing specifications.

```k
configuration <dexterTop>
                <michelsonTop/>
                <storage>
                  <tokens> .Map </tokens>
                  <allowances> .Map </allowances>
                  <adminAddress> #Address("") </adminAddress>
                  <totalSupply> 0:Int </totalSupply>
                </storage>
                <operations> .InternalList </operations>
              </dexterTop>
```

## Entrypoint Summaries

We define our list of entrypoints below.
Each entrypoint is given a unique abstract parameter type that we use to simplify our proof structure,
as well as operations to serialize and deserialize the parameter.

```k
  syntax EntryPointParams

  syntax Bool ::= wellTypedParams(EntryPointParams) [function, functional]
  syntax Data ::= #loadLqtParams(EntryPointParams)  [function, functional]
```

### GetTotalSupply

```k
  syntax EntryPointParams ::= GetTotalSupplyParams(callback: Entrypoint)
  rule wellTypedParams(GetTotalSupplyParams(_)) => true [simplification, anywhere]
  rule #loadLqtParams(GetTotalSupplyParams(Callback)) => Left Right Right Pair Unit #Contract(Callback, nat)
```

### GetBalance

```k
  syntax EntryPointParams ::= GetBalanceParams(owner: Address, callback: Entrypoint)
  rule wellTypedParams(GetBalanceParams(_Address, _Callback)) => true [simplification, anywhere]
  rule #loadLqtParams(GetBalanceParams(Address, Callback)) => Left Right Left Pair Address #Contract(Callback, nat)
```

### GetAllowence

```k
  syntax EntryPointParams ::= GetAllowanceParams(owner: Address, spender: Address, callback: Entrypoint)
  rule wellTypedParams(GetAllowanceParams(_Owner, _Spender, _Callback)) => true [simplification, anywhere]
  rule #loadLqtParams(GetAllowanceParams(Owner, Spender, Callback)) => Left Left Right Pair (Pair Owner Spender) #Contract(Callback, nat)
```

### MintOrBurn

```k
  syntax EntryPointParams ::= MintOrBurnParams(quantity: Int, target: Address)
  rule wellTypedParams(MintOrBurnParams(_Quantity, _Target)) => true [simplification, anywhere]
  rule #loadLqtParams(MintOrBurnParams(Quantity, Target)) => Right Left Pair Quantity Target
```

### Approve

```k
  syntax EntryPointParams ::= ApproveParams(spender: Address, value: Int)
  rule wellTypedParams(ApproveParams(_Spender, Value)) => Value >=Int 0 [simplification, anywhere]
  rule #loadLqtParams(ApproveParams(Spender, Value)) => Left Left Left Pair Spender Value
```

### Transfer

```k
  syntax EntryPointParams ::= TransferParams(from: Address, to: Address, value: Int)
  rule wellTypedParams(TransferParams(_From, _To, Value)) => Value >=Int 0 [simplification, anywhere]
  rule #loadLqtParams(TransferParams(From, To, Value)) => Right Right Pair From Pair To Value
```

## State Abstraction

We use a few helper routines to convert between our abstract and concrete proof state.
We first define functions which build our parameter and our storage types.

```k
  syntax TypeName ::= #lqtParamType()  [function, functional]
  // --------------------------------------------------------------------
  rule #lqtParamType()
    => (or (or (or (pair (address) (nat))
                  (pair
                       (pair (address) (address))
                       (contract nat)))
               (or (pair (address) (contract nat))
                   (pair (unit) (contract nat))))
           (or (pair (int) (address))
               (pair (address) (pair (address) (nat)))))

  syntax TypeName ::= #lqtStorageType() [function, functional]
  // ---------------------------------------------------------------------
  rule #lqtStorageType()
    => (pair (big_map address nat)
             (pair (big_map (pair (address) (address)) nat)
                   (pair (address)
                         (nat))))
```

We also define a functions that serialize and deserialize our abstract parameters and state.

```k
  syntax KItem ::= #loadLqtState(EntryPointParams)
  // ---------------------------------------------------------
  rule <k> #loadLqtState(Params) => . ... </k>
       <stack> .Stack
            => [ pair #lqtParamType() #lqtStorageType()
                 Pair #loadLqtParams(Params)
                   Pair TokensMap
                     Pair AllowanceMap
                       Pair AdminAddress
                            TotalSupply ]
       </stack>
       <tokens> TokensMap </tokens>
       <allowances> AllowanceMap </allowances>
       <adminAddress> AdminAddress </adminAddress>
       <totalSupply> TotalSupply </totalSupply>

  syntax KItem ::= #storeLqtState()
  // ------------------------------------------
  rule <k> #storeLqtState() => . ... </k>
       <stack>  [ pair list operation _
                 Pair OpList
                   Pair TokensMap
                     Pair AllowanceMap
                       Pair AdminAddress
                            TotalSupply ]
            => .Stack
       </stack>
       <tokens> _ => TokensMap </tokens>
       <allowances> _ => AllowanceMap </allowances>
       <adminAddress> _ => AdminAddress </adminAddress>
       <totalSupply> _ => TotalSupply </totalSupply>
       <operations> _ => OpList </operations>
```

If the contract execution fails, storage is not updated.

```k
  rule <k> Aborted(_, _, _, _) ~> (#storeLqtState() => .) ... </k>
```

## Proof Helper Functions

```k
  syntax Map ::= #incrementTokens(Map, Address, quantity: Int) [function, functional]
  rule #incrementTokens(Tokens, Address, Quantity) => Tokens[ Address <- undef ]                                     requires         #tokensFor(Tokens, Address) +Int Quantity ==Int 0 [simplification, anywhere]
  rule #incrementTokens(Tokens, Address, Quantity) => Tokens[ Address <- #tokensFor(Tokens, Address) +Int Quantity ] requires notBool #tokensFor(Tokens, Address) +Int Quantity ==Int 0 [simplification, anywhere]

  syntax Int ::= #tokensFor(Map, owner: Address) [function, functional]
  rule #tokensFor(Tokens, Owner) => {Tokens[Owner]}:>Int requires         Owner in_keys(Tokens) [simplification, anywhere]
  rule #tokensFor(Tokens, Owner) => 0                    requires notBool Owner in_keys(Tokens) [simplification, anywhere]
```

```k
  syntax Pair ::= #allowanceKey(owner: Address, spender: Address)
// -----------------------------------------------------------------
  rule #allowanceKey(Owner, Spender) => (Pair Owner Spender) [macro]

  syntax Map ::= #updateAllowances(Map, owner: Address, spender: Address, newValue: Int) [function, functional]
// ------------------------------------------------------------------------------------------------------------
  rule #updateAllowances(Allowances, Owner, Spender, NewValue) => Allowances[ #allowanceKey(Owner, Spender) <- undef ]    requires         NewValue ==K 0 [simplification, anywhere]
  rule #updateAllowances(Allowances, Owner, Spender, NewValue) => Allowances[ #allowanceKey(Owner, Spender) <- NewValue ] requires notBool NewValue ==K 0 [simplification, anywhere]

  syntax Int ::= #allowanceFor(Map, owner: Address, spender: Address) [function, functional]
// -----------------------------------------------------------------------------------------
  rule #allowanceFor(Allowances, Owner, Spender) => {Allowances[ #allowanceKey(Owner, Spender) ]}:>Int requires         (Pair Owner Spender) in_keys(Allowances) [simplification, anywhere]
  rule [allowanceForZero] : #allowanceFor(Allowances, Owner, Spender) => 0                                                  requires notBool (Pair Owner Spender) in_keys(Allowances) [simplification, anywhere]
```

## Putting It All Together

All contract call specifications have common steps:

1. Load parameters and storage onto the stack.
2. Execute the LQT contract code.
3. Save the resulting storage.

If all steps are completed, only the LQT specific storage is updated.

```k
  syntax KItem ::= #runProof(EntryPointParams)
 // ------------------------------------------
  rule <k> #runProof(Params)
        => #loadLqtState(Params)
        ~> #lqtTokenCode
        ~> #storeLqtState()
        ...
       </k>
    ensures wellTypedParams(Params)
```

## Epilogue

We close out our module context now, which contains all of the information necessary to complete our proof.

```k
endmodule
```
